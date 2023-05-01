/*

`gui-tools` is free software: you can redistribute it and/or modify it under the terms of one of
the following licenses:

- The GNU Affero General Public License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.
- The Patron License at https://github.com/notgull/gui-tools/blob/main/LICENSE-PATRON.md, for
  sponsors and contributors, who can ignore the copyleft provisions of the GNU AGPL for this project.

`gui-tools` is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General
Public License and the Patron License for more details.

You should have received a copy of the GNU Affero General Public License and the corresponding Patron
License along with `gui-tools`. If not, see <https://www.gnu.org/licenses/>.

*/

//! A simple interface for creating GUIs in Rust.
//!
//! This crate acts as a collective interface to the functionality provided by other crates. The
//! crates involved are:
//!
//! - **Windowing** is provided by [`async-winit`].
//! - **Drawing** is provided by [`theo`].
//! - **Accessibility** is provided by [`accesskit`].
//!
//! None of these crates are publicly exposed, in order to prevent breaking changes in them from
//! breaking this crate. The only publicly exposed dependency is [`piet`].

use async_winit::dpi::{
    LogicalPosition, LogicalSize, PhysicalPosition, PhysicalSize, Position as WinitPosition,
    Size as WinitSize,
};
use async_winit::event_loop::{EventLoop, EventLoopBuilder, EventLoopWindowTarget};
use async_winit::window::{Window as WinitWindow, WindowBuilder as WinitWindowBuilder};

use std::cell::{Cell, RefCell};
use std::convert::Infallible;
use std::future::Future;
use std::marker::PhantomData;
use std::pin::Pin;
use std::rc::Rc;
use std::task::{Context, Poll};
use std::{fmt, mem};

use __private::EventSealed;

// Use kurbo as a public dependency here, since piet is public as well.
#[doc(inline)]
pub use kurbo::{Point, Rect, Size};

#[cfg(x11_platform)]
macro_rules! cfg_x11 {
    ($($i:item)*) => {
        $($i)*
    };
    ($($e:stmt)*) => {$($e)*};
}

#[cfg(not(x11_platform))]
macro_rules! cfg_x11 {
    ($($i:item)*) => {};
    ($($e:item)*) => {};
}

#[cfg(wayland_platform)]
macro_rules! cfg_wayland {
    ($($i:item)*) => {
        $($i)*
    };
}

#[cfg(not(wayland_platform))]
macro_rules! cfg_wayland {
    ($($i:item)*) => {};
}

#[cfg(target_os = "android")]
macro_rules! cfg_android {
    ($($i:item)*) => {
        $($i)*
    };
}

#[cfg(not(target_os = "android"))]
macro_rules! cfg_android {
    ($($i:item)*) => {};
}

#[derive(Debug)]
pub struct Error(Repr);

#[derive(Debug)]
enum Repr {
    OsError(async_winit::error::OsError),
    Piet(piet::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            Repr::OsError(e) => write!(f, "OS error: {}", e),
            Repr::Piet(e) => write!(f, "Piet error: {}", e),
        }
    }
}

impl std::error::Error for Error {}

impl Error {
    fn os_error(e: async_winit::error::OsError) -> Error {
        Error(Repr::OsError(e))
    }

    fn piet(e: piet::Error) -> Error {
        Error(Repr::Piet(e))
    }
}

/// Set up the program entry point.
#[macro_export]
macro_rules! main {
    (
        fn main($bident:ident: $bty:ty) $(-> $rty:ty)? $body:block
    ) => {
        // On non-android platforms, use the `main` function as the entry point.
        #[cfg(not(target_os = "android"))]
        fn main() $(-> $rty)? {
            #[inline]
            fn __gui_tools_main_inner($bident: $bty) $(-> $rty)? $body
            __gui_tools_main_inner($crate::__private::new_display_builder())
        }

        // On android platforms, use the android_main function as the entry point.
        #[cfg(target_os = "android")]
        #[no_mangle]
        fn android_main(app: $crate::__private::activity::AndroidApp) {
            #[inline]
            fn __gui_tools_main_inner($bident: $bty) $(-> $rty)? $body
            let result = __gui_tools_main_inner(
                $crate::__private::with_android_app(app)
            );

            // TODO: Handle the result in some way.
            let _ = result;
        }
    };
}

// TODO: Add a test harness.

/// Tell the application to exit.
#[inline]
pub async fn exit() -> Exit {
    // TODO: Properly handle Android exit
    let x: Infallible = DisplayInner::get().elwt.exit().await;
    match x {}
}

/// Tell the application to exit with a specific exit code.
#[inline]
pub async fn exit_with_code(code: i32) -> Exit {
    // TODO: Properly handle Android exit
    let x: Infallible = DisplayInner::get().elwt.exit_with_code(code).await;
    match x {}
}

/// The application has exited.
#[derive(Debug)]
pub struct Exit {
    _private: (),
}

/// A position that is either physical or logical.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum WindowPosition {
    /// A physical position.
    Physical(Point),

    /// A logical position.
    Logical(Point),
}

impl<T: Into<Point>> From<T> for WindowPosition {
    fn from(p: T) -> WindowPosition {
        WindowPosition::Logical(p.into())
    }
}

/// A size that is either physical or logical.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum WindowSize {
    /// A physical size.
    Physical(Size),

    /// A logical size.
    Logical(Size),
}

impl<T: Into<Size>> From<T> for WindowSize {
    fn from(s: T) -> WindowSize {
        WindowSize::Logical(s.into())
    }
}

/// Themes available for windows.
#[derive(Clone, Copy, Debug, PartialEq)]
#[non_exhaustive]
pub enum Theme {
    Light,
    Dark,
}

/// A handle to a monitor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Monitor(async_winit::monitor::MonitorHandle);

/// A video mode.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VideoMode(async_winit::monitor::VideoMode);

/// An icon for a window.
pub struct Icon(async_winit::window::Icon);

/// The ordering of this window with respect to its Z position.
#[derive(Clone, Copy, Debug, PartialEq)]
#[non_exhaustive]
pub enum WindowLevel {
    /// The window does not enforce any ordering.
    Normal,

    /// The window is at the bottom of the stack.
    Bottom,

    /// The window is at the top of the stack.
    Top,
}

/// Whether to display the window in fullscreen mode.
#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum Fullscreen {
    /// Display the window in fullscreen mode.
    Exclusive(VideoMode),

    /// Take up the given monitor (or the primary monitor if `None`).
    Borderless(Option<Monitor>),
}

/// The available window buttons.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct WindowButtons {
    inner: async_winit::window::WindowButtons,
}

impl Default for WindowButtons {
    fn default() -> Self {
        Self {
            inner: async_winit::window::WindowButtons::all(),
        }
    }
}

impl WindowButtons {
    /// Create a new `WindowButtons` with no buttons.
    pub fn none() -> Self {
        Self {
            inner: async_winit::window::WindowButtons::empty(),
        }
    }

    /// Set the status of the "close" button.
    pub fn set_close(&mut self, enabled: bool) {
        if enabled {
            self.inner.insert(async_winit::window::WindowButtons::CLOSE);
        } else {
            self.inner.remove(async_winit::window::WindowButtons::CLOSE);
        }
    }

    /// Set the status of the "minimize" button.
    pub fn set_minimize(&mut self, enabled: bool) {
        if enabled {
            self.inner
                .insert(async_winit::window::WindowButtons::MINIMIZE);
        } else {
            self.inner
                .remove(async_winit::window::WindowButtons::MINIMIZE);
        }
    }

    /// Set the status of the "maximize" button.
    pub fn set_maximize(&mut self, enabled: bool) {
        if enabled {
            self.inner
                .insert(async_winit::window::WindowButtons::MAXIMIZE);
        } else {
            self.inner
                .remove(async_winit::window::WindowButtons::MAXIMIZE);
        }
    }
}

/// The type of an event that a handler can return.
pub trait Event: EventSealed {
}
impl<T: EventSealed + ?Sized> Event for T {
}

/// The event handler for some kind of event.
pub struct Handler<'a, T: Event> {
    inner: &'a async_winit::Handler<T::AsEvent>
}

impl<'a, T: Event> Future for Handler<'a, T> {
    type Output = (); 

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Pin::new(&mut self.inner).poll(cx)
    }
}

/// The connection to the display server.
pub struct Display {
    /// The underlying event loop.
    event_loop: Cell<Option<EventLoop>>,

    /// Inner display.
    inner: Rc<DisplayInner>,
}

struct DisplayInner {
    /// The handle associated with the display.
    handle: raw_window_handle::RawDisplayHandle,

    /// The event loop handle.
    elwt: EventLoopWindowTarget,

    /// The inner drawing context.
    draw: RefCell<DrawState>,
}

enum DrawState {
    /// We are still being initialized.
    Initializing(theo::DisplayBuilder),

    /// We are ready to draw.
    Ready(theo::Display),

    /// Empty hole.
    Hole,
}

impl DrawState {
    fn ready_mut(&mut self) -> &mut theo::Display {
        match self {
            DrawState::Ready(display) => display,
            _ => panic!("Display not ready"),
        }
    }
}

impl DisplayInner {
    fn get() -> Rc<DisplayInner> {
        std::thread_local! {
            static DISPLAY: RefCell<Option<Rc<DisplayInner>>> = RefCell::new(None);
        }

        impl Display {
            pub(crate) fn set_inner(inner: Rc<DisplayInner>) {
                DISPLAY.with(|display| {
                    *display.borrow_mut() = Some(inner);
                })
            }
        }

        DISPLAY.with(|display| {
            display
                .borrow()
                .clone()
                .expect("No display available on this thread")
        })
    }
}

impl Display {
    /// Run a future.
    pub fn block_on(&self, f: impl Future<Output = Exit> + 'static) -> ! {
        Self::set_inner(self.inner.clone());

        self.event_loop
            .take()
            .expect("Cannot call `block_on` more than once per program")
            .block_on(async move {
                let Exit { _private: () } = f.await;

                panic!("The `block_on` future returned, but it should never return")
            })
    }
}

/// A window.
pub struct Window {
    /// Handle involving the windowing system.
    inner: WinitWindow,

    /// Surface for drawing.
    surface: theo::Surface,
}

impl Window {
    pub async fn new() -> Result<Window, Error> {
        WindowBuilder::new().build().await
    }
}

pub struct DisplayBuilder {
    winit: EventLoopBuilder,
    theo: Option<theo::DisplayBuilder>,
    _unsend: PhantomData<*const ()>,
}

impl DisplayBuilder {
    /// Create a new display builder.
    pub(crate) fn new() -> DisplayBuilder {
        let mut this = DisplayBuilder {
            winit: EventLoopBuilder::new(),
            theo: Some(theo::DisplayBuilder::new()),
            _unsend: PhantomData,
        };

        #[cfg(x11_platform)]
        {
            // Register the X11 platform hook.
            this.theo = Some(
                this.theo
                    .unwrap()
                    .glx_error_hook(async_winit::platform::x11::register_xlib_error_hook),
            );
        }

        this
    }

    /// Force the use of software rasterization in drawing.
    pub fn with_swrast(&mut self, swrast: bool) -> &mut Self {
        self.theo = Some(self.theo.take().unwrap().force_swrast(swrast));
        self
    }

    /// Set whether or not we should support transparent backgrounds.
    pub fn with_transparency(&mut self, transparent: bool) -> &mut Self {
        self.theo = Some(self.theo.take().unwrap().transparent(transparent));
        self
    }

    cfg_android! {
        pub(crate) fn with_android_app(
            &mut self,
            app: crate::__private::activity::AndroidApp
        ) -> &mut Self {
            self.winit.with_android_app(app);
            self
        }
    }

    /// Build a new display.
    pub fn build(self) -> Result<Display, Error> {
        let Self {
            mut winit, theo, ..
        } = self;
        let evl = winit.build();

        Ok(Display {
            inner: Rc::new(DisplayInner {
                handle: raw_window_handle::HasRawDisplayHandle::raw_display_handle(&*evl),
                elwt: evl.window_target().clone(),
                draw: RefCell::new(DrawState::Initializing(theo.unwrap())),
            }),
            event_loop: Cell::new(Some(evl)),
        })
    }
}

#[cfg(any(
    windows,
    all(
        unix,
        not(target_os = "macos"),
        not(target_os = "android"),
        not(target_os = "ios"),
        any(feature = "x11", feature = "wayland")
    )
))]
impl DisplayBuilder {
    /// Make it so this display can run on any thread.
    pub fn with_any_thread(&mut self, any_thread: bool) -> &mut Self {
        cfg_if::cfg_if! {
            if #[cfg(windows)] {
                use async_winit::platform::windows::EventLoopBuilderExtWindows;
            } else if #[cfg(feature = "x11")] {
                use async_winit::platform::x11::EventLoopBuilderExtX11;
            } else if #[cfg(feature = "wayland")] {
                use async_winit::platform::wayland::EventLoopBuilderExtWayland;
            }
        }

        self.winit.with_any_thread(any_thread);
        self
    }
}

cfg_x11! {
    impl DisplayBuilder {
        /// Force this display to use X11.
        pub fn with_x11(&mut self) -> &mut Self {
            use async_winit::platform::x11::EventLoopBuilderExtX11;
            self.winit.with_x11();
            self
        }
    }
}

cfg_wayland! {
    impl DisplayBuilder {
        /// Force this display to use Wayland.
        pub fn with_wayland(&mut self) -> &mut Self {
            use async_winit::platform::wayland::EventLoopBuilderExtWayland;
            self.winit.with_wayland();
            self
        }
    }
}

impl Default for DisplayBuilder {
    fn default() -> DisplayBuilder {
        DisplayBuilder::new()
    }
}

pub struct WindowBuilder {
    inner: WinitWindowBuilder,
}

impl WindowBuilder {
    /// Create a new window builder.
    pub fn new() -> WindowBuilder {
        WindowBuilder {
            inner: WinitWindowBuilder::new(),
        }
    }

    fn map(self, f: impl FnOnce(WinitWindowBuilder) -> WinitWindowBuilder) -> Self {
        Self {
            inner: f(self.inner),
        }
    }

    /// Requests the window to be of specific dimensions.
    #[inline]
    pub fn with_inner_size(self, size: impl Into<WindowSize>) -> Self {
        self.map(|x| x.with_inner_size(cvt_size(size)))
    }

    /// Sets the minimum dimensions that a window can have.
    #[inline]
    pub fn with_min_inner_size(self, size: impl Into<WindowSize>) -> Self {
        self.map(|x| x.with_min_inner_size(cvt_size(size)))
    }

    /// Sets the maximum dimensions that a window can have.
    #[inline]
    pub fn with_max_inner_size(self, size: impl Into<WindowSize>) -> Self {
        self.map(|x| x.with_max_inner_size(cvt_size(size)))
    }

    /// Set the initial position of the window.
    #[inline]
    pub fn with_position(self, position: impl Into<WindowPosition>) -> Self {
        self.map(|x| x.with_position(cvt_position(position)))
    }

    /// Set whether or not the window is resizable.
    #[inline]
    pub fn with_resizable(self, resizable: bool) -> Self {
        self.map(|x| x.with_resizable(resizable))
    }

    /// Set the buttons on the window.
    #[inline]
    pub fn with_window_buttons(self, buttons: impl Into<WindowButtons>) -> Self {
        self.map(|x| x.with_enabled_buttons(buttons.into().inner))
    }

    /// Set the title of the window.
    #[inline]
    pub fn with_title(self, title: impl Into<String>) -> Self {
        self.map(|x| x.with_title(title.into()))
    }

    /// Set whether the window is maximized when first created.
    #[inline]
    pub fn with_maximized(self, maximized: bool) -> Self {
        self.map(|x| x.with_maximized(maximized))
    }

    /// Set whether the window is visible when first created.
    #[inline]
    pub fn with_visible(self, visible: bool) -> Self {
        self.map(|x| x.with_visible(visible))
    }

    /// Set whether the window should have borders and bars.
    #[inline]
    pub fn with_decorations(self, decorations: bool) -> Self {
        self.map(|x| x.with_decorations(decorations))
    }

    /// Set whether the window is in fullscreen mode.
    #[inline]
    pub fn with_fullscreen(self, fullscreen: impl Into<Option<Fullscreen>>) -> Self {
        use async_winit::window::Fullscreen as Fs;
        self.map(|x| {
            x.with_fullscreen(match fullscreen.into() {
                None => None,
                Some(Fullscreen::Borderless(b)) => Some(Fs::Borderless(b.map(|b| b.0))),
                Some(Fullscreen::Exclusive(e)) => Some(Fs::Exclusive(e.0)),
            })
        })
    }

    /// Set the level of the window.
    #[inline]
    pub fn with_window_level(self, level: impl Into<WindowLevel>) -> Self {
        use async_winit::window::WindowLevel as Wl;
        self.map(|x| {
            x.with_window_level(match level.into() {
                WindowLevel::Normal => Wl::Normal,
                WindowLevel::Bottom => Wl::AlwaysOnBottom,
                WindowLevel::Top => Wl::AlwaysOnTop,
            })
        })
    }

    /// Set the icon of the window.
    #[inline]
    pub fn with_window_icon(self, icon: impl Into<Option<Icon>>) -> Self {
        self.map(|x| x.with_window_icon(icon.into().map(|x| x.0)))
    }

    /// Set the theme of the window.
    #[inline]
    pub fn with_theme(self, theme: impl Into<Option<Theme>>) -> Self {
        self.map(|x| {
            x.with_theme(match theme.into() {
                Some(Theme::Dark) => Some(async_winit::window::Theme::Dark),
                Some(Theme::Light) => Some(async_winit::window::Theme::Light),
                None => None,
            })
        })
    }

    /// Sets the resize increments for the window.
    #[inline]
    pub fn with_resize_increments(self, increments: impl Into<WindowSize>) -> Self {
        self.map(|x| x.with_resize_increments(cvt_size(increments.into())))
    }

    /// Prevents the contents of the window from being captured by other apps.
    #[inline]
    pub fn with_content_protected(self, protected: bool) -> Self {
        self.map(|x| x.with_content_protected(protected))
    }

    /// Sets whether the window will be initially active or not.
    #[inline]
    pub fn with_active(self, active: bool) -> Self {
        self.map(|x| x.with_active(active))
    }

    /// Build the window.
    #[allow(clippy::let_unit_value)]
    pub async fn build(self) -> Result<Window, Error> {
        let display = DisplayInner::get();
        let mut window_builder = Some(self.inner);

        let inner = {
            // On Windows, we need to initialize the display using a window. Therefore, we can just
            // build the window and use it to initialize the display if it isn't already.
            let window = {
                #[cfg(wgl_backend)]
                {
                    if display.draw.borrow().is_some() {
                        None
                    } else {
                        Some(
                            window_builder
                                .take()
                                .unwrap()
                                .build()
                                .await
                                .map_err(Error::os_error)?,
                        )
                    }
                }

                #[cfg(not(wgl_backend))]
                {
                    None
                }
            };

            // Query the display for window construction parameters.
            let (transparent, _x11_visual) = {
                let mut theo_display = display.draw.borrow_mut();

                let theo_display = if let DrawState::Ready(theo_display) = &mut *theo_display {
                    theo_display
                } else {
                    // We need to initialize the display.
                    let mut display_builder =
                        match mem::replace(&mut *theo_display, DrawState::Hole) {
                            DrawState::Initializing(init) => init,
                            _ => unreachable!("cannot poll an empty hole"),
                        };

                    // On Windows, build the window and use it to initialize the display.
                    #[cfg(wgl_backend)]
                    {
                        drop(theo_display);
                        display_builder = display_builder.window(window.as_ref().unwrap());
                        theo_display = display.draw.borrow_mut();
                    }

                    #[cfg(x11_platform)]
                    {
                        display_builder = display_builder
                            .glx_error_hook(async_winit::platform::x11::register_xlib_error_hook);
                    }

                    // Use the window to initialize the display.
                    *theo_display = DrawState::Ready(unsafe {
                        display_builder
                            .build_from_raw(display.handle)
                            .map_err(Error::piet)?
                    });

                    theo_display.ready_mut() 
                };

                // The window wants the transparency support and the X11 visual info.
                let x11_visual = {
                    #[cfg(x11_platform)]
                    {
                        theo_display.x11_visual()
                    }

                    #[cfg(not(x11_platform))]
                    {}
                };

                (theo_display.supports_transparency(), x11_visual)
            };

            // Either use the window or create it now.
            match window {
                Some(window) => window,
                None => {
                    let mut builder = window_builder.take().unwrap();

                    if !transparent {
                        builder = builder.with_transparent(false);
                    }

                    #[cfg(x11_platform)]
                    {
                        use async_winit::platform::x11::WindowBuilderExtX11;

                        if let Some(visual) = _x11_visual {
                            // TODO
                            //builder = builder.with_x11_visual(visual.as_ptr());
                        }
                    }

                    builder.build().await.map_err(Error::os_error)?
                }
            }
        };

        // Get the size to create the surface with.
        let size = inner.inner_size().await;

        // Create the surface.
        let surface = unsafe {
            display
                .draw
                .borrow_mut()
                .ready_mut()
                .make_surface(&inner, size.width, size.height)
        }
        .map_err(Error::piet)?;

        Ok(Window { inner, surface })
    }
}

impl Default for WindowBuilder {
    fn default() -> WindowBuilder {
        WindowBuilder::new()
    }
}

#[inline]
fn cvt_size(size: impl Into<WindowSize>) -> WinitSize {
    match size.into() {
        WindowSize::Physical(sz) => {
            PhysicalSize::new(sz.width.round() as u32, sz.height.round() as u32).into()
        }
        WindowSize::Logical(sz) => LogicalSize::new(sz.width, sz.height).into(),
    }
}

#[inline]
fn cvt_position(posn: impl Into<WindowPosition>) -> WinitPosition {
    match posn.into() {
        WindowPosition::Physical(posn) => {
            PhysicalPosition::new(posn.x as i32, posn.y as i32).into()
        }
        WindowPosition::Logical(posn) => LogicalPosition::new(posn.x, posn.y).into(),
    }
}

// Semver exempt.
#[doc(hidden)]
pub mod __private {
    use async_winit::Event;
    use crate::DisplayBuilder;

    #[cfg(not(target_os = "android"))]
    pub fn new_display_builder() -> DisplayBuilder {
        DisplayBuilder::new()
    }

    #[cfg(target_os = "android")]
    pub use async_winit::platform::android::activity;

    #[cfg(target_os = "android")]
    pub fn with_android_app(app: activity::AndroidApp) -> DisplayBuilder {
        DisplayBuilder::new().with_android_app(app)
    }

    #[doc(hidden)]
    pub trait EventSealed {
        type AsEvent: Event;
    }
}
