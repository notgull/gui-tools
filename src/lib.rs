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

use std::cell::{Cell, RefCell};
use std::convert::Infallible;
use std::future::Future;
use std::marker::PhantomData;
use std::rc::Rc;

mod draw;
mod error;
mod handler;
mod props;
mod window;

pub use draw::{Brush, Image, RenderContext, Text, TextLayout, TextLayoutBuilder};
pub use error::Error;
pub use handler::{Event, Handler};
pub use props::{
    Fullscreen, Icon, Monitor, Theme, VideoMode, WindowButtons, WindowLevel, WindowPosition,
    WindowSize,
};
pub use window::{Window, WindowBuilder};

// Use kurbo as a public dependency here, since piet is public as well.
#[doc(inline)]
pub use kurbo::{Point, Rect, Size};
pub use piet;

pub mod prelude {
    pub use piet::{Image, IntoBrush, RenderContext, Text, TextLayout, TextLayoutBuilder};
}

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
}
