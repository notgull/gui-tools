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

use async_winit::event_loop::{EventLoop, EventLoopBuilder};
use async_winit::window::Window as WinitWindow;

use std::cell::{Cell, RefCell};
use std::convert::Infallible;
use std::future::Future;
use std::rc::Rc;

#[cfg(free_unix)]
macro_rules! cfg_free_unix {
    ($($i:item)*) => {
        $($i)*
    };
}

#[cfg(not(free_unix))]
macro_rules! cfg_free_unix {
    ($($i:item)*) => {};
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

#[derive(Debug)]
pub struct Error(Repr);

#[derive(Debug)]
enum Repr {
    OsError(async_winit::error::OsError),
    Piet(piet::Error),
}

impl Error {
    fn os_error(e: async_winit::error::OsError) -> Error {
        Error(Repr::OsError(e))
    }

    fn piet(e: piet::Error) -> Error {
        Error(Repr::Piet(e))
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
    /// The inner drawing context.
    draw: RefCell<theo::Display>,
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
    /// Create a new `Display`.
    pub fn new() -> Result<Display, Error> {
        DisplayBuilder::new().build()
    }

    /// Run a future.
    pub fn block_on(&self, f: impl Future<Output = Infallible> + 'static) -> ! {
        Self::set_inner(self.inner.clone());

        self.event_loop
            .take()
            .expect("Cannot call `block_on` more than once per program")
            .block_on(f)
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
        // Create the winit window.
        let inner = WinitWindow::new().await.map_err(Error::os_error)?;
        let size = inner.inner_size().await;

        // Create the surface.
        let surface = unsafe {
            DisplayInner::get()
                .draw
                .borrow_mut()
                .make_surface(&inner, size.width, size.height)
        }
        .map_err(Error::piet)?;

        Ok(Self { inner, surface })
    }
}

pub struct DisplayBuilder {
    winit: EventLoopBuilder,
    theo: Option<theo::DisplayBuilder>,
}

impl DisplayBuilder {
    /// Create a new display builder.
    pub fn new() -> DisplayBuilder {
        let mut this = DisplayBuilder {
            winit: EventLoopBuilder::new(),
            theo: Some(theo::DisplayBuilder::new()),
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

    /// Build a new display.
    pub fn build(self) -> Result<Display, Error> {
        let Self { mut winit, theo } = self;
        let evl = winit.build();
        let draw = unsafe {
            theo.unwrap().build({
                // TODO
                let x: async_winit::window::Window = todo!();
                x
            })
        }
        .map_err(Error::piet)?;

        Ok(Display {
            event_loop: Cell::new(Some(evl)),
            inner: Rc::new(DisplayInner {
                draw: RefCell::new(draw),
            }),
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
