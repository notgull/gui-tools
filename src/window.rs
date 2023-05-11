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

use super::{
    cvt_position, cvt_size, DisplayInner, DrawState, Error, Fullscreen, Handler, Icon,
    RenderContext, Theme, WindowButtons, WindowLevel, WindowPosition, WindowSize,
};
use async_winit::window::{Window as WinitWindow, WindowBuilder as WinitWindowBuilder};

use std::cell::RefCell;
use std::mem;
use std::rc::Rc;

/// A window.
#[derive(Clone)]
pub struct Window(Rc<WindowState>);

struct WindowState {
    /// Handle involving the windowing system.
    inner: WinitWindow,

    /// Surface for drawing.
    surface: RefCell<theo::Surface>,
}

impl Window {
    pub async fn new() -> Result<Window, Error> {
        WindowBuilder::new().build().await
    }
}

/// Builder for a window.
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
                        /*use async_winit::platform::x11::WindowBuilderExtX11;

                        if let Some(visual) = _x11_visual {
                            // TODO
                            builder = builder.with_x11_visual(visual.as_ptr());
                        }
                        */
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

        Ok(Window(Rc::new(WindowState {
            inner,
            surface: RefCell::new(surface),
        })))
    }
}

impl Default for WindowBuilder {
    fn default() -> WindowBuilder {
        WindowBuilder::new()
    }
}

impl Window {
    /// Wait for the window to be closed.
    pub fn close_requested(&self) -> Handler<'_, ()> {
        Handler::new(self.0.inner.close_requested())
    }

    /// Wait for a redraw request.
    pub fn redraw_requested(&self) -> Handler<'_, ()> {
        Handler::new(self.0.inner.redraw_requested())
    }

    /// Run a closure with a rendering context.
    pub async fn draw<R>(
        &self,
        f: impl FnOnce(&mut RenderContext<'_, '_>) -> Result<R, Error>,
    ) -> Result<R, Error> {
        let inner_size = self.0.inner.inner_size().await;
        let display = DisplayInner::get();
        let mut draw = display.draw.borrow_mut();
        let mut surface = self.0.surface.borrow_mut();
        let display = match &mut *draw {
            DrawState::Ready(display) => display,
            _ => unreachable!("cannot poll an empty hole"),
        };

        let rc =
            theo::RenderContext::new(display, &mut surface, inner_size.width, inner_size.height)?;
        let ret = f(&mut RenderContext::new(rc))?;

        Ok(ret)
    }
}
