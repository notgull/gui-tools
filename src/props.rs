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

use piet::kurbo::{Point, Size};

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
pub struct Monitor(pub(crate) async_winit::monitor::MonitorHandle);

/// A video mode.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VideoMode(pub(crate) async_winit::monitor::VideoMode);

/// An icon for a window.
pub struct Icon(pub(crate) async_winit::window::Icon);

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
    pub(crate) inner: async_winit::window::WindowButtons,
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
