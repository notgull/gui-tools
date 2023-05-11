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

use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};

use __private::EventSealed;

/// The type of an event that a handler can return.
pub trait Event: EventSealed {}
impl<T: EventSealed + ?Sized> Event for T {}

/// The event handler for some kind of event.
pub struct Handler<'a, T: Event> {
    inner: &'a async_winit::Handler<T::AsEvent>,
}

impl<'a, T: Event> Handler<'a, T> {
    pub(crate) fn new(inner: &'a async_winit::Handler<T::AsEvent>) -> Self {
        Self { inner }
    }
}

impl<'a, T: Event> Future for Handler<'a, T> {
    type Output = <<T as EventSealed>::AsEvent as async_winit::Event>::Clonable;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        Pin::new(&mut self.inner).poll(cx)
    }
}

mod __private {
    use async_winit::Event;

    #[doc(hidden)]
    pub trait EventSealed {
        type AsEvent: Event;
        fn dibs(window: &crate::Window, clonable: <Self::AsEvent as Event>::Clonable);
    }

    impl<T: Clone + 'static> EventSealed for T {
        type AsEvent = Self;

        fn dibs(_window: &crate::Window, _clonable: Self) {}
    }
}
