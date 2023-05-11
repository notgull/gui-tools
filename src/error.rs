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

use std::fmt;

/// The error type for `gui-tools`.
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

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.0 {
            Repr::Piet(e) => Some(e),
            _ => None,
        }
    }
}

impl Error {
    pub(crate) fn os_error(e: async_winit::error::OsError) -> Error {
        Error(Repr::OsError(e))
    }

    pub(crate) fn piet(e: piet::Error) -> Error {
        Error(Repr::Piet(e))
    }
}

impl From<piet::Error> for Error {
    fn from(e: piet::Error) -> Error {
        Error::piet(e)
    }
}
