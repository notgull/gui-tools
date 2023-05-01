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

use gui_tools::{Display, DisplayBuilder, Error, Exit, WindowBuilder};

gui_tools::main! {
    fn main(builder: DisplayBuilder) -> Result<(), Error> {
        match builder.build()?.block_on(main2()) {}
    }
}

async fn main2() -> Exit {
    // Create a window.
    let window = WindowBuilder::new()
        .with_title("Hello, world!")
        .with_inner_size((400.0, 300.0))
        .build()
        .await
        .unwrap();

    gui_tools::exit().await
}
