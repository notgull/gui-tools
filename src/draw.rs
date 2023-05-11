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

use super::piet::kurbo::{Point, Rect, Size};

/// Rendering context for the window.
pub struct RenderContext<'display, 'surface> {
    inner: theo::RenderContext<'display, 'surface>,
    text: Text,
}

impl<'d, 's> RenderContext<'d, 's> {
    pub(crate) fn new(mut inner: theo::RenderContext<'d, 's>) -> Self {
        let text = piet::RenderContext::text(&mut inner).clone();
        Self {
            inner,
            text: Text { inner: text },
        }
    }
}

impl piet::RenderContext for RenderContext<'_, '_> {
    type Brush = Brush;
    type Image = Image;
    type Text = Text;
    type TextLayout = TextLayout;

    fn status(&mut self) -> Result<(), piet::Error> {
        self.inner.status()
    }

    fn solid_brush(&mut self, color: piet::Color) -> Self::Brush {
        Brush {
            inner: self.inner.solid_brush(color),
        }
    }

    fn gradient(
        &mut self,
        gradient: impl Into<piet::FixedGradient>,
    ) -> Result<Self::Brush, piet::Error> {
        self.inner.gradient(gradient).map(|inner| Brush { inner })
    }

    fn clear(&mut self, region: impl Into<Option<Rect>>, color: piet::Color) {
        self.inner.clear(region, color)
    }

    fn stroke(&mut self, shape: impl kurbo::Shape, brush: &impl piet::IntoBrush<Self>, width: f64) {
        let brush = brush.make_brush(self, || shape.bounding_box());
        self.inner.stroke(shape, &brush.inner, width)
    }

    fn stroke_styled(
        &mut self,
        shape: impl kurbo::Shape,
        brush: &impl piet::IntoBrush<Self>,
        width: f64,
        style: &piet::StrokeStyle,
    ) {
        let brush = brush.make_brush(self, || shape.bounding_box());
        self.inner.stroke_styled(shape, &brush.inner, width, style)
    }

    fn fill(&mut self, shape: impl kurbo::Shape, brush: &impl piet::IntoBrush<Self>) {
        let brush = brush.make_brush(self, || shape.bounding_box());
        self.inner.fill(shape, &brush.inner)
    }

    fn fill_even_odd(&mut self, shape: impl kurbo::Shape, brush: &impl piet::IntoBrush<Self>) {
        let brush = brush.make_brush(self, || shape.bounding_box());
        self.inner.fill_even_odd(shape, &brush.inner)
    }

    fn clip(&mut self, shape: impl kurbo::Shape) {
        self.inner.clip(shape)
    }

    fn text(&mut self) -> &mut Self::Text {
        &mut self.text
    }

    fn draw_text(&mut self, layout: &Self::TextLayout, pos: impl Into<Point>) {
        self.inner.draw_text(&layout.inner, pos.into())
    }

    fn save(&mut self) -> Result<(), piet::Error> {
        self.inner.save()
    }

    fn restore(&mut self) -> Result<(), piet::Error> {
        self.inner.restore()
    }

    fn finish(&mut self) -> Result<(), piet::Error> {
        self.inner.finish()
    }

    fn transform(&mut self, transform: kurbo::Affine) {
        self.inner.transform(transform)
    }

    fn make_image(
        &mut self,
        width: usize,
        height: usize,
        buf: &[u8],
        format: piet::ImageFormat,
    ) -> Result<Self::Image, piet::Error> {
        self.inner
            .make_image(width, height, buf, format)
            .map(|inner| Image { inner })
    }

    fn draw_image(
        &mut self,
        image: &Self::Image,
        dst_rect: impl Into<Rect>,
        interp: piet::InterpolationMode,
    ) {
        self.inner.draw_image(&image.inner, dst_rect.into(), interp)
    }

    fn draw_image_area(
        &mut self,
        image: &Self::Image,
        src_rect: impl Into<Rect>,
        dst_rect: impl Into<Rect>,
        interp: piet::InterpolationMode,
    ) {
        self.inner
            .draw_image_area(&image.inner, src_rect.into(), dst_rect.into(), interp)
    }

    fn capture_image_area(
        &mut self,
        src_rect: impl Into<Rect>,
    ) -> Result<Self::Image, piet::Error> {
        self.inner
            .capture_image_area(src_rect.into())
            .map(|inner| Image { inner })
    }

    fn blurred_rect(&mut self, rect: Rect, blur_radius: f64, brush: &impl piet::IntoBrush<Self>) {
        let brush = brush.make_brush(self, || rect);
        self.inner.blurred_rect(rect, blur_radius, &brush.inner)
    }

    fn current_transform(&self) -> kurbo::Affine {
        self.inner.current_transform()
    }
}

/// Text context for the window.
#[derive(Clone)]
pub struct Text {
    inner: theo::Text,
}

impl piet::Text for Text {
    type TextLayout = TextLayout;
    type TextLayoutBuilder = TextLayoutBuilder;

    fn font_family(&mut self, family_name: &str) -> Option<piet::FontFamily> {
        self.inner.font_family(family_name)
    }

    fn load_font(&mut self, data: &[u8]) -> Result<piet::FontFamily, piet::Error> {
        self.inner.load_font(data)
    }

    fn new_text_layout(&mut self, text: impl piet::TextStorage) -> Self::TextLayoutBuilder {
        TextLayoutBuilder {
            inner: self.inner.new_text_layout(text),
        }
    }
}

/// Text layout builder for the window.
pub struct TextLayoutBuilder {
    inner: theo::TextLayoutBuilder,
}

impl piet::TextLayoutBuilder for TextLayoutBuilder {
    type Out = TextLayout;

    fn max_width(self, width: f64) -> Self {
        Self {
            inner: self.inner.max_width(width),
        }
    }

    fn alignment(self, alignment: piet::TextAlignment) -> Self {
        Self {
            inner: self.inner.alignment(alignment),
        }
    }

    fn default_attribute(self, attribute: impl Into<piet::TextAttribute>) -> Self {
        Self {
            inner: self.inner.default_attribute(attribute),
        }
    }

    fn range_attribute(
        self,
        range: impl std::ops::RangeBounds<usize>,
        attribute: impl Into<piet::TextAttribute>,
    ) -> Self {
        Self {
            inner: self.inner.range_attribute(range, attribute),
        }
    }

    fn build(self) -> Result<Self::Out, piet::Error> {
        Ok(TextLayout {
            inner: self.inner.build()?,
        })
    }
}

/// Text layout for the window.
#[derive(Clone)]
pub struct TextLayout {
    inner: theo::TextLayout,
}

impl piet::TextLayout for TextLayout {
    fn size(&self) -> Size {
        self.inner.size()
    }

    fn trailing_whitespace_width(&self) -> f64 {
        self.inner.trailing_whitespace_width()
    }

    fn image_bounds(&self) -> Rect {
        self.inner.image_bounds()
    }

    fn text(&self) -> &str {
        self.inner.text()
    }

    fn line_text(&self, line_number: usize) -> Option<&str> {
        self.inner.line_text(line_number)
    }

    fn line_metric(&self, line_number: usize) -> Option<piet::LineMetric> {
        self.inner.line_metric(line_number)
    }

    fn line_count(&self) -> usize {
        self.inner.line_count()
    }

    fn hit_test_point(&self, point: Point) -> piet::HitTestPoint {
        self.inner.hit_test_point(point)
    }

    fn hit_test_text_position(&self, idx: usize) -> piet::HitTestPosition {
        self.inner.hit_test_text_position(idx)
    }
}

/// Image for the window.
#[derive(Clone)]
pub struct Image {
    inner: theo::Image,
}

impl piet::Image for Image {
    fn size(&self) -> Size {
        self.inner.size()
    }
}

/// Brush for the window.
#[derive(Clone)]
pub struct Brush {
    inner: theo::Brush,
}

impl<'d, 's> piet::IntoBrush<RenderContext<'d, 's>> for Brush {
    fn make_brush<'a>(
        &'a self,
        _piet: &mut RenderContext<'d, 's>,
        _bbox: impl FnOnce() -> Rect,
    ) -> std::borrow::Cow<'a, <RenderContext as piet::RenderContext>::Brush> {
        std::borrow::Cow::Borrowed(self)
    }
}
