import gleam/deque
import gleam/float
import gleam/int
import gleam/list

/// A black and white image.
///
pub type Image {
  Image(
    width: Int,
    height: Int,
    /// The image's pixels as a flat bit array. Each pixel is 1 byte.
    ///
    pixels: BitArray,
  )
}

// TRANSFORMATIONS -------------------------------------------------------------

/// Maps the pixels of an image using the given function. The callback is passed
/// the pixel's value, row, and column (1-based).
///
pub fn map(image: Image, with fun: fn(Int, Int, Int) -> Int) {
  Image(..image, pixels: map_loop(image.pixels, image.width, 1, 1, <<>>, fun))
}

fn map_loop(
  pixels: BitArray,
  columns: Int,
  row: Int,
  column: Int,
  acc: BitArray,
  fun: fn(Int, Int, Int) -> Int,
) -> BitArray {
  case pixels {
    <<pixel, pixels:bits>> -> {
      let pixel = fun(pixel, row, column)
      let acc = <<acc:bits, pixel>>
      case column == columns {
        True -> map_loop(pixels, columns, row + 1, 1, acc, fun)
        False -> map_loop(pixels, columns, row, column + 1, acc, fun)
      }
    }
    _ -> acc
  }
}

/// Maps the pixels of an image using the given function. The given accumulator
/// is updated and carried around at each step.
/// The callback is passed the accumulator, the pixel's value, row, and column
/// (1-based).
///
pub fn map_fold(
  image: Image,
  from state: state,
  with fun: fn(state, Int, Int, Int) -> #(state, Int),
) -> Image {
  Image(
    ..image,
    pixels: map_fold_loop(image.pixels, image.width, 1, 1, <<>>, state, fun),
  )
}

fn map_fold_loop(
  data: BitArray,
  columns: Int,
  row: Int,
  column: Int,
  acc: BitArray,
  state: state,
  fun: fn(state, Int, Int, Int) -> #(state, Int),
) -> BitArray {
  case data {
    <<pixel, rest:bits>> -> {
      let #(state, pixel) = fun(state, pixel, row, column)
      let acc = <<acc:bits, pixel>>
      case column == columns {
        True -> map_fold_loop(rest, columns, row + 1, 1, acc, state, fun)
        False -> map_fold_loop(rest, columns, row, column + 1, acc, state, fun)
      }
    }
    _ -> acc
  }
}

// DITHERING -------------------------------------------------------------------

/// Dithers the image applying a fixed threshold: all pixels with a value above
/// the threshold will become white, all the other ones black.
/// The threshold value should be between 0 and 255.
///
pub fn fixed_threshold_dithering(image: Image, threshold: Int) -> Image {
  use pixel, _row, _column <- map(image)
  case pixel > threshold {
    True -> 255
    False -> 0
  }
}

/// Dithers each pixel with a random threshold: each pixel has a 50% chance of
/// becoming black or white.
///
/// You'd be surprised by how detailed an image can actually turn up using a
/// random threshold!
///
pub fn random_threshold_dithering(image: Image) -> Image {
  use pixel, _row, _column <- map(image)
  case pixel > int.random(255) {
    True -> 255
    False -> 0
  }
}

/// Dithers the image using the 2 by 2
/// [Bayer matrix.](https://en.wikipedia.org/wiki/Ordered_dithering)
///
pub fn bayer_2_by_2_dithering(image: Image) -> Image {
  use pixel, row, column <- map(image)
  let threshold = case row % 2 + 1, column % 2 + 1 {
    1, 1 -> 0
    1, 2 -> 127
    2, 1 -> 191
    2, 2 -> 64
    _, _ -> panic
  }

  case pixel > threshold {
    True -> 255
    False -> 0
  }
}

/// Dithers the image using the 4 by 4
/// [Bayer matrix.](https://en.wikipedia.org/wiki/Ordered_dithering)
///
pub fn bayer_4_by_4_dithering(image: Image) -> Image {
  use pixel, row, column <- map(image)
  let threshold = case row % 4 + 1, column % 4 + 1 {
    1, 1 -> 0
    1, 2 -> 127
    1, 3 -> 32
    1, 4 -> 159

    2, 1 -> 191
    2, 2 -> 64
    2, 3 -> 223
    2, 4 -> 96

    3, 1 -> 48
    3, 2 -> 175
    3, 3 -> 16
    3, 4 -> 143

    4, 1 -> 239
    4, 2 -> 112
    4, 3 -> 207
    4, 4 -> 80

    _, _ -> panic
  }

  case pixel > threshold {
    True -> 255
    False -> 0
  }
}

/// Dithers the image using the
/// [Floyd-Steinberg](https://en.wikipedia.org/wiki/Floyd–Steinberg_dithering)
/// dithering algorithm.
///
pub fn floyd_steinberg_dithering(image: Image) -> Image {
  let errors =
    deque.from_list(list.repeat(0, times: int.max(5, image.width + 1)))

  // This technique spreads the dithering error among the neighbours of a pixel.
  // It can be implemented somewhat efficiently by keeping an array of errors
  // around: this saves on space: we can store just a couple of lines of errors
  // rather than in entire dictionary for each pixel in the image.
  use errors, pixel, _row, _col <- map_fold(image, errors)

  let assert Ok(#(first, errors)) = deque.pop_front(errors)
  let errors = deque.push_back(errors, 0)

  let pixel = pixel + first
  let new_pixel = case pixel > 127 {
    True -> 255
    False -> 0
  }

  let error = float.round(int.to_float(pixel - new_pixel))
  let assert Ok(#(b, errors)) = deque.pop_back(errors)
  let assert Ok(#(c, errors)) = deque.pop_back(errors)
  let assert Ok(#(d, errors)) = deque.pop_back(errors)
  let assert Ok(#(a, errors)) = deque.pop_front(errors)

  let errors =
    deque.push_front(errors, a + error * 7 / 16)
    |> deque.push_back(d + error * 3 / 16)
    |> deque.push_back(c + error * 5 / 16)
    |> deque.push_back(b + error / 16)

  #(errors, new_pixel)
}

// PARSING ---------------------------------------------------------------------

/// Parses an `Image` from a
/// [`pgm`](https://netpbm.sourceforge.net/doc/pgm.html) image.
///
pub fn from_pgm(pgm: BitArray) -> Result(Image, Nil) {
  case pgm {
    <<"P5\n", rest:bits>> -> pgm_width(rest, 0)
    _ -> Error(Nil)
  }
}

fn pgm_width(pgm: BitArray, width: Int) -> Result(Image, Nil) {
  case pgm {
    <<"0", rest:bits>> -> pgm_width(rest, width * 10 + 0)
    <<"1", rest:bits>> -> pgm_width(rest, width * 10 + 1)
    <<"2", rest:bits>> -> pgm_width(rest, width * 10 + 2)
    <<"3", rest:bits>> -> pgm_width(rest, width * 10 + 3)
    <<"4", rest:bits>> -> pgm_width(rest, width * 10 + 4)
    <<"5", rest:bits>> -> pgm_width(rest, width * 10 + 5)
    <<"6", rest:bits>> -> pgm_width(rest, width * 10 + 6)
    <<"7", rest:bits>> -> pgm_width(rest, width * 10 + 7)
    <<"8", rest:bits>> -> pgm_width(rest, width * 10 + 8)
    <<"9", rest:bits>> -> pgm_width(rest, width * 10 + 9)
    <<" ", rest:bits>> -> pgm_height(rest, width, 0)
    _ -> Error(Nil)
  }
}

fn pgm_height(pgm: BitArray, width: Int, height: Int) -> Result(Image, Nil) {
  case pgm {
    <<"0", rest:bits>> -> pgm_height(rest, width, height * 10 + 0)
    <<"1", rest:bits>> -> pgm_height(rest, width, height * 10 + 1)
    <<"2", rest:bits>> -> pgm_height(rest, width, height * 10 + 2)
    <<"3", rest:bits>> -> pgm_height(rest, width, height * 10 + 3)
    <<"4", rest:bits>> -> pgm_height(rest, width, height * 10 + 4)
    <<"5", rest:bits>> -> pgm_height(rest, width, height * 10 + 5)
    <<"6", rest:bits>> -> pgm_height(rest, width, height * 10 + 6)
    <<"7", rest:bits>> -> pgm_height(rest, width, height * 10 + 7)
    <<"8", rest:bits>> -> pgm_height(rest, width, height * 10 + 8)
    <<"9", rest:bits>> -> pgm_height(rest, width, height * 10 + 9)
    <<"\n255\n", pixels:bytes>> -> Ok(Image(width:, height:, pixels:))

    _ -> Error(Nil)
  }
}
