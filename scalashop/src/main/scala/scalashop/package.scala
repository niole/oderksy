
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    /*The boxBlurKernel method takes the source image sr
      c, coordinates x and y of the pixel, and the radius o
      f the blur. It returns the resulting average value of
      the surrounding pixels. We compute the average value by separating the pixel int
      o four channels, computing the average of each of the channel
      s, and using the four average values to produce the final pixel
    value. In the previous figure, the radius parameter is equal to 1 an
    d the average is computed from 9 pixels*/
    //TODO I think this is correct but am not sure
    val allRGBAs = for {
      xPos <- ((x-radius) until (x+radius)).toList
      yPos <- ((y-radius) until (y+radius)).toList
    } yield clamp(src.apply(xPos, yPos), 0, src.width*src.height

    val totalRGBAs = allRGBAs.length

    (allRGBAs map ( px => {
      (red(px) + green(px) + blue(px) + alpha(px))/4
    }) sum)/totalRGBAs
  }
}
