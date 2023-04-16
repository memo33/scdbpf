package scdbpf

import compat.{Image, RGBA}
import java.nio.ByteBuffer
import Fsh.conversions._
import Fsh.FshFormat
import DbpfUtil._

private object DxtDecoding {

  private class DxtImage(val width: Int, val height: Int) extends Image[RGBA] {
    private[this] val arr = new Array[Int](width * height) // ints in rgba format
    def apply(x: Int, y: Int): RGBA = RGBA(arr(x + y * width))
    @inline def update(x: Int, y: Int, i: Int): Unit = arr(x + y * width) = i
  }

  @inline private def interpolate(a: Double, col0: RGBA, b: Double, col1: RGBA): RGBA = rgbaFromChannels(
    Math.round(a * (col0.red   & 0xFF) + b * (col1.red   & 0xFF)).toInt,
    Math.round(a * (col0.green & 0xFF) + b * (col1.green & 0xFF)).toInt,
    Math.round(a * (col0.blue  & 0xFF) + b * (col1.blue  & 0xFF)).toInt,
    Math.round(a * (col0.alpha & 0xFF) + b * (col1.alpha & 0xFF)).toInt
  )

  private def readDxtBlock(buf: ByteBuffer, img: DxtImage, xPos: Int, yPos: Int, dxt3: Boolean): Unit = {
    val sCol0 = buf.getShort() & 0xFFFF
    val sCol1 = buf.getShort() & 0xFFFF
    val col0 = short0565toRGBA(sCol0.toShort)
    val col1 = short0565toRGBA(sCol1.toShort)
    val noAlpha = sCol0 > sCol1 || dxt3
    val col2 =
      if (noAlpha) interpolate(2.0/3, col0, 1.0/3, col1)
      else interpolate(0.5, col0, 0.5, col1)
    val col3 =
      if (noAlpha) interpolate(1.0/3, col0, 2.0/3, col1)
      else RGBA(0) // transparent
    val col = Array[Int](col0.i, col1.i, col2.i, col3.i)

    for (y <- 0 until 4; bits = buf.get() & 0xFF; x <- 0 until 4) {
      if (yPos + y < img.height && xPos + x < img.width) {
        val code = (bits >> (x * 2)) & 0x03
        img(xPos + x, yPos + y) =
          if (dxt3) col(code) & 0xFFFFFF | img(xPos + x, yPos + y).i & 0xFF000000 // preserve pre-existing alpha
          else      col(code)
      }
    }
  }

  private def readAlphaBlock(buf: ByteBuffer, img: DxtImage, xPos: Int, yPos: Int): Unit = {
    for (y <- 0 until 4; bits = buf.getShort() & 0xFFFF; x <- 0 until 4) {
      if (yPos + y < img.height && xPos + x < img.width) {
        val alpha = (bits >> (x * 4) & 0xF) * 17 // 4 bit alpha channel with linear interpolation
        img(xPos + x, yPos + y) = alpha << 24
      }
    }
  }

  def decode(dxtFormat: FshFormat, data: Array[Byte], offset: Int, width: Int, height: Int): Image[RGBA] = {
    require(dxtFormat == FshFormat.Dxt1 || dxtFormat == FshFormat.Dxt3)
    val dxt3 = dxtFormat == FshFormat.Dxt3
    val img = new DxtImage(width, height)
    val buf = wrapLEBB(data)
    buf.position(offset)
    for (y <- 0 until height by 4; x <- 0 until width by 4) {
      if (dxt3) {
        readAlphaBlock(buf, img, x, y)
      }
      readDxtBlock(buf, img, x, y, dxt3)
    }
    img
  }
}
