package io.github.memo33
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
    @inline final def update(x: Int, y: Int, i: Int): Unit = arr(x + y * width) = i
  }

  @inline private def interpolate(a: Double, col0: RGBA, b: Double, col1: RGBA): RGBA = rgbaFromChannels(
    Math.round(a * (col0.red   & 0xFF) + b * (col1.red   & 0xFF)).toInt,
    Math.round(a * (col0.green & 0xFF) + b * (col1.green & 0xFF)).toInt,
    Math.round(a * (col0.blue  & 0xFF) + b * (col1.blue  & 0xFF)).toInt,
    Math.round(a * (col0.alpha & 0xFF) + b * (col1.alpha & 0xFF)).toInt
  )

  private def readDxtBlock(buf: ByteBuffer, img: DxtImage, xPos: Int, yPos: Int, dxt1: Boolean): Unit = {
    val sCol0 = buf.getShort() & 0xFFFF
    val sCol1 = buf.getShort() & 0xFFFF
    val col0 = short0565toRGBA(sCol0.toShort)
    val col1 = short0565toRGBA(sCol1.toShort)
    val noAlpha = sCol0 > sCol1 || !dxt1
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
          if (!dxt1) col(code) & 0xFFFFFF | img(xPos + x, yPos + y).i & 0xFF000000 // preserve pre-existing alpha
          else       col(code)
      }
    }
  }

  private def readDxt5AlphaBlock(buf: ByteBuffer, img: DxtImage, xPos: Int, yPos: Int): Unit = {
    val a0 = buf.get(buf.position()) & 0xFF
    val a1 = buf.get(buf.position() + 1) & 0xFF
    val bits = buf.getLong() >> 16
    val table =
      if (a0 > a1)
        Array[Int](a0, a1,
          { val z7 = (6*a0 + 1*a1); val z = z7 / 7; ((z7 - z) / 2 + z) / 4 },
          { val z7 = (5*a0 + 2*a1); val z = z7 / 7; ((z7 - z) / 2 + z) / 4 },
          { val z7 = (4*a0 + 3*a1); val z = z7 / 7; ((z7 - z) / 2 + z) / 4 },
          { val z7 = (3*a0 + 4*a1); val z = z7 / 7; ((z7 - z) / 2 + z) / 4 },
          { val z7 = (2*a0 + 5*a1); val z = z7 / 7; ((z7 - z) / 2 + z) / 4 },
          { val z7 = (1*a0 + 6*a1); val z = z7 / 7; ((z7 - z) / 2 + z) / 4 })
      else
        Array[Int](a0, a1,
          (4*a0 + 1*a1) / 5,
          (3*a0 + 2*a1) / 5,
          (2*a0 + 3*a1) / 5,
          (1*a0 + 4*a1) / 5,
          0, 0xFF)

    for (y <- 0 until 4; x <- 0 until 4) {
      val code = ((bits >> ((y * 4 + x) * 3)) & 0x07).toInt
      val alpha = table(code)
      img(xPos + x, yPos + y) = alpha << 24
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
    val img = new DxtImage(width, height)
    val buf = wrapLEBB(data)
    buf.position(offset)
    for (y <- 0 until height by 4; x <- 0 until width by 4) {
      dxtFormat match {
        case FshFormat.Dxt1 =>
          readDxtBlock(buf, img, x, y, dxt1 = true)
        case FshFormat.Dxt3 =>
          readAlphaBlock(buf, img, x, y)
          readDxtBlock(buf, img, x, y, dxt1 = false)
        case FshFormat.Dxt5 =>
          readDxt5AlphaBlock(buf, img, x, y)
          readDxtBlock(buf, img, x, y, dxt1 = false)
        case _ => throw new IllegalArgumentException(s"not a DXT format: ${dxtFormat}")
      }
    }
    img
  }
}
