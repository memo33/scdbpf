package scdbpf

import java.nio.ByteBuffer
import DbpfUtil._

sealed trait LText extends DbpfType {

  val text: String
}

object LText {

  def apply(text: String): LText = new FreeLText(text)

  implicit val converter = new Converter[DbpfType, LText] {
    def apply(from: DbpfType): LText = {
      new BufferedLText(from.dataView)
    }
  }

  private val controlChar: Short = 0x1000

  private class BufferedLText(arr: Array[Byte]) extends RawType(arr) with LText {
    val text: String = {
      val buf = wrapLEBB(data)
      if (data.length < 4) {
        throw new DbpfDecodeFailedException(f"length is only ${data.length}, minimum is 4 bytes")
      }
      val expectedSize = data.length - 4
      val count = buf.getShort() * 2
      val cc = buf.getShort() // 0x1000
      if (cc != controlChar) {
        throw new DbpfDecodeFailedException(f"control character was 0x$cc%04X, expected 0x1000")
      } else if (count != expectedSize) { // currently treats UTF-16 as fixed-width encoding
        throw new DbpfDecodeFailedException(f"declared length was $count, expected $expectedSize")
      }
      new String(data, 4, expectedSize, "UTF-16LE")
    }
  }

  private class FreeLText(val text: String) extends LText {
    protected lazy val data: Array[Byte] = {
      val buf = allocLEBB(4 + 2 * text.length)
      buf.putShort(text.length.toShort)
      buf.putShort(LText.controlChar)
      buf.put(text.getBytes("UTF-16LE"))
      assert(buf.remaining() == 0)
      buf.array()
    }
  }
}
