package io.github.memo33
package scdbpf

import java.nio.ByteBuffer
import DbpfUtil._

sealed trait LText extends DbpfType {
  val text: String
  val format: LText.Format.Value
}

object LText extends DbpfTypeCompanion[LText] {

  object Format extends Enumeration {
    val Utf16, Utf8, Ascii, AsciiNoHeader = Value
    def toCharset(format: Format.Value): java.nio.charset.Charset = format match {
      case Format.Utf16 => java.nio.charset.StandardCharsets.UTF_16LE
      case Format.Utf8 => java.nio.charset.StandardCharsets.UTF_8
      case Format.Ascii | Format.AsciiNoHeader => java.nio.charset.StandardCharsets.US_ASCII
    }
  }

  def apply(text: String, format: Format.Value = Format.Utf16): LText = new FreeLText(text, format)

  implicit val converter: Converter[DbpfType, LText] = new Converter[DbpfType, LText] {
    def apply(from: DbpfType): LText = {
      new BufferedLText(from.dataView)
    }
  }

  private val ControlChar16: Short = 0x1000
  private val ControlChar8: Short = 0x0800
  private val ControlChar0: Short = 0x0000

  private def isAsciiPrintable(arr: Array[Byte], offset: Int): Boolean = {
    var i = offset;
    while (i < arr.length && { val c = arr(i); c >= 32 && c <= 126 || c == '\n'.toByte || c == '\r'.toByte || c == '\t'.toByte }) {
      i += 1
    }
    i == arr.length
  }

  private class BufferedLText(arr: Array[Byte]) extends RawType(arr) with LText {
    val (format, text): (Format.Value, String) = {
      val buf = wrapLEBB(data)
      var count = -1
      var format = Format.AsciiNoHeader
      if (data.length >= 4) {
        count = buf.getShort().toInt & 0xffff
        val cc = buf.getShort()
        cc match {
          case ControlChar16 => format = Format.Utf16
          case ControlChar8 => format = Format.Utf8
          case ControlChar0 => format = Format.Ascii
          case _ => buf.position(0); count = -1; format = Format.AsciiNoHeader
        }
      }
      if ((format == Format.AsciiNoHeader || format == Format.Ascii) && !isAsciiPrintable(data, buf.position())) {
        throw new DbpfDecodeFailedException("bytes contain non-printable ASCII characters")
      }
      val s = new String(data, buf.position(), data.length - buf.position(), Format.toCharset(format))
      if (count != -1 && s.length != count) {
        throw new DbpfDecodeFailedException(f"declared length was $count, expected ${s.length}")
      }
      (format, s)
    }
  }

  private class FreeLText(val text: String, val format: LText.Format.Value) extends LText {
    protected lazy val data: Array[Byte] = {
      val bytes = text.getBytes(Format.toCharset(format))
      if (format == Format.AsciiNoHeader) {
        bytes
      } else {
        val buf = allocLEBB(4 + bytes.length)
        buf.putShort(text.length.toShort)  // TODO support up to 200k characters
        buf.putShort(format match {
          case Format.Utf16 => ControlChar16
          case Format.Utf8 => ControlChar8
          case _ => ControlChar0
        })
        buf.put(bytes)
        assert(buf.remaining() == 0)
        buf.array()
      }
    }
  }
}
