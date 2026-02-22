package io.github.memo33
package scdbpf

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LTextSpec extends AnyWordSpec with Matchers {

  import strategy.throwExceptions

  "LText" should {
    def check(ltext: LText) = {
      val e = BufferedEntry(Tgi.Blank, ltext: DbpfType, true).toRawEntry.toBufferedEntry
      val ltext2 = e.content.convertTo(LText)
      assert(ltext ne ltext2)
      ltext.text should be (ltext2.text)
      ltext.unsafeArray.toSeq should be (ltext2.unsafeArray.toSeq)
      ltext.format should be (ltext2.format)
    }
    "encode/decode inversely (UTF-16)" in {
      check(LText("abcdefgäöüßéèčñçαβγℕℝ∂¬∨∧∡∥→∞∅⇒↦Θ⇔⇐", format = LText.Format.Utf16))
      check(LText("", format = LText.Format.Utf16))
    }
    "encode/decode inversely (UTF-8)" in {
      check(LText("abcdefgäöüßéèčñçαβγℕℝ∂¬∨∧∡∥→∞∅⇒↦Θ⇔⇐", format = LText.Format.Utf8))
      check(LText("", format = LText.Format.Utf8))
    }
    "encode/decode inversely (ASCII)" in {
      check(LText("abcdefg0123XYZ~^_&", format = LText.Format.Ascii))
      check(LText("", format = LText.Format.Ascii))
    }
    "encode/decode inversely (ASCII-no-header)" in {
      check(LText("abcdefg0123XYZ~^_&", format = LText.Format.AsciiNoHeader))
      check(LText("", format = LText.Format.AsciiNoHeader))
    }
    "not decode non-printable ASCII characters" in {
      def except(raw: RawType) = {
        intercept[DbpfDecodeFailedException](raw.convertTo(LText)).getMessage.shouldBe("bytes contain non-printable ASCII characters")
      }
      except(RawType(Array[Byte](1, 0, 0, 0, 0)))
      except(RawType(Array[Byte](1, 0, 0, 0, 31)))
      except(RawType(Array[Byte](1, 0, 0, 0, 127)))
      except(RawType(Array[Byte](1, 0, 0, 0, 'ä'.toByte)))
      val l1 = RawType(Array[Byte](0, 0, 0, 0)).convertTo(LText)
      l1.format.shouldBe(LText.Format.Ascii)
      l1.text.shouldBe("")
      except(RawType(Array[Byte](31)))
      except(RawType(Array[Byte](127)))
      val l2 = RawType(Array[Byte](126)).convertTo(LText)
      l2.format.shouldBe(LText.Format.AsciiNoHeader)
      l2.text.shouldBe("~")
    }
    "honor maximum number of characters" in {
      intercept[DbpfDecodeFailedException](RawType(Array[Byte](0xff.toByte, 0xff.toByte, 0xff.toByte, 0x10)).convertTo(LText)).getMessage.shouldBe(
        "LText length exceeds maximum number of characters (16777215 > 200000)"
      )
      intercept[DbpfDecodeFailedException](RawType(Array[Byte](0x41, 0x0d, 0x03, 0x10)).convertTo(LText)).getMessage.shouldBe(
        "LText length exceeds maximum number of characters (200001 > 200000)"
      )
      intercept[DbpfDecodeFailedException](RawType(Array[Byte](0x40, 0x0d, 0x03, 0x10)).convertTo(LText)).getMessage.shouldBe(
        "declared length was 200000, expected 0"
      )
    }
  }
}
