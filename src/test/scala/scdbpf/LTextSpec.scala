package io.github.memo33
package scdbpf

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LTextSpec extends AnyWordSpec with Matchers {

  import strategy.throwExceptions

  "LText" should {
    "encode/decode inversively" in {
      val ltext = LText("abcdefgäöüßéèčñçαβγℕℝ∂¬∨∧∡∥→∞∅⇒↦Θ⇔⇐")
      val e = BufferedEntry(Tgi.Blank, ltext: DbpfType, true)
      val ltext2 = e.content.convertTo(LText)
      assert(ltext ne ltext2)
      ltext.text should be (ltext2.text)
      ltext.dataView.toSeq should be (ltext2.dataView.toSeq)
    }
  }
}
