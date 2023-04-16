package scdbpf

import org.scalatest.{WordSpec, Matchers}

class LTextSpec extends WordSpec with Matchers {

  import strategy.throwExceptions

  "LText" should {
    "encode/decode inversively" in {
      val ltext = LText("abcdefgäöüßéèčñçαβγℕℝ∂¬∨∧∡∥→∞∅⇒↦Θ⇔⇐")
      val e = BufferedEntry(Tgi.Blank, ltext: DbpfType, true)
      val ltext2 = e.content.convertTo(LText)
      assert(ltext ne ltext2)
      ltext.text should be (ltext2.text)
      ltext.dataView.deep should be (ltext2.dataView.deep)
    }
  }
}
