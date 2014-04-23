package scdbpf

import org.scalatest.{WordSpec, Matchers}
import rapture.io._

class LTextSpec extends WordSpec with Matchers {

  import strategy.throwExceptions

  "LText" should {
    "encode/decode inversively" in {
      val ltext = LText("abcdefgäöüßéèčñçαβγℕℝ∂¬∨∧∡∥→∞∅⇒↦Θ⇔⇐")
      val e = BufferedEntry(Tgi.Blank, ltext: DbpfType, true)
      val ltext2 = e.convert[LText].content
      assert(ltext ne ltext2)
      ltext.text should be (ltext2.text)
      ltext.dataView.deep should be (ltext2.dataView.deep)
    }
  }
}
