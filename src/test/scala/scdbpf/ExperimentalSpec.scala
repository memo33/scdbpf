package scdbpf

import org.scalatest.{WordSpec, Matchers}
import Experimental._

class ExperimentalSpec extends WordSpec with Matchers {

  "PreviewEffect" should {
    "be constructible from resource" in {
      val eff = PreviewEffect(0x12345678, "road_puzzlepiece002")
      import strategy.throwExceptions
      BufferedEntry(Tgi.Blank.copy(Tgi.EffDir), eff, compressed = true).toRawEntry
    }
  }
}
