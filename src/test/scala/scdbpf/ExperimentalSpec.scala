package io.github.memo33
package scdbpf

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import Experimental._

class ExperimentalSpec extends AnyWordSpec with Matchers {

  "PreviewEffect" should {
    "be constructible from resource" in {
      val eff = PreviewEffect(0x12345678, "road_puzzlepiece002")
      import strategy.throwExceptions
      BufferedEntry(Tgi.Blank.copy(Tgi.EffDir), eff, compressed = true).toRawEntry
    }
  }
}
