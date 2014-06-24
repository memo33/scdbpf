package scdbpf

import Fsh._
import org.scalatest.{Matchers, WordSpec, PrivateMethodTester}
import ps.tricerato.pureimage._

class FshSpec extends WordSpec with Matchers with PrivateMethodTester {

  val interpolate = PrivateMethod[Int]('interpolate)
  val toRGBA = PrivateMethod[RGBA]('toRGBA)
  val fromRGBA = PrivateMethod[Int]('fromRGBA)

  "conversions" should {
    "respect max and min values" in {
      import conversions._
      conversions invokePrivate interpolate(16, 256, 15) should be (255)
      conversions invokePrivate interpolate(1 << 4, 1 << 8, 15 & (1 << 4) - 1) should be (255)

      short4444toRGBA(15) should be (rgbaFromChannels(0,0,255,0))
      short4444toRGBA(15 << 4) should be (rgbaFromChannels(0,255,0,0))
      rgbaToShort4444(rgbaFromChannels(0,255,0,0)) should be ((15 << 4).toShort)

      short0565toRGBA(31) should be (rgbaFromChannels(0,0,255,255))
      rgbaToShort0565(rgbaFromChannels(0,0,255,0)) should be (31.toShort)
    }
  }
}
