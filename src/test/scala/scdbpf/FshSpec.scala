package io.github.memo33
package scdbpf

import Fsh._
import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import compat.RGBA

class FshSpec extends AnyWordSpec with Matchers with PrivateMethodTester {

  val interpolate = PrivateMethod[Int]('interpolate)
  val toRGBA = PrivateMethod[RGBA]('toRGBA)
  val fromRGBA = PrivateMethod[Int]('fromRGBA)

  private def interpolateOld(sup: Int, newSup: Int, v: Int): Int = {
    if (newSup <= sup)
      v * newSup / sup
    else if (sup == 1)
      newSup - 1
    else {
      val v2 = v * (newSup / sup)
      v2 + v2 / sup
    }
  }

  "conversions" should {
    "respect max and min values" in {
      import conversions._
      conversions invokePrivate interpolate(4, 8, 15) should be (255)
      conversions invokePrivate interpolate(4, 8, 15 & (1 << 4) - 1) should be (255)

      short4444toRGBA(15) should be (rgbaFromChannels(0,0,255,0))
      short4444toRGBA(15 << 4) should be (rgbaFromChannels(0,255,0,0))
      rgbaToShort4444(rgbaFromChannels(0,255,0,0)) should be ((15 << 4).toShort)

      short0565toRGBA(31) should be (rgbaFromChannels(0,0,255,255))
      rgbaToShort0565(rgbaFromChannels(0,0,255,0)) should be (31.toShort)
    }
    "interpolate symmetrically" in {
      import conversions._
      for (e <- 4 to 7) {
        val vs = (0 until 1 << e).map(conversions invokePrivate interpolate(e, 8, _))
        vs shouldBe vs.reverse.map(v => 255 - v)
      }
    }
    "agree with old implementation" in {
      for (e <- 4 to 7) {
        for (i <- 0 until 1 << e) {
          conversions invokePrivate interpolate(e, 8, i) shouldBe interpolateOld(1 << e, 1 << 8, i)
        }
        for (i <- 0 until 1 << 8) {
          conversions invokePrivate interpolate(8, e, i) shouldBe interpolateOld(1 << 8, 1 << e, i)
        }
      }
      for (e <- 1 to 8) {
        conversions invokePrivate interpolate(e, 0, 0) shouldBe interpolateOld(1 << e, 1, 0)
      }
    }
  }
}
