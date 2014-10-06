package scdbpf

import org.scalatest.{WordSpec, Matchers}
import DbpfUtil._

class DbpfUtilSpec extends WordSpec with Matchers {

  "toHex" should {
    "pass test cases" in {
      toHex(0) should be ("0x00000000")
      toHex(0xA) should be ("0x0000000A")
      toHex(0x7fffffff) should be ("0x7FFFFFFF")
      toHex(0x80000000) should be ("0x80000000")
      toHex(0xFFFFFFFF) should be ("0xFFFFFFFF")
    }
  }

  import Sc4Path.Cardinal
  import Sc4Path.Cardinal._
  import Dihedral._
  import RotFlip._
  "Dihedral" should {
    "rotate Cardinals correctly" in {
      North *: R1F0 should be (East)
      North *: R2F0 should be (South)
      North *: R3F1 should be (East)
      South *: R2F0 should be (North)
      Special *: R2F0 should be (Special)
    }
  }

  import scala.language.implicitConversions
  implicit def value2val(v: RotFlip.Value): RotFlip = v.asInstanceOf[RotFlip]

  "RotFlip" should {
    "multiply associatively" in {
      for (a <- RotFlip.values;
          b <- RotFlip.values;
          c <- RotFlip.values) {
        (a * b) * c should be (a * (b * c))
      }
    }
    "invert correctly" in {
      for (a <- RotFlip.values) {
        a / a should be (R0F0)
      }
    }
    "have proper neutral element" in {
      for (a <- RotFlip.values) {
        a * R0F0 should be (a)
        R0F0 * a should be (a)
      }
    }
    "multiply correctly" in {
      R1F0 * R1F0 should be (R2F0)
      R1F0 * R2F0 should be (R3F0)
      R1F0 * R3F0 should be (R0F0)
      R1F0 * R2F1 should be (R3F1)
      R2F0 * R2F1 should be (R0F1)
      R0F1 * R2F1 should be (R2F0)
      R1F1 * R2F1 should be (R3F0)
      R2F1 * R2F1 should be (R0F0)
      R3F1 * R2F1 should be (R1F0)
      R1F1 * R1F1 should be (R0F0)
    }
  }

  "Dihedral" should {
    "allow extensions" in {
      case class DihOrientation(val x: Int, val y: Int) { override def toString = s"($x,$y)" }
      implicit object DihOrientationIsDihedral extends Dihedral[DihOrientation, Int] {
        def x(a: DihOrientation): Int = a.x
        def y(a: DihOrientation): Int = a.y
        def build(from: DihOrientation, x: Int, y: Int) = new DihOrientation(x, y)
      }
      val orient = DihOrientation(1, -1)
      orient *: R1F0 should be (DihOrientation(-1, -1))
    }
  }

}
