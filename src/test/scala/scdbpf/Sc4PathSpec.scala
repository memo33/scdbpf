package scdbpf

import org.scalatest.{WordSpec, Matchers}
import collection.immutable._
import Sc4Path._

class Sc4PathSpec extends WordSpec with Matchers {

  "Sc4Paths" should {
    "correctly renumber class numbers" in {
      def path(c: Int) = Path(None, TransportType.Car, c, if (c >= 10) Cardinal.North else Cardinal.East, Cardinal.South, false, Seq((0,0,0), (0,0,0)))
      def test(is: Seq[Int], expected: Seq[Int]) =
        Sc4Path(false, is map path, Nil).renumberClassNumbers.paths.map(_.classNumber) shouldBe expected
      test(Seq(10), Seq(0))
      test(Seq(10,11,13,11,10,11,13), 1 to 7)
      test(Seq(4,5,10), Seq(1,2,0))
      test(Seq(4,5,13,12,4,14), Seq(1,2,1,2,3,3))
      test(Seq(4,13), Seq(0,0))
    }

    "correctly validate against duplicate class numbers" in {
      def path(c: Int, tt: TransportType, dir: Cardinal = Cardinal.East) = Path(None, tt, c, dir, Cardinal.South, false, Seq((0,0,0), (0,0,0)))
      def stop(c: Int, tt: TransportType, uk: Boolean, dir: Cardinal = Cardinal.East) = StopPath(None, uk, tt, c, dir, Cardinal.Special, (0,0,0))
      import TransportType._
      Sc4Path(false, Seq(path(1, Car), path(1, Car))).validateClassNumbers shouldBe false
      Sc4Path(false, Seq(path(1, Car), path(2, Car))).validateClassNumbers shouldBe true
      Sc4Path(false, Seq(path(1, Car), path(1, Sim))).validateClassNumbers shouldBe true
      Sc4Path(false, Seq(path(1, Car), path(1, Car, dir = Cardinal.North))).validateClassNumbers shouldBe true
      Sc4Path(false, Seq(), Seq(stop(1, Car, false), stop(1, Car, false))).validateClassNumbers shouldBe false
      Sc4Path(false, Seq(), Seq(stop(1, Car, false), stop(2, Car, false))).validateClassNumbers shouldBe true
      Sc4Path(false, Seq(), Seq(stop(1, Car, false), stop(1, Car, true))).validateClassNumbers shouldBe true
      Sc4Path(false, Seq(), Seq(stop(1, Car, false), stop(1, Sim, false))).validateClassNumbers shouldBe true
      Sc4Path(false, Seq(), Seq(stop(1, Car, false), stop(1, Car, false, dir = Cardinal.North))).validateClassNumbers shouldBe true
    }

    "support custom decimal formatting" in {
      val path = Sc4Path(false, Seq(Path(None, TransportType.Car, 0, Cardinal.North, Cardinal.South, false, Seq((0,0,0), (0f,-16f,1f/3)))))
      path.toString.trim.linesWithSeparators.toSeq.last.stripLineEnd shouldBe "0.0,-16.0,0.33333334"
      path.copy(decFormat = Some(Sc4Path.threeDecimals)).toString.trim.linesWithSeparators.toSeq.last.stripLineEnd shouldBe "0,-16,0.333"
      val stop = Sc4Path(false, Seq(), Seq(StopPath(None, false, TransportType.Car, 0, Cardinal.North, Cardinal.Special, (0f,-16f,1f/3))))
      stop.toString.trim.linesWithSeparators.toSeq.last.stripLineEnd shouldBe "0.0,-16.0,0.33333334"
      stop.copy(decFormat = Some(Sc4Path.threeDecimals)).toString.trim.linesWithSeparators.toSeq.last.stripLineEnd shouldBe "0,-16,0.333"
    }
  }
}
