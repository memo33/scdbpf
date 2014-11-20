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
  }
}
