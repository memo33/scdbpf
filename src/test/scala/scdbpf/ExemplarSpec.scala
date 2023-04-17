package io.github.memo33
package scdbpf

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import DbpfProperty._
import io.github.memo33.passera.unsigned._

class ExemplarSpec extends AnyWordSpec with Matchers {

  val propLists = Seq(
    Multi(1,2,3,4),
    Single(0),
    Single("foo"),
    Multi(5,6,7,8).map(UInt(_)),
    Single(true),
    Single(UShort(42)),
    Single(-1L))
  val props: Seq[Property] = propLists.zipWithIndex.map { case (p, id) => (UInt(id), p) }

  "Properties" should {
    "allow proper extraction" in {
      import ValueType._
      def extract(p: PropList) = p match {
        case Sint32(Multi(_)) => 0
        case Sint32(Single(_)) => 1
        case String(_) => 2
        case Uint32(Multi(_)) => 3
        case Bool(Single(_)) => 4
        case Uint16(Single(_)) => 5
        case Sint64(Single(_)) => 6
      }

      for ((p, i) <- propLists.zipWithIndex) {
        extract(p) should be (i)
      }
    }
  }

  import strategy.throwExceptions
  "Exemplar" should {
    "encode/decode inversively" in {
      val ex = Exemplar(isCohort = false, props = props)
      val be = BufferedEntry(Tgi.Blank.copy(Tgi.Exemplar), ex, true)
      val ex2 = be.toRawEntry.toBufferedEntry.convertContentTo(Exemplar).content

      assert(ex ne ex2)
      ex.parent shouldBe ex2.parent
      ex.isCohort shouldBe ex2.isCohort
      ex.properties shouldBe ex2.properties
      ex.dataView.deep shouldBe ex2.dataView.deep
    }
  }
}
