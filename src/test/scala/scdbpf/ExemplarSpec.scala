package scdbpf

import org.scalatest.{WordSpec, Matchers}
import DbpfProperty._
import passera.unsigned._

class ExemplarSpec extends WordSpec with Matchers {

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
        case Multi(Sint32(_)) => 0
        case Single(Sint32(_)) => 1
        case String(_) => 2
        case Multi(Uint32(_)) => 3
        case Single(Bool(_)) => 4
        case Single(Uint16(_)) => 5
        case Sint64(Single(_)) => 6
      }

      for ((p, i) <- propLists.zipWithIndex) {
        extract(p) should be (i)
      }
    }
  }

  import rapture.core.strategy.throwExceptions
  "Exemplar" should {
    "encode/decode inversively" in {
      val ex = Exemplar(isCohort = false, props = props)
      val be = BufferedEntry(Tgi.Blank.copy(Tgi.Exemplar), ex, true)
      val ex2 = be.toRawEntry.toBufferedEntry.convert[Exemplar].content

      assert(ex ne ex2)
      ex.parent shouldBe ex2.parent
      ex.isCohort shouldBe ex2.isCohort
      ex.properties shouldBe ex2.properties
      ex.dataView.deep shouldBe ex2.dataView.deep
    }
  }
}
