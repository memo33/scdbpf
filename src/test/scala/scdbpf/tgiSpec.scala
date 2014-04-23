package scdbpf

import org.scalatest.{WordSpec, Matchers}
import Tgi._

class TgiSpec extends WordSpec with Matchers {

  "TGI" should {

    "allow basic operations" in {
      val tgi = Tgi(4, 2, 3)
      val mask = TgiMask(4, None, None)
      val mask2 = TgiMask(5, None, None)
      tgi matches Tgi.Directory should be (false)
      tgi matches Tgi.Cohort should be (false)
      tgi matches mask should be (true)
      tgi.copy(mask2) matches mask2 should be (true)
      tgi.copy(mask2) matches tgi.copy(tid = 5) should be (true)
      tgi matches mask.copy(gid = Some(2), iid= Some(3)) should be (true)
    }

    "have implicit ITG-Ordering" in {
      val l = List(
        Tgi(0,0,0),
        Tgi(0xffffffff,0,0),
        Tgi(0xffffffff,0xffffffff,0),
        Tgi(0,0,0x7fffffff),
        Tgi(0,0,0x80000000),
        Tgi(0,0,0xffffffff),
        Tgi(0xffffffff,0xffffffff,0xffffffff))
      l should be (l.sorted)
    }

    "support explicit TIG-Ordering" in {
      val l = List(
        Tgi(0,0,0),
        Tgi(0,0,0x7fffffff),
        Tgi(0,0,0x80000000),
        Tgi(0,0,0xffffffff),
        Tgi(0xffffffff,0,0),
        Tgi(0xffffffff,0xffffffff,0),
        Tgi(0xffffffff,0xffffffff,0xffffffff))
      l should be (l.sorted(Tgi.tigOrdering))
    }

    "have properly ordered labels" in {
      val masks = Tgi.LabeledTgis.values.toSeq map (_.asInstanceOf[LabeledTgi])
      for (Seq(a,b) <- masks.combinations(2)) {
        withClue(s"$a $b") {
          def asTgi(m: LabeledTgi) = m match {
            case m: TgiMask => Tgi.Blank.copy(m).copy(iid = 1)
            case t: Tgi => t
          }
          asTgi(a).label should not be (asTgi(b).label)
        }
      }
    }
  }
}
