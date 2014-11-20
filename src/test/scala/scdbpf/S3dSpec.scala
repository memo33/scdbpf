package scdbpf

import org.scalatest.{WordSpec, Matchers}
import collection.immutable._
import S3d._

class S3dSpec extends WordSpec with Matchers {

  // mock objects
  val vert = IndexedSeq(1,2,3).map(i => VertGroup(IndexedSeq.tabulate(10 * i)(j => Vert(j,j,j,0,0))))
  val indx = IndexedSeq(1,2,3).map(i => IndxGroup((0 until 10 * i).toIndexedSeq))
  val prim = IndexedSeq(1,2,3).map(i => PrimGroup(IndexedSeq.tabulate(10 * i)(j => Prim(PrimType.Triangle, 0, j))))
  val mats = IndexedSeq(1,2,3,4).map(i => (null: MatsGroup))

  "S3d models" should {
    "get trimmed correctly" in {
      val anim = AnimSection(0, 0, PlayMode.PingPong, 0, IndexedSeq(AnimGroup(None, 0,
        vertBlock = IndexedSeq(0), indxBlock = IndexedSeq(1), primBlock = IndexedSeq(2), matsBlock = IndexedSeq(3))))
      val model = S3d(vert, indx, prim, mats, anim, IndexedSeq.empty, IndexedSeq.empty).trim
      model.vert should have size 1
      model.indx should have size 1
      model.prim should have size 1
      model.mats should have size 1
      model.anim(0).vertBlock shouldBe IndexedSeq(0)
      model.anim(0).indxBlock shouldBe IndexedSeq(0)
      model.anim(0).primBlock shouldBe IndexedSeq(0)
      model.anim(0).matsBlock shouldBe IndexedSeq(0)
      model.vert(0) shouldBe vert(0)
      model.indx(0) shouldBe indx(1)
      model.prim(0) shouldBe prim(2)
    }
  }

}
