package io.github.memo33
package scdbpf

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import collection.immutable._
import S3d._

class S3dSpec extends AnyWordSpec with Matchers {

  // mock objects
  val vert = IndexedSeq(1,2,3).map(i => VertGroup(IndexedSeq.tabulate(10 * i)(j => Vert(j,j,j,0,0))))
  val indx = IndexedSeq(1,2,3).map(i => IndxGroup((0 until 10 * i).toIndexedSeq))
  val prim = IndexedSeq(1,2,3).map(i => PrimGroup(IndexedSeq.tabulate(10 * i)(j => Prim(PrimType.Triangle, 0, j))))
  val mats = IndexedSeq(0,1,2,3).map(i =>
      MatsGroup(flags = MatsFlags.ValueSet.empty, alphaThreshold = Short.MaxValue, materials = IndexedSeq.fill(i) {
        Material(i * 0x100, WrapMode.Clamp, WrapMode.Repeat, MagnifFilter.Nearest, MinifFilter.NearestNoMipmap,
          name = if (i >= 2) None else Some(i.toString))
      }))

  "S3d models" should {
    val anim = AnimSection(1, 0, PlayMode.PingPong, 0, IndexedSeq(AnimGroup(None, 0,
      vertBlock = IndexedSeq(0), indxBlock = IndexedSeq(1), primBlock = IndexedSeq(2), matsBlock = IndexedSeq(3))))
    "get trimmed correctly" in {
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
    "be encoded and decoded properly" in {
      import strategy.throwExceptions
      val model = S3d(vert, indx, prim, mats, anim, IndexedSeq.empty, IndexedSeq.empty)
      val raw = BufferedEntry(Tgi.Blank, model, compressed = true).toRawEntry
      val model2 = raw.toBufferedEntry.convert[S3d].content
      model.vert shouldBe model2.vert
      model.indx shouldBe model2.indx
      model.prim shouldBe model2.prim
      model.mats shouldBe model2.mats
      model.anim shouldBe model2.anim
      model.regp shouldBe model2.regp
      model.prop shouldBe model2.prop
    }
  }

}
