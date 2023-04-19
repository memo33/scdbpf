package io.github.memo33
package scdbpf

import scala.collection.immutable._
import S3d._
import DbpfUtil._
import java.nio.ByteBuffer

private class FreeS3d(
  val vert: IndexedSeq[VertGroup],
  val indx: IndexedSeq[IndxGroup],
  val prim: IndexedSeq[PrimGroup],
  val mats: IndexedSeq[MatsGroup],
  val anim: AnimSection,
  val prop: IndexedSeq[PropGroup],
  val regp: IndexedSeq[RegpGroup]) extends S3d {

  protected lazy val data: Array[Byte] = {
    val buf = allocLEBB(binarySize)
    // header
    buf.putInt(MagicNumber.`3DMD`)
    buf.putInt(buf.limit)
    // head
    buf.putInt(HEAD)
    buf.putInt(12) // head size
    buf.putShort(1) // major
    buf.putShort(5) // minor

    def encodeCommon(sig: Int, groups: Seq[S3dGroup]): Unit = {
      buf.putInt(sig)
      buf.putInt(sectionSize(groups))
      buf.putInt(groups.size)
      groups foreach (_.encode(buf))
    }

    encodeCommon(VERT, vert)
    encodeCommon(INDX, indx)
    encodeCommon(PRIM, prim)
    encodeCommon(MATS, mats)

    // anim
    buf.putInt(ANIM)
    buf.putInt(sectionSize(anim))
    buf.putShort(anim.numFrames)
    buf.putShort(anim.frameRate)
    buf.putShort(anim.playMode.id.toShort)
    buf.putInt(0) // flags
    buf.putFloat(anim.displacement)
    buf.putShort(anim.size.toShort)
    anim foreach (_.encode(buf))

    encodeCommon(PROP, prop)
    encodeCommon(REGP, regp)

    assert(buf.remaining() == 0)
    buf.array
  }

}
