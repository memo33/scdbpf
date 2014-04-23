package scdbpf

import scala.collection.immutable._
import S3d._
import BufferedS3d._
import DbpfUtil._
import java.nio.ByteBuffer


private class BufferedS3d(arr: Array[Byte]) extends RawType(arr) with S3d {
  private[this] val buf = wrapLEBB(data)

  decodeCommonHead(buf, MagicNumber.`3DMD`, "3DMD")
  decodeCommonHead(buf, HEAD, "HEAD")

  val majorVersion = buf.getShort()
  val minorVersion = buf.getShort()
  if (majorVersion != 1 || minorVersion != 3 && minorVersion != 5)
    throw new DbpfDecodeFailedException(f"S3D version was ${majorVersion}.${minorVersion}, supported are 1.3 and 1.5 only")

  val vert = decodeVertSection(buf)
  val indx = decodeIndxSection(buf)
  val prim = decodePrimSection(buf)
  val mats = decodeMatsSection(buf, minorVersion)
  val anim = decodeAnimSection(buf)
  val prop = decodePropSection(buf)
  val regp = decodeRegpSection(buf)
  if (buf.remaining() != 0) {
    throw new DbpfDecodeFailedException(s"End of S3D buffer was not reached: ${buf.remaining()}")
  }
  if (minorVersion == 5) {
    assert(data.length == this.binarySize, s"data.length ${data.length}, binary size ${this.binarySize}")
    //if (data.length != this.binarySize) {
    //  val c = this.copy()
    //  val g1 = data map (b => f"$b%02X") grouped 16 map (x => x mkString " ")
    //  val g2 = c.dataView map (b => f"$b%02X") grouped 16 map (x => x mkString " ")
    //  g1.zipAll(g2, "         ", "        ") map (tup => tup._1 + " | " + tup._2) foreach println
    //  assert(false)
    //}
  }
}

private object BufferedS3d {

  private def getString(buf: ByteBuffer, len: Int): Option[String] = {
    if (len == 0) {
      None
    } else {
      val arr = new Array[Byte](len - 1)
      buf.get(arr)
      val term = buf.get()
      if (term != 0)
        throw new DbpfDecodeFailedException(f"String terminator was 0x$term%02X")
      Some(new String(arr, asciiEncoding))
    }
  }

  private def decodeCommonHead(buf: ByteBuffer, signature: Int, name: String): Unit = {
    val sig = buf.getInt()
    if (sig != signature)
      throw new DbpfDecodeFailedException(f"$name signature was 0x$sig%08X")
    val size = buf.getInt() // unused
  }

  private def decodeCommon(buf: ByteBuffer, signature: Int, name: String): Int = {
    decodeCommonHead(buf, signature, name)
    val numGroups = buf.getInt()
    if (numGroups < 0)
      throw new DbpfDecodeFailedException(f"$name group number was negative $numGroups")
    numGroups
  }

  private def decodeVertSection(buf: ByteBuffer): IndexedSeq[VertGroup] = {
    val numGroups = decodeCommon(buf, VERT, "VERT")
    for (_ <- 1 to numGroups) yield {
      val flags = buf.getShort() // unused
      if (flags != 0) throw new DbpfDecodeFailedException(f"Unknown flags were not 0: 0x$flags%04X")
      val numVerts = buf.getShort()
      val format = buf.getInt()
      if (format != VertFormat && (format & 0xFFFF) != 2) // 2 represents the same vertex format
        throw new DbpfDecodeFailedException(f"Unknown vert format 0x$format%08X")

      val verts = for (_ <- 1 to numVerts) yield {
        Vert(buf.getFloat(), buf.getFloat(), buf.getFloat(), buf.getFloat(), buf.getFloat())
      }
      VertGroup(verts)
    }
  }

  private def decodeIndxSection(buf: ByteBuffer): IndexedSeq[IndxGroup] = {
    val numGroups = decodeCommon(buf, INDX, "INDX")
    for (_ <- 1 to numGroups) yield {
      val flags = buf.getShort() // unused
      if (flags != 0) throw new DbpfDecodeFailedException(f"Unknown flags were not 0: 0x$flags%04X")
      val stride = buf.getShort()
      if (stride != IndxStride)
        throw new DbpfDecodeFailedException(f"Unknown indx stride 0x$stride%04X")
      val numIndxs = buf.getShort()

      val indxs = for (_ <- 1 to numIndxs) yield {
        buf.getShort() & 0xFFFF
      }
      IndxGroup(indxs)
    }
  }

  private def decodePrimSection(buf: ByteBuffer): IndexedSeq[PrimGroup] = {
    val numGroups = decodeCommon(buf, PRIM, "PRIM")
    for (_ <- 1 to numGroups) yield {
      val numSubGroups = buf.getShort()
      val subs = for (_ <- 1 to numSubGroups) yield {
        val primType = PrimType(buf.getInt())
        val first = buf.getInt()
        val numIndxs = buf.getInt()
        Prim(primType, first, numIndxs)
      }
      PrimGroup(subs)
    }
  }

  private def decodeMatsSection(buf: ByteBuffer, minor: Int): IndexedSeq[MatsGroup] = {
    val numGroups = decodeCommon(buf, MATS, "MATS")
    for (_ <- 1 to numGroups) yield {
      MatsGroup(
        flags = MatsFlags.ValueSet.fromBitMask(Array(buf.getInt() & 0xFFFFFFFFL)),
        alphaFunc = MatsFunc(buf.get()),
        depthFunc = MatsFunc(buf.get()),
        sourceBlend = MatsBlend(buf.get()),
        destBlend = MatsBlend(buf.get()),
        alphaThreshold = buf.getShort(),
        matClass = buf.getInt(),
        reserved = buf.get(),
        textureCount = buf.get(),
        iid = buf.getInt(),
        wrapU = WrapMode(buf.get()),
        wrapV = WrapMode(buf.get()),
        magFilter = MagnifFilter(if (minor < 5) (0: Byte) else buf.get()),
        minFilter = MinifFilter(if (minor < 5) (0: Byte) else buf.get()),
        animRate = buf.getShort(),
        animMode = buf.getShort(),
        name = getString(buf, buf.get() & 0xFF)
      )
    }
  }

  private def decodeAnimSection(buf: ByteBuffer): AnimSection = {
    decodeCommonHead(buf, ANIM, "ANIM")
    val numFrames = buf.getShort()
    val frameRate = buf.getShort()
    val playMode = PlayMode(buf.getShort())
    val flags = buf.getInt() // unused
    if (flags != 0) throw new DbpfDecodeFailedException(f"Unknown flags were not 0: 0x$flags%08X")
    val displacement = buf.getFloat()
    val numGroups = buf.getShort()

    val groups = for (_ <- 1 to numGroups) yield {
      val nameLen = buf.get() & 0xFF
      val groupFlags = buf.get()
      val name = getString(buf, nameLen)
      val blockIndices = for (_ <- 1 to numFrames) yield {
        // vert, indx, prim, mats
        Seq(buf.getShort(), buf.getShort(), buf.getShort(), buf.getShort()) map (_ & 0xFFFF)
      }
      val trans = blockIndices.transpose
      AnimGroup(name, groupFlags, trans(0), trans(1), trans(2), trans(3))
    }
    AnimSection(numFrames, frameRate, playMode, displacement, groups)
  }

  private def decodePropSection(buf: ByteBuffer): IndexedSeq[PropGroup] = {
    val numGroups = decodeCommon(buf, PROP, "PROP")
    for (_ <- 1 to numGroups) yield {
      val meshIndex = buf.getShort()
      val frameIndex = buf.getShort()
      val assignmentType = getString(buf, buf.get() & 0xFF)
      val assignedValue = getString(buf, buf.get() & 0xFF)
      if (assignmentType.isEmpty) {
        throw new DbpfDecodeFailedException("PROP assignment type was empty")
      } else if (assignedValue.isEmpty) {
        throw new DbpfDecodeFailedException("PROP assigned value was empty")
      } else {
        PropGroup(meshIndex, frameIndex, assignmentType.get, assignedValue.get)
      }
    }
  }

  private def decodeRegpSection(buf: ByteBuffer): IndexedSeq[RegpGroup] = {
    val numGroups = decodeCommon(buf, REGP, "REGP")
    for (_ <- 1 to numGroups) yield {
      val name = getString(buf, buf.get() & 0xFF)
      if (name.isEmpty)
        throw new DbpfDecodeFailedException("REGP effect name was empty")
      val numSubGroups = buf.getShort()
      val subGroups = for (_ <- 1 to numSubGroups) yield {
        val trans = Translation(buf.getFloat(), buf.getFloat(), buf.getFloat())
        val orient = Orientation(buf.getFloat(), buf.getFloat(), buf.getFloat(), buf.getFloat())
        (trans, orient)
      }
      RegpGroup(name.get, subGroups.map(_._1), subGroups.map(_._2))
    }
  }
}
