package scdbpf

import scala.collection.immutable._
import S3d._
import DbpfUtil._
import java.nio.ByteBuffer


trait S3d extends DbpfType {
  // only 1.5 encoding supported anyway
  //val majorVersion: Short
  //val minorVersion: Short

  val vert: IndexedSeq[VertGroup]
  val indx: IndexedSeq[IndxGroup]
  val prim: IndexedSeq[PrimGroup]
  val mats: IndexedSeq[MatsGroup]
  val anim: AnimSection
  val prop: IndexedSeq[PropGroup]
  val regp: IndexedSeq[RegpGroup]

  def copy(
    vert: IndexedSeq[VertGroup] = vert,
    indx: IndexedSeq[IndxGroup] = indx,
    prim: IndexedSeq[PrimGroup] = prim,
    mats: IndexedSeq[MatsGroup] = mats,
    anim: AnimSection = anim,
    prop: IndexedSeq[PropGroup] = prop,
    regp: IndexedSeq[RegpGroup] = regp): S3d =
      new FreeS3d(vert, indx, prim, mats, anim, prop, regp)

  private[scdbpf] def binarySize: Int = {
    List(vert, indx, prim, mats, anim, prop, regp).foldLeft(20)(_ + sectionSize(_))
  }
  private[scdbpf] def sectionSize(groups: Seq[S3dGroup]): Int = groups match {
    case a: AnimSection => a.foldLeft(24)(_ + _.binarySize)
    case _ => groups.foldLeft(12)(_ + _.binarySize)
  }
}

object S3d {

  private[scdbpf] val HEAD = MagicNumber.fromString("HEAD")
  private[scdbpf] val VERT = MagicNumber.fromString("VERT")
  private[scdbpf] val INDX = MagicNumber.fromString("INDX")
  private[scdbpf] val PRIM = MagicNumber.fromString("PRIM")
  private[scdbpf] val MATS = MagicNumber.fromString("MATS")
  private[scdbpf] val ANIM = MagicNumber.fromString("ANIM")
  private[scdbpf] val PROP = MagicNumber.fromString("PROP")
  private[scdbpf] val REGP = MagicNumber.fromString("REGP")

  implicit val converter = new Converter[DbpfType, S3d] {
    def apply(from: DbpfType): S3d = {
      try {
        new BufferedS3d(from.dataView)
      } catch {
        case e @ (_: NoSuchElementException
                 |_: java.nio.BufferUnderflowException
                 |_: IllegalArgumentException
                 |_: IndexOutOfBoundsException) =>
          throw new DbpfDecodeFailedException(e.toString, e)
      }
    }
  }

  def apply(
    vert: IndexedSeq[VertGroup],
    indx: IndexedSeq[IndxGroup],
    prim: IndexedSeq[PrimGroup],
    mats: IndexedSeq[MatsGroup],
    anim: AnimSection,
    prop: IndexedSeq[PropGroup],
    regp: IndexedSeq[RegpGroup]): S3d =
      new FreeS3d(vert, indx, prim, mats, anim, prop, regp)

  object PlayMode extends Enumeration {
    val PingPong = Value(1)
    val OneShot = Value(2)
    val Looping = Value(3)
  }

  object PrimType extends Enumeration {
    val Triangle, TriangleStrip, TriangleFan = Value
    val Quad = Value(6)
    val QuadStrip = Value(7)
  }

  object MatsFlags extends Enumeration {
    val AlphaTest = Value(0)
    val DepthTest = Value(1)
    val BackfaceCulling = Value(3)
    val FrameBufferBlending = Value(4)
    val Texturing = Value(5)
    val ColorWrites = Value(6)
    val DepthWrites = Value(7)
  }

  object MatsFunc extends Enumeration {
    val Never, Less, Equal, LessEqual, Greater, Unequal, GreaterEqual, Always = Value
  }

  object MatsBlend extends Enumeration {
    val Zero, One, SourceColor, OneMinusSourceColor, SourceAlpha, OneMinusSourceAlpha = Value
    val DestColor = Value(8)
    val OneMinusDestColor = Value(9)
  }

  object WrapMode extends Enumeration {
    val Clamp = Value(2)
    val Repeat = Value(3)
  }

  object MagnifFilter extends Enumeration {
    val Nearest, Bilinear = Value
  }

  object MinifFilter extends Enumeration {
    val NearestNoMipmap, BilinearNoMipmap, NearestMipmapNearest, LinearMipmapNearest, NearestMipmapLinear, LinearMipmapLinear = Value
  }

  private[scdbpf] trait S3dGroup {
    private[scdbpf] def binarySize: Int
    private[scdbpf] def encode(buf: ByteBuffer): Unit
  }

  case class Vert(x: Float, y: Float, z: Float, u: Float, v: Float) extends IndexedSeq[Float] {
    def length = 5
    def apply(idx: Int) = productElement(idx).asInstanceOf[Float]
  }

  case class VertGroup(verts: IndexedSeq[Vert]) extends IndexedSeqProxy(verts) with S3dGroup {
    override def stringPrefix: String = "VertGroup"
    private[scdbpf] def binarySize = 8 + verts.size * 20 // assumes format 0x80004001
    private[scdbpf] def encode(buf: ByteBuffer): Unit = {
      buf.putShort(0) // flags
      buf.putShort(verts.size.toShort)
      buf.putInt(VertFormat)
      for (v <- verts; f <- v) {
        buf.putFloat(f)
      }
    }
  }

  case class IndxGroup(indxs: IndexedSeq[Int]) extends IndexedSeqProxy(indxs) with S3dGroup {
    override def stringPrefix: String = "IndxGroup"
    private[scdbpf] def binarySize = 6 + indxs.size * 2 // assumes stride 2
    private[scdbpf] def encode(buf: ByteBuffer): Unit = {
      buf.putShort(0) // flags
      buf.putShort(IndxStride)
      buf.putShort(indxs.size.toShort)
      for (i <- indxs) {
        buf.putShort(i.toShort)
      }
    }
  }

  case class Prim(primType: PrimType.Value, firstIndx: Int, numIndxs: Int)

  case class PrimGroup(prims: IndexedSeq[Prim]) extends IndexedSeqProxy(prims) with S3dGroup {
    override def stringPrefix: String = "PrimGroup"
    private[scdbpf] def binarySize = 2 + prims.size * 12
    private[scdbpf] def encode(buf: ByteBuffer): Unit = {
      buf.putShort(prims.size.toShort)
      for (p <- prims) {
        buf.putInt(p.primType.id)
        buf.putInt(p.firstIndx)
        buf.putInt(p.numIndxs)
      }
    }
  }

  case class MatsGroup(
    flags: MatsFlags.ValueSet,
    alphaFunc: MatsFunc.Value,
    depthFunc: MatsFunc.Value,
    sourceBlend: MatsBlend.Value,
    destBlend: MatsBlend.Value,
    alphaThreshold: Short,
    matClass: Int,
    reserved: Byte,
    textureCount: Byte,
    iid: Int,
    wrapU: WrapMode.Value,
    wrapV: WrapMode.Value,
    magFilter: MagnifFilter.Value,
    minFilter: MinifFilter.Value,
    animRate: Short,
    animMode: Short,
    name: Option[String]
  ) extends S3dGroup {

    require(name forall (_.length < 255), "max name length is 254")

    private[scdbpf] def binarySize = 29 + stringLength(name) // assumes version >= 1.5
    private[scdbpf] def encode(buf: ByteBuffer): Unit = {    // assumes version >= 1.5
      buf.putInt(flags.toBitMask(0).toInt)
      buf.put(alphaFunc.id.toByte)
      buf.put(depthFunc.id.toByte)
      buf.put(sourceBlend.id.toByte)
      buf.put(destBlend.id.toByte)
      buf.putShort(alphaThreshold)
      buf.putInt(matClass)
      buf.put(reserved)
      buf.put(textureCount)
      buf.putInt(iid)
      buf.put(wrapU.id.toByte)
      buf.put(wrapV.id.toByte)
      buf.put(magFilter.id.toByte)
      buf.put(minFilter.id.toByte)
      buf.putShort(animRate)
      buf.putShort(animMode)
      putString(buf, name)
    }
  }

  case class AnimGroup(
    name: Option[String],
    flags: Int,
    vertBlock: IndexedSeq[Int],
    indxBlock: IndexedSeq[Int],
    primBlock: IndexedSeq[Int],
    matsBlock: IndexedSeq[Int]
  ) extends S3dGroup {

    require(name forall (_.length < 255), "max name length is 254")
    require(vertBlock.size == indxBlock.size && vertBlock.size == primBlock.size &&
      vertBlock.size == matsBlock.size, "blocks must be of equal size")

    private[scdbpf] def binarySize = 2 + stringLength(name) + vertBlock.size * 8
    private[scdbpf] def encode(buf: ByteBuffer): Unit = {
      putString(buf, name, asShort = true)
      for (i <- 0 until vertBlock.size) {
        buf.putShort(vertBlock(i).toShort)
        buf.putShort(indxBlock(i).toShort)
        buf.putShort(primBlock(i).toShort)
        buf.putShort(matsBlock(i).toShort)
      }
    }
  }

  case class AnimSection(
    numFrames: Short,
    frameRate: Short,
    playMode: PlayMode.Value,
    displacement: Float,
    groups: IndexedSeq[AnimGroup]) extends IndexedSeqProxy(groups)

  case class PropGroup(
    meshIndex: Short,
    frameIndex: Short,
    assignmentType: String,
    assignedValue: String
  ) extends S3dGroup {

    require(assignmentType.length < 255 && assignedValue.length < 255, "max name length is 254")

    private[scdbpf] def binarySize = 8 + assignmentType.length + assignedValue.length
    private[scdbpf] def encode(buf: ByteBuffer): Unit = {
      buf.putShort(meshIndex)
      buf.putShort(frameIndex)
      putString(buf, Some(assignmentType))
      putString(buf, Some(assignedValue))
    }
  }

  case class Translation(x: Float, y: Float, z: Float)
  case class Orientation(x: Float, y: Float, z: Float, w: Float)

  case class RegpGroup(
    name: String,
    translations: IndexedSeq[Translation],
    orientations: IndexedSeq[Orientation]
  ) extends S3dGroup {

    require(name.length < 255, "max name length is 254")
    require(translations.size == orientations.size, "blocks must be of equal size")

    private[scdbpf] def binarySize = 4 + name.length + translations.size * 7 * 4
    private[scdbpf] def encode(buf: ByteBuffer): Unit = {
      putString(buf, Some(name))
      buf.putShort(translations.size.toShort)
      for (i <- 0 until translations.size; t = translations(i); o = orientations(i)) {
        buf.putFloat(t.x)
        buf.putFloat(t.y)
        buf.putFloat(t.z)
        buf.putFloat(o.x)
        buf.putFloat(o.y)
        buf.putFloat(o.z)
        buf.putFloat(o.w)
      }
    }
  }

  private[scdbpf] val VertFormat: Int = 0x80004001
  private[scdbpf] val IndxStride: Short = 2

  private[S3d] class IndexedSeqProxy[A](seq: IndexedSeq[A]) extends IndexedSeq[A] {
    def length = seq.length
    def apply(idx: Int) = seq(idx)
  }

  private def putString(buf: ByteBuffer, name: Option[String], asShort: Boolean = false): Unit = {
    //val pos = buf.position
    if (name.isEmpty) {
      if (asShort) buf.putShort(0) else buf.put(0: Byte) // string length
    } else {
      buf.put((name.get.length + 1).toByte)
      if (asShort) { // intermediate empty byte
        buf.put(0: Byte)
      }
      buf.put(name.get.getBytes(asciiEncoding))
      buf.put(0: Byte) // string terminator
    }
    //assert(buf.position == pos + 1 + stringLength(name) + (if (asShort) 1 else 0))
  }

  private def stringLength(s: Option[String]): Int = s match {
    case None => 0
    case Some(s) => 1 + s.length
  }

}
