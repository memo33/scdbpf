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

  private[this] def reversedIndx =
    if (prim.exists(_.exists(_.primType != PrimType.Triangle)))
      throw new UnsupportedOperationException("currently, normals of models consisting of polygons other than triangles cannot be flipped")
    else
      indx map (ig => IndxGroup(ig.grouped(3).flatMap(_.reverse).toIndexedSeq))

  /** Flips the normals by reversing the order of every triangle in the
    * PrimGroups. Currently, only supported for models consisting of triangles.
    * @throws UnsupportedOperationException if PrimGroup has other than
    * triangles.
    */
  def withNormalsFlipped: S3d = copy(indx = reversedIndx)

  /** Rotates and flips the model. If it is flipped, the normals will be flipped
    * back automatically. Currently only supported for models without RegpGroup,
    * and flipping is only supported for models consisting of triangles.
    * @throws UnsupportedOperationException if model has a non-empty RegpGroup
    * or if rf.flipped and PrimGroup has other than triangles.
    */
  def * (rf: RotFlip): S3d = {
    if (rf == RotFlip.R0F0) this
    else if (regp.nonEmpty) throw new UnsupportedOperationException("currently, models with a REGP group cannot be rotated")
    else copy(vert = vert map (_ map (_ *: rf)),
              indx = if (!rf.flipped) indx else reversedIndx)
  }

  /** Removes Mats, Indx, Prim and Vert groups that are unused, i.e. are not
    * referenced in the Anim section. It does not perform a deep check, i.e. to
    * the individual vertices that might be redundant.
    */
  def trim = {
    def reindex(is: Iterator[Int]) = is.to[SortedSet].zipWithIndex.toMap
    def filtered[G](gs: IndexedSeq[G], reindex: Map[Int, Int]) =
      gs.zipWithIndex collect { case (g, i) if reindex.contains(i) => g }

    val reindexVert = reindex(anim.iterator.flatMap(_.vertBlock))
    val reindexIndx = reindex(anim.iterator.flatMap(_.indxBlock))
    val reindexPrim = reindex(anim.iterator.flatMap(_.primBlock))
    val reindexMats = reindex(anim.iterator.flatMap(_.matsBlock))

    val animReindexed = anim.copy(groups = anim map { ag => ag.copy(
      vertBlock = ag.vertBlock map reindexVert,
      indxBlock = ag.indxBlock map reindexIndx,
      primBlock = ag.primBlock map reindexPrim,
      matsBlock = ag.matsBlock map reindexMats)
    })

    this.copy(
      vert = filtered(vert, reindexVert),
      indx = filtered(indx, reindexIndx),
      prim = filtered(prim, reindexPrim),
      mats = filtered(mats, reindexMats),
      anim = animReindexed)
  }

  /** Combines two S3d-models by appending `that` to `this`. Anim-properties
    * `playMode`, `numFrames`, `frameRate` and `displacement` are copied from
    * `this`, only.
    */
  def ++ (that: S3d): S3d = S3d(
    vert = this.vert ++ that.vert,
    indx = this.indx ++ that.indx,
    prim = this.prim ++ that.prim,
    mats = this.mats ++ that.mats,
    prop = this.prop ++ that.prop,
    regp = this.regp ++ that.regp,
    anim = this.anim.copy(groups = this.anim.groups ++ that.anim.groups.map { ag => ag.copy(
      vertBlock = ag.vertBlock map (_ + this.vert.size),
      indxBlock = ag.indxBlock map (_ + this.indx.size),
      primBlock = ag.primBlock map (_ + this.prim.size),
      matsBlock = ag.matsBlock map (_ + this.mats.size))
    })
  )

  /** Scales the model uniformly by a factor. */
  def scale(s: Float) =
    copy(vert = vert map (_ map (v => Vert(v.x * s, v.y * s, v.z * s, v.u, v.v))))

  /** Translates the model on the three axes. */
  def translate(t: Translation) =
    copy(vert = vert map (_ map (v => Vert(v.x + t.x, v.y + t.y, v.z + t.z, v.u, v.v))))

}

object S3d extends DbpfTypeCompanion[S3d] {

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

  /** Construct a new S3D model.
    */
  def apply(
    vert: IndexedSeq[VertGroup],
    indx: IndexedSeq[IndxGroup],
    prim: IndexedSeq[PrimGroup],
    mats: IndexedSeq[MatsGroup],
    anim: AnimSection,
    prop: IndexedSeq[PropGroup],
    regp: IndexedSeq[RegpGroup]): S3d =
      new FreeS3d(vert, indx, prim, mats, anim, prop, regp)

  /** Construct a new S3D model (shorthand for non-animated models).
    */
  def apply(
    vert: IndexedSeq[VertGroup],
    indx: IndexedSeq[IndxGroup],
    prim: IndexedSeq[PrimGroup],
    mats: IndexedSeq[MatsGroup],
    anim: IndexedSeq[AnimGroup],
    prop: IndexedSeq[PropGroup] = IndexedSeq.empty,
    regp: IndexedSeq[RegpGroup] = IndexedSeq.empty): S3d = {
      val animSec = AnimSection(numFrames = 1, frameRate = 0, playMode = PlayMode.Looping, displacement = 0, groups = anim)
      S3d.apply(vert, indx, prim, mats, animSec, prop, regp)
  }

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
  private[scdbpf] abstract class AbstractCBF[-From, -Elem, +To] extends collection.generic.CanBuildFrom[From, Elem, To] {
    def apply(from: From) = apply()
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
  implicit object VertGroupCBF extends AbstractCBF[Seq[Vert], Vert, VertGroup] {
    def apply() = IndexedSeq.newBuilder[Vert].mapResult(vs => VertGroup(vs))
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
  implicit object IndxGroupCBF extends AbstractCBF[Seq[Int], Int, IndxGroup] {
    def apply() = IndexedSeq.newBuilder[Int].mapResult(is => IndxGroup(is))
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
  implicit object PrimGroupCBF extends AbstractCBF[Seq[Prim], Prim, PrimGroup] {
    def apply() = IndexedSeq.newBuilder[Prim].mapResult(ps => PrimGroup(ps))
  }

  /** A material referencing a specific ID.
    * @param id
    * @param wrapU
    * @param wrapV
    * @param magFilter
    * @param minFilter
    * @param animRate 0 or 33
    * @param animMode 0 or 2
    * @param name
    */
  case class Material(
    id: Int,
    wrapU: WrapMode.Value,
    wrapV: WrapMode.Value,
    magFilter: MagnifFilter.Value,
    minFilter: MinifFilter.Value,
    animRate: Short = 0,
    animMode: Short = 0,
    name: Option[String]) {

    require(name forall (_.length < 255), "max name length is 254")
  }

  /** Settings of a material.
    * @param flags
    * @param alphaFunc greater
    * @param depthFunc less equal
    * @param sourceBlend one
    * @param destBlend zero
    * @param alphaThreshold 0, 0xFF or 0x7FF
    * @param matClass 0
    * @param reserved 0
    * @param materials usually has length 1 (or 0).
    */
  case class MatsGroup(
    flags: MatsFlags.ValueSet,
    alphaFunc: MatsFunc.Value = MatsFunc.Greater,
    depthFunc: MatsFunc.Value = MatsFunc.LessEqual,
    sourceBlend: MatsBlend.Value = MatsBlend.One,
    destBlend: MatsBlend.Value = MatsBlend.Zero,
    alphaThreshold: Short,
    matClass: Int = 0,
    reserved: Byte = 0,
    materials: IndexedSeq[Material]
  ) extends S3dGroup {

    private[scdbpf] def binarySize =                         // assumes version >= 1.5
      materials.foldLeft(16)((sum, mat) => sum + 12 + 1 + stringLength(mat.name))
    private[scdbpf] def encode(buf: ByteBuffer): Unit = {    // assumes version >= 1.5
      buf.putInt(flags.toBitMask(0).toInt)
      buf.put(alphaFunc.id.toByte)
      buf.put(depthFunc.id.toByte)
      buf.put(sourceBlend.id.toByte)
      buf.put(destBlend.id.toByte)
      buf.putShort(alphaThreshold)
      buf.putInt(matClass)
      buf.put(reserved)
      buf.put(materials.size.toByte)
      // 16 bytes before here
      for (mat <- materials) {
        import mat._
        buf.putInt(id)
        buf.put(wrapU.id.toByte)
        buf.put(wrapV.id.toByte)
        buf.put(magFilter.id.toByte)
        buf.put(minFilter.id.toByte)
        buf.putShort(animRate)
        buf.putShort(animMode)
        // 12 bytes before here
        putString(buf, name) // 1 + stringLength(name) bytes
      }
    }
  }

  case class AnimGroup(
    vertBlock: IndexedSeq[Int],
    indxBlock: IndexedSeq[Int],
    primBlock: IndexedSeq[Int],
    matsBlock: IndexedSeq[Int],
    name: Option[String] = None,
    flags: Int = 0  // almost always 0 (currently ignored)
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
  object AnimGroup {
    /** Deprecated: For backward compatibility (use other constructor instead).
      */
    def apply(
      name: Option[String],
      flags: Int,  // almost always 0 (currently ignored)
      vertBlock: IndexedSeq[Int],
      indxBlock: IndexedSeq[Int],
      primBlock: IndexedSeq[Int],
      matsBlock: IndexedSeq[Int]
    ): AnimGroup = AnimGroup(vertBlock, indxBlock, primBlock, matsBlock, name, flags)

    /** Shorthand constructor for non-animated models (each block is a single integer).
      */
    def vipm(
      vertBlock: Int,
      indxBlock: Int,
      primBlock: Int,
      matsBlock: Int,
      name: Option[String] = None
    ): AnimGroup = AnimGroup(IndexedSeq(vertBlock), IndexedSeq(indxBlock), IndexedSeq(primBlock), IndexedSeq(matsBlock), name)
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

  object Transparency extends Enumeration {
    val Opaque, Transparent, Semitransparent = Value
  }

  /** Commonly used settings for materials.
    */
  def defaultMats(transparency: Transparency.Value,
      id: Int,
      name: Option[String] = None,
      mipmap: Boolean = false,  // for use with embedded mipmaps
      wrapU: WrapMode.Value = WrapMode.Clamp,
      wrapV: WrapMode.Value = WrapMode.Clamp): MatsGroup = {
    val flags0 = MatsFlags.ValueSet(
      MatsFlags.DepthTest,
      MatsFlags.BackfaceCulling,
      MatsFlags.Texturing)
    import Transparency._
    MatsGroup(
      flags = transparency match {
        case Opaque => flags0
        case Transparent => flags0 + MatsFlags.AlphaTest
        case Semitransparent => flags0 + MatsFlags.AlphaTest + MatsFlags.FrameBufferBlending
      },
      alphaFunc = MatsFunc.Greater,
      depthFunc = MatsFunc.LessEqual,
      sourceBlend = transparency match {
        case Opaque => MatsBlend.One
        case Transparent => MatsBlend.One
        case Semitransparent => MatsBlend.SourceAlpha
      },
      destBlend = transparency match {
        case Opaque => MatsBlend.Zero
        case Transparent => MatsBlend.Zero
        case Semitransparent => MatsBlend.OneMinusSourceAlpha
      },
      alphaThreshold = transparency match {
        case Opaque => 0x7FFF  // 32767
        case Transparent => 0x7FFF  // 32767
        case Semitransparent => 0xFF  // 255
      },
      materials = IndexedSeq(Material(
        id = id,
        wrapU = wrapU,
        wrapV = wrapV,
        magFilter = MagnifFilter.Bilinear,
        minFilter = if (mipmap) MinifFilter.LinearMipmapLinear else MinifFilter.BilinearNoMipmap,
        name = name))
    )
  }
}
