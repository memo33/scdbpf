package scdbpf

import passera.unsigned._
import DbpfUtil.toHex
import Tgi._

/** Represents Type, Group, Instance identifiers of `DbpfEntries`.
  * `Tgi` objects are immutable.
  *
  * Instances of this class may be obtained via the companion object's `apply`
  * method, for example:
  *
  * {{{
  * val tgi = Tgi(0, 0, 0x12345678)
  * }}}
  *
  * Alternatively, the `copy` methods can be used to create modified copies.
  *
  * {{{
  * tgi.copy(iid = 0x87654321)
  * tgi.copy(Tgi.Sc4Path)
  * }}}
  *
  * The [[matches]] method is used to test whether a `Tgi` matches another `Tgi`
  * object or `TgiMask`.
  *
  * @define SELF `Tgi`
  */
sealed trait Tgi extends LabeledTgi {
  type IdType = Int
  type SelfType = Tgi

  /** Creates a new `Tgi` from this object with the non-`None` parameters of
    * `mask` replaced. For example, `copy(Tgi.Sc4Path)` would replace the `tid`.
    */
  def copy(mask: TgiMask): Tgi = {
    Tgi(mask.tid.getOrElse(this.tid),
      mask.gid.getOrElse(this.gid),
      mask.iid.getOrElse(this.iid))
  }

  def copy(tid: Int = tid, gid: Int = gid, iid: Int = iid): Tgi = Tgi(tid, gid, iid)

  /** Tests if all the IDs match the non-masked IDs of `tgi`.
    */
  def matches(tgi: TgiLike): Boolean = tgi match {
    case tgi: Tgi => this.ids == tgi.ids
    case mask: TgiMask => this.ids zip mask.ids forall {
      case (a, bOpt) => bOpt forall (_ == a)
    }
  }

  def label: String = {
    val labeledTgiOpt = Tgi.LabeledTgis.values.find(this.matches(_))
    labeledTgiOpt.get.label
  }

  override def toString: String = {
    "T:" + toHex(tid) + ", G:" + toHex(gid) + ", I:" + toHex(iid)
  }
}

/** Provides various masks that `Tgi`s can be matched against.
  */
object Tgi {

  /** @define SELF `TgiLike` */
  sealed trait TgiLike {
    type IdType
    type SelfType
    val tid, gid, iid: IdType

    /** Creates a new $SELF from this object with the specified parameters
      * replaced.
      */
    def copy(tid: IdType = tid, gid: IdType = gid, iid: IdType = iid): SelfType

    private[scdbpf] def ids = Iterable(tid, gid, iid)

    final override def equals(obj: Any): Boolean = obj match {
      case that: TgiLike => this.ids == that.ids
      case _ => false
    }
    final override def hashCode(): Int = {
      val p = 4229
      var result = 1
      result = p * result + tid.hashCode
      result = p * result + iid.hashCode
      result = p * result + gid.hashCode
      result
    }
  }

  sealed trait LabeledTgi extends TgiLike {
    /** a descriptive label specifying the general type like `Exemplar`, `S3D`
      * or `Unknown`.
      */
    def label: String
  }

  private class TgiImpl(val tid: Int, val gid: Int, val iid: Int) extends Tgi
  def apply(tid: Int, gid: Int, iid: Int): Tgi = new TgiImpl(tid, gid, iid)

  private[scdbpf] object LabeledTgis extends Enumeration {
    import scala.language.implicitConversions
    implicit def value2LabeledTgi(v: Value): LabeledTgi = v.asInstanceOf[LabeledTgi]
    private[Tgi] class TgiValImpl(val tid: Int, val gid: Int, val iid: Int, override val label: String)
      extends Val with Tgi
    private[Tgi] class TgiMaskValImpl(val tid: Option[Int], val gid: Option[Int], val iid: Option[Int], val label: String)
      extends Val with TgiMask with LabeledTgi
  }
  import LabeledTgis.{ TgiValImpl, TgiMaskValImpl }

  private def TgiVal(tid: Int, gid: Int, iid: Int, label: String): Tgi =
    new TgiValImpl(tid, gid, iid, label)
  private def MaskVal(tid: Option[Int], gid: Option[Int], iid: Option[Int], label: String): TgiMask with LabeledTgi =
    new TgiMaskValImpl(tid, gid, iid, label)
  private def MaskVal(tid: Int, gid: Option[Int], iid: Option[Int], label: String): TgiMask with LabeledTgi =
    new TgiMaskValImpl(Some(tid), gid, iid, label)
  private def MaskVal(tid: Int, gid: Int, iid: Option[Int], label: String): TgiMask with LabeledTgi =
    new TgiMaskValImpl(Some(tid), Some(gid), iid, label)

  private implicit val uintOnIntOrdering = UIntOrdering.on[Int](UInt(_))
  private val tupOrd = Ordering[(Int, Int, Int)]
  /** the default implicit `Tgi` ordering that sorts by IID, TID, GID */
  implicit val itgOrdering: Ordering[Tgi] = tupOrd.on(x => (x.iid, x.tid, x.gid))
  /** a `Tgi` ordering that sorts by IID, GID, TID */
  val igtOrdering: Ordering[Tgi] = tupOrd.on(x => (x.iid, x.gid, x.iid))
  /** a `Tgi` ordering that sorts by TID, IID, GID */
  val tigOrdering: Ordering[Tgi] = tupOrd.on(x => (x.tid, x.iid, x.gid))

  val Blank                 = TgiVal (0, 0, 0, "-")
  val Directory             = TgiVal (0xe86b1eef, 0xe86b1eef, 0x286b1f03, "Directory")
  val Ld                    = MaskVal(0x6be74c60, 0x6be74c60, None,       "LD");
  val S3dMaxis              = MaskVal(0x5ad0e817, 0xbadb57f1, None,       "S3D (Maxis)");
  val S3d                   = MaskVal(0x5ad0e817, None,       None,       "S3D");
  val Cohort                = MaskVal(0x05342861, None,       None,       "Cohort")

  val ExemplarRoad          = MaskVal(0x6534284a, 0x2821ed93, None,       "Exemplar (Road)");
  val ExemplarStreet        = MaskVal(0x6534284a, 0xa92a02ea, None,       "Exemplar (Street)");
  val ExemplarOnewayroad    = MaskVal(0x6534284a, 0xcbe084cb, None,       "Exemplar (One-Way Road)");
  val ExemplarAvenue        = MaskVal(0x6534284a, 0xcb730fac, None,       "Exemplar (Avenue)");
  val ExemplarHighway       = MaskVal(0x6534284a, 0xa8434037, None,       "Exemplar (Highway)");
  val ExemplarGroundhighway = MaskVal(0x6534284a, 0xebe084d1, None,       "Exemplar (Ground Highway)");
  val ExemplarDirtroad      = MaskVal(0x6534284a, 0x6be08658, None,       "Exemplar (Dirtroad)");
  val ExemplarRail          = MaskVal(0x6534284a, 0xe8347989, None,       "Exemplar (Rail)");
  val ExemplarLightrail     = MaskVal(0x6534284a, 0x2b79dffb, None,       "Exemplar (Lightrail)");
  val ExemplarMonorail      = MaskVal(0x6534284a, 0xebe084c2, None,       "Exemplar (Monorail)");
  val ExemplarSubway        = MaskVal(0x6534284a, 0x8a15f3f2, None,       "Exemplar (Subway)");
  val ExemplarPowerpole     = MaskVal(0x6534284a, 0x088e1962, None,       "Exemplar (Power Pole)");
  val ExemplarT21           = MaskVal(0x6534284a, 0x89ac5643, None,       "Exemplar (T21)");
  val Exemplar              = MaskVal(0x6534284a, None,       None,       "Exemplar")

  val FshMisc               = MaskVal(0x7ab50e44, 0x1abe787d, None,       "FSH (Misc)");
  val FshBaseOverlay        = MaskVal(0x7ab50e44, 0x0986135e, None,       "FSH (Base/Overlay Texture)");
  val FshShadow             = MaskVal(0x7ab50e44, 0x2BC2759a, None,       "FSH (Shadow Mask)");
  val FshAnimProps          = MaskVal(0x7ab50e44, 0x2a2458f9, None,       "FSH (Animation Sprites (Props))");
  val FshAnimNonprops       = MaskVal(0x7ab50e44, 0x49a593e7, None,       "FSH (Animation Sprites (Non Props))");
  val FshTerrainFoundation  = MaskVal(0x7ab50e44, 0x891b0e1a, None,       "FSH (Terrain/Foundation)");
  val FshUi                 = MaskVal(0x7ab50e44, 0x46a006b0, None,       "FSH (UI Image)");
  val Fsh                   = MaskVal(0x7ab50e44, None,       None,       "FSH")

  val Sc4Path2d             = MaskVal(0x296678f7, 0x69668828, None,       "SC4Path (2D)");
  val Sc4Path3d             = MaskVal(0x296678f7, 0xa966883f, None,       "SC4Path (3D)");
  val Sc4Path               = MaskVal(0x296678f7, None,       None,       "SC4Path");

  val PngIcon               = MaskVal(0x856ddbac, 0x6a386d26, None,       "PNG (Icon)");
  val Png                   = MaskVal(0x856ddbac, None,       None,       "PNG");
  val Lua                   = MaskVal(0xca63e2a3, 0x4a5e8ef6, None,       "Lua");
  val LuaGen                = MaskVal(0xca63e2a3, 0x4a5e8f3f, None,       "Lua (Generators)");
  val Wav                   = MaskVal(0x2026960b, 0xaa4d1933, None,       "WAV");
  val LText                 = MaskVal(0x2026960b, None,       None,       "LText");
  val IniFont               = TgiVal (0,          0x4a87bfe8, 0x2a87bffc, "INI (Font Table)");
  val IniNetwork            = TgiVal (0,          0x8a5971c5, 0x8a5993b9, "INI (Networks)");
  val Ini                   = MaskVal(0,          0x8a5971c5, None,       "INI");
  val Rul                   = MaskVal(0x0a5bcf4b, 0xaa5bcf57, None,       "RUL");
  val EffDir                = MaskVal(0xea5118b0, None,       None,       "EffDir");

  val Null                  = MaskVal(None,       None,       None,       "Unknown")
}

/** Represents masks of TGIs that are used for the `match` method of [[Tgi]].
  *
  * Instances of this class may be obtained via the companion object's `apply`
  * methods.
  *
  * @define SELF `TgiMask`
  */
sealed trait TgiMask extends TgiLike {
  type IdType = Option[Int]
  type SelfType = TgiMask

  def copy(tid: Option[Int] = tid, gid: Option[Int] = gid, iid: Option[Int] = iid): TgiMask = TgiMask(tid, gid, iid)

  /** Creates a `Tgi` from this mask. If one of its IDs is `None`, a
    * `NoSuchElementException` is thrown.
    */
  def toTgi: Tgi = Tgi(tid.get, gid.get, iid.get)

  override def toString: String = {
    val s = "__________"
    "T:" + tid.map(toHex(_)).getOrElse(s) +
      ", G:" + gid.map(toHex(_)).getOrElse(s) +
      ", I:" + iid.map(toHex(_)).getOrElse(s)
  }
}

/** Provides factory methods for creating `TgiMask`s.
  */
object TgiMask {
  private class TgiMaskImpl(val tid: Option[Int], val gid: Option[Int], val iid: Option[Int]) extends TgiMask
  def apply(tid: Int,         gid: Int,         iid: Int        ): TgiMask = TgiMask(Some(tid), Some(gid), Some(iid))
  def apply(tid: Int,         gid: Int,         iid: Option[Int]): TgiMask = TgiMask(Some(tid), Some(gid), iid)
  def apply(tid: Int,         gid: Option[Int], iid: Option[Int]): TgiMask = TgiMask(Some(tid), gid, iid)
  def apply(tid: Option[Int], gid: Option[Int], iid: Option[Int]): TgiMask = new TgiMaskImpl(tid, gid, iid)
}
