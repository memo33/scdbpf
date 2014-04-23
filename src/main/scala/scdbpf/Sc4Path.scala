package scdbpf

import scala.collection.immutable._
import Sc4Path._
import DbpfUtil._

trait Sc4Path extends DbpfType {
  def terrainVariance: Boolean
  def paths: Seq[Path]
  def stopPaths: Seq[StopPath]

  def copy(
    terrainVariance: Boolean = terrainVariance,
    paths: Seq[Path] = paths,
    stopPaths: Seq[StopPath] = stopPaths): Sc4Path = Sc4Path(terrainVariance, paths, stopPaths)

  def * (rf: RotFlip): Sc4Path = copy(paths = paths map (_ * rf), stopPaths = stopPaths map (_ * rf))

  override def toString: String = {
    val sb = new StringBuilder
    def addln(s: String): Unit = { sb ++= s + "\r\n" } // explicitly use windows line breaks to ensure compatibility across editors

    val version =
      if (paths.exists(_.junction)) 2
      else if (!stopPaths.isEmpty) 1
      else 0

    addln("SC4PATHS")
    addln("1." + version)
    addln(paths.size.toString)
    if (version > 0)
      addln(stopPaths.size.toString)
    addln(if (terrainVariance) "1" else "0")

    paths flatMap (_.lines(version)) foreach addln
    stopPaths flatMap (_.lines) foreach addln

    sb.toString
  }
}

object Sc4Path {

  def apply(terrainVariance: Boolean, paths: Seq[Path], stopPaths: Seq[StopPath] = Seq()): Sc4Path =
    new FreeSc4Path(terrainVariance, paths, stopPaths)

  implicit val converter = new Converter[DbpfType, Sc4Path] {
    def apply(from: DbpfType): Sc4Path = {
      try {
        new BufferedSc4Path(from.dataView)
      } catch {
        case e @ (_: NoSuchElementException
                 |_: IllegalArgumentException
                 |_: IndexOutOfBoundsException
                 |_: NumberFormatException
                 |_: org.parboiled.errors.ParserRuntimeException) =>
          throw new DbpfDecodeFailedException(e.toString, e)
      }
    }
  }

  type Coord = (Float, Float, Float)

  type TransportType = TransportType.Value
  object TransportType extends Enumeration {
    val Car = Value(1)
    val Sim = Value(2)
    val Train = Value(3)
    val Subway = Value(4)
    val ElTrain = Value(6)
    val Monorail = Value(7)
  }

  type Cardinal = Cardinal.Value
  object Cardinal extends Enumeration {
    val West, North, East, South = Value
    val Special = Value(255)
  }

  sealed trait PathLike {
    type Self <: PathLike
    val comment: Option[String]
    val transportType: TransportType
    val classNumber: Int
    val entry: Cardinal
    val exit: Cardinal

    def header: String

    def * (rf: RotFlip): Self

    private[Sc4Path] def classAsString = if (classNumber == 0) "" else ('a' + classNumber - 1).toChar + "_"
    private[Sc4Path] def commentLines = { // adds -- delimiters if missing
      comment.toList flatMap (_.lines) map (_.trim) map
        (c => if (c.startsWith("--")) c else "-- " + c)
    }
    private[Sc4Path] def coordString(c: Coord): String = c.productIterator.mkString(",")
  }

  case class Path(
    comment: Option[String],
    transportType: TransportType,
    classNumber: Int,
    entry: Cardinal,
    exit: Cardinal,
    junction: Boolean = false,
    coords: Seq[Coord]) extends PathLike {

    require(coords.size >= 2, "at least 2 coords are required")
    // TODO ???
    //require(coords zip coords.tail forall { case (a, b) => a != b } , "coords need to be distinct")

    type Self = Path

    def header: String =
      s"-- ${transportType.toString}_${classAsString}${entry.id}_${exit.id}${if (junction) "_J" else ""}"

    def lines(version: Int): List[String] = {
      var res: List[Any] = commentLines ++ List(header, transportType.id, classNumber, entry.id, exit.id)
      if (version >= 2) {
        res :+= (if (junction) 1 else 0)
      }
      res :+= coords.size
      res ++= coords map (c => coordString(c))
      res map (_.toString)
    }

    /** If rf.flipped, this also reverses the path. */
    def * (rf: RotFlip): Path = {
      val res = copy(entry = entry *: rf, exit = exit *: rf, coords = coords map (_ *: rf))
      if (rf.flipped) res.reverse else res
    }

    def reverse: Path = copy(entry = exit, exit = entry, coords = coords.reverse)
  }

  case class StopPath(
    comment: Option[String],
    uk: Boolean,
    transportType: TransportType,
    classNumber: Int,
    entry: Cardinal,
    exit: Cardinal,
    coord: Coord) extends PathLike {

    type Self = StopPath

    def header: String =
      s"-- Stop${if (uk) "UK" else ""}_${transportType.toString}_${classAsString}${entry.id}_${exit.id}"

    def lines: List[String] = {
      commentLines ++ List(
        header, if (uk) 2 else 1, transportType.id, classNumber, entry.id, exit.id, coordString(coord)
      ) map (_.toString)
    }

    /** If rf.flipped, this will also toggle the uk flag. */
    def * (rf: RotFlip): StopPath = {
      copy(uk = uk ^ rf.flipped, entry = entry *: rf, exit = exit *: rf, coord = coord *: rf)
    }
  }

  private class FreeSc4Path(val terrainVariance: Boolean, val paths: Seq[Path], val stopPaths: Seq[StopPath]) extends Sc4Path {

    protected lazy val data: Array[Byte] = toString.getBytes(DbpfUtil.asciiEncoding)
  }

  private lazy val parser = new Sc4PathParser() // needs to be locked for concurrent access

  private class BufferedSc4Path(arr: Array[Byte]) extends RawType(arr) with Sc4Path {

    override lazy val toString = new String(data, DbpfUtil.asciiEncoding)

    val (terrainVariance, paths, stopPaths) = {
      val text = toString
      parser.synchronized {
        val p = parser.parseSc4Path(text)
        (p.terrainVariance, p.paths, p.stopPaths)
      }
    }
  }

}
