package scdbpf

import scala.collection.immutable.{Seq, StringOps}
import org.parboiled.scala._
import Sc4Path._

/** A path that has not been validated yet. */
private case class ParsedPath(
    comment: Option[String],
    transportType: TransportType,
    classNumber: Int,
    entry: Cardinal,
    exit: Cardinal,
    junction: Option[Boolean],
    numCoords: Int,
    coords: Seq[Coord])

private case class ParsedSc4Path(head: (Int, Int, Option[Int], Boolean), paths: Seq[ParsedPath], stopPaths: Seq[StopPath])

private class Sc4PathParser extends Parser {

  def LineBreak = rule { optional("\r") ~ "\n" | "\r" }

  def Line = rule { zeroOrMore(!LineBreak ~ ANY) }

  def WhiteSpace = rule(SuppressNode) { zeroOrMore(anyOf(" \t\f")) }

  def Comment = rule { "--" ~ Line } // matches maximally

  def LineEnd = rule { WhiteSpace ~ optional(Comment) }

  def MultiLineEnd = oneOrMore(LineEnd ~ LineBreak)

  def Digit = rule(SuppressNode) { "0" - "9" }

  def Number = rule(SuppressNode) { oneOrMore(Digit) ~> (_.toInt) }

  def FloatNumber: Rule1[Float] = rule(SuppressNode) {
    optional("-") ~ zeroOrMore(Digit) ~
    optional("." ~ zeroOrMore(Digit)) ~
    optional(ignoreCase("E") ~ optional("-") ~ Number ~~% {n => /*pop*/ } ) // exponential format
  } ~? (!_.isEmpty) ~> (java.lang.Double.parseDouble(_).toFloat)

  def Bool = rule { anyOf("01") ~> (_ == "1") }

  def Signature = "SC4PATHS"

  def Version = rule { "1." ~ Number ~~~? (n => n >= 0 && n <= 2) }

  def Coordinate: Rule1[(Float, Float, Float)] = rule {
    !(Number ~ LineBreak) ~ // <-- for better error message, if coord is expected
    FloatNumber ~ WhiteSpace ~ "," ~ WhiteSpace ~
    FloatNumber ~ WhiteSpace ~ "," ~ WhiteSpace ~
    FloatNumber ~~> ((a: Float, b: Float, c: Float) => (a, b, c))
  }

  def EntryExit = rule { Number ~~~? (n => n >= 0 && n < 4 || n == 255) ~~> (Cardinal(_)) }

  def Transport = rule { Number ~~~? (n => n >= 1 && n <= 7 && n != 5) ~~> (TransportType(_)) }

  def UkFlag = rule { anyOf("12") ~> (_ == "2") }

  // starts with comment (optional) and line break,
  // does not end with comment or line break
  def StopSection = rule {
    MultiLineEnd ~> identity ~ UkFlag ~
    MultiLineEnd ~ Transport ~
    MultiLineEnd ~ Number ~
    MultiLineEnd ~ EntryExit ~
    MultiLineEnd ~ EntryExit ~
    MultiLineEnd ~ Coordinate ~~> {
      (comment: String, uk: Boolean, tt: TransportType, cl: Int, entry: Cardinal, exit: Cardinal, c: Coord) =>
        StopPath(stripSectionHeader(comment), uk, tt, cl, entry, exit, c)
    }
  }

  def CoordinateSection: Rule1[List[Coord]] = rule {
    MultiLineEnd ~ Coordinate ~
    oneOrMore(MultiLineEnd ~ Coordinate) ~~> ((c: Coord, cs) => c :: cs)
  }

  def PathSection: Rule1[ParsedPath] = rule {
    MultiLineEnd ~> identity ~ Transport ~
    MultiLineEnd ~ Number ~
    MultiLineEnd ~ EntryExit ~
    MultiLineEnd ~ EntryExit ~
    MultiLineEnd ~ optional(Bool ~
    MultiLineEnd) ~ Number ~~>
      ((a, b, c, d, e, f, g) => (a, b, c, d, e, f, g)) ~ // collect current values in tuple as max type params is 7
    CoordinateSection ~~> {
      (tup: Tuple7[String, TransportType, Int, Cardinal, Cardinal, Option[Boolean], Int], cs: List[Coord]) => {
        val (comment, tt, cl, entry, exit, junc, cnt) = tup
        ParsedPath(stripSectionHeader(comment), tt, cl, entry, exit, junc, cnt, cs)
      }
    }
  }

  def Header = rule {
    Signature ~ MultiLineEnd ~ (
      "1.0" ~ push(0)      ~ MultiLineEnd ~ Number ~ MultiLineEnd ~ push(Option.empty[Int])               ~ Bool |
      Version ~~~? (_ > 0) ~ MultiLineEnd ~ Number ~ MultiLineEnd ~ (Number ~~> (Some(_))) ~ MultiLineEnd ~ Bool
    ) ~~> ((a, b, c, d) => (a, b, c, d)) // collect in tuple for testing
  }

  def Sc4PathSection = rule {
    Header ~ zeroOrMore(PathSection) ~ zeroOrMore(StopSection) ~
    zeroOrMore(LineEnd, separator = LineBreak) ~ EOI ~~> {
      (head: (Int, Int, Option[Int], Boolean), paths: Seq[ParsedPath], stopPaths: Seq[StopPath]) => ParsedSc4Path(head, paths, stopPaths)
    }
  }

  // effectively simply drops the last line of the trimmed string
  // which commonly is something like "-- Sim_1_2" and will be recreated computationally
  private[scdbpf] def stripSectionHeader(s: String): Option[String] = {
    val lines = new StringOps(s.trim).lines.toSeq
    if (lines.isEmpty)
      None
    else
      Some(lines.init mkString "\r\n") filterNot (_.isEmpty)
  }

  private def validatePath(parsed: ParsedSc4Path): Option[String] = {
    val (version, numPaths, numStopPaths, terrain) = parsed.head
    if (numPaths != parsed.paths.size) {
      Some(s"number of path sections was ${parsed.paths.size}, declared $numPaths")
    } else if (version == 0 && parsed.stopPaths.nonEmpty) {
      Some(s"version was 1.0 but path file contains stop paths")
    } else if (version == 0 && numStopPaths.nonEmpty) {
      Some(s"version was 1.0 but path file has declared ${numStopPaths.get} stop paths")
    } else if (version != 0 && numStopPaths.isEmpty) {
      Some(s"version was 1.$version but path file has not declared stop paths")
    } else if (numStopPaths.exists(_ != parsed.stopPaths.size)) {
      Some(s"number of stop path sections was ${parsed.stopPaths.size}, declared ${numStopPaths.get}")
    } else {
      val idx = parsed.paths.indexWhere(p => p.numCoords != p.coords.size)
      if (idx != -1) {
        val p = parsed.paths(idx)
        Some(s"path section $idx has ${p.coords.size} coordinates but declared ${p.numCoords}")
      } else {
        val idx = parsed.paths.indexWhere(p => p.junction.isEmpty ^ (version < 2)) // if valid, both booleans should be equal
        if (idx != -1) {
          if (version < 2) {
            Some(s"version was 1.$version but path section $idx has junction key")
          } else {
            Some(s"version was 1.2 but path section $idx lacks junction key")
          }
        } else {
          None
        }
      }
    }
  }

  /** Convert parsing result without any validation */
  private def parsingResultToSc4Path(parsed: ParsedSc4Path): Sc4Path = {
    val paths2 = for (p <- parsed.paths) yield {
      Path(p.comment, p.transportType, p.classNumber, p.entry, p.exit, p.junction exists identity, p.coords)
    }
    val terrain = parsed.head._4
    Sc4Path(terrain, paths2, parsed.stopPaths)
  }

  def parseSc4Path(text: String, strict: Boolean = false): Sc4Path = {
    val parsingResult = ReportingParseRunner(Sc4PathSection).run(text)
    parsingResult.result match {
      case Some(parsedSc4Path) => {
        val err = validatePath(parsedSc4Path)
        if (strict) {
          err match {
            case Some(msg) => throw new DbpfDecodeFailedException("Invalid SC4Paths: " + msg)
            case None => parsingResultToSc4Path(parsedSc4Path)
          }
        } else {
          err.foreach(println)  // the validation errors are an issue for the game, but are recoverable in scdbpf
          parsingResultToSc4Path(parsedSc4Path)
        }
      }
      case None => throw new DbpfDecodeFailedException("Invalid SC4Paths: " +
        org.parboiled.errors.ErrorUtils.printParseErrors(parsingResult))
    }
  }
}
