package scdbpf

import scala.collection.immutable.Seq
import scala.collection.JavaConversions
import org.parboiled.scala._
import Sc4Path._

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

  def PathSection: Rule1[Path] = rule {
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
        if (cnt != cs.size) {
          // TODO ???
          println(s"number of path sections was ${cs.size}, declared $cnt")
        }
        Path(stripSectionHeader(comment), tt, cl, entry, exit, junc exists identity, cs)
      }
    }
  }

  def Header = rule {
    Signature ~ MultiLineEnd ~ (
      "1.0" ~ push(0)      ~ MultiLineEnd ~ Number ~ MultiLineEnd ~ push(0)               ~ Bool |
      Version ~~~? (_ > 0) ~ MultiLineEnd ~ Number ~ MultiLineEnd ~ Number ~ MultiLineEnd ~ Bool
    ) ~~> ((a, b, c, d) => (a, b, c, d)) // collect in tuple for testing
  }

  def Sc4PathSection = rule {
    Header ~ zeroOrMore(PathSection) ~ zeroOrMore(StopSection) ~
    zeroOrMore(LineEnd, separator = LineBreak) ~ EOI ~~> {
      (head: (Int, Int, Int, Boolean), paths: Seq[Path], stopPaths: Seq[StopPath]) =>
        val (v, numPaths, numStopPaths, terrain) = head
        if (numPaths != paths.size || numStopPaths != stopPaths.size) {
          // TODO ???
          println(s"number of path sections was ${paths.size}, declared $numPaths; stop path sections ${stopPaths.size}, declared $numStopPaths")
        }
        Sc4Path(terrain, paths, stopPaths)
    }
  }

  // effectively simply drops the last line of the trimmed string
  // which commonly is something like "-- Sim_1_2" and will be recreated computationally
  private def stripSectionHeader(s: String): Option[String] = {
    val lines = JavaConversions.asScalaIterator(s.trim.lines.iterator).toSeq
    if (lines.isEmpty)
      None
    else
      Some(lines.init mkString "\r\n") filterNot (_.isEmpty)
  }

  def parseSc4Path(text: String): Sc4Path = {
    val parsingResult = ReportingParseRunner(Sc4PathSection).run(text)
    parsingResult.result match {
      case Some(a) => a
      case None => throw new DbpfDecodeFailedException("Invalid SC4Paths: " +
        org.parboiled.errors.ErrorUtils.printParseErrors(parsingResult))
    }
  }
}
