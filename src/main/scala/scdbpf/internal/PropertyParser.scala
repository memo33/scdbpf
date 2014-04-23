package scdbpf

import org.parboiled.scala._
import DbpfProperty._
import passera.unsigned._

private class PropertyParser extends Parser {

  def HexDigit = rule(SuppressNode) { "0" - "9" | "a" - "f" | "A" - "F" }

  def HexNumber = rule(SuppressNode) { rule { "0" ~ ignoreCase("x") ~ oneOrMore(HexDigit) } ~> (java.lang.Long.decode(_).toLong) }

  def Digit = rule(SuppressNode) { "0" - "9" }

  def Number = rule(SuppressNode) { oneOrMore(Digit) ~> (_.toLong) }

  def Text = rule(SuppressNode) { "{\"" ~ zeroOrMore(!"\"}" ~ ANY) ~> identity ~ "\"}" }

//  def ValType = rule {
//    zeroOrMore(!":" ~ ANY) ~> { s =>
//      ValueType.values.find(_.toString equalsIgnoreCase s)
//    } ~~~? (_.isDefined) ~~> (_.get)
//  }

  def True = rule { ignoreCase("true") ~ push(1L) }

  def False = rule { ignoreCase("false") ~ push(0L) }

  private def buildProp[A](id: Int, desc: String, reps: Long, vals: Seq[A])(implicit vt: ValueType[A] with ValueType.Numerical[A]) = {
    if (reps == 0 && vals.size == 1)
      (UInt(id), Single(vals.head))
    else
      (UInt(id), Multi(vals: _*))
  }

  def Prop: Rule1[Property] = rule {
    SInt32Rule ~ ":" ~ Text ~ "=" ~ rule {
      rule { ignoreCase("Float32") ~ ":" ~ Float32Vals } |
      rule { ignoreCase("Sint64")  ~ ":" ~ SInt64Vals  } |
      rule { ignoreCase("Sint32")  ~ ":" ~ SInt32Vals  } |
      rule { ignoreCase("Uint32")  ~ ":" ~ UInt32Vals  } |
      rule { ignoreCase("Uint16")  ~ ":" ~ UInt16Vals  } |
      rule { ignoreCase("Uint8")   ~ ":" ~ UInt8Vals   } |
      rule { ignoreCase("Bool")    ~ ":" ~ BoolVals    } |
      rule { ignoreCase("String")  ~ ":" ~ Number ~ ":" ~ StringValue ~~>
        ((id: Int, _: String, reps: Long, vals: Seq[String]) => (UInt(id), Single(vals.head)))
      }
    }
  }

  def StringValue: Rule1[Seq[String]] = rule { Text ~~> (Seq(_)) }

  // couldn't figure out how to factor out common components
  def UInt32Vals  = rule { Number ~ ":" ~ "{" ~ zeroOrMore(UInt32Rule , separator = ",") ~ "}" ~~> buildProp[UInt]    _ }
  def UInt16Vals  = rule { Number ~ ":" ~ "{" ~ zeroOrMore(UInt16Rule , separator = ",") ~ "}" ~~> buildProp[UShort]  _ }
  def UInt8Vals   = rule { Number ~ ":" ~ "{" ~ zeroOrMore(UInt8Rule  , separator = ",") ~ "}" ~~> buildProp[UByte]   _ }
  def SInt32Vals  = rule { Number ~ ":" ~ "{" ~ zeroOrMore(SInt32Rule , separator = ",") ~ "}" ~~> buildProp[Int]     _ }
  def SInt64Vals  = rule { Number ~ ":" ~ "{" ~ zeroOrMore(SInt64Rule , separator = ",") ~ "}" ~~> buildProp[Long]    _ }
  def BoolVals    = rule { Number ~ ":" ~ "{" ~ zeroOrMore(BoolRule   , separator = ",") ~ "}" ~~> buildProp[Boolean] _ }
  def Float32Vals = rule { Number ~ ":" ~ "{" ~ zeroOrMore(Float32Rule, separator = ",") ~ "}" ~~> buildProp[Float]   _ }

  def BoolRule = rule { rule { HexNumber | Number | False | True } ~~> (l => if (l == 0) false else true) }
  def UInt8Rule = rule { SInt64Rule ~~> (l => UByte(l.toByte)) }
  def UInt16Rule = rule { SInt64Rule ~~> (l => UShort(l.toShort)) }
  def UInt32Rule: Rule1[UInt] = rule { SInt64Rule ~~> (l => UInt(l.toInt)) }
  def SInt32Rule: Rule1[Int] = rule { SInt64Rule ~~> (_.toInt) }
  def SInt64Rule: Rule1[Long] = rule { HexNumber | Number }
  def Float32Rule = rule { rule {
    optional("-") ~ zeroOrMore(Digit) ~ optional("." ~ zeroOrMore(Digit))
  } ~> (_.toDouble.toFloat) }


  def Parent = rule { ignoreCase("ParentCohort=Key:") ~ "{" ~ zeroOrMore(SInt32Rule , separator = ",") ~ "}" ~~> {
    s: Seq[Int] => s match {
      case (Seq(t, g, i)) => Some(Tgi(t, g, i))
      case _ => None
    }
  } ~~~? (_.isDefined) ~~> (_.get) }

  def PropCount = rule { ignoreCase("PropCount=") ~ SInt64Rule }

  def Header = rule(SuppressSubnodes) { anyOf("EC") ~> { _ == "C" } ~ "QZT1###" }

  def Exemplar = rule {
    Header ~ WhiteSpace ~ Parent ~ WhiteSpace ~ PropCount ~ WhiteSpace ~
      zeroOrMore(Prop, separator = WhiteSpace) ~ optional(WhiteSpace) ~ EOI ~~> {
        (isCohort: Boolean, parent: Tgi, _: Long, props: Seq[Property]) =>
          scdbpf.Exemplar(parent, isCohort, props)
    }
  }

  def WhiteSpace = rule(SuppressNode) { oneOrMore(anyOf(" \r\n\t\f")) }

  def parseProperty(text: String): Property = {
    val parsingResult = ReportingParseRunner(Prop).run(text)
    parsingResult.result match {
      case Some(p) => p
      case None => throw new DbpfDecodeFailedException("Invalid property: " +
        org.parboiled.errors.ErrorUtils.printParseErrors(parsingResult))
    }
  }

  def parseExemplar(text: String): scdbpf.Exemplar = {
    val parsingResult = ReportingParseRunner(this.Exemplar).run(text)
    parsingResult.result match {
      case Some(e) => e
      case None => throw new DbpfDecodeFailedException("Invalid exemplar: " +
        org.parboiled.errors.ErrorUtils.printParseErrors(parsingResult))
    }
  }
}
