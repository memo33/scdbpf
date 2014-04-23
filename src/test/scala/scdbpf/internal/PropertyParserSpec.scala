package scdbpf

import org.scalatest.{WordSpec, Matchers}
import org.parboiled.scala._
import org.parboiled.scala.{ReportingParseRunner => RPR}
import passera.unsigned._
import DbpfProperty._

class PropertyParserSpec extends WordSpec with Matchers {

  "PropertyParser" should {
    val parser = new PropertyParser()
    "push numbers on stack" in {
      val s = "0xABCD1234"
      val res = RPR(parser.HexNumber).run(s)
      res.result should be ('defined)
      res.result.get.toInt should be (0xABCD1234)
      res.result.get should not be (0xABCD1234)
    }
    "push ints on stack" in {
      ((RPR(parser.SInt32Rule).run("12").result): Option[Int]).get should be (12)
      ((RPR(parser.SInt32Rule).run("0x12").result): Option[Int]).get should be (0x12)
      ((RPR(parser.UInt32Rule).run("12").result): Option[UInt]).get should not be (12)
      ((RPR(parser.UInt32Rule).run("0x12").result): Option[UInt]).get should not be (0x12)
      RPR(parser.UInt32Rule).run("12").result.get should be (UInt(12))
      RPR(parser.UInt32Rule).run("0x12").result.get should be (UInt(0x12))
    }
    "push bools on stack" in {
      RPR(parser.BoolRule).run("1").result.get should be (true)
      RPR(parser.BoolRule).run("0x0").result.get should be (false)
      RPR(parser.BoolRule).run("false").result.get should be (false)
      RPR(parser.BoolRule).run("true").result.get should be (true)
    }
    "push floats on stack" in {
      RPR(parser.Float32Rule).run("-0.123").result.get should be (-0.123f)
    }
    "push text on stack" in {
      val s = """{"fo"o"}"""
      val res = RPR(parser.Text).run(s)
      res.result should be ('defined)
      res.result.get should be ("fo\"o")
    }
//    "push value types on stack" in {
//      val res = RPR(parser.ValType).run("uint16")
//      res.result should be ('defined)
//      res.result.get should be (DbpfProperty.ValueType.Uint16)
//      val res2 = RPR(parser.ValType).run("abc")
//      res2.result should be ('empty)
//    }
//    "match prop values" in {
//      val res = RPR(parser.Values).run("{}")
//      res.result should be ('defined)
//      res.result.get should be ('empty)
//      RPR(parser.Values).run("{\"abcd\"}").result.get should be (Seq("abcd"))
//      RPR(parser.Values).run("{true,false,0x01,0xFFFFFFFF}").result.get should be (Seq(1, 0, 1, 0xFFFFFFFFL))
//    }
    "parse text properties" in {
      import ValueType._
      val a = """0x8A416A99:{"User Visible Name Key"}=Uint32:3:{0x2026960B,0x0A554AE0,0x0C9DB098}"""
      val b = """0x27812810:{"Occupant Size"}=Float32:3:{0.2,10,-0.2}"""
      val c = """0x69F14D33:{"Orient To Slope"}=Bool:0:{False}"""
      val d = """0x00000020:{"Exemplar Name"}=String:0:{"RTMT_Prop_Ninja_BusSign_Road"}"""
      parser.parseProperty(a) match { case (id, Multi(Uint32(p))) => p should have size (3) }
      parser.parseProperty(b) match { case (id, Multi(Float32(p))) => p should have size (3) }
      parser.parseProperty(c) match { case (id, Single(Bool(p))) => p.value should be (false) }
      parser.parseProperty(d) match { case (id, String(p)) => p.value should be ("RTMT_Prop_Ninja_BusSign_Road") }
    }
    "parse parent cohort" in {
      val res = RPR(parser.Parent).run("""ParentCohort=Key:{0x00000000,0x00000000,0x00000000}""")
      res.result.get should be (Tgi.Blank)
    }
    "parse prop count" in {
      val res = RPR(parser.PropCount).run("""PropCount=0x0000001A""")
      res.result.get should be (0x1A)
    }
    "parse entire exemplar" in {
      val s = """|EQZT1###
                 |ParentCohort=Key:{0x00000000,0x00000000,0x00000000}
                 |PropCount=0x00000010
                 |0x00000010:{"Exemplar Type"}=Uint32:0:{0x0000001e}
                 |0x0abfc024:{"AppearanceZoomsFlag"}=Uint32:0:{0x0000001e}
                 | """.stripMargin
      val res = RPR(parser.Exemplar).run(s)
      if (res.result.isEmpty) {
        println(org.parboiled.errors.ErrorUtils.printParseErrors(res))
        assert(false)
      } else {
        res.result.get.properties should have size (2)
        res.result.get should not be ('cohort)
        parser.parseExemplar("C" + s.drop(1)) should be ('cohort)
      }
    }
  }
}

