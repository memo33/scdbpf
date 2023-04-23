package io.github.memo33
package scdbpf

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.parboiled.scala._
import org.parboiled.scala.{ReportingParseRunner => RPR}
import io.github.memo33.passera.unsigned._
import DbpfProperty._

class PropertyParserSpec extends AnyWordSpec with Matchers {

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
    "parse named text properties" in {
      import ValueType._
      val a = """0x27812810:{"Occupant Size"}=Float32:3:{Width: 0.00999, Height: 0.00999, Depth: 0.00999}"""
      val b = """0x27812850:{"Park Effect"}=Sint32:2:{Magnitude: 0x00000000, Radius: 0x00000000}"""
      val c = """0x27812851:{"Pollution at center"}=Sint32:4:{Air: 0x00000000, Water: 0x00000000, Garbage: 0x00000000, Radiation: 0x00000000}"""
      val d = """0x68EE9764:{"Pollution radii"}=Float32:4:{Air: 0, Water: 0, Garbage: 0, Radiation: 0}"""
      val e = """0x87CD6399:{"Landmark Effect"}=Float32:2:{Magnitude: 5, Radius: 5}"""
      parser.parseProperty(a) match { case (id, Multi(Float32(p))) => p should be (Multi(0.00999f, 0.00999f, 0.00999f)) }
      parser.parseProperty(b) match { case (id, Multi(Sint32(p))) => p should be (Multi(0, 0)) }
      parser.parseProperty(c) match { case (id, Multi(Sint32(p))) => p should be (Multi(0, 0, 0, 0)) }
      parser.parseProperty(d) match { case (id, Multi(Float32(p))) => p should be (Multi[Float](0, 0, 0, 0)) }
      parser.parseProperty(e) match { case (id, Multi(Float32(p))) => p should be (Multi[Float](5, 5)) }
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
                 |""".stripMargin
      val res = RPR(parser.Exemplar).run(s)
      if (res.result.isEmpty) {
        println(org.parboiled.errors.ErrorUtils.printParseErrors(res))
        assert(false)
      } else {
        res.result.get.properties should have size (2)
        res.result.get should not be ('cohort)
        parser.parseExemplar("C" + s.drop(1)) should be ('cohort)
      }
      val s2 = """|EQZT1###
                  |ParentCohort=Key:{0x00000000,0x00000000,0x00000000}
                  |PropCount=0x00000020
                  |0x00000010:{"Exemplar Type"}=Uint32:0:{0x00000002}
                  |0x00000020:{"Exemplar Name"}=String:0:{"Highway Lamp IM1dg-RHW4"}
                  |0x099AFACD:{"Bulldoze Cost"}=Sint64:0:{0x0000000000000032}
                  |0x27812810:{"Occupant Size"}=Float32:3:{Width: 0.00999, Height: 0.00999, Depth: 0.00999}
                  |0x27812821:{"Resource Key Type 1"}=Uint32:3:{0x5AD0E817, 0x342E29CB, 0x000A0000}
                  |0x27812832:{"Wealth"}=Uint8:0:{0x02}
                  |0x27812840:{"Demand Satisfied"}=Uint32:6:{0x00001810, 0x000002EE, 0x00001820, 0x000002EE, 0x00001830, 0x000002EE}
                  |0x27812850:{"Park Effect"}=Sint32:2:{Magnitude: 0x00000000, Radius: 0x00000000}
                  |0x27812851:{"Pollution at center"}=Sint32:4:{Air: 0x00000000, Water: 0x00000000, Garbage: 0x00000000, Radiation: 0x00000000}
                  |""".stripMargin
      val res2 = RPR(parser.Exemplar).run(s2)
      if (res2.result.isEmpty) {
        println(org.parboiled.errors.ErrorUtils.printParseErrors(res2))
        assert(false)
      } else {
        res2.result.get.properties should have size (9)
        res2.result.get should not be ('cohort)
      }
    }
  }
}

