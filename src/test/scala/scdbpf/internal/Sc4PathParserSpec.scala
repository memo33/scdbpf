package scdbpf

import org.scalatest.{WordSpec, Matchers}
import org.parboiled.scala._
import org.parboiled.scala.{ReportingParseRunner => RPR}
import org.parboiled.errors.ErrorUtils._
import Sc4Path._

class Sc4PathParserSpec extends WordSpec with Matchers {

  "Sc4PathParser" should {
    val parser = new Sc4PathParser()
    def handleResult[A](res: ParsingResult[A]): ParsingResult[A] = {
      if (res.result.isEmpty) {
        println(printParseErrors(res))
        assert(false)
      } else {
        res.result.get
      }
      res
    }
    "decode stop path sections" in {
      val s = """|
                 |2
                 |1 -- foo
                 |0
                 |1
                 |255
                 |2.54713,7.26555,0.0""".stripMargin
      handleResult(RPR(parser.StopSection).run(s))
    }
    "decode path sections" in {
      val paths = Seq("""
        |-- Sim_3_1
        |2
        |0
        |3 -- foo
        |1
        |2
        |2.5,-8.0,0.0
        |2.5,8.0,0.0""".stripMargin,"""
        |-- Sim_3_1
        |2
        |0
        |3 -- foo
        |1
        |0 -- << junction key
        |2
        |2.5,-8.0,0.0
        |2.5,8.0,0.0""".stripMargin)
      for (s <- paths) {
        val res = handleResult(RPR(parser.PathSection).run(s))
        res.result.get.comment should be ('empty)
        val res2 = handleResult(RPR(parser.PathSection).run("--foo\n" + s))
        res2.result.get.comment should not be ('empty)
      }
    }
    "decode header sections" in {
      val headers = Seq(
        """SC4PATHS
          |1.1
          |2
          |3
          |1""".stripMargin,
        """SC4PATHS
          |1.0
          |2
          |1""".stripMargin,
        """SC4PATHS
          |1.0
          |2
          |3
          |1""".stripMargin)
      for (s <- headers.take(2)) {
        handleResult(RPR(parser.Header).run(s))
      }
      RPR(parser.Header).run(headers.last).result should be ('empty)
    }
    "parse entire path file" in {
      val s =
        """SC4PATHS
          |1.1
          |1
          |0
          |1
          |-- Sim_3_1
          |2
          |0
          |3 -- foo
          |1
          |2
          |2.5,-8.0,0.0
          |2.5,8.0,0.0""".stripMargin
      handleResult(RPR(parser.Sc4PathSection).run(s))
      handleResult(RPR(parser.Sc4PathSection).run(s + "--foo"))
      handleResult(RPR(parser.Sc4PathSection).run(s + "--foo\n"))
      handleResult(RPR(parser.Sc4PathSection).run(s + "--foo\n\n"))
      handleResult(RPR(parser.Sc4PathSection).run(s + "--foo\n\n --foo"))
    }
    "validate number of path sections" in {
      val s =
        """SC4PATHS
          |1.0
          |2
          |1
          |-- Sim_3_1
          |2
          |0
          |3
          |1
          |2
          |2.5,-8.0,0.0
          |2.5,8.0,0.0""".stripMargin
      intercept[DbpfDecodeFailedException](parser.parseSc4Path(s, strict=true)).getMessage shouldBe "Invalid SC4Paths: number of path sections was 1, declared 2"
      val s2 =
        """SC4PATHS
          |1.0
          |0
          |1
          |-- Sim_3_1
          |2
          |0
          |3
          |1
          |2
          |2.5,-8.0,0.0
          |2.5,8.0,0.0""".stripMargin
      intercept[DbpfDecodeFailedException](parser.parseSc4Path(s2, strict=true)).getMessage shouldBe "Invalid SC4Paths: number of path sections was 1, declared 0"
    }
    "validate number of stop paths" in {
      val s =
        """SC4PATHS
          |1.1
          |0
          |2
          |1
          |-- Stop
          |2
          |1
          |0
          |1
          |255
          |2.54713,7.26555,0.0""".stripMargin
      intercept[DbpfDecodeFailedException](parser.parseSc4Path(s, strict=true)).getMessage shouldBe "Invalid SC4Paths: number of stop path sections was 1, declared 2"
      val s2 =
        """SC4PATHS
          |1.1
          |0
          |0
          |1
          |-- Stop
          |2
          |1
          |0
          |1
          |255
          |2.54713,7.26555,0.0""".stripMargin
      intercept[DbpfDecodeFailedException](parser.parseSc4Path(s2, strict=true)).getMessage shouldBe "Invalid SC4Paths: number of stop path sections was 1, declared 0"
    }
    "validate against stop paths in version 1.0" in {
      val s =
        """SC4PATHS
          |1.0
          |0
          |1
          |-- Stop
          |2
          |1
          |0
          |1
          |255
          |2.54713,7.26555,0.0""".stripMargin
      intercept[DbpfDecodeFailedException](parser.parseSc4Path(s, strict=true)).getMessage shouldBe "Invalid SC4Paths: version was 1.0 but path file contains stop paths"
    }
    "validate number of coordinates" in {
      val s =
        """SC4PATHS
          |1.0
          |1
          |1
          |-- Sim_3_1
          |2
          |0
          |3
          |1
          |3
          |2.5,-8.0,0.0
          |2.5,8.0,0.0""".stripMargin
      intercept[DbpfDecodeFailedException](parser.parseSc4Path(s, strict=true)).getMessage shouldBe "Invalid SC4Paths: path section 0 has 2 coordinates but declared 3"
    }
    "validate junction keys" in {
      val s =
        """SC4PATHS
          |1.1
          |1
          |0
          |1
          |-- Sim_3_1
          |2
          |0
          |3
          |1
          |0
          |2
          |2.5,-8.0,0.0
          |2.5,8.0,0.0""".stripMargin
      intercept[DbpfDecodeFailedException](parser.parseSc4Path(s, strict=true)).getMessage shouldBe "Invalid SC4Paths: version was 1.1 but path section 0 has junction key"
      val s2 =
        """SC4PATHS
          |1.2
          |1
          |0
          |1
          |-- Sim_3_1
          |2
          |0
          |3
          |1
          |2
          |2.5,-8.0,0.0
          |2.5,8.0,0.0""".stripMargin
      intercept[DbpfDecodeFailedException](parser.parseSc4Path(s2, strict=true)).getMessage shouldBe "Invalid SC4Paths: version was 1.2 but path section 0 lacks junction key"
    }
  }
}

