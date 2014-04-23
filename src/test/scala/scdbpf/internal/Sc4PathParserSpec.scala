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
  }
}

