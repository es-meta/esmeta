package esmeta

import java.io._
import esmeta.error.NotSupported
import esmeta.phase._
import esmeta.util.BaseUtils._
import esmeta.util.SystemUtils._
import org.scalatest._
import io.circe._, io.circe.syntax._, io.circe.parser._
import esmeta.util.{BasicParser, BasicParsers}
import scala.runtime.ScalaRunTime

trait ESMetaTest extends funsuite.AnyFunSuite with BeforeAndAfterAll {
  // results
  trait Result
  case object Pass extends Result
  case class Yet(msg: String) extends Result
  case object Fail extends Result
  protected var resMap: Map[String, Result] = Map()
  implicit val ResultDecoder: Decoder[Result] = new Decoder[Result] {
    final def apply(c: HCursor): Decoder.Result[Result] = c.value match {
      case Json.True       => Right(Pass)
      case Json.False      => Right(Fail)
      case v if v.isString => Right(Yet(v.asString.get))
      case _ => Left(DecodingFailure(s"unknown Result: ${c.value}", c.history))
    }
  }
  implicit val ResultEncoder: Encoder[Result] = Encoder.instance {
    case Pass     => Json.True
    case Fail     => Json.False
    case Yet(msg) => Json.fromString(msg)
  }

  // count tests
  protected var count: Int = 0

  // check result
  def check[T](name: String)(tester: => T): Unit = {
    count += 1
    test(s"[$tag] $name") {
      try {
        tester
        resMap += name -> Pass
      } catch {
        case e @ NotSupported(msg) =>
          resMap += name -> Yet(msg)
        case e: Throwable =>
          resMap += name -> Fail
          throw e
      }
    }
  }

  // check stringify
  def checkStringify[T](desc: String)(cases: (T, String)*): Unit =
    check(desc)(cases.foreach { case (obj, expected) =>
      val result = obj.toString
      if (result != expected) {
        println(s"[FAILED] $desc")
        println(s"- expected: $expected")
        println(s"- result: $result")
        assert(result == expected)
      }
    })

  // original toString of case class
  def origToString(x: Any): String = x match
    case p: Product =>
      p.productPrefix + (for {
        elem <- p.productIterator
      } yield origToString(elem)).mkString("(", ",", ")")
    case it: Iterable[_] =>
      it.getClass.getName +
        (for {
          elem <- it
        } yield origToString(elem)).mkString("(", ",", ")")
    case _ => x.toString

  // check parse and stringify
  def checkParseAndStringify[T](desc: String, parse: (String) => T)(
    cases: (T, String)*,
  ): Unit =
    check(desc)(cases.foreach { case (obj, string) =>
      // check parse
      val parsed = optional(parse(string))
      val stringified = obj.toString
      if (parsed != Some(obj)) {
        println(s"[FAILED] $desc")
        val parsedStr = parsed.fold("<parsing failed>")(origToString)
        println(s"- result: ${parsedStr}")
        println(s"- expected: ${origToString(obj)}")
      }
      // check stringify
      if (stringified != string) {
        println(s"[FAILED] $desc")
        println(s"- result: $stringified")
        println(s"- expected: $string")
      }
      assert(parsed == Some(obj))
      assert(stringified == string)
    })

  // get score
  def getScore(res: Map[String, Result]): (Int, Int) = (
    res.count { case (k, r) => r == Pass },
    res.size,
  )

  // tag name
  def category: String
  lazy val tag: String = s"$category.$this"

  // check backward-compatibility after all tests
  override def afterAll(): Unit = {
    // check backward-compatibility
    var breakCount = 0
    def error(msg: String): Unit = {
      breakCount += 1
      scala.Console.err.println(s"[Backward-Compatibility] $msg")
    }

    // show abstract result
    val filename = s"$TEST_DIR/result/$category/$this.json"
    for {
      str <- optional(readFile(filename))
      json <- parse(str)
      map <- json.as[Map[String, Result]]
      (name, result) <- map.toSeq.sortBy(_._1)
    } (resMap.get(name), result) match {
      case (None, _)          => error(s"'[$tag] $name' test is removed")
      case (Some(Fail), Pass) => error(s"'[$tag] $name' test becomes failed")
      case _                  =>
    }

    // save abstract result if backward-compatible
    val dirname = s"$TEST_DIR/result/$category"
    mkdir(dirname)
    val pw = getPrintWriter(s"$dirname/$this")
    val (x, y) = getScore(resMap)
    pw.println(s"$tag: $x / $y")
    pw.close()

    val jpw = getPrintWriter(filename)
    jpw.println(resMap.asJson.spaces2SortKeys)
    jpw.close()
  }

  // test name
  val name: String

  // registration
  def init: Unit
}
