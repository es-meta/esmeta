package esmeta.state.util

import esmeta.cfg.*
import esmeta.ir.*
import esmeta.ir.util.{Parsers => IRParsers}
import esmeta.state.*
import esmeta.util.BaseUtils.*
import esmeta.util.BasicParsers

/** state parser */
case class Parser(cfg: CFG) extends Parsers

/** state parsers */
trait Parsers extends IRParsers {
  val cfg: CFG
  given CFG = cfg

  given value: Parser[Value] = {
    addr |
    clo |
    cont |
    // TODO astValue |
    // TODO grammarSymbol |
    infinity |
    enumValue |
    codeUnitValue |
    simpleValue |
    math
  }.named("state.Value")

  given addr: Parser[Addr] = {
    "#" ~> long ^^ { DynamicAddr(_) } |
    "#" ~> """[\[\]:%.\w]+""".r ^^ { NamedAddr(_) }
  }.named("state.Addr")

  val captured = "[" ~> repsep(
    word ~ "->" ~ value ^^ { case n ~ _ ~ v => Name(n) -> v },
    ",",
  ) <~ "]" ^^ { _.toMap }

  given clo: Parser[Clo] = {
    "clo<" ~> funcName ~ opt("," ~> captured) <~ ">" ^^ {
      case f ~ c => Clo(cfg.fnameMap(f), c.getOrElse(Map()))
    }
  }.named("state.Clo")

  given cont: Parser[Cont] = {
    // TODO: callStack
    "cont<" ~> funcName ~ opt("," ~> captured) <~ ">" ^^ {
      case f ~ c => Cont(cfg.fnameMap(f), c.getOrElse(Map()), Nil)
    }
  }.named("state.Cont")

  given math: Parser[Math] = {
    decimal ^^ { Math(_) }
  }.named("state.Math")

  given infinity: Parser[Infinity] = {
    "+INF" ^^^ Infinity(pos = true) |
    "-INF" ^^^ Infinity(pos = false)
  }.named("state.Infinity")

  given enumValue: Parser[Enum] = {
    "~" ~> "[^~]+".r <~ "~" ^^ { Enum(_) }
  }.named("state.Enum")

  given codeUnitValue: Parser[CodeUnit] = {
    s"${integer}cu".r ^^ { s => CodeUnit(s.dropRight(2).toInt.toChar) }
  }.named("state.CodeUnit")

  given simpleValue: Parser[SimpleValue] = {
    s"${integer}n".r ^^ { s => BigInt(scala.BigInt(s.dropRight(1))) } |
    s"${number}f".r ^^ { s => Number(s.dropRight(1).toDouble) } |
    "+NUM_INF" ^^^ Number(Double.PositiveInfinity) |
    "-NUM_INF" ^^^ Number(Double.NegativeInfinity) |
    "NaN" ^^^ Number(Double.NaN) |
    string ^^ { Str(_) } |
    bool ^^ { Bool(_) } |
    "undefined" ^^^ Undef |
    "null" ^^^ Null
  }.named("state.SimpleValue")
}
