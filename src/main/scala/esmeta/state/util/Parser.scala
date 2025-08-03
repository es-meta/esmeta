package esmeta.state.util

import esmeta.state.*
import esmeta.util.BaseUtils.*
import esmeta.util.BasicParsers

/** state parser */
object Parser extends Parsers

/** state parsers */
trait Parsers extends BasicParsers {

  given value: Parser[Value] = {
    addr |
    // TODO clo |
    // TODO cont |
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
    "#" ~> "[\\[\\]._a-zA-Z]+".r ^^ { NamedAddr(_) }
  }.named("state.Addr")

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
