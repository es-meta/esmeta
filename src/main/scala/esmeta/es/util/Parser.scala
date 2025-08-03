package esmeta.es.util

import esmeta.es.*
import esmeta.es.builtin.*
import esmeta.state.util.{Parsers => StateParsers}
import esmeta.util.BaseUtils.*
import esmeta.util.BasicParsers

/** ECMAScript parser */
object Parser extends Parsers

/** ECMAScript parsers */
trait Parsers extends StateParsers {

  given struct: Parser[Struct] = {
    val imap = "[" ~> rep {
      (word ~ ":" ~ value ^^ { case k ~ _ ~ v => (k, v) }) <~ opt(",")
    } <~ "]"
    val nmap = "{" ~> rep {
      (propKey ~ ":" ~ propDesc ^^ { case k ~ _ ~ d => (k, d) }) <~ opt(",")
    } <~ "}"
    word ~ opt(imap) ~ opt(nmap) ^^ {
      case t ~ im ~ nm => Struct(t, im.getOrElse(Nil), nm.getOrElse(Nil))
    }
  }.named("es.builtin.Struct")

  given propKey: Parser[PropKey] = {
    "%Symbol." ~> word <~ "%" ^^ { sym => PropKey.Sym(sym) } |
    word ^^ { str => PropKey.Str(str) }
  }.named("es.builtin.PropKey")

  given propDesc: Parser[PropDesc] = {
    val bool = "T" ^^^ true | "F" ^^^ false
    val accessor =
      ("(" ~ "get" ~ "=" ~> value) ~
      ("," ~ "set" ~ "=" ~> value) ~
      (")" ~ "[" ~> bool ~ bool <~ "]") ^^ {
        case g ~ s ~ (e ~ c) => AccessorDesc(g, s, e, c)
      }
    val data =
      value ~ ("[" ~> bool ~ bool ~ bool <~ "]") ^^ {
        case v ~ (w ~ e ~ c) => DataDesc(v, w, e, c)
      }
    accessor | data
  }.named("es.builtin.PropDesc")
}
