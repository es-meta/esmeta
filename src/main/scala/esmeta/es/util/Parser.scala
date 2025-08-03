package esmeta.es.util

import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.builtin.*
import esmeta.spec.util.{Parsers => SpecParsers}
import esmeta.state.util.{Parsers => StateParsers}
import esmeta.util.BaseUtils.*
import esmeta.util.BasicParsers

/** ECMAScript parser */
case class Parser(cfg: CFG) extends Parsers

/** ECMAScript parsers */
trait Parsers extends StateParsers {

  given intrMap: Parser[Map[String, Struct]] = {
    val name = """[\[\]%:.\w]+""".r
    val pair = name ~ "=" ~ struct ^^ { case n ~ _ ~ s => n -> s }
    rep(pair) ^^ { _.toMap }
  }

  given struct: Parser[Struct] = {
    val imap = "[" ~> rep {
      (word ~ ":" ~ value ^^ { case k ~ _ ~ v => (k, v) }) <~ opt(";")
    } <~ "]"
    val nmap = "{" ~> rep {
      (propKey ~ ":" ~ propDesc ^^ { case k ~ _ ~ d => (k, d) }) <~ opt(";")
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
    val accessor = ("[" ~> bool ~ bool <~ "]") ~ ("{" ~>
    ("get" ~ ":" ~> value <~ ";") ~
    ("set" ~ ":" ~> value <~ ";") <~ "}") ^^ {
      case (e ~ c) ~ (g ~ s) => AccessorDesc(g, s, e, c)
    }
    val data =
      ("[" ~> bool ~ bool ~ bool <~ "]") ~ value ^^ {
        case (w ~ e ~ c) ~ v => DataDesc(v, w, e, c)
      }
    accessor | data
  }.named("es.builtin.PropDesc")
}
