package esmeta.es.builtin

import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.state.*
import esmeta.state.util.{Parsers => StateParsers}

// property descriptor
sealed trait PropDesc extends ESElem

// data properties
case class DataDesc(
  value: Value,
  writable: Boolean,
  enumerable: Boolean,
  configurable: Boolean,
) extends PropDesc
object DataDesc {
  case class Parser(cfg: CFG) extends Parsers
  trait Parsers extends StateParsers {
    given propDesc: this.Parser[PropDesc] = {
      val bool = "T" ^^^ true | "F" ^^^ false
      val accessor = ("[" ~> bool ~ bool <~ "]") ~ ("{" ~>
      ("Get" ~ ":" ~> value <~ ";") ~
      ("Set" ~ ":" ~> value <~ ";") <~ "}") ^^ {
        case (e ~ c) ~ (g ~ s) => AccessorDesc(g, s, e, c)
      }
      val data = ("[" ~> bool ~ bool ~ bool <~ "]") ~ value ^^ {
        case (w ~ e ~ c) ~ v => DataDesc(v, w, e, c)
      }
      accessor | data
    }.named("es.builtin.PropDesc")
  }
}

// accessor properties
case class AccessorDesc(
  get: Value,
  set: Value,
  enumerable: Boolean,
  configurable: Boolean,
) extends PropDesc
