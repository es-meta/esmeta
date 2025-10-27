package esmeta.es.util

import esmeta.LINE_SEP
import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.builtin.*
import esmeta.ir.util.{Parsers => IRParsers}
import esmeta.util.BaseUtils.*
import esmeta.util.BasicParsers

/** ECMAScript parser */
object Parser extends Parsers

/** ECMAScript parsers */
trait Parsers extends IRParsers {

  given intr: Parser[Intrinsics] = {
    val elem: Parser[Template | Model] = template | model
    rep(elem) ^^ { elems =>
      val (models, templates) = elems.partitionMap {
        case t: Template => Right(t)
        case m: Model    => Left(m)
      }
      Intrinsics(templates, models)
    }
  }

  given model: Parser[Model] = {
    val name = """[\[\]%:.\w]+""".r <~ "="
    val imap = "[" ~> rep {
      (word <~ ":") ~ valueStr ^^ { case k ~ v => k -> v }
    } <~ "]"
    val block = "{" ~> rep(valueStr) <~ "}" ~ ";" ^^ { ss =>
      ss.map("  " + _ + ";").mkString("{" + LINE_SEP, LINE_SEP, LINE_SEP + "}")
    }
    val desc = """\[\w+\]""".r ~ block ^^ { case t ~ b => s"$t $b" } | valueStr
    val nmap = "{" ~> rep {
      (propKey <~ ":") ~ desc ^^ { case k ~ d => k -> d }
    } <~ "}"
    name ~ word ~ opt(imap) ~ opt(nmap) ^^ {
      case x ~ t ~ im ~ nm => Model(x, t, im.getOrElse(Nil), nm.getOrElse(Nil))
    }
  }.named("es.builtin.Struct")

  given template: Parser[Template] = {
    val name = """_\w+_""".r ^^ { _.drop(1).dropRight(1) }
    val field = (word <~ ":") ~ valueStr ^^ { case k ~ v => k -> v }
    val fields = opt("{" ~> rep(field) <~ "}") ^^ { _.getOrElse(Nil).toMap }
    val instance = word ~ fields ^^ { case n ~ f => n -> f }
    name ~ ("is" ~ "{" ~> rep(instance) <~ "}") ^^ {
      case n ~ is => Template(n, is.toMap)
    }
  }

  given propKey: Parser[PropKey] = {
    "%Symbol." ~> word <~ "%" ^^ { sym => PropKey.Sym(sym) } |
    word ^^ { str => PropKey.Str(str) }
  }.named("es.builtin.PropKey")

  given valueStr: Parser[String] = "[^{};]+".r <~ ";" ^^ { _.trim }
}
