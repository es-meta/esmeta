package esmeta.spec.parsers

import esmeta.spec.*
import scala.util.parsing.combinator.*

/** parsers for algorithm heads */
trait HeadParsers extends BaseParsers {
  import Head.*

  lazy val id = "_[^_]+_".r ^^ { s => s.substring(1, s.length - 1) }
  lazy val algoName = "[a-zA-Z0-9:]+".r
  lazy val headParamType = "(.+),".r ^^ { _.dropRight(1) }

  // heads
  lazy val head: Parser[Head] = absOpHead

  // abstract opration (AO) heads
  lazy val absOpHead: Parser[AbstractOperationHead] = baseHead ^^ {
    case _ ~ name ~ params => AbstractOperationHead(name, params)
  }

  // basic head parser
  lazy val baseHead = opt(semanticsKind) ~ algoName ~ params

  // runtime/static semantics
  lazy val semanticsKind: Parser[Boolean] =
    (("Runtime" | "Static") <~ "Semantics" ~ ":") ^^ { _ == "Static" }

  lazy val synDirOpHeadName: Parser[(Boolean, String, List[Param])] =
    baseHead ^^ { case isStatic ~ x ~ params => (isStatic.get, x, params) }

  // algorithm parameters
  lazy val params: Parser[List[Param]] =
    opt("(" ~ newline ~> rep(param <~ newline) <~ ")") ^^ { _.getOrElse(Nil) }
  lazy val param: Parser[Param] =
    import Param.Kind.*
    opt("optional") ~ id ~ opt(":" ~> headParamType) ^^ {
      case opt ~ name ~ ty =>
        // TODO more precisely represent parameter types and kinds
        val kind = if (opt.isDefined) Optional else Normal
        Param(name, kind, ty)
    }
}
