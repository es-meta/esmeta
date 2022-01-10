package esmeta.spec.parsers

import esmeta.spec.*
import scala.util.parsing.combinator.*

/** parsers for algorithm heads */
trait HeadParsers extends BaseParsers {
  import Head.*
  import Param.Kind.*

  lazy val id = "_[^_]+_".r ^^ { s => s.substring(1, s.length - 1) }
  lazy val name = "[a-zA-Z0-9/]+".r
  lazy val headParamType = "([^_,]|, )+".r ^^ { _.dropRight(1) }
  lazy val refName = "[_`%a-zA-Z0-9.\\[\\]@ ]+".r

  // runtime/static semantics
  lazy val semanticsKind: Parser[Boolean] =
    (("Runtime" | "Static") <~ "Semantics" ~ ":") ^^ { _ == "Static" }

  // abstract opration (AO) heads
  lazy val absOpHeadGen: Parser[Boolean => AbstractOperationHead] =
    opt(semanticsKind) ~> name ~ params ^^ { case name ~ params =>
      (isHostDefined: Boolean) =>
        AbstractOperationHead(name, params, isHostDefined)
    }

  // numeric method heads
  lazy val numMethodHead: Parser[NumericMethodHead] =
    (name <~ "::") ~ name ~ params ^^ { case t ~ x ~ ps =>
      NumericMethodHead(t, x, ps)
    }

  // algorithm parameters
  lazy val params: Parser[List[Param]] =
    opt(
      "(" ~ opt(newline) ~> repsep(param, "," ~ opt(newline)) <~
        opt("," ~ newline) ~ ")",
    ) ^^ { _.getOrElse(Nil) }
  lazy val param: Parser[Param] =
    opt("optional") ~ id ~ opt(":" ~> headParamType) ^^ {
      case opt ~ name ~ ty =>
        // TODO consider variadic parameters
        val kind = if (opt.isDefined) Optional else Normal
        Param(name, kind, ty.getOrElse("unknown"))
    }
  lazy val paramDesc: Parser[Param] =
    headParamType ~ opt(id) ^^ { case ty ~ name =>
      Param(name.getOrElse("this"), Normal, ty)
    }

  // syntax-directed operation (SDO) head generator
  lazy val sdoHeadGen
    : Parser[(String, Int, Int, List[Param]) => SyntaxDirectedOperationHead] =
    semanticsKind ~ name ~ params ^^ { case isStatic ~ x ~ params =>
      (lhsName: String, idx: Int, subIdx: Int, rhsParams: List[Param]) =>
        SyntaxDirectedOperationHead(
          lhsName,
          idx,
          subIdx,
          rhsParams,
          x,
          isStatic,
          params,
        )
    }

  // concrete method head generator
  lazy val concMethodHeadGen: Parser[Param => ConcreteMethodHead] =
    name ~ params ^^ { case name ~ params =>
      (receiverParam: Param) => ConcreteMethodHead(name, receiverParam, params)
    }

  // internal method head generator
  lazy val inMethodHeadGen: Parser[Param => InternalMethodHead] =
    ("[[" ~> name <~ "]]") ~ params ^^ { case name ~ params =>
      (receiverParam: Param) => InternalMethodHead(name, receiverParam, params)
    }

  // built-in heads
  lazy val builtinHead: Parser[BuiltinHead] =
    refName ~ params ^^ { case name ~ params => BuiltinHead(name, params) }
}
