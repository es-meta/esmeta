package esmeta.spec

import esmeta.util.BasicParsers

import Symbol.*, NtArg.Kind.*, Production.Kind.*, Head.*

/** specification parsers */
trait Parsers extends BasicParsers {
  // production lists
  lazy val prods: Parser[List[Production]] =
    rep1(rep(newline) ~> prod)

  // productions
  lazy val prod: Parser[Production] =
    lhs ~ prodKind ~ opt("one of") ~ rep1(opt(newline) ~> rhs) ^^ {
      case l ~ k ~ Some(_) ~ origRs =>
        val rs =
          for (r <- origRs; s <- r.symbols)
            yield Rhs(None, List(s), None)
        Production(l, k, true, rs)
      case l ~ k ~ None ~ rs =>
        Production(l, k, false, rs)
    }

  // production kinds
  lazy val prodKind: Parser[Production.Kind] =
    ":::" ^^^ NumericString | "::" ^^^ Lexical | ":" ^^^ Normal

  // production left-hand-sides (LHSs)
  lazy val lhs: Parser[Lhs] =
    word ~ opt("[" ~> repsep(word, ",") <~ "]") ^^ { case name ~ params =>
      Lhs(name, params.getOrElse(Nil))
    }

  // production alternative right-hand-sides (RHSs)
  lazy val rhs: Parser[Rhs] =
    opt(rhsCond) ~ rep1(symbol) ~ opt(rhsId) ^^ { case c ~ ss ~ i =>
      Rhs(c, ss, i)
    }

  // RHS conditions
  lazy val rhsCond: Parser[RhsCond] =
    "[" ~> ("[+~]".r) ~ word <~ "]" ^^ { case str ~ name =>
      RhsCond(name, str == "+")
    }

  // RHS ids
  lazy val rhsId: Parser[String] = "#" ~> "[-a-zA-Z0-9]+".r

  // skip only white spaces and comments
  protected override val whiteSpace = "[ \t]*//.*|[ \t]+".r

  // grammar symbols
  lazy val symbol: Parser[Symbol] =
    term | butnot | lookahead | nt | empty | nlt | character

  // terminals
  lazy val term: Parser[Terminal] =
    "`[^`]+`|```".r ^^ { case str =>
      Terminal(str.substring(1, str.length - 1))
    }

  // nonterminals
  lazy val nt: Parser[Nonterminal] =
    word ~ opt("[" ~> rep1sep(ntArg, ",") <~ "]") ~ opt("?") ^^ {
      case name ~ args ~ opt =>
        Nonterminal(name, args.getOrElse(Nil), opt.isDefined)
    }

  // butnot symbols
  lazy val butnot: Parser[ButNot] =
    (nt <~ ("but not" ~ opt("one of"))) ~ rep1sep(symbol, opt("or")) ^^ {
      case base ~ cases => ButNot(base, cases)
    }

  // empty symbols
  lazy val empty: Parser[Empty.type] = "[empty]" ^^^ Empty

  // no-line-terminator symbols
  lazy val nlt: Parser[NoLineTerminator.type] =
    "\\[no [\\|]?LineTerminator[\\|]? here\\]".r ^^^ NoLineTerminator

  // character symbols
  lazy val character: Parser[Symbol] = (
    "<" ~> word <~ ">" ^^ { Unicode(_) } |
      ".*code point.*ID_Start.*".r ^^^ UnicodeIdStart |
      ".*code point.*ID_Continue.*".r ^^^ UnicodeIdContinue |
      ".*code point.*0xD800 to 0xDBFF.*".r ^^^ UnicodeLeadSurrogate |
      ".*code point.*0xDC00 to 0xDFFF.*".r ^^^ UnicodeTrailSurrogate |
      ".*any.*code point.*".r ^^^ UnicodeAny |
      ".*HexDigits.*> 0x10FFFF.*".r ^^^ NotCodePoint |
      ".*HexDigits.*≤ 0x10FFFF.*".r ^^^ CodePoint |
      ".*Hex4Digits.*0xD800 to 0xDBFF.*".r ^^^ HexLeadSurrogate |
      ".*Hex4Digits.*0xDC00 to 0xDFFF.*".r ^^^ HexTrailSurrogate |
      ".*Hex4Digits.*not.*0xD800 to 0xDFFF.*".r ^^^ HexNonSurrogate |
      ".*DecimalEscape.*CapturingGroupNumber.*|DecimalEscape|.*≤.*_NcapturingParens.*".r ^^^ NonUnicodeModeDecimalEscape
  )

  // lookahead symbol
  lazy val lookahead: Parser[Lookahead] =
    "[lookahead " ~> containsSymbol ~ laList <~ "]" ^^ { case b ~ cases =>
      Lookahead(b, cases)
    }
  lazy val laList: Parser[List[List[Symbol]]] =
    opt("{") ~> repsep(rep(symbol), ",") <~ opt("}")
  lazy val containsSymbol: Parser[Boolean] =
    ("==" | "<" | "∈") ^^^ true | ("!=" | "<!" | "∉") ^^^ false

  // nonterminal arguments
  lazy val ntArg: Parser[NtArg] =
    ("+" ^^^ True | "~" ^^^ False | "?" ^^^ Pass) ~ word ^^ {
      case kind ~ name => NtArg(kind, name)
    }

  lazy val id: Parser[String] = "_[^_]+_".r ^^ { s =>
    s.substring(1, s.length - 1)
  }
  lazy val name: Parser[String] = "[a-zA-Z0-9/]+".r
  lazy val headParamType: Parser[String] = "([^_,]|, )+".r ^^ { _.dropRight(1) }
  lazy val refName: Parser[String] = "[_`%a-zA-Z0-9.\\[\\]@ ]+".r

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
    import Param.Kind.*
    opt("optional") ~ id ~ opt(":" ~> headParamType) ^^ {
      case opt ~ name ~ ty =>
        // TODO consider variadic parameters
        val kind = if (opt.isDefined) Optional else Normal
        Param(name, kind, ty.getOrElse("unknown"))
    }
  lazy val paramDesc: Parser[Param] =
    import Param.Kind.*
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
