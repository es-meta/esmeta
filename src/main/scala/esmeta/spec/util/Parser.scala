package esmeta.spec.util

import scala.io.Source
import esmeta.*
import esmeta.lang.*
import esmeta.lang.util.{Parsers => LangParsers}
import esmeta.spec.{*, given}
import esmeta.typing.UnknownTy
import esmeta.util.BaseUtils.*
import esmeta.util.BasicParsers
import esmeta.util.HtmlUtils.*
import esmeta.util.SystemUtils.*
import org.jsoup.nodes.*
import esmeta.spec.Summary.AlgorithmElem

/** specification parsers */
object Parser extends Parsers
trait Parsers extends LangParsers {
  // skip only white spaces and comments
  override val whiteSpace = "[ \t]*//.*|[ \t]+".r

  // ---------------------------------------------------------------------------
  // Summary
  // ---------------------------------------------------------------------------
  given summary: Parser[Summary] = {
    import Summary.*
    val version = opt("- version:" ~> ".*".r) ^^ {
      case vOpt => vOpt.flatMap(v => optional(Spec.Version(v)))
    }
    val empty = rep(newline)
    val grammar = {
      (empty ~ "- grammar:") ~>
      (empty ~ "- productions:" ~ ".*".r) ~>
      (empty ~ "- lexical:" ~> int) ~
      (empty ~ "- numeric string:" ~> int) ~
      (empty ~ "- syntactic:" ~> int) ~
      (empty ~ "- extended productions for web:" ~> int)
    } ^^ { case l ~ n ~ s ~ w => GrammarElem(l, n, s, w) }
    val algo = {
      (empty ~ "- algorithms:" ~ ".*".r) ~>
      (empty ~ "- complete:" ~> int) ~
      (empty ~ "- incomplete:" ~> int)
    } ^^ { case c ~ i => AlgorithmElem(c, i) }
    val step = {
      (empty ~ "- algorithm steps:" ~ ".*".r) ~>
      (empty ~ "- complete:" ~> int) ~
      (empty ~ "- incomplete:" ~> int)
    } ^^ { case c ~ i => StepElem(c, i) }
    val tables = empty ~ "- tables:" ~> int
    val tyModel = empty ~ "- type model:" ~> int <~ empty
    version ~ grammar ~ algo ~ step ~ tables ~ tyModel ^^ {
      case v ~ g ~ a ~ s ~ t ~ m => Summary(v, g, a, s, t, m)
    }
  }.named("spec.Summary")

  // ---------------------------------------------------------------------------
  // Grammar
  // ---------------------------------------------------------------------------
  /** grammar */
  given grammarRule: Parser[Grammar] = {
    (rep(newline) ~ "<Productions>" ~> prods) ~
    (rep(newline) ~ "<Productions for Web>" ~> prods) ^^ {
      case p ~ w => Grammar(p, w)
    }
  }.named("spec.Grammar")

  /** production lists */
  given prods: Parser[List[Production]] = {
    rep1(rep(newline) ~> prod)
  }.named("List[spec.Production]")

  /** productions */
  given prod: Parser[Production] = {
    lhs ~ prodKind ~ opt("one of") ~ rep1(opt(newline) ~> rhs) ^^ {
      case l ~ k ~ Some(_) ~ origRs =>
        val rs = for {
          r <- origRs
          s <- r.symbols
        } yield Rhs(None, List(s), None)
        Production(l, k, true, rs)
      case l ~ k ~ None ~ rs => Production(l, k, false, rs)
    }
  }.named("spec.Production")

  /** production kinds */
  given prodKind: Parser[Production.Kind] = {
    import Production.Kind.*
    ":::" ^^^ NumericString | "::" ^^^ Lexical | ":" ^^^ Syntactic
  }.named("spec.Production.Kind")

  /** production left-hand-sides (LHSs) */
  given lhs: Parser[Lhs] = {
    word ~ opt("[" ~> repsep(word, ",") <~ "]") ^^ {
      case name ~ params =>
        Lhs(name, params.getOrElse(Nil))
    }
  }.named("spec.Lhs")

  /** production alternative right-hand-sides (RHSs) */
  given rhs: Parser[Rhs] = {
    lazy val rhsId: Parser[String] = "#" ~> "[-a-zA-Z0-9]+".r
    opt(rhsCond) ~ rep1(symbol) ~ opt(rhsId) ^^ {
      case c ~ ss ~ i =>
        Rhs(c, ss, i)
    }
  }.named("spec.Rhs")

  /** RHS conditions */
  given rhsCond: Parser[RhsCond] = {
    "[" ~> ("[+~]".r) ~ word <~ "]" ^^ {
      case str ~ name =>
        RhsCond(name, str == "+")
    }
  }.named("spec.RhsCond")

  /** grammar symbols */
  given symbol: Parser[Symbol] = {
    term | butnot | lookahead | butOnlyIf | nt | abbr | unicodeSet | empty | nlt
  }.named("spec.Symbol")

  /** terminals */
  lazy val term: Parser[Terminal] = {
    "`[^`]+`|```".r ^^ {
      case str =>
        Terminal(str.substring(1, str.length - 1))
    }
  }.named("spec.Terminal")

  /** nonterminals */
  lazy val nt: Parser[Nonterminal] = {
    word ~ opt("[" ~> rep1sep(ntArg, ",") <~ "]") ~ opt("?") ^^ {
      case name ~ args ~ opt =>
        Nonterminal(name, args.getOrElse(Nil), opt.isDefined)
    }
  }.named("spec.Nonterminal")

  /** butnot symbols */
  lazy val butnot: Parser[ButNot] = {
    (nt <~ ("but not" ~ opt("one of"))) ~ rep1sep(symbol, opt("or")) ^^ {
      case base ~ cases => ButNot(base, cases)
    }
  }.named("spec.ButNot")

  /** but-only-if symbols
    *
    * e.g., [> but only if MV of |HexDigits| > 0x10FFFF]
    */
  lazy val butOnlyIf: Parser[ButOnlyIf] = {
    nt ~ ("[> but only if" ~ opt("the") ~> word <~ "of |" ~ word ~ "|") ~
    "[^\\]]+".r <~ "]" ^^ {
      case base ~ name ~ cond => ButOnlyIf(base, name, cond)
    }
  }.named("spec.ButOnlyIf")

  /** empty symbols */
  lazy val empty: Parser[Empty.type] = {
    "[empty]" ^^^ Empty
  }.named("spec.Empty")

  /** no-line-terminator symbols */
  lazy val nlt: Parser[NoLineTerminator.type] = {
    "\\[no [\\|]?LineTerminator[\\|]? here\\]".r ^^^ NoLineTerminator
  }.named("spec.NoLineTerminator")

  /** symbols for code point abbreviations */
  lazy val abbr: Parser[CodePointAbbr] = {
    "<" ~> word <~ ">" ^^ { CodePointAbbr(_) }
  }.named("spec.CodePointAbbr")

  /** symbols for sets of unicode code points with a condition */
  lazy val unicodeSet: Parser[UnicodeSet] = {
    ">" ~ "any Unicode code point" ~> opt(".+".r) ^^ { UnicodeSet(_) }
  }.named("spec.UnicodeSet")

  /** lookahead symbol */
  lazy val lookahead: Parser[Lookahead] = {
    lazy val laList: Parser[List[List[Symbol]]] =
      opt("{") ~> repsep(rep(symbol), ",") <~ opt("}")
    lazy val containsSymbol: Parser[Boolean] =
      ("!=" | "<!" | "∉") ^^^ false | ("==" | "<" | "∈") ^^^ true
    "[lookahead " ~> containsSymbol ~ laList <~ "]" ^^ {
      case b ~ cases => Lookahead(b, cases)
    }
  }.named("spec.Lookahead")

  /** nonterminal arguments */
  given ntArg: Parser[NtArg] = {
    ntArgKind ~ word ^^ { case kind ~ name => NtArg(kind, name) }
  }.named("spec.NtArg")

  /** nonterminal argument kinds */
  given ntArgKind: Parser[NtArg.Kind] = {
    import NtArg.Kind.*
    "+" ^^^ True | "~" ^^^ False | "?" ^^^ Pass
  }.named("spec.NtArg.Kind")

  // ---------------------------------------------------------------------------
  // Algorithms
  // ---------------------------------------------------------------------------
  /** types
    *
    * TODO handle type more precisely such as:
    *   - ~normal~, ~generator~, ~async~, or ~asyncGenerator~
    *   - an ECMAScript language value, but not *undefined* or *null*
    *   - a possibly empty List, each of whose elements is a String or
    *     *undefined* ...
    */
  given specTy: Parser[Type] = {
    "([^_,:]|, )+".r ^^ { case s => Type(UnknownTy(s)) }
  }.named("lang.Type (specTy)")

  // abstract opration (AO) heads
  lazy val absOpHeadGen: Parser[Boolean => AbstractOperationHead] = {
    opt(semanticsKind) ~> name ~ params ~ retTy ^^ {
      case name ~ params ~ rty => AbstractOperationHead(_, name, params, rty)
    }
  }.named("spec.AbstractOperationHead")

  // numeric method heads
  lazy val numMethodHead: Parser[NumericMethodHead] = {
    (specTy <~ "::") ~ name ~ params ~ retTy ^^ {
      case t ~ x ~ ps ~ rty => NumericMethodHead(t, x, ps, rty)
    }
  }.named("spec.NumericMethodHead")

  // algorithm parameters
  given param: Parser[Param] = {
    import Param.Kind.*
    opt("optional") ~ specId ~ opt(":" ~> specTy) ^^ {
      case opt ~ name ~ specTy =>
        val kind = if (opt.isDefined) Optional else Normal
        Param(name, kind, specTy.getOrElse(AnyType))
    } | opt(",") ~ "…" ^^^ Param("", Ellipsis, AnyType)
  }.named("spec.Param")

  // algorithm parameter description
  lazy val paramDesc: Parser[Param] =
    import Param.Kind.*
    specTy ~ opt(specId) ^^ {
      case specTy ~ name => Param(name.getOrElse("this"), Normal, specTy)
    }

  // multiple algorithm parameters
  lazy val params: Parser[List[Param]] = {
    import Param.Kind.Ellipsis
    opt(
      "(" ~ opt(newline) ~> repsep(param, "," ~ opt(newline)) ~ oldOptParams <~
      opt("," ~ newline) ~ ")",
    ) ^^ {
      case None           => Nil
      case Some(ps ~ ops) => ps ++ ops
    }
  }.named("List[spec.Param]")

  // TODO remove this legacy parser later
  lazy val oldOptParams: Parser[List[Param]] =
    import Param.Kind.*
    "[" ~> opt(",") ~> param ~ opt(oldOptParams) <~ "]" ^^ {
      case p ~ psOpt => p.copy(kind = Optional) :: psOpt.getOrElse(Nil)
    } | opt(",") ~ "..." ~> param ^^ {
      case p => List(p.copy(kind = Variadic))
    } | success(Nil)

  // syntax-directed operation (SDO) head generator
  lazy val sdoHeadGen: Parser[
    Option[SyntaxDirectedOperationHead.Target] => SyntaxDirectedOperationHead,
  ] = {
    semanticsKind ~ name ~ params ~ retTy ^^ {
      case isStatic ~ x ~ params ~ rty =>
        SyntaxDirectedOperationHead(_, x, isStatic, params, rty)
    }
  }.named("spec.SyntaxDirectedOperationHead")

  // concrete method head generator
  lazy val concMethodHeadGen: Parser[Param => ConcreteMethodHead] = {
    name ~ params ~ retTy ^^ {
      case name ~ params ~ rty =>
        ConcreteMethodHead(name, _, params, rty)
    }
  }.named("spec.ConcreteMethodHead")

  // internal method head generator
  lazy val inMethodHeadGen: Parser[Param => InternalMethodHead] = {
    ("[[" ~> name <~ "]]") ~ params ~ retTy ^^ {
      case name ~ params ~ rty =>
        InternalMethodHead(name, _, params, rty)
    }
  }.named("spec.InternalMethodHead")

  // built-in heads
  lazy val builtinHead: Parser[BuiltinHead] = {
    builtinRef ~ params ~ retTy ^^ {
      case r ~ params ~ rty => BuiltinHead(r, params, rty)
    }
  }.named("spec.BuiltinHead")

  // built-in head references
  given builtinRef: Parser[BuiltinHead.Ref] = {
    import BuiltinHead.Ref
    import BuiltinHead.Ref.*
    lazy val name: Parser[String] = "[_`a-zA-Z0-9]+".r ^^ { _.trim }
    lazy val yet: Parser[String] = "[_`%a-zA-Z0-9.\\[\\]@: ]+".r ^^ { _.trim }
    lazy val pre: Parser[Ref => Ref] =
      "get " ^^^ { Getter(_) } | "set " ^^^ { Setter(_) } | "" ^^^ { x => x }
    lazy val base: Parser[Ref] =
      opt("%") ~> name <~ opt("%") ^^ { Base(_) }
    lazy val access: Parser[Ref => Ref] =
      "." ~> name ^^ { case n => NormalAccess(_, n) } |
      "[" ~> "@@" ~> name <~ "]" ^^ { case s => SymbolAccess(_, s) }
    pre ~ base ~ rep(access) <~ (guard("(") | not(".".r)) ^^ {
      case p ~ b ~ as => p(as.foldLeft(b) { case (b, a) => a(b) })
    } | yet ^^ { YetRef(_) }
  }.named("spec.BuiltinHead.Ref")

  // ---------------------------------------------------------------------------
  // Basic Helpers
  // ---------------------------------------------------------------------------
  /** identifiers */
  lazy val specId: Parser[String] =
    "_[^_]+_".r ^^ { s => s.substring(1, s.length - 1) }

  /** names */
  lazy val name: Parser[String] =
    "[a-zA-Z0-9/]+".r

  lazy val retTy: Parser[Type] =
    opt(":" ~> specTy) ^^ { _.getOrElse(AnyType) }

  // runtime/static semantics
  lazy val semanticsKind: Parser[Boolean] =
    (("Runtime" | "Static") <~ "Semantics" ~ ":") ^^ { _ == "Static" }
}
