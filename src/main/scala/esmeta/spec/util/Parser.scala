package esmeta.spec.util

import esmeta.*
import esmeta.lang.*
import esmeta.lang.util.{Parsers => LangParsers}
import esmeta.spec.{*, given}
import esmeta.ty.*
import esmeta.util.BaseUtils.*
import esmeta.util.HtmlUtils.*
import esmeta.util.SystemUtils.*

/** specification parsers */
object Parser extends Parsers
trait Parsers extends LangParsers {
  // skip only white spaces and comments
  override val whiteSpace = "[ \t]*//.*|[ \t]+".r

  type NtArg = NonterminalArgument
  type NtArgKind = NonterminalArgumentKind

  // ---------------------------------------------------------------------------
  // Grammar
  // ---------------------------------------------------------------------------
  /** grammar */
  given grammarParser: Parser[Grammar] = {
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
          r <- origRs.toVector
          s <- r.symbols
        } yield Rhs(Nil, List(s), None)
        Production(l, k, true, rs)
      case l ~ k ~ None ~ rs => Production(l, k, false, rs.toVector)
    }
  }.named("spec.Production")

  /** production kinds */
  given prodKind: Parser[ProductionKind] = {
    import ProductionKind.*
    ":::" ^^^ NumericString | "::" ^^^ Lexical | ":" ^^^ Syntactic
  }.named("spec.ProductionKind")

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
    opt(rhsConds) ~ rep1(symbol) ~ opt(rhsId) ^^ {
      case cs ~ ss ~ i => Rhs(cs.getOrElse(Nil), ss, i)
    }
  }.named("spec.Rhs")

  /** RHS conditions */
  given rhsConds: Parser[List[RhsCond]] = {
    "[" ~> rep1sep(rhsCond, ",") <~ "]"
  }.named("List[spec.RhsCond]")

  given rhsCond: Parser[RhsCond] = {
    ("[+~]".r) ~ word ^^ { case str ~ name => RhsCond(name, str == "+") }
  }.named("spec.RhsCond")

  /** grammar symbols */
  given symbol: Parser[Symbol] = {
    {
      term | butnot | lookahead | butOnlyIf | nt | cp | abbr | unicodeSet |
      empty | nlt
    } ~ opt("?") ^^ { case s ~ o => if (o.isDefined) Optional(s) else s }
  }.named("spec.Symbol")

  /** terminals */
  lazy val term: Parser[Terminal] = {
    "`[^`]+`|`[`]+`".r ^^ {
      case str =>
        Terminal(str.substring(1, str.length - 1))
    }
  }.named("spec.Terminal")

  /** nonterminals */
  lazy val nt: Parser[Nonterminal] = {
    word ~ opt("[" ~> rep1sep(ntArg, ",") <~ "]") ^^ {
      case name ~ args => Nonterminal(name, args.getOrElse(Nil))
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
  lazy val cp: Parser[CodePoint] = {
    lazy val unicode = "U[+]([1-9A-F]|10)?[0-9A-F]{4}".r
    // https://www.unicode.org/reports/tr34/#UAX34-R1
    lazy val desc = rep1("[-A-Z0-9]+".r) ^^ { _.mkString(" ") }
    "<" ~> unicode ~ opt(desc | "(" ~> desc <~ ")") <~ ">" ^^ {
      case c ~ d => CodePoint(Integer.parseInt(c.drop(2), 16), d.getOrElse(""))
    }
  }.named("spec.CodePoint")

  /** symbols for code point abbreviations */
  lazy val abbr: Parser[CodePointAbbr] = {
    "<" ~> word <~ ">" ^^ { CodePointAbbr(_) }
  }.named("spec.CodePointAbbr")

  /** symbols for sets of unicode code points with a condition */
  lazy val unicodeSet: Parser[UnicodeSet] = {
    ">" ~ ("any" ~ opt("Unicode") ~ "code point") ~> opt(".+".r) ^^ {
      UnicodeSet(_)
    }
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
    ntArgKind ~ word ^^ { case kind ~ name => NonterminalArgument(kind, name) }
  }.named("spec.NtArg")

  /** nonterminal argument kinds */
  given ntArgKind: Parser[NtArgKind] = {
    import NonterminalArgumentKind.*
    "+" ^^^ True | "~" ^^^ False | "?" ^^^ Pass
  }.named("spec.NtArg.Kind")

  // ---------------------------------------------------------------------------
  // Algorithms
  // ---------------------------------------------------------------------------

  // abstract operation (AO) heads
  lazy val absOpHeadGen: Parser[Boolean => AbstractOperationHead] = {
    opt(semanticsKind) ~> name ~ params ~ retTy ^^ {
      case name ~ params ~ rty => AbstractOperationHead(_, name, params, rty)
    }
  }.named("spec.AbstractOperationHead")

  // numeric method heads
  lazy val numMethodHead: Parser[NumericMethodHead] = {
    (langType <~ "::") ~ name ~ params ~ retTy ^^ {
      case t ~ x ~ ps ~ rty => NumericMethodHead(t, x, ps, rty)
    }
  }.named("spec.NumericMethodHead")

  // algorithm parameters
  given param: Parser[Param] = {
    opt(paramKind) ~ specId ~ specType ^^ {
      case opt ~ name ~ ty =>
        val kind = opt.getOrElse(ParamKind.Normal)
        Param(name, ty, kind)
    }
  }.named("spec.Param")

  // algorithm parameter kinds
  lazy val paramKind: Parser[ParamKind] =
    import ParamKind.*
    "optional" ^^^ Optional | "..." ^^^ Variadic | "…" ~ "," ^^^ Ellipsis

  // algorithm parameter description
  lazy val paramDesc: Parser[Param] =
    langTypeWithUnknown ~ opt(specId) <~ opt("(.*)".r) ^^ {
      case ty ~ name => Param(name.getOrElse("this"), ty)
    }

  // multiple algorithm parameters
  lazy val params: Parser[List[Param]] = {
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
    import ParamKind.*
    "[" ~> opt(",") ~> param ~ opt(oldOptParams) <~ "]" ^^ {
      case p ~ psOpt => p.copy(kind = Optional) :: psOpt.getOrElse(Nil)
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
    builtinPath ~ params ~ retTy ^^ {
      case r ~ params ~ rty => BuiltinHead(r, params, rty)
    }
  }.named("spec.BuiltinHead")

  // built-in head paths
  given builtinPath: Parser[BuiltinPath] = {
    import BuiltinPath.*
    type Path = BuiltinPath
    lazy val name: Parser[String] = "[_`a-zA-Z0-9]+".r ^^ { _.trim }
    lazy val yet: Parser[String] = "[_`%a-zA-Z0-9.\\[\\]@: ]+".r ^^ { _.trim }
    lazy val pre: Parser[Path => Path] =
      "get " ^^^ { Getter(_) } | "set " ^^^ { Setter(_) } | "" ^^^ { x => x }
    lazy val base: Parser[Path] =
      opt("%") ~> name <~ opt("%") ^^ { Base(_) }
    lazy val access: Parser[Path => Path] =
      "." ~> name ^^ { case n => NormalAccess(_, n) } |
      "[" ~> "@@" ~> name <~ "]" ^^ { case s => SymbolAccess(_, s) }
    pre ~ base ~ rep(access) <~ (guard("(") | not(".".r)) ^^ {
      case p ~ b ~ as => p(as.foldLeft(b) { case (b, a) => a(b) })
    } | yet ^^ { YetPath(_) }
  }.named("spec.BuiltinPath")

  // ---------------------------------------------------------------------------
  // Types
  // ---------------------------------------------------------------------------
  // optional metalanguage types with colon
  lazy val specType: PL[Type] =
    opt(":" ~> langTypeWithUnknown) ^^ { case ty => ty.getOrElse(UnknownType) }

  // metalanguage return types
  lazy val retTy: PL[Type] = specType

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
    } ^^ { case l ~ n ~ s ~ w => GrammarSummary(l, n, s, w) }
    val algos = {
      (empty ~ "- algorithms:" ~ ".*".r) ~>
      (empty ~ "- complete:" ~> int) ~
      (empty ~ "- incomplete:" ~> int)
    } ^^ { case c ~ i => AlgorithmSummary(c, i) }
    val steps = {
      (empty ~ "- algorithm steps:" ~ ".*".r) ~>
      (empty ~ "- complete:" ~> int) ~
      (empty ~ "- incomplete:" ~> int)
    } ^^ { case c ~ i => StepSummary(c, i) }
    val types = {
      (empty ~ "- types:" ~ ".*".r) ~>
      (empty ~ "- known:" ~> int) ~
      (empty ~ "- yet:" ~> int) ~
      (empty ~ "- unknown:" ~> int)
    } ^^ { case c ~ u ~ n => TypeSummary(c, u, n) }
    val tables = empty ~ "- tables:" ~> int
    val tyModel = empty ~ "- type model:" ~> int <~ empty
    version ~ grammar ~ algos ~ steps ~ types ~ tables ~ tyModel ^^ {
      case v ~ g ~ a ~ s ~ ty ~ t ~ m => Summary(v, g, a, s, ty, t, m)
    }
  }.named("spec.Summary")

  // ---------------------------------------------------------------------------
  // Basic Helpers
  // ---------------------------------------------------------------------------
  /** identifiers */
  lazy val specId: Parser[String] =
    "_[^_]+_".r ^^ { s => s.substring(1, s.length - 1) }

  /** names */
  lazy val name: Parser[String] = "[a-zA-Z0-9/]+".r

  // runtime/static semantics
  lazy val semanticsKind: Parser[Boolean] =
    (("Runtime" | "Static") <~ "Semantics" ~ ":") ^^ { _ == "Static" }
}
