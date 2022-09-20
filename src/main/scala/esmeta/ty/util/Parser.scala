package esmeta.ty.util

import esmeta.state.{Nt, Number}
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.BasicParsers

/** metalanguage parser */
object Parser extends Parsers
trait Parsers extends BasicParsers {
  // types
  given ty: Parser[Ty] = {
    unknownTy |
    valueTy
  }.named("ty.Ty")

  lazy val unknownTy: Parser[UnknownTy] = {
    "Unknown" ~> opt("[" ~> str <~ "]") ^^ { UnknownTy(_) }
  }.named("ty.UnknownTy")

  lazy val valueTy: Parser[ValueTy] = {
    rep1sep(singleValueTy, "|") ^^ {
      case ts => ts.foldLeft(ValueTy.Bot)(_ || _)
    }
  }.named("ty.ValueTy")

  private lazy val singleValueTy: Parser[ValueTy] = {
    singleCompTy ^^ { case t => ValueTy(comp = t) } |
    singleSubMapTy ^^ { case t => ValueTy(subMap = t) } |
    singlePureValueTy ^^ { case t => ValueTy(pureValue = t) }
  }.named("ty.ValueTy (single)")

  /** completion record types */
  given compTy: Parser[CompTy] = {
    rep1sep(singleCompTy, "|") ^^ { case ts => ts.foldLeft(CompTy.Bot)(_ || _) }
  }.named("ty.CompTy")

  private lazy val singleCompTy: Parser[CompTy] = {
    "Normal[" ~> pureValueTy <~ "]" ^^ { case v => CompTy(normal = v) } |
    "Abrupt" ^^^ CompTy(abrupt = true)
  }.named("ty.CompTy (single)")

  /** pure value types (non-completion record types) */
  given pureValueTy: Parser[PureValueTy] = {
    rep1sep(singlePureValueTy, "|") ^^ {
      case ts => ts.foldLeft(PureValueTy.Bot)(_ || _)
    }
  }.named("ty.PureValueTy")

  private lazy val singlePureValueTy: Parser[PureValueTy] = {
    // ECMAScript value
    "ESValue" ^^^ ESValueT.pureValue |
    // closure
    "Clo[" ~> rep1sep(str, ",") <~ "]" ^^ {
      case s => PureValueTy(clo = Fin(s.toSet))
    } | "Clo" ^^^ PureValueTy(clo = Inf) |
    // continuation
    "Cont[" ~> rep1sep(int, ",") <~ "]" ^^ {
      case s => PureValueTy(cont = Fin(s.toSet))
    } | "Cont" ^^^ PureValueTy(cont = Inf) |
    // record
    singleRecordTy ^^ { case r => PureValueTy(record = r) } |
    // list
    singleListTy ^^ { case l => PureValueTy(list = l) } |
    // symbol
    "Symbol" ^^^ PureValueTy(symbol = true) |
    // AST value
    singleAstValueTy ^^ { case ast => PureValueTy(astValue = ast) } |
    // nt
    "Nt[" ~> rep1sep(nt, ",") <~ "]" ^^ {
      case s => PureValueTy(nt = Fin(s.toSet))
    } | "Nt" ^^^ PureValueTy(nt = Inf) |
    // code unit
    "CodeUnit" ^^^ PureValueTy(codeUnit = true) |
    // constant
    "Const[" ~> rep1sep(const, ",") <~ "]" ^^ {
      case s => PureValueTy(const = s.toSet)
    } |
    // mathematical value
    "Math[" ~> rep1sep(decimal, ",") <~ "]" ^^ {
      case m => PureValueTy(math = Fin(m.toSet))
    } | "Math" ^^^ PureValueTy(math = Inf) |
    // number
    "Number[" ~> rep1sep(numberWithSpecial, ",") <~ "]" ^^ {
      case n => PureValueTy(number = Fin(n.toSet))
    } | "Number" ^^^ PureValueTy(number = Inf) |
    // big integer
    "BigInt" ^^^ PureValueTy(bigInt = true) |
    // string
    "String[" ~> rep1sep(str, ",") <~ "]" ^^ {
      case s => PureValueTy(str = Fin(s.toSet))
    } | "String" ^^^ PureValueTy(str = Inf) |
    // boolean
    "Boolean" ^^^ PureValueTy(bool = Set(true, false)) |
    "True" ^^^ PureValueTy(bool = Set(true)) |
    "False" ^^^ PureValueTy(bool = Set(false)) |
    // undefined
    "Undefined" ^^^ PureValueTy(undef = true) |
    // null
    "Null" ^^^ PureValueTy(nullv = true) |
    // absent
    "Absent" ^^^ PureValueTy(absent = true) |
    // name
    singleNameTy ^^ { case name => PureValueTy(name = name) }
  }.named("ty.PureValueTy (single)")

  private lazy val numberWithSpecial: Parser[Number] =
    double ^^ { Number(_) } |
    ("+INF" | "INF") ^^^ Number(Double.PositiveInfinity) |
    "-INF" ^^^ Number(Double.NegativeInfinity) |
    "NaN" ^^^ Number(Double.NaN)

  private lazy val nt: Parser[Nt] =
    ("|" ~> word <~ "|") ~ opt(parseParams) ^^ {
      case x ~ ps => Nt(x, ps.getOrElse(Nil))
    }
  private lazy val parseParams: Parser[List[Boolean]] =
    opt("[" ~> rep(simpleBool) <~ "]") ^^ { _.getOrElse(Nil) }
  private lazy val simpleBool: Parser[Boolean] =
    "T" ^^^ true | "F" ^^^ false
  private lazy val const: Parser[String] =
    "~" ~> "[^~]+".r <~ "~"
  private lazy val str: Parser[String] =
    """"[^"]*"""".r ^^ { case s => s.substring(1, s.length - 1) }

  /** named record types */
  given nameTy: Parser[NameTy] = {
    rep1sep(singleNameTy, "|") ^^ {
      case ts => ts.foldLeft(NameTy.Bot)(_ || _)
    }
  }.named("ty.NameTy")

  private lazy val singleNameTy: Parser[NameTy] = {
    camel ^^ { case name => NameTy(Set(name)) }
  }.named("ty.NameTy (single)")

  /** record types */
  given recordTy: Parser[RecordTy] = {
    rep1sep(singleRecordTy, "|") ^^ {
      case ts => ts.foldLeft(RecordTy.Bot)(_ || _)
    }
  }.named("ty.RecordTy")

  private lazy val singleRecordTy: Parser[RecordTy] = {
    "{" ~> rep1sep(field, ",") <~ "}" ^^ {
      case pairs => RecordTy(pairs.toMap)
    }
  }.named("ty.RecordTy (single)")

  private lazy val field: Parser[(String, Option[ValueTy])] =
    ("[[" ~> word <~ "]]") ~ opt(":" ~> valueTy) ^^ { case k ~ v => (k, v) }

  /** list types */
  given listTy: Parser[ListTy] = {
    rep1sep(singleListTy, "|") ^^ { case ts => ts.foldLeft(ListTy.Bot)(_ || _) }
  }.named("ty.ListTy")

  private lazy val singleListTy: Parser[ListTy] = {
    "List[" ~> valueTy <~ "]" ^^ { case v => ListTy(Some(v)) } |
    "Nil" ^^^ ListTy(Some(ValueTy.Bot))
  }.named("ty.ListTy (single)")

  /** AST value types */
  given astValueTy: Parser[AstValueTy] = {
    rep1sep(singleAstValueTy, "|") ^^ {
      case ts => ts.foldLeft[AstValueTy](AstValueTy.Bot)(_ || _)
    }
  }.named("ty.AstValueTy")

  private lazy val singleAstValueTy: Parser[AstValueTy] = {
    "Ast:" ~> word ~ ("[" ~> int) ~ ("," ~> int <~ "]") ^^ {
      case x ~ i ~ j => AstSingleTy(x, i, j)
    } | "Ast[" ~> repsep(word, ",") <~ "]" ^^ {
      case xs => AstNameTy(xs.toSet)
    } | "Ast" ^^^ AstTopTy
  }.named("ty.ListTy (single)")

  /** sub map types */
  given subMapTy: Parser[SubMapTy] = {
    rep1sep(singleSubMapTy, "|") ^^ {
      case ts => ts.foldLeft(SubMapTy.Bot)(_ || _)
    }
  }.named("ty.SubMapTy")

  private lazy val singleSubMapTy: Parser[SubMapTy] = {
    "SubMap[" ~> pureValueTy ~
    ("|->" ~> pureValueTy) <~ "]" ^^ { case k ~ v => SubMapTy(k, v) }
  }.named("ty.SubMapTy (single)")
}
