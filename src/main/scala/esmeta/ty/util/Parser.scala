package esmeta.ty.util

import esmeta.state.{GrammarSymbol, Number, Math}
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.BasicParsers

/** metalanguage parser */
object Parser extends Parsers
trait Parsers extends BasicParsers {

  override protected val whiteSpace = whiteSpaceWithComment

  // type model
  given tyModel: Parser[TyModel] = {
    rep(tyDecl) ^^ { case ds => TyModel(ds) }
  }.named("ty.TyModel")

  // type declarations
  given tyDecl: Parser[TyDecl] = {
    lazy val extend = "extends " ~> ident
    lazy val tyStr = "[^;]+".r ^^ { _.trim }
    lazy val field = word ~ opt(":" ~> tyStr) <~ ";" ^^ {
      case k ~ v => (k, v.getOrElse("Any"))
    }
    "type " ~> ident ~ opt(extend) ~
    opt("{" ~> rep(tyDeclElem <~ ";") <~ "}") ^^ {
      case x ~ p ~ es => TyDecl(x, p, es.getOrElse(Nil))
    }
  }.named("ty.TyDecl")

  // type declaration elements
  given tyDeclElem: Parser[TyDecl.Elem] = {
    lazy val remain = "[^;]+".r ^^ { _.trim }
    "def " ~> ident ~ opt("?") ~ opt("=" ~> remain) ^^ {
      case x ~ q ~ t => TyDecl.Elem.Method(x, q.isDefined, t)
    } | ident ~ opt("?") ~ (":" ~> remain) ^^ {
      case x ~ q ~ t => TyDecl.Elem.Field(x, q.isDefined, t)
    }
  }.named("ty.TyDecl.Elem")

  // field map
  given fieldMap: Parser[FieldMap] = {
    "{" ~> rep(field <~ opt(",")) <~ "}" ^^ { case ts => FieldMap(ts.toMap) }
  }.named("ty.FieldMap")

  // optional value types
  given optValueTy: Parser[OptValueTy] = {
    valueTy ~ opt("?") ^^ { case v ~ o => OptValueTy(v, o.isDefined) }
  }.named("ty.OptValueTy")

  // types
  given ty: Parser[Ty] = {
    unknownTy |
    valueTy
  }.named("ty.Ty")

  lazy val unknownTy: Parser[UnknownTy] = {
    "Unknown" ~> opt("[" ~> string <~ "]") ^^ { UnknownTy(_) }
  }.named("ty.UnknownTy")

  lazy val valueTy: Parser[ValueTy] = {
    rep1sep(singleValueTy, "|") ^^ {
      case ts => ts.foldLeft(ValueTy.Bot)(_ || _)
    }
  }.named("ty.ValueTy")

  private lazy val singleValueTy: Parser[ValueTy] = {
    "Any" ^^^ AnyT |
    "Bot" ^^^ BotT |
    // completion record
    "Completion" ^^^ CompT |
    "Normal" ~> opt("[" ~> valueTy <~ "]") ^^ {
      case None    => NormalT
      case Some(v) => NormalT(v)
    } | "Abrupt" ~> opt("[" ~> rep1(ident) <~ "]") ^^ {
      case None        => AbruptT
      case Some(names) => AbruptT(names.toSet)
    } |
    // ECMAScript value
    "ESValue" ^^^ ESValueT |
    // closure
    "Clo[" ~> rep1sep(string, ",") <~ "]" ^^ {
      case s => ValueTy(clo = Fin(s.toSet))
    } | "Clo" ^^^ ValueTy(clo = Inf) |
    // continuation
    "Cont[" ~> rep1sep(int, ",") <~ "]" ^^ {
      case s => ValueTy(cont = Fin(s.toSet))
    } | "Cont" ^^^ ValueTy(cont = Inf) |
    // record
    singleRecordTy ^^ { case r => ValueTy(record = r) } |
    // map
    singleMapTy ^^ { case t => ValueTy(map = t) } |
    // list
    singleListTy ^^ { case l => ValueTy(list = l) } |
    // AST value
    singleAstTy ^^ { case ast => ValueTy(ast = ast) } |
    // grammar symbol
    "GrammarSymbol[" ~> rep1sep(grammarSymbol, ",") <~ "]" ^^ {
      case s => ValueTy(grammarSymbol = Fin(s.toSet))
    } | "GrammarSymbol" ^^^ ValueTy(grammarSymbol = Inf) |
    // code unit
    "CodeUnit" ^^^ ValueTy(codeUnit = true) |
    // enum
    "Enum[" ~> rep1sep(enumv, ",") <~ "]" ^^ {
      case s => ValueTy(enumv = Fin(s.toSet))
    } |
    // mathematical value
    singleMathTy ^^ { case m => ValueTy(math = m) } |
    // infinity
    singleInfinityTy ^^ { case i => ValueTy(infinity = i) } |
    // number
    singleNumberTy ^^ { case n => ValueTy(number = n) } |
    // big integer
    "BigInt" ^^^ ValueTy(bigInt = true) |
    // string
    "String[" ~> rep1sep(string, ",") <~ "]" ^^ {
      case s => ValueTy(str = Fin(s.toSet))
    } | "String" ^^^ ValueTy(str = Inf) |
    // boolean
    singleBoolTy ^^ { case b => ValueTy(bool = b) } |
    // undefined
    "Undefined" ^^^ ValueTy(undef = true) |
    // null
    "Null" ^^^ ValueTy(nullv = true)
  }.named("ty.ValueTy (single)")

  private lazy val numberWithSpecial: Parser[Number] =
    doubleWithSpecial ^^ { Number(_) }
  private lazy val doubleWithSpecial: Parser[Double] =
    double |
    ("+INF" | "INF") ^^^ Double.PositiveInfinity |
    "-INF" ^^^ Double.NegativeInfinity |
    "NaN" ^^^ Double.NaN

  private lazy val grammarSymbol: Parser[GrammarSymbol] =
    ("|" ~> word <~ "|") ~ opt(parseParams) ^^ {
      case x ~ ps => GrammarSymbol(x, ps.getOrElse(Nil))
    }
  private lazy val parseParams: Parser[List[Boolean]] =
    opt("[" ~> rep(simpleBool) <~ "]") ^^ { _.getOrElse(Nil) }
  private lazy val simpleBool: Parser[Boolean] =
    "T" ^^^ true | "F" ^^^ false
  private lazy val enumv: Parser[String] =
    "~" ~> "[^~]+".r <~ "~"

  /** record types */
  given recordTy: Parser[RecordTy] = {
    rep1sep(singleRecordTy, "|") ^^ {
      case ts => ts.foldLeft(RecordTy.Bot)(_ || _)
    }
  }.named("ty.RecordTy")

  private lazy val singleRecordTy: Parser[RecordTy] = {
    import RecordTy.*
    lazy val pair = opt(word) ~ opt(fieldMap) ^^ {
      case k ~ v => (k.getOrElse(""), v.getOrElse(FieldMap.Top))
    }
    "Record[" ~> repsep(pair, "|") <~ "]" ^^ {
      case fs => Elem(fs.toMap).normalized
    } | "Record" ^^^ Top
  }.named("ty.RecordTy (single)")

  /** mathematical value types */
  given mathTy: Parser[MathTy] = {
    rep1sep(singleMathTy, "|") ^^ { case ts => ts.foldLeft(MathTy.Bot)(_ || _) }
  }.named("ty.MathTy")

  private lazy val singleMathTy: Parser[MathTy] =
    "Math[" ~> rep1sep(decimal, ",") <~ "]"
    ^^ { case ds => MathSetTy(ds.toSet.map(Math(_))) } |
    camel
    ^? {
      case "Int"       => IntTy
      case "NonPosInt" => NonPosIntTy
      case "NonNegInt" => NonNegIntTy
      case "NegInt"    => NegIntTy
      case "PosInt"    => PosIntTy
      case "Math"      => MathTopTy
    }

  /** infinity types */
  given infTy: Parser[InfinityTy] = {
    rep1sep(singleInfinityTy, "|") ^^ {
      case ts => ts.foldLeft(InfinityTy.Bot)(_ || _)
    }
  }.named("ty.InfinityTy")

  /** number types */
  given numberTy: Parser[NumberTy] = {
    rep1sep(singleNumberTy, "|") ^^ {
      case ts => ts.foldLeft(NumberTy.Bot)(_ || _)
    }
  }.named("ty.NumberTy")

  private lazy val singleNumberTy: Parser[NumberTy] =
    "NumberInt" ^^^ NumberIntTy |
    "Number[" ~> rep1sep(numberWithSpecial, ",") <~ "]" ^^ {
      case n => NumberSetTy(n.toSet)
    } | "Number" ^^^ NumberTopTy

  private lazy val singleInfinityTy: Parser[InfinityTy] =
    "INF" ^^^ InfinityTy.Top | "+INF" ^^^ InfinityTy.Pos | "-INF" ^^^ InfinityTy.Neg

  given boolTy: Parser[BoolTy] = {
    rep1sep(singleBoolTy, "|") ^^ { case ts => ts.foldLeft(BoolTy.Bot)(_ || _) }
  }.named("ty.BoolTy")

  private lazy val singleBoolTy: Parser[BoolTy] =
    // boolean
    "Boolean" ^^^ BoolTy(Set(false, true)) |
    "True" ^^^ BoolTy(Set(true)) |
    "False" ^^^ BoolTy(Set(false))

  private lazy val field: Parser[(String, OptValueTy)] =
    word ~ opt("?") ~ opt(":" ~> valueTy) ^^ {
      case k ~ o ~ v => (k, OptValueTy(v.getOrElse(AnyT), o.isDefined))
    }

  /** list types */
  given listTy: Parser[ListTy] = {
    rep1sep(singleListTy, "|") ^^ { case ts => ts.foldLeft(ListTy.Bot)(_ || _) }
  }.named("ty.ListTy")

  private lazy val singleListTy: Parser[ListTy] = {
    import ListTy.*
    "List[" ~> valueTy <~ "]" ^^ { case v => Elem(v) } |
    "List" ^^^ Top |
    "Nil" ^^^ Nil
  }.named("ty.ListTy (single)")

  /** AST value types */
  given astTy: Parser[AstTy] = {
    rep1sep(singleAstTy, "|") ^^ {
      case ts => ts.foldLeft[AstTy](AstTy.Bot)(_ || _)
    }
  }.named("ty.AstTy")

  private lazy val singleAstTy: Parser[AstTy] = {
    "Ast[" ~> word ~ ("[" ~> int <~ "]") <~ "]" ^^ {
      case x ~ i => AstTy.Detail(x, i)
    } | "Ast[" ~> repsep(word, "|") <~ "]" ^^ {
      case xs => AstTy.Simple(xs.toSet)
    } | "Ast" ^^^ AstTy.Top
  }.named("ty.ListTy (single)")

  /** map types */
  given mapTy: Parser[MapTy] = {
    rep1sep(singleMapTy, "|") ^^ {
      case ts => ts.foldLeft(MapTy.Bot)(_ || _)
    }
  }.named("ty.MapTy")

  private lazy val singleMapTy: Parser[MapTy] = {
    import MapTy.*
    "Map[" ~> valueTy ~ ("->" ~> valueTy) <~ "]" ^^ {
      case k ~ v => Elem(k, v)
    } | "Map" ^^^ Top
  }.named("ty.MapTy (single)")
}
