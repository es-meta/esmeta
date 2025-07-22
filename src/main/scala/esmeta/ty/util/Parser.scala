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
    lazy val parent =
      ("extends " ^^^ true | "=" ^^^ false) ~ ident ^^ { case e ~ x => (x, e) }
    lazy val tyStr = "[^;]+".r ^^ { _.trim }
    "type " ~> ident ~ opt(parent) ~
    opt("{" ~> rep(tyDeclElem <~ ";") <~ "}") ^^ {
      case x ~ p ~ es => TyDecl(x, p, es.getOrElse(Nil))
    }
  }.named("ty.TyDecl")

  // type declaration elements
  given tyDeclElem: Parser[TyDecl.Elem] = {
    import TyDecl.Elem.*
    lazy val remain = "[^;]+".r ^^ { _.trim }
    "abstract " ~ "def " ~> ident ^^ {
      case x => AbsMethod(x)
    } | "def " ~> ident ~ opt("?") ~ opt("=" ~> remain) ^^ {
      case x ~ q ~ t => ConMethod(x, q.isDefined, t)
    } | ident ~ opt("?") ~ opt(":" ~> remain) ^^ {
      case x ~ q ~ t => TyDecl.Elem.Field(x, q.isDefined, t.getOrElse("Any"))
    }
  }.named("ty.TyDecl.Elem")

  // field type map
  given fieldMap: Parser[FieldMap] = {
    lazy val field = word ~ binding ^^ { case f ~ b => f -> b }
    "{" ~> rep(field <~ opt(",")) <~ "}" ^^ { case ts => FieldMap(ts.toMap) }
  }.named("ty.FieldMap")

  // field bindings
  given binding: Parser[Binding] = {
    opt(opt("?") ~ (":" ~> valueTy)) ^^ {
      case None        => Binding.Exist
      case Some(o ~ v) => Binding(v, o.isDefined)
    }
  }.named("ty.Binding")

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
    "Normal" ~> opt(
      "[" ~> valueTy <~ "]" ^^ { NormalT(_) } |
      fieldMap ^^ { RecordT("NormalCompletion", _) },
    ) ^^ { _.getOrElse(NormalT) } |
    "Abrupt" ~> opt(
      "[" ~> rep1sep(ident, ",") <~ "]" ^^ { xs => AbruptT(xs.toSet) } |
      fieldMap ^^ { RecordT("AbruptCompletion", _) },
    ) ^^ { _.getOrElse(AbruptT) } |
    "Break" ~> opt(
      fieldMap ^^ { RecordT("BreakCompletion", _) },
    ) ^^ { _.getOrElse(ReturnT) } |
    "Continue" ~> opt(
      fieldMap ^^ { RecordT("ContinueCompletion", _) },
    ) ^^ { _.getOrElse(ReturnT) } |
    "Return" ~> opt(
      fieldMap ^^ { RecordT("ReturnCompletion", _) },
    ) ^^ { _.getOrElse(ReturnT) } |
    "Throw" ~> opt(
      fieldMap ^^ { RecordT("ThrowCompletion", _) },
    ) ^^ { _.getOrElse(ThrowT) } |
    // ECMAScript value
    "ESValue" ^^^ ESValueT |
    // closures
    singleCloTy ^^ { case c => ValueTy(clo = c) } |
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
    "Enum" ~> opt("[" ~> rep1sep(enumv, ",") <~ "]") ^^ {
      case s => ValueTy(enumv = s.fold(Inf)(es => Fin(es.toSet)))
    } |
    "Enum" ^^^ ValueTy(enumv = Inf) |
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

  /** closure types */
  given cloTy: Parser[CloTy] = {
    rep1sep(singleCloTy, "|") ^^ { case ts => ts.foldLeft(CloTy.Bot)(_ || _) }
  }.named("ty.CloTy")

  private lazy val singleCloTy: Parser[CloTy] =
    "Clo[" ~> rep1sep(string, ",") <~ "]" ^^ {
      case s => CloSetTy(s.toSet)
    } | ("Clo[(" ~> rep1sep(valueTy, ",") <~ ")" ~ "=>") ~ valueTy <~ "]" ^^ {
      case ps ~ r => CloArrowTy(ps, r)
    } | "Clo" ^^^ CloTopTy

  /** record types */
  given recordTy: Parser[RecordTy] = {
    rep1sep(singleRecordTy, "|") ^^ {
      case ts => ts.foldLeft(RecordTy.Bot)(_ || _)
    }
  }.named("ty.RecordTy")

  private lazy val singleRecordTy: Parser[RecordTy] = {
    import RecordTy.*
    lazy val pair =
      "FunctionObject" ^^^ ("Object" -> FieldMap.init("Call")) |
      "Constructor" ^^^ ("Object" -> FieldMap.init("Call", "Construct")) |
      opt(word) ~ opt(fieldMap) ^^ {
        case k ~ v => (k.getOrElse(""), v.getOrElse(FieldMap.Top))
      }
    "Record[" ~> repsep(pair, "|") <~ "]" ^^ {
      case fs => Elem(fs.toMap)
    } | "Record" ^^^ Top
  }.named("ty.RecordTy (single)")

  given sign: Parser[Sign] = {
    val neg = "-" ^^^ Sign.Neg | "" ^^^ Sign.Bot
    val zero = "0" ^^^ Sign.Zero | "" ^^^ Sign.Bot
    val pos = "+" ^^^ Sign.Pos | "" ^^^ Sign.Bot
    neg ~ zero ~ pos ^^ { case n ~ z ~ p => n || z || p }
  }.named("ty.Sign")

  private lazy val intTy: Parser[IntTy] = {
    lazy val intSignTy =
      "Int" ~> "[" ~> sign <~ "]" ^^ { case s => IntSignTy(s) }
    lazy val intSetTy =
      "Int" ~> "[" ~> rep1sep(bigInt, ",") <~ "]" ^^ {
        case ds => IntSetTy(ds.toSet)
      }
    lazy val intTop = "Int" ^^^ IntSignTy(Sign.Top)
    intSignTy | intSetTy | intTop
  }.named("ty.IntTy")

  /** mathematical value types */
  given mathTy: Parser[MathTy] = {
    rep1sep(singleMathTy, "|") ^^ { case ts => ts.foldLeft(MathTy.Bot)(_ || _) }
  }.named("ty.MathTy")

  private lazy val singleMathTy: Parser[MathTy] =
    lazy val mathSignTy =
      "Math[" ~> sign <~ "]" ^^ { case s => MathSignTy(s) }
    lazy val mathIntTy = intTy.map(MathIntTy(_))
    lazy val mathSetTy =
      "Math[" ~> rep1sep(decimal, ",") <~ "]" ^^ {
        case ds => MathSetTy(ds.toSet.map(Math(_)))
      }
    lazy val mathTop = "Math" ^^^ MathSignTy(Sign.Top)
    mathSignTy | mathIntTy | mathSetTy | mathTop

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

  private lazy val numberIntTy: Parser[(IntTy, Boolean)] = {
    lazy val nan: Parser[Boolean] = "|" ~ "NaN" ^^^ true | "" ^^^ false
    lazy val intSignTy =
      ("NumberInt" ~> "[" ~> sign <~ "]") ~ nan ^^ {
        case s ~ n => (IntSignTy(s), n)
      }
    lazy val intSetTy =
      ("NumberInt" ~> "[" ~> rep1sep(bigInt, ",") <~ "]") ~ nan ^^ {
        case ds ~ n => (IntSetTy(ds.toSet), n)
      }
    lazy val intTop = "NumberInt" ~> nan ^^ {
      case n => (IntSignTy(Sign.Top), n)
    }
    intSignTy | intSetTy | intTop
  }.named("ty.NumberIntTy")

  private lazy val singleNumberTy: Parser[NumberTy] =
    lazy val nan: Parser[Boolean] = "|" ~ "NaN" ^^^ true | "" ^^^ false
    lazy val numSignTy =
      ("Number[" ~> sign <~ "]") ~ nan ^^ { case s ~ n => NumberSignTy(s, n) }
    lazy val numIntTy = numberIntTy.map(NumberIntTy(_, _))
    lazy val numSetTy =
      "Number[" ~> rep1sep(numberWithSpecial, ",") <~ "]" ^^ {
        case ns => NumberSetTy(ns.toSet)
      }
    lazy val numTop = "Number" ^^^ NumberTy.Top
    numSignTy | numIntTy | numSetTy | numTop | "NaN" ^^^ NumberTy.NaN

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
