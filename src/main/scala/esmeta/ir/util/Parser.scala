package esmeta.ir.util

import esmeta.ir.*
import esmeta.util.{Locational, BasicParsers}
import esmeta.util.BaseUtils.*
import scala.collection.mutable.ListBuffer

/** IR parser */
object Parser extends Parsers

/** IR parsers */
trait Parsers extends BasicParsers {
  override protected val whiteSpace = whiteSpaceWithComment

  // programs
  given program: Parser[Program] = rep(func) ^^ { Program(_) }

  // functions
  given func: Parser[Func] =
    (main <~ "def") ~ funcKind ~ "(\\w|:)+".r ~ params ~ inst ^^ {
      case m ~ k ~ n ~ ps ~ b => Func(m, k, n, ps, b)
    }

  lazy val main: Parser[Boolean] = opt("@main") ^^ { _.isDefined }

  // function kinds
  given funcKind: Parser[Func.Kind] =
    import Func.Kind.*
    "<NUM>:" ^^^ NumMeth |
    "<SYNTAX>:" ^^^ SynDirOp |
    "<CONC>:" ^^^ ConcMeth |
    "<INTERNAL>:" ^^^ InternalMeth |
    "<BUILTIN>:" ^^^ Builtin |
    "<CLO>:" ^^^ Clo |
    "<CONT>:" ^^^ Cont |
    "" ^^^ AbsOp

  // function parameters
  lazy val params: Parser[List[Func.Param]] = "(" ~> repsep(param, ",") <~ ")"
  given param: Parser[Func.Param] = name ~ opt("?") ~ (":" ~> ty) ^^ {
    case x ~ o ~ t => Func.Param(x, o.isDefined, t)
  }

  // instructions
  given inst: Parser[Inst] = withLoc {
    "let" ~> name ~ ("=" ~> expr) ^^ {
      case x ~ e => ILet(x, e)
    } | "delete" ~> ref ^^ {
      case r => IDelete(r)
    } | "push" ~> expr ~ (">" ^^^ true | "<" ^^^ false) ~ expr ^^ {
      case x ~ f ~ y => if (f) IPush(x, y, f) else IPush(y, x, f)
    } | "return" ~> expr ^^ {
      case e => IReturn(e)
    } | "assert" ~> expr ^^ {
      case e => IAssert(e)
    } | "print" ~> expr ^^ {
      case e => IPrint(e)
    } | "nop" ^^ {
      case _ => INop()
    } | "{" ~> rep(inst) <~ "}" ^^ {
      ISeq(_)
    } | ("if " ~> expr) ~ ("then" ~> inst) ~ ("else" ~> inst) ^^ {
      case c ~ t ~ e => IIf(c, t, e)
    } | ("loop[" ~> "[^\\]]+".r <~ "]") ~ expr ~ ("then" ~> inst) ^^ {
      case k ~ c ~ b => ILoop(k, c, b)
    } | ("call " ~> id <~ "=") ~ expr ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
      case lhs ~ f ~ as => ICall(lhs, f, as)
    } | ref ~ ("=" ~> expr) ^^ {
      case r ~ e => IAssign(r, e)
    } | expr ^^ {
      case e => IExpr(e)
    }
  }

  // expressions
  given expr: Parser[Expr] =
    "comp" ~> ("[" ~> expr ~ ("/" ~> expr) <~ "]") ~ ("(" ~> expr <~ ")") ^^ {
      case ty ~ tgt ~ e => EComp(ty, e, tgt)
    } | "(" ~ "comp?" ~> expr <~ ")" ^^ {
      case e => EIsCompletion(e)
    } | "[" ~> ("?" ^^^ true | "!" ^^^ false) ~ expr <~ "]" ^^ {
      case c ~ e => EReturnIfAbrupt(e, c)
    } | "(" ~ "pop" ~> ("<" ^^^ true | ">" ^^^ false) ~ expr <~ ")" ^^ {
      case f ~ e => EPop(e, f)
    } | "(" ~ "parse" ~> expr ~ expr <~ ")" ^^ {
      case c ~ r => EParse(c, r)
    } | "(" ~ "rule" ~> ("|" ~> word <~ "|") ~ parseParams <~ ")" ^^ {
      case x ~ ps => EParseRule(x, ps)
    } | "(" ~ "source-text" ~> expr <~ ")" ^^ {
      ESourceText(_)
    } | "(" ~ "yet" ~> string <~ ")" ^^ {
      case msg => EYet(msg)
    } | "(" ~ "contains" ~> expr ~ expr <~ ")" ^^ {
      case l ~ e => EContains(l, e)
    } | "(" ~ "str-concat" ~> rep(expr) <~ ")" ^^ {
      case es => EStrConcat(es)
    } | "(" ~ "substring" ~> expr ~ expr ~ expr <~ ")" ^^ {
      case e ~ f ~ t => ESubstring(e, f, t)
    } | "(" ~> uop ~ expr <~ ")" ^^ {
      case u ~ e => EUnary(u, e)
    } | "(" ~> bop ~ expr ~ expr <~ ")" ^^ {
      case b ~ l ~ r => EBinary(b, l, r)
    } | "(" ~> vop ~ rep(expr) <~ ")" ^^ {
      case v ~ es => EVariadic(v, es)
    } | "(" ~> cop ~ expr <~ ")" ^^ {
      case c ~ e => EConvert(c, e)
    } | "(" ~ "typeof" ~> expr <~ ")" ^^ {
      case e => ETypeOf(e)
    } | "(" ~ "?" ~> expr ~ (":" ~> ty) <~ ")" ^^ {
      case e ~ t => ETypeCheck(e, t)
    } | "clo<" ~> fname ~ opt("," ~ "[" ~> repsep(name, ",") <~ "]") <~ ">" ^^ {
      case s ~ cs => EClo(s, cs.getOrElse(Nil))
    } | ("cont<" ~> fname <~ ">") ^^ {
      case s => ECont(s)
    } | astExpr | allocExpr | literal | ref ^^ { ERef(_) }

  // function name
  lazy val fname: Parser[String] = "[^<>, ]+".r

  // abstract syntax tree (AST) expressions
  lazy val astExpr: Parser[AstExpr] =
    ("|" ~> word <~ "|") ~ parseParams ~
    ("<" ~> int <~ ">") ~
    (opt("(" ~> repsep(opt(expr), ",") <~ ")") ^^ { _.getOrElse(Nil) }) ^^ {
      case n ~ as ~ i ~ es => ESyntactic(n, as, i, es)
    } ||| ("|" ~> word <~ "|") ~ ("(" ~> expr <~ ")") ^^ {
      case n ~ e => ELexical(n, e)
    }
  lazy val parseParams: Parser[List[Boolean]] =
    opt("[" ~> rep(simpleBool) <~ "]") ^^ { _.getOrElse(Nil) }
  lazy val simpleBool: Parser[Boolean] = "T" ^^^ true | "F" ^^^ false

  // allocation expressions
  lazy val allocExpr: Parser[AllocExpr] = (
    ("(" ~ "new" ~> ident ~ opt(fields) <~ ")") ~ asite ^^ {
      case t ~ fields ~ a => EMap(t, fields.getOrElse(Nil), a)
    } | ("(" ~ "new" ~ "[" ~> repsep(expr, ",") <~ "]" ~ ")") ~ asite ^^ {
      case es ~ a => EList(es, a)
    } | ("(" ~ "list-concat" ~> rep(expr) <~ ")") ~ asite ^^ {
      case es ~ a => EListConcat(es, a)
    } | ("(" ~ "new" ~> "'" ~> expr <~ ")") ~ asite ^^ {
      case e ~ a => ESymbol(e, a)
    } | ("(" ~ "copy" ~> expr <~ ")") ~ asite ^^ {
      case e ~ a => ECopy(e, a)
    } | ("(" ~ "keys" ~> opt("-int") ~ expr <~ ")") ~ asite ^^ {
      case i ~ e ~ a => EKeys(e, i.isDefined, a)
    }
  )

  // fields
  lazy val fields: Parser[List[(Expr, Expr)]] = "(" ~> repsep(field, ",") <~ ")"
  lazy val field: Parser[(Expr, Expr)] =
    expr ~ ("->" ~> expr) ^^ { case k ~ v => k -> v }

  // allocation sites
  lazy val asite: Parser[Int] = "[#" ~> int <~ "]"

  // literals
  lazy val literal: Parser[LiteralExpr] =
    bigint <~ "n" ^^ { EBigInt(_) } |
    double <~ "f" ^^ { ENumber(_) } |
    ("+INF" | "INF") ^^^ ENumber(Double.PositiveInfinity) |
    "-INF" ^^^ ENumber(Double.NegativeInfinity) |
    "NaN" ^^^ ENumber(Double.NaN) |
    decimal ^^ { EMathVal(_) } |
    string ^^ { EStr(_) } |
    bool ^^ { EBool(_) } |
    "undefined" ^^^ EUndef |
    "null" ^^^ ENull |
    "absent" ^^^ EAbsent |
    "~" ~> "[^~]+".r <~ "~" ^^ { EConst(_) }

  // unary operators
  given uop: Parser[UOp] =
    import UOp.*
    "abs" ^^^ Abs |
    "floor" ^^^ Floor |
    "-" ^^^ Neg |
    "!" ^^^ Not |
    "~" ^^^ BNot

  // binary operators
  given bop: Parser[BOp] =
    import BOp.*
    "+" ^^^ Plus |
    "-" ^^^ Sub |
    "**" ^^^ Pow |
    "*" ^^^ Mul |
    "/" ^^^ Div |
    "%%" ^^^ UMod |
    "%" ^^^ Mod |
    "==" ^^^ Equal |
    "=" ^^^ Eq |
    "&&" ^^^ And |
    "||" ^^^ Or |
    "^^" ^^^ Xor |
    "&" ^^^ BAnd |
    "|" ^^^ BOr |
    "^" ^^^ BXOr |
    "<<" ^^^ LShift |
    "<" ^^^ Lt |
    ">>>" ^^^ URShift |
    ">>" ^^^ SRShift
  // "str+" ^^^ Concat |
  // "str<" ^^^ StrLt

  // variadic operators
  given vop: Parser[VOp] =
    import VOp.*
    "min" ^^^ Min |
    "max" ^^^ Max

  // conversion operators
  given cop: Parser[COp] =
    import COp.*
    "[bigint]" ^^^ ToBigInt |
    "[number]" ^^^ ToNumber |
    "[math]" ^^^ ToMath |
    "[str" ~> opt(expr) <~ "]" ^^ { ToStr(_) }

  // references
  given ref: Parser[Ref] =
    val prop = "." ~> ident ^^ { EStr(_) } | "[" ~> expr <~ "]"
    id ~ rep(prop) ^^ { case x ~ es => es.foldLeft[Ref](x)(Prop(_, _)) }

  // identifiers
  lazy val id: Parser[Id] =
    "@[A-Z_]+".r ^^ { case s => Global(s.substring(1)) } |
    "%(0|[1-9][0-9]*)".r ^^ { case s => Temp(s.substring(1).toInt) } |
    name

  // named local identifiers
  lazy val name: Parser[Name] = "[_a-zA-Z][_a-zA-Z0-9]*".r ^^ { Name(_) }

  // TODO types
  given ty: Parser[Type] = ident ^^ { Type(_) }

  // helper for locations
  private def withLoc[T <: Locational](parser: Parser[T]): Parser[T] =
    parser ~ opt("@" ~> loc) ^^ { case t ~ loc => t.loc = loc; t }
}
