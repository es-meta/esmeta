package esmeta.ir.util

import esmeta.ir.*
import esmeta.lang.Syntax
import esmeta.ty.*
import esmeta.ty.util.{Parsers => TyParsers}
import esmeta.util.BaseUtils.*
import esmeta.util.{Locational, BasicParsers}

/** IR parser */
object Parser extends Parsers

/** IR parsers */
trait Parsers extends TyParsers {
  override protected val whiteSpace = whiteSpaceWithComment

  // programs
  given program: Parser[Program] = {
    rep(func) ^^ { Program(_) }
  }.named("ir.Program")

  // functions
  given func: Parser[Func] = {
    (main <~ "def") ~
    funcKind ~
    "[<>\\w|:\\.\\[\\],@]+".r ~
    params ~
    retTy ~
    ("=" ~> inst) ^^ {
      case m ~ k ~ n ~ ps ~ rty ~ b => Func(m, k, n, ps, rty, b)
    }
  }.named("ir.Func")

  lazy val main: Parser[Boolean] = opt("@main") ^^ { _.isDefined }

  // function kinds
  given funcKind: Parser[FuncKind] = {
    import FuncKind.*
    "<NUM>:" ^^^ NumMeth |
    "<SYNTAX>:" ^^^ SynDirOp |
    "<CONC>:" ^^^ ConcMeth |
    "<INTERNAL>:" ^^^ InternalMeth |
    "<BUILTIN>:" ^^^ Builtin |
    "<CLO>:" ^^^ Clo |
    "<CONT>:" ^^^ Cont |
    "<BUILTIN-CLO>:" ^^^ BuiltinClo |
    "" ^^^ AbsOp
  }.named("ir.FuncKind")

  // function parameters
  lazy val params: Parser[List[Param]] =
    "(" ~> repsep(param, ",") <~ opt(",") ~ ")"
  given param: Parser[Param] = {
    name ~ opt("?") ~ (":" ~> irType) ^^ {
      case x ~ o ~ t => Param(x, t, o.isDefined)
    }
  }.named("ir.Param")

  // return types
  lazy val retTy: Parser[Type] =
    opt(":" ~> irType) ^^ { _.getOrElse(UnknownType) }

  // instructions
  given inst: Parser[Inst] = withLoc {
    "{" ~> rep(inst) <~ "}" ^^ {
      ISeq(_)
    } | branchInst | callInst | normalInst
  }.named("ir.Inst")

  lazy val callInst: Parser[CallInst] =
    lazy val args: Parser[List[Expr]] = ("(" ~> repsep(expr, ",") <~ ")")
    ("call" ~> local <~ "=") ~ expr ~ args ^^ {
      case lhs ~ f ~ as => ICall(lhs, f, as)
    } | ("sdo-call" ~> local <~ "=") ~ expr ~ ("->" ~> word) ~ args ^^ {
      case lhs ~ a ~ m ~ as => ISdoCall(lhs, a, m, as)
    }

  given normalInsts: Parser[List[NormalInst]] =
    rep(normalInst).named("List[ir.NormalInst]")
  lazy val normalInst: Parser[NormalInst] =
    "let" ~> name ~ ("=" ~> expr) ^^ {
      case x ~ e => ILet(x, e)
    } | "delete" ~> ref ^^ {
      case r => IDelete(r)
    } | "push" ~> expr ~ (">" ^^^ true | "<" ^^^ false) ~ expr ^^ {
      case x ~ f ~ y => if (f) IPush(x, y, f) else IPush(y, x, f)
    } | "remove" ~> expr ~ expr ^^ {
      case e ~ l => IRemove(e, l)
    } | "return" ~> expr ^^ {
      case e => IReturn(e)
    } | "assert" ~> expr ^^ {
      case e => IAssert(e)
    } | "print" ~> expr ^^ {
      case e => IPrint(e)
    } | "nop" ^^ {
      case _ => INop()
    } | ref ~ ("=" ~> expr) ^^ {
      case r ~ e => IAssign(r, e)
    } | expr ^^ {
      case e => IExpr(e)
    }

  given branchInst: Parser[BranchInst] = {
    ("if " ~> expr) ~ inst ~ opt("else" ~> inst) ^^ {
      case (c ~ t ~ Some(e)) => IIf(c, t, e)
      case (c ~ t ~ None)    => IIf(c, t, ISeq(Nil))
    } | ("while " ~> expr) ~ inst ^^ {
      case c ~ b => IWhile(c, b)
    }
  }.named("ir.BranchInst")

  // expressions
  given expr: Parser[Expr] = {
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
    } | "(" ~ "nt" ~> ("|" ~> word <~ "|") ~ parseParams <~ ")" ^^ {
      case x ~ ps => ENt(x, ps)
    } | "(" ~ "source-text" ~> expr <~ ")" ^^ {
      ESourceText(_)
    } | "(" ~ "yet" ~> string <~ ")" ^^ {
      case msg => EYet(msg)
    } | "(" ~ "contains" ~> expr ~ expr <~ ")" ^^ {
      case l ~ e => EContains(l, e)
    } | "(" ~ "substring" ~> expr ~ expr ~ opt(expr) <~ ")" ^^ {
      case e ~ f ~ t => ESubstring(e, f, t)
    } | "(" ~ "trim" ~ ">" ~> expr <~ ")" ^^ {
      case e => ETrim(e, true)
    } | "(" ~ "trim" ~> expr <~ "<" ~ ")" ^^ {
      case e => ETrim(e, false)
    } | "(" ~> uop ~ expr <~ ")" ^^ {
      case u ~ e => EUnary(u, e)
    } | "(" ~> bop ~ expr ~ expr <~ ")" ^^ {
      case b ~ l ~ r => EBinary(b, l, r)
    } | "(" ~> vop ~ rep(expr) <~ ")" ^^ {
      case v ~ es => EVariadic(v, es)
    } | "(clamp" ~> expr ~ expr ~ expr <~ ")" ^^ {
      case t ~ l ~ u => EClamp(t, l, u)
    } | "(" ~> mop ~ rep(expr) <~ ")" ^^ {
      case m ~ es => EMathOp(m, es)
    } | "(" ~> cop ~ expr <~ ")" ^^ {
      case c ~ e => EConvert(c, e)
    } | "(" ~ "typeof" ~> expr <~ ")" ^^ {
      case e => ETypeOf(e)
    } | "(" ~ "?" ~> expr ~ (":" ~> expr) <~ ")" ^^ {
      case e ~ t => ETypeCheck(e, t)
    } | "(" ~ "duplicated" ~> expr <~ ")" ^^ {
      case e => EDuplicated(e)
    } | "(" ~ "array-index" ~> expr <~ ")" ^^ {
      case e => EIsArrayIndex(e)
    } | "clo<" ~> fname ~ opt("," ~ "[" ~> repsep(name, ",") <~ "]") <~ ">" ^^ {
      case s ~ cs => EClo(s, cs.getOrElse(Nil))
    } | ("cont<" ~> fname <~ ">") ^^ {
      case s => ECont(s)
    } | "(" ~ "debug" ~> expr <~ ")" ^^ {
      case e => EDebug(e)
    } | "(" ~ "random" ~ ")" ^^^ {
      ERandom()
    } | astExpr | allocExpr | literal | ref ^^ { ERef(_) }
  }.named("ir.Expr")

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
  lazy val allocExpr: Parser[AllocExpr] = asite(
    ("(" ~ "new" ~> opt(word) ~ opt(fields) <~ ")") ^^ {
      case t ~ fs => ERecord(t.getOrElse("Record"), fs.getOrElse(Nil))
    } | ("(" ~ "map" ~> opt(pairs) <~ ")") ^^ {
      case ps => EMap(ps.getOrElse(Nil))
    } | ("(" ~ "new" ~ "[" ~> repsep(expr, ",") <~ "]" ~ ")") ^^ {
      case es => EList(es)
    } | ("(" ~ "list-concat" ~> rep(expr) <~ ")") ^^ {
      case es => EListConcat(es)
    } | ("(" ~ "new" ~> "'" ~> expr <~ ")") ^^ {
      case e => ESymbol(e)
    } | ("(" ~ "copy" ~> expr <~ ")") ^^ {
      case e => ECopy(e)
    } | ("(" ~ "keys" ~> opt("-int") ~ expr <~ ")") ^^ {
      case i ~ e => EKeys(e, i.isDefined)
    } | "(" ~ "get-children" ~> expr <~ ")" ^^ {
      case e => EGetChildren(e)
    } | "(" ~ "get-items" ~> expr ~ expr <~ ")" ^^ {
      case n ~ a => EGetItems(n, a)
    },
  )

  // fields
  lazy val fields: Parser[List[(String, Expr)]] =
    "{" ~> repsep(pair(string ~ (":" ~> expr)), ",") <~ "}"

  // key-value pairs
  lazy val pairs: Parser[List[(Expr, Expr)]] =
    "{" ~> repsep(pair(expr ~ ("->" ~> expr)), ",") <~ "}"

  // allocation sites
  def asite(parser: Parser[AllocExpr]): Parser[AllocExpr] =
    parser ~ opt("[#" ~> int <~ "]") ^^ {
      case e ~ k => e.asite = k.getOrElse(-1); e
    }

  // literals
  lazy val literal: Parser[LiteralExpr] =
    s"${integer}n".r ^^ { s => EBigInt(BigInt(s.dropRight(1))) } |
    s"${number}f".r ^^ { s => ENumber(s.dropRight(1).toDouble) } |
    s"${integer}cu".r ^^ { s => ECodeUnit(s.dropRight(2).toInt.toChar) } |
    "+NUM_INF" ^^^ ENumber(Double.PositiveInfinity) |
    "-NUM_INF" ^^^ ENumber(Double.NegativeInfinity) |
    "NaN" ^^^ ENumber(Double.NaN) |
    decimal ^^ { EMath(_) } |
    "+INF" ^^^ EInfinity(pos = true) |
    "-INF" ^^^ EInfinity(pos = false) |
    string ^^ { EStr(_) } |
    bool ^^ { EBool(_) } |
    "undefined" ^^^ EUndef() |
    "null" ^^^ ENull() |
    "absent" ^^^ EAbsent() |
    "~" ~> "[^~]+".r <~ "~" ^^ { EEnum(_) }

  // unary operators
  given uop: Parser[UOp] = {
    import UOp.*
    "abs" ^^^ Abs |
    "floor" ^^^ Floor |
    "-" ^^^ Neg |
    "!" ^^^ Not |
    "~" ^^^ BNot
  }.named("ir.UOp")

  // binary operators
  given bop: Parser[BOp] = {
    import BOp.*
    "+" ^^^ Add |
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
  }.named("ir.BOp")

  // variadic operators
  given vop: Parser[VOp] = {
    import VOp.*
    "min" ^^^ Min |
    "max" ^^^ Max |
    "concat" ^^^ Concat
  }.named("ir.VOp")

  // mathematical operators
  given mop: Parser[MOp] = {
    import MOp.*
    "[math:expm1]" ^^^ Expm1 |
    "[math:log10]" ^^^ Log10 |
    "[math:log2]" ^^^ Log2 |
    "[math:cos]" ^^^ Cos |
    "[math:cbrt]" ^^^ Cbrt |
    "[math:exp]" ^^^ Exp |
    "[math:cosh]" ^^^ Cosh |
    "[math:sinh]" ^^^ Sinh |
    "[math:tanh]" ^^^ Tanh |
    "[math:acos]" ^^^ Acos |
    "[math:acosh]" ^^^ Acosh |
    "[math:asinh]" ^^^ Asinh |
    "[math:atanh]" ^^^ Atanh |
    "[math:asin]" ^^^ Asin |
    "[math:atan2]" ^^^ Atan2 |
    "[math:atan]" ^^^ Atan |
    "[math:log1p]" ^^^ Log1p |
    "[math:log]" ^^^ Log |
    "[math:sin]" ^^^ Sin |
    "[math:sqrt]" ^^^ Sqrt |
    "[math:tan]" ^^^ Tan
  }.named("ir.MOp")

  // conversion operators
  given cop: Parser[COp] = {
    import COp.*
    "[approx-number]" ^^^ ToApproxNumber |
    "[number]" ^^^ ToNumber |
    "[bigInt]" ^^^ ToBigInt |
    "[math]" ^^^ ToMath |
    "[str" ~> opt(expr) <~ "]" ^^ { ToStr(_) }
  }.named("ir.COp")

  // references
  given ref: Parser[Ref] = {
    val field = "." ~> ident ^^ { EStr(_) } | "[" ~> expr <~ "]"
    x ~ rep(field) ^^ { case x ~ es => es.foldLeft[Ref](x)(Field(_, _)) }
  }.named("ir.Ref")

  // identifiers
  lazy val x: Parser[Var] =
    "@[A-Za-z_]+".r ^^ { case s => Global(s.substring(1)) } |
    local
  lazy val local: Parser[Local] = temp | name

  // named local identifiers
  lazy val name: Parser[Name] = "[_a-zA-Z][_a-zA-Z0-9]*".r ^^ { Name(_) }
  lazy val temp: Parser[Temp] = "%(0|[1-9][0-9]*)".r ^^ {
    case s => Temp(s.substring(1).toInt)
  }

  // types
  given irType: Parser[Type] = {
    ty ^^ { Type(_) }
  }.named("ir.Type")

  // helper for locations
  private def withLoc(parser: Parser[Inst]): Parser[Inst] =
    parser ~ opt("@" ~> loc) ^^ {
      case i ~ l =>
        i.langOpt = Some(new Syntax { loc = l })
        i
    }

  // helper for pairs
  private def pair[A, B](parser: Parser[A ~ B]): Parser[(A, B)] =
    parser ^^ { case a ~ b => (a, b) }
}
