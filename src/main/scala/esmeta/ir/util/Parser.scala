package esmeta.ir.util

import esmeta.ir.*
import esmeta.lang.Syntax
import esmeta.ty.*
import esmeta.util.BaseUtils.*
import esmeta.util.{Locational, BasicParsers}

/** IR parser */
object Parser extends Parsers

/** IR parsers */
trait Parsers extends BasicParsers {
  override protected val whiteSpace = whiteSpaceWithComment

  // programs
  given program: Parser[Program] = {
    rep(func) ^^ { Program(_) }
  }.named("ir.Program")

  // functions
  given func: Parser[Func] = {
    (main <~ "def") ~ funcKind ~ "[<>\\w|:\\.\\[\\],@]+".r ~ params ~ retTy ~ inst ^^ {
      case m ~ k ~ n ~ ps ~ rty ~ b => Func(m, k, n, ps, rty, b)
    }
  }.named("ir.Func")

  lazy val main: Parser[Boolean] = opt("@main") ^^ { _.isDefined }

  // function kinds
  given funcKind: Parser[Func.Kind] = {
    import Func.Kind.*
    "<NUM>:" ^^^ NumMeth |
    "<SYNTAX>:" ^^^ SynDirOp |
    "<CONC>:" ^^^ ConcMeth |
    "<INTERNAL>:" ^^^ InternalMeth |
    "<BUILTIN>:" ^^^ Builtin |
    "<CLO>:" ^^^ Clo |
    "<CONT>:" ^^^ Cont |
    "<BUILTIN-CLO>:" ^^^ BuiltinClo |
    "" ^^^ AbsOp
  }.named("ir.Func.Kind")

  // function parameters
  lazy val params: Parser[List[Func.Param]] = "(" ~> repsep(param, ",") <~ ")"
  given param: Parser[Func.Param] = {
    name ~ opt("?") ~ (":" ~> ty) ^^ {
      case x ~ o ~ t => Func.Param(x, o.isDefined, t)
    }
  }.named("ir.Func.Param")

  // return types
  lazy val retTy: Parser[Type] = opt(":" ~> ty) ^^ { _.getOrElse(Type()) }

  // instructions
  given inst: Parser[Inst] = withLoc {
    "{" ~> rep(inst) <~ "}" ^^ {
      ISeq(_)
    } | ("if " ~> expr) ~ inst ~ ("else" ~> inst) ^^ {
      case c ~ t ~ e => IIf(c, t, e)
    } | ("loop[" ~> "[^\\]]+".r <~ "]") ~ expr ~ inst ^^ {
      case k ~ c ~ b => ILoop(k, c, b)
    } | callInst | normalInst
  }.named("ir.Inst")

  lazy val callInst: Parser[CallInst] =
    lazy val args: Parser[List[Expr]] = ("(" ~> repsep(expr, ",") <~ ")")
    ("call" ~> local <~ "=") ~ expr ~ args ^^ {
      case lhs ~ f ~ as => ICall(lhs, f, as)
    } | ("method-call" ~> local <~ "=") ~ ref ~ ("->" ~> word) ~ args ^^ {
      case lhs ~ b ~ m ~ as => IMethodCall(lhs, b, m, as)
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
    } | "remove-elem" ~> expr ~ expr ^^ {
      case l ~ e => IRemoveElem(l, e)
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
    } | "(" ~ "grammar" ~> ("|" ~> word <~ "|") ~ parseParams <~ ")" ^^ {
      case x ~ ps => EGrammar(x, ps)
    } | "(" ~ "source-text" ~> expr <~ ")" ^^ {
      ESourceText(_)
    } | "(" ~ "yet" ~> string <~ ")" ^^ {
      case msg => EYet(msg)
    } | "(" ~ "contains" ~> expr ~ expr ~ opt(
      ":" ~> pair(ty ~ word),
    ) <~ ")" ^^ {
      case l ~ e ~ f => EContains(l, e, f)
    } | "(" ~ "substring" ~> expr ~ expr ~ opt(expr) <~ ")" ^^ {
      case e ~ f ~ t => ESubstring(e, f, t)
    } | "(" ~> uop ~ expr <~ ")" ^^ {
      case u ~ e => EUnary(u, e)
    } | "(" ~> bop ~ expr ~ expr <~ ")" ^^ {
      case b ~ l ~ r => EBinary(b, l, r)
    } | "(clamp" ~> expr ~ expr ~ expr <~ ")" ^^ {
      case t ~ l ~ u => EClamp(t, l, u)
    } | "(" ~> vop ~ rep(expr) <~ ")" ^^ {
      case v ~ es => EVariadic(v, es)
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
    ("(" ~ "new" ~> ty ~ opt(fields) <~ ")") ^^ {
      case t ~ fields => EMap(t, fields.getOrElse(Nil))
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
    } | "(" ~ "get-children" ~> opt(expr) ~ expr <~ ")" ^^ {
      case k ~ a => EGetChildren(k, a)
    },
  )

  // fields
  lazy val fields: Parser[List[(Expr, Expr)]] = "(" ~> repsep(field, ",") <~ ")"
  lazy val field: Parser[(Expr, Expr)] =
    expr ~ ("->" ~> expr) ^^ { case k ~ v => k -> v }

  // allocation sites
  def asite(parser: Parser[AllocExpr]): Parser[AllocExpr] =
    parser ~ opt("[#" ~> int <~ "]") ^^ {
      case e ~ k => e.asite = k.getOrElse(-1); e
    }

  // literals
  lazy val literal: Parser[LiteralExpr] =
    bigInt <~ "n" ^^ { EBigInt(_) } |
    double <~ "f" ^^ { ENumber(_) } |
    codeUnit <~ "cu" ^^ { ECodeUnit(_) } |
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
  }.named("ir.BOp")

  // variadic operators
  given vop: Parser[VOp] = {
    import VOp.*
    "min" ^^^ Min |
    "max" ^^^ Max |
    "concat" ^^^ Concat
  }.named("ir.VOp")

  // conversion operators
  given cop: Parser[COp] = {
    import COp.*
    "[bigInt]" ^^^ ToBigInt |
    "[number]" ^^^ ToNumber |
    "[math]" ^^^ ToMath |
    "[str" ~> opt(expr) <~ "]" ^^ { ToStr(_) }
  }.named("ir.COp")

  // references
  given ref: Parser[Ref] = {
    val prop = "." ~> ident ^^ { EStr(_) } | "[" ~> expr <~ "]"
    id ~ rep(prop) ^^ { case x ~ es => es.foldLeft[Ref](x)(Prop(_, _)) }
  }.named("ir.Ref")

  // identifiers
  lazy val id: Parser[Id] =
    "@[A-Za-z_]+".r ^^ { case s => Global(s.substring(1)) } |
    local
  lazy val local: Parser[Local] =
    "%(0|[1-9][0-9]*)".r ^^ { case s => Temp(s.substring(1).toInt) } |
    name

  // named local identifiers
  lazy val name: Parser[Name] = "[_a-zA-Z][_a-zA-Z0-9]*".r ^^ { Name(_) }

  // TODO types
  given ty: Parser[Type] = {
    ident ^^ { case s => Type(s) }
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
