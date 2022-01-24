package esmeta.cfg

import esmeta.util.{Loc, BasicParsers}

/** CFG parsers */
trait Parsers extends BasicParsers {
  // treat comments as white spaces
  override protected val whiteSpace =
    """(\s|/\*+[^*]*\*+(?:[^/*][^*]*\*+)*/|//[^\u000A\u000D\u2028\u2029]*)+""".r

  // control flow graphs (CFGs)
  given cfg: Parser[CFG] = rep(funcWithMain) ^^ {
    case pairs =>
      var main = -1
      // TODO check multiple main functions
      val funcs = for ((func, isMain) <- pairs) yield {
        if (isMain) main = func.id
        func
      }
      CFG(main, funcs)
  }

  // functions
  given func: Parser[Func] = (
    getId ~ funcKind ~ ident ~ params ~ nodes
  ) ^^ { case i ~ k ~ n ~ ps ~ (en ~ ns ~ ex) => Func(i, k, n, ps, en, ns, ex) }

  // functions with main
  lazy val funcWithMain: Parser[(Func, Boolean)] =
    opt("@main") ~ func ^^ { case m ~ f => (f, m.isDefined) }

  // function kinds
  given funcKind: Parser[Func.Kind] =
    import Func.Kind.*
    "<NUM>:" ^^^ NumMeth |
    "<SYNTAX>:" ^^^ SynDirOp |
    "<CONC>:" ^^^ ConcMeth |
    "<BUILTIN>:" ^^^ Builtin |
    "<CLO>:" ^^^ Clo |
    "<CONT>:" ^^^ Cont |
    "" ^^^ AbsOp

  // function parameters
  lazy val params: Parser[List[Param]] = "(" ~> repsep(param, ",") <~ ")"
  given param: Parser[Param] =
    name ~ (":" ~> ty) ^^ { case x ~ t => Param(x, t) }

  // nodes
  lazy val nodes: Parser[Entry ~ List[Block] ~ Exit] =
    "{" ~> entry ~ rep(block) ~ exit <~ "}"
  given node: Parser[Node] = entry | exit | block

  // entry nodes
  lazy val entry: Parser[Entry] =
    getId("entry") ~ ("->" ~> int) ^^ { case i ~ n => Entry(i, n) }

  // exit nodes
  lazy val exit: Parser[Exit] =
    getId("exit") ^^ { case i => Exit(i) }

  // block nodes
  lazy val block: Parser[Block] =
    getId ~ insts ~ ("->" ~> int) ^^ {
      case i ~ is ~ n => Linear(i, is.toVector, n)
    } | getId("branch") ~ cond ~ ("-t>" ~> int) ~ ("-f>" ~> int) ^^ {
      case i ~ (k ~ c ~ l) ~ t ~ e => Branch(i, k, c, l, t, e)
    } | getId("call") ~ id ~ ("=" ~> expr) ~ args ~ ("->" ~> int) ^^ {
      case i ~ x ~ f ~ (as ~ l) ~ n => Call(i, x, f, as, l, n)
    }

  // conditions with locations
  lazy val cond: Parser[Branch.Kind ~ Expr ~ Option[Loc]] =
    branchKind ~ ("(" ~> expr <~ ")") ~ locOpt

  // branch kinds
  given branchKind: Parser[Branch.Kind] =
    import Branch.Kind.*
    "if" ^^^ If | "while" ^^^ While | "foreach" ^^^ Foreach

  // arguments
  lazy val args: Parser[List[Expr] ~ Option[Loc]] =
    ("(" ~> repsep(expr, ",") <~ ")") ~ locOpt

  // instructions
  lazy val insts: Parser[List[Inst]] =
    inst ^^ { List(_) } | "{" ~> rep(inst) <~ "}"
  given inst: Parser[Inst] =
    "let" ~> name ~ ("=" ~> expr) ~ locOpt ^^ {
      case x ~ e ~ l => ILet(x, e, l)
    } | "delete" ~> ref ~ locOpt ^^ {
      case r ~ l => IDelete(r, l)
    } | "push" ~> expr ~ (">" ^^^ true | "<" ^^^ false) ~ expr ~ locOpt ^^ {
      case x ~ f ~ y ~ l => if (f) IPush(x, y, f, l) else IPush(y, x, f, l)
    } | "return" ~> expr ~ locOpt ^^ {
      case e ~ l => IReturn(e, l)
    } | "assert" ~> expr ~ locOpt ^^ {
      case e ~ l => IAssert(e, l)
    } | "print" ~> expr ~ locOpt ^^ {
      case e ~ l => IPrint(e, l)
    } | ref ~ ("=" ~> expr) ~ locOpt ^^ {
      case r ~ e ~ l => IAssign(r, e, l)
    }

  lazy val locOpt: Parser[Option[Loc]] = opt("@" ~> loc)

  // expressions
  given expr: Parser[Expr] =
    "comp" ~> ("[" ~> expr ~ ("/" ~> expr) <~ "]") ~ ("(" ~> expr <~ ")") ^^ {
      case ty ~ tgt ~ e => EComp(ty, tgt, e)
    } | "(" ~ "comp?" ~> expr <~ ")" ^^ {
      case e => EIsCompletion(e)
    } | "[" ~> ("?" ^^^ true | "!" ^^^ false) ~ expr <~ "]" ^^ {
      case c ~ e => EReturnIfAbrupt(e, c)
    } | "(" ~ "pop" ~> ("<" ^^^ true | ">" ^^^ false) ~ expr <~ ")" ^^ {
      case f ~ e => EPop(e, f)
    } | "(" ~ "yet" ~> string <~ ")" ^^ {
      case msg => EYet(msg)
    } | "(" ~ "contains" ~> expr ~ expr <~ ")" ^^ {
      case l ~ e => EContains(l, e)
    } | "(" ~> uop ~ expr <~ ")" ^^ {
      case u ~ e => EUnary(u, e)
    } | "(" ~> bop ~ expr ~ expr <~ ")" ^^ {
      case b ~ l ~ r => EBinary(b, l, r)
    } | "(" ~> cop ~ expr <~ ")" ^^ {
      case c ~ e => EConvert(c, e)
    } | "(" ~ "typeof" ~> expr <~ ")" ^^ {
      case e => ETypeOf(e)
    } | "(" ~ "?" ~> expr ~ (":" ~> ty) <~ ")" ^^ {
      case e ~ t => ETypeCheck(e, t)
    } | ("clo[" ~> int <~ "]") ~ opt("(" ~> repsep(name, ",") <~ ")") ^^ {
      case fid ~ as => EClo(fid, as.getOrElse(Nil))
    } | ("cont[" ~> int <~ "]") ^^ {
      case fid => ECont(fid)
    } | astExpr | allocExpr | literal | ref ^^ { ERef(_) }

  // abstract syntax tree (AST) expressions
  lazy val astExpr: Parser[AstExpr] =
    ("|" ~> word <~ "|") ~
    (opt("[" ~> rep(simpleBool) <~ "]") ^^ { _.getOrElse(Nil) }) ~
    ("<" ~> int ~ ("," ~> int) <~ ">") ~
    (opt("(" ~> repsep(expr, ",") <~ ")") ^^ { _.getOrElse(Nil) }) ^^ {
      case n ~ as ~ (i ~ j) ~ es => AstExpr(n, as, i, j, es)
    }
  lazy val simpleBool: Parser[Boolean] = "T" ^^^ true | "F" ^^^ false

  // allocation expressions
  lazy val allocExpr: Parser[AllocExpr] = (
    ("(" ~ "new" ~> ident ~ opt(fields) <~ ")") ~ asite ^^ {
      case t ~ fields ~ a => EMap(t, fields.getOrElse(Nil), a)
    } | ("(" ~ "new" ~ "[" ~> repsep(expr, ",") <~ "]" ~ ")") ~ asite ^^ {
      case es ~ a => EList(es, a)
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
  lazy val literal: Parser[Literal] =
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
    "-" ^^^ Neg | "!" ^^^ Not | "~" ^^^ BNot

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
    "@[A-Z]+".r ^^ { case s => Global(s.substring(1)) } |
    "%(0|[1-9][0-9]*)".r ^^ { case s => Temp(s.substring(1).toInt) } |
    name

  // named local identifiers
  lazy val name: Parser[Name] = "[_a-zA-Z][_a-zA-Z0-9]*".r ^^ { Name(_) }

  // TODO types
  given ty: Parser[Type] = ident ^^ { Type(_) }

  // helper for id getter
  private def getId: Parser[Int] = int <~ ":"
  private def getId(name: String): Parser[Int] = getId <~ "<" ~ name ~ ">"
}
