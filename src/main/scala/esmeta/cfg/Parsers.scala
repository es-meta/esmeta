package esmeta.cfg

import esmeta.util.BasicParsers

/** CFG parsers */
trait Parsers extends BasicParsers {
  // control flow graphs (CFGs)
  given cfg: Parser[CFG] = x => ???

  // functions
  given func: Parser[Func] = x => ???

  // function parameters
  given param: Parser[Param] = x => ???

  // nodes
  given node: Parser[Node] = x => ???

  // instructions
  given inst: Parser[Inst] = x => ???

  // expressions
  given expr: Parser[Expr] =
    "comp" ~> ("[" ~> expr ~ ("/" ~> expr) <~ "]") ~ ("(" ~> expr <~ ")") ^^ {
      case ty ~ tgt ~ e => EComp(ty, tgt, e)
    } | "(" ~ "comp?" ~> expr <~ ")" ^^ { case e =>
      EIsCompletion(e)
    } | "[" ~> ("?" ^^^ true | "!" ^^^ false) ~ expr <~ "]" ^^ { case c ~ e =>
      EReturnIfAbrupt(e, c)
    } | "(" ~ "pop" ~> ("<" ^^^ true | ">" ^^^ false) ~ expr <~ ")" ^^ {
      case f ~ e => EPop(e, f)
    } | "(" ~ "yet" ~> string <~ ")" ^^ { case msg =>
      EYet(msg)
    } | "(" ~ "contains" ~> expr ~ expr <~ ")" ^^ { case l ~ e =>
      EContains(l, e)
    } | "(" ~> uop ~ expr <~ ")" ^^ { case u ~ e =>
      EUnary(u, e)
    } | "(" ~> bop ~ expr ~ expr <~ ")" ^^ { case b ~ l ~ r =>
      EBinary(b, l, r)
    } | "(" ~> cop ~ expr <~ ")" ^^ { case c ~ e =>
      EConvert(c, e)
    } | "(" ~ "typeof" ~> expr <~ ")" ^^ { case e =>
      ETypeOf(e)
    } | "(" ~ "?" ~> expr ~ (":" ~> ty) <~ ")" ^^ { case e ~ t =>
      ETypeCheck(e, t)
    } | allocExpr | literal | ref ^^ { ERef(_) }

  // allocation expressions
  lazy val allocExpr: Parser[AllocExpr] = (
    ("(" ~ "new" ~> ident ~ opt(props) <~ ")") ~ asite ^^ {
      case t ~ props ~ a => EMap(t, props.getOrElse(Nil), a)
    } | ("(" ~ "new" ~ "[" ~> repsep(expr, ",") <~ "]" ~ ")") ~ asite ^^ {
      case es ~ a => EList(es, a)
    } | ("(" ~ "new" ~> "'" ~> expr <~ ")") ~ asite ^^ { case e ~ a =>
      ESymbol(e, a)
    } | ("(" ~ "copy" ~> expr <~ ")") ~ asite ^^ { case e ~ a =>
      ECopy(e, a)
    } | ("(" ~ "keys" ~> opt("-int") ~ expr <~ ")") ~ asite ^^ {
      case i ~ e ~ a => EKeys(e, i.isDefined, a)
    }
  )

  // properties
  lazy val props: Parser[List[(Expr, Expr)]] = "(" ~> repsep(prop, ",") <~ ")"
  lazy val prop: Parser[(Expr, Expr)] =
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
      "~" ~> "[^~]+".r <~ "~" ^^ { EConst(_) } |
      ("clo[" ~> int <~ "]") ~ opt("(" ~> repsep(local, ",") <~ ")") ^^ {
        case fid ~ as => EClo(fid, as.getOrElse(Nil))
      }

  // "???" ~> string ^^ { ENotSupported(_) } |
  // "(" ~> (uop ~ expr) <~ ")" ^^ { case u ~ e => EUOp(u, e) } |
  // "(" ~> (bop ~ expr ~ expr) <~ ")" ^^ { case b ~ l ~ r => EBOp(b, l, r) } |
  // "(" ~> ("typeof" ~> expr) <~ ")" ^^ { case e => ETypeOf(e) } |
  // "(" ~> ("is-completion" ~> expr) <~ ")" ^^ { case e =>
  //   EIsCompletion(e)
  // } |
  // ("(" ~ "comp" ~ "[" ~> expr <~ "]") ~ expr ~ ("=>" ~> expr <~ ")") ^^ {
  //   case y ~ v ~ t => EComp(y, v, t)
  // } |
  // ("(" ~> "new" ~> ty) ~ ("(" ~> repsep(prop, ",") <~ ")" <~ ")") ^^ {
  //   case t ~ props => EMap(t, props)
  // } |
  // ("(" ~> "new" ~> ty <~ ")") ^^ { case t => EMap(t, Nil) } |
  // ("(" ~> "new" ~> "[" ~> repsep(expr, ",") <~ "]" <~ ")") ^^ { EList(_) } |
  // ("(" ~> "new" ~> "'" ~> expr <~ ")") ^^ { ESymbol(_) } |
  // ("(" ~> "pop" ~> expr ~ expr <~ ")") ^^ { case l ~ x => EPop(l, x) } |
  // ("(" ~> "is-instance-of" ~> expr) ~ (ident <~ ")") ^^ { case e ~ x =>
  //   EIsInstanceOf(e, x)
  // } |
  // ("(" ~> "get-elems" ~> expr) ~ (ident <~ ")") ^^ { case e ~ x =>
  //   EGetElems(e, x)
  // } |
  // "(" ~> "get-syntax" ~> expr <~ ")" ^^ { case e => EGetSyntax(e) } |
  // "(" ~> "parse-syntax" ~> expr ~ expr ~ rep(bool) <~ ")" ^^ {
  //   case e ~ r ~ ps => EParseSyntax(e, r, ps)
  // } |
  // "(" ~> "convert" ~> expr ~ cop ~ opt(expr) <~ ")" ^^ { case e ~ r ~ l =>
  //   EConvert(e, r, l)
  // } |
  // "(" ~> "contains" ~> expr ~ expr <~ ")" ^^ { case l ~ e =>
  //   EContains(l, e)
  // } |
  // "[" ~> "?" ~> expr <~ "]" ^^ { case e => EReturnIfAbrupt(e, true) } |
  // "[" ~> "!" ~> expr <~ "]" ^^ { case e => EReturnIfAbrupt(e, false) } |
  // "(" ~> "copy-obj" ~> expr <~ ")" ^^ { case e => ECopy(e) } |
  // "(" ~> "map-keys" ~> expr <~ ")" ^^ { case e => EKeys(e, false) } |
  // "(" ~> "map-keys" ~> expr <~ "[int-sorted]" ~ ")" ^^ { case e =>
  //   EKeys(e, true)
  // }

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
      local

  // local identifiers
  lazy val local: Parser[Local] = "[_a-zA-Z][_a-zA-Z0-9]*".r ^^ { Local(_) }

  // TODO types
  given ty: Parser[Type] = ident ^^ { Type(_) }
}
