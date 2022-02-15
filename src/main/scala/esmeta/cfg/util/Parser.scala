package esmeta.cfg.util

import esmeta.cfg.*
import esmeta.util.{Locational, BasicParsers}
import esmeta.util.BaseUtils.*
import scala.collection.mutable.ListBuffer

/** CFG parser */
object Parser extends Parsers

/** CFG parsers */
trait Parsers extends BasicParsers {
  // treat comments as white spaces
  override protected val whiteSpace = whiteSpaceWithComment

  // control flow graphs (CFGs)
  given cfg: Parser[CFG] = rep(funcGen) ~ rep(nodeLink) ^^ {
    case gens ~ links =>
      val builder = new Builder
      val funcs = ListBuffer.from(gens.map(_(builder)))
      if (!links.isEmpty)
        val fb = builder.FuncBuilder(true, Func.Kind.AbsOp, MAIN_FUNC, Nil)
        funcs += getFunc(fb, links)
      val main = funcs.filter(_.main) match {
        case ListBuffer(main) => main
        case ListBuffer()     => error("no main function")
        case _                => error("multiple main functions")
      }
      CFG(main, ListBuffer.from(funcs))
  }

  // functions
  given func: Parser[Func] = funcGen ^^ { case gen => gen(new Builder) }

  // function generators
  lazy val funcGen: Parser[Builder => Func] =
    (opt(getId) ~> main <~ "def") ~ funcKind ~ "(\\w|:)+".r ~ params ~
    ("{" ~> rep(nodeLink) <~ "}") ^^ {
      case main ~ kind ~ name ~ params ~ links =>
        builder => getFunc(builder.FuncBuilder(main, kind, name, params), links)
    }

  def getFunc(fb: Builder#FuncBuilder, links: List[NodeLink]): Func = {
    links.foreach {
      case BlockLink(node, label, next) =>
        fb.addInsts(node.insts, next, Some(label))
      case CallLink(node, label, next) =>
        fb.addCall(
          node.lhs,
          node.fexpr,
          node.args,
          next,
          Some(label),
        )
      case BranchLink(node, label, thenLabel, elseLabel) =>
        fb.addBranchWithLabel(
          node.kind,
          node.cond,
          thenLabel,
          elseLabel,
          Some(label),
        )
    }
    fb.func
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

  lazy val nodeLink: Parser[NodeLink] =
    guard(getLabel) ~ call ~ opt("->" ~> word) ^^ {
      case label ~ call ~ next => CallLink(call, label, next)
    } | guard(getLabel) ~ branch ~ opt("then" ~> word) ~ opt(
      "else" ~> word,
    ) ^^ {
      case label ~ branch ~ thenLabel ~ elseLabel =>
        BranchLink(branch, label, thenLabel, elseLabel)
    } | guard(getLabel) ~ block ~ opt("->" ~> word) ^^ {
      case label ~ block ~ next => BlockLink(block, label, next)
    }
  given node: Parser[Node] = call | branch | block
  lazy val block: Parser[Block] = withLoc {
    getLabel ~> insts ^^ {
      case insts => Block(0, ListBuffer.from(insts), None)
    }
  }
  lazy val call: Parser[Call] = withLoc {
    getLabel ~> ("call" ~> id <~ "=") ~
    expr ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
      case lhs ~ fexpr ~ args => Call(0, lhs, fexpr, args, None)
    }
  }
  lazy val branch: Parser[Branch] = withLoc {
    getLabel ~> branchKind ~ expr ^^ {
      case kind ~ cond => Branch(0, kind, cond, None, None)
    }
  }

  // branch kinds
  given branchKind: Parser[Branch.Kind] =
    import Branch.Kind.*
    "if" ^^^ If |
    "loop[" ~> "[^\\]]+".r <~ "]" ^^ { Loop(_) }

  // instructions
  lazy val insts: Parser[ListBuffer[Inst]] =
    "{" ~> rep(inst) <~ "}" ^^ { ListBuffer.from(_) }
    | inst ^^ { ListBuffer(_) }
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
    ("<" ~> int ~ ("," ~> int) <~ ">") ~
    (opt("(" ~> repsep(expr, ",") <~ ")") ^^ { _.getOrElse(Nil) }) ^^ {
      case n ~ as ~ (i ~ j) ~ es => ESyntactic(n, as, i, j, es)
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
    "@[A-Z]+".r ^^ { case s => Global(s.substring(1)) } |
    "%(0|[1-9][0-9]*)".r ^^ { case s => Temp(s.substring(1).toInt) } |
    name

  // named local identifiers
  lazy val name: Parser[Name] = "[_a-zA-Z][_a-zA-Z0-9]*".r ^^ { Name(_) }

  // TODO types
  given ty: Parser[Type] = ident ^^ { Type(_) }

  // helper for id getter
  private def getId: Parser[Int] = int <~ ":"
  private def getLabel: Parser[String] = word <~ ":"

  // helper for locations
  private def withLoc[T <: Locational](parser: Parser[T]): Parser[T] =
    parser ~ opt("@" ~> loc) ^^ { case t ~ loc => t.loc = loc; t }
}

// node links
sealed trait NodeLink { val node: Node; val label: String }
case class BlockLink(node: Block, label: String, nextLabel: Option[String])
  extends NodeLink
case class CallLink(node: Call, label: String, nextLabel: Option[String])
  extends NodeLink
case class BranchLink(
  node: Branch,
  label: String,
  thenLabel: Option[String],
  elseLabel: Option[String],
) extends NodeLink
