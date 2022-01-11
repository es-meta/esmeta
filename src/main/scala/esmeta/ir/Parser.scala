package esmeta.ir

import esmeta.util.BaseUtils.*
import java.io._
import java.nio.charset.Charset
import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}
import esmeta.ir.*
import Inst.*, Expr.*, Ref.*, UOp.*, BOp.*, COp.*, Obj.*, RefValue.*, Value.*

/** parser for IR */
object Parser extends Parsers

trait Parser[T] extends Parsers {
  def fromFile(str: String)(implicit parser: Parser[T]): T =
    fromFileWithParser(str, parser)
  def apply(str: String)(using parser: Parser[T]): T =
    parse[T](str)
}

trait Parsers extends JavaTokenParsers with RegexParsers {
  // treat comments as white spaces
  override protected val whiteSpace = """(\s|//.*)+""".r

  // parse from file
  def fromFileWithParser[T](f: String, parser: Parser[T]): T = {
    var fileName = new File(f).getCanonicalPath
    val fs = new FileInputStream(new File(f))
    val sr = new InputStreamReader(fs, Charset.forName("UTF-8"))
    val in = new BufferedReader(sr)
    val result = errHandle(parseAll(parser, in))
    in.close; sr.close; fs.close
    result
  }
  // parse with error message
  def errHandle[T](result: ParseResult[T]): T = result match {
    case Success(result, _) => result
    case err                => error(s"[Parser] $err")
  }

  // parse
  def parse[T](str: String)(implicit parser: Parser[T]): T =
    errHandle(parseAll(parser, str))

  // string literal
  lazy val string = ("\"[\u0000-\u000F]\"".r | stringLiteral) ^^ { case s =>
    StringContext processEscapes s.substring(1, s.length - 1)
  }

  // boolean
  lazy val bool = "true" ^^^ true | "false" ^^^ false

  // integers
  val integer = "(0|-?[1-9]\\d*)".r
  // //////////////////////////////////////////////////////////////////////////////
  // Syntax
  // //////////////////////////////////////////////////////////////////////////////
  // programs
  given Parser[Program] = rep(inst) ^^ { Program(_) }

  // instructions
  given Parser[List[Inst]] = rep(inst)
  given inst: Parser[Inst] =
    opt(integer <~ ":") ~ opt("(" ~> integer <~ ")") ~ (
      "delete " ~> ref ^^ { IDelete(_) } |
        ("append " ~> expr <~ "->") ~ expr ^^ { case e ~ l => IAppend(e, l) } |
        ("prepend " ~> expr <~ "->") ~ expr ^^ { case e ~ l =>
          IPrepend(e, l)
        } |
        "return " ~> expr ^^ { case e => IReturn(e) } |
        "throw " ~> ident ^^ { case x => IThrow(x) } |
        ("if " ~> expr) ~ inst ~ ("else" ~> inst) ^^ { case c ~ t ~ e =>
          IIf(c, t, e)
        } |
        ("while " ~> expr) ~ inst ^^ { case c ~ b => IWhile(c, b) } |
        "{" ~> rep(inst) <~ "}" ^^ { case seq => ISeq(seq) } |
        "assert " ~> expr ^^ { case e => IAssert(e) } |
        "print " ~> expr ^^ { case e => IPrint(e) } |
        ("let " ~> id <~ "=") ~ expr ^^ { case x ~ e => ILet(x, e) } |
        ("app " ~> id <~ "=") ~ ("(" ~> expr) ~ (rep(expr) <~ ")") ^^ {
          case x ~ f ~ as => IApp(x, f, as)
        } |
        ("access " ~> id <~ "=") ~ ("(" ~> expr) ~ expr ~ (rep(
          expr,
        ) <~ ")") ^^ { case x ~ e1 ~ e2 ~ e3 =>
          IAccess(x, e1, e2, e3)
        } |
        ("clo " ~> id <~ "=") ~ ("(" ~> repsep(
          id,
          ",",
        ) <~ ")") ~ ("[" ~> repsep(
          id,
          ",",
        ) <~ "]") ~ ("=>" ~> inst) ^^ { case x ~ ps ~ cs ~ b =>
          IClo(x, ps, cs, b)
        } |
        ("cont " ~> id <~ "=") ~ ("(" ~> repsep(
          id,
          ",",
        ) <~ ")") ~ ("[=>]" ~> inst) ^^ { case x ~ ps ~ b => ICont(x, ps, b) } |
        ("withcont " ~> id) ~ ("(" ~> repsep(id, ",") <~ ")" <~ "=") ~ inst ^^ {
          case x ~ ps ~ b => IWithCont(x, ps, b)
        } |
        (ref <~ "=") ~ expr ^^ { case r ~ e => IAssign(r, e) } |
        expr ^^ { case e => IExpr(e) }
    )
      ^^ {
        // TODO case l ~ Some(k) ~ (i: AllocSite) =>
        // i.line = l.map(_.toInt); i.setASite(k.toInt)
        case l ~ _ ~ i =>
          // i.line = l.map(_.toInt);
          i
      }

  // expressions
  given expr: Parser[Expr] = opt("(" ~> integer <~ ")") ~ (
    ref ^^ { ERef(_) } |
      s"${integer}i".r ^^ { case s => EINum(s.dropRight(1).toLong) } |
      s"${integer}n".r ^^ { case s => EBigINum(BigInt(s.dropRight(1))) } |
      floatingPointNumber ^^ { case s => ENum(s.toDouble) } |
      "Infinity" ^^ { case s => ENum(Double.PositiveInfinity) } |
      "+Infinity" ^^ { case s => ENum(Double.PositiveInfinity) } |
      "-Infinity" ^^ { case s => ENum(Double.NegativeInfinity) } |
      "NaN" ^^ { case s => ENum(Double.NaN) } |
      string ^^ { EStr(_) } |
      bool ^^ { EBool(_) } |
      "undefined" ^^^ EUndef |
      "null" ^^^ ENull |
      "absent" ^^^ EAbsent |
      "~" ~> "[^~]+".r <~ "~" ^^ { EConst(_) } |
      "???" ~> string ^^ { ENotSupported(_) } |
      "(" ~> (uop ~ expr) <~ ")" ^^ { case u ~ e => EUOp(u, e) } |
      "(" ~> (bop ~ expr ~ expr) <~ ")" ^^ { case b ~ l ~ r => EBOp(b, l, r) } |
      "(" ~> ("typeof" ~> expr) <~ ")" ^^ { case e => ETypeOf(e) } |
      "(" ~> ("is-completion" ~> expr) <~ ")" ^^ { case e =>
        EIsCompletion(e)
      } |
      ("(" ~ "comp" ~ "[" ~> expr <~ "]") ~ expr ~ ("=>" ~> expr <~ ")") ^^ {
        case y ~ v ~ t => EComp(y, v, t)
      } |
      ("(" ~> "new" ~> ty) ~ ("(" ~> repsep(prop, ",") <~ ")" <~ ")") ^^ {
        case t ~ props => EMap(t, props)
      } |
      ("(" ~> "new" ~> ty <~ ")") ^^ { case t => EMap(t, Nil) } |
      ("(" ~> "new" ~> "[" ~> repsep(expr, ",") <~ "]" <~ ")") ^^ { EList(_) } |
      ("(" ~> "new" ~> "'" ~> expr <~ ")") ^^ { ESymbol(_) } |
      ("(" ~> "pop" ~> expr ~ expr <~ ")") ^^ { case l ~ x => EPop(l, x) } |
      ("(" ~> "is-instance-of" ~> expr) ~ (ident <~ ")") ^^ { case e ~ x =>
        EIsInstanceOf(e, x)
      } |
      ("(" ~> "get-elems" ~> expr) ~ (ident <~ ")") ^^ { case e ~ x =>
        EGetElems(e, x)
      } |
      "(" ~> "get-syntax" ~> expr <~ ")" ^^ { case e => EGetSyntax(e) } |
      "(" ~> "parse-syntax" ~> expr ~ expr ~ rep(bool) <~ ")" ^^ {
        case e ~ r ~ ps => EParseSyntax(e, r, ps)
      } |
      "(" ~> "convert" ~> expr ~ cop ~ rep(expr) <~ ")" ^^ { case e ~ r ~ l =>
        EConvert(e, r, l)
      } |
      "(" ~> "contains" ~> expr ~ expr <~ ")" ^^ { case l ~ e =>
        EContains(l, e)
      } |
      "[" ~> "?" ~> expr <~ "]" ^^ { case e => EReturnIfAbrupt(e, true) } |
      "[" ~> "!" ~> expr <~ "]" ^^ { case e => EReturnIfAbrupt(e, false) } |
      "(" ~> "copy-obj" ~> expr <~ ")" ^^ { case e => ECopy(e) } |
      "(" ~> "map-keys" ~> expr <~ ")" ^^ { case e => EKeys(e, false) } |
      "(" ~> "map-keys" ~> expr <~ "[int-sorted]" ~ ")" ^^ { case e =>
        EKeys(e, true)
      }
  ) ^^ {
    // TODOcase Some(k) ~ (e: AllocSite) => e.setASite(k.toInt)
    case _ ~ e => e
  }

  // properties
  lazy val prop: Parser[(Expr, Expr)] =
    (expr <~ "->") ~ expr ^^ { case k ~ v => (k, v) }

  // references
  given ref: Parser[Ref] = {
    id ~ rep(propExpr) ^^ { case x ~ es =>
      es.foldLeft[Ref](RefId(x)) { case (ref, expr) =>
        RefProp(ref, expr)
      }
    }
  }
  lazy val propExpr: Parser[Expr] =
    "." ~> ident ^^ { case x => EStr(x) } | "[" ~> expr <~ "]"

  // types
  given ty: Parser[Ty] = ident ^^ { Ty(_) }

  // identifiers
  given id: Parser[Id] =
    ident.withFilter(s => !(keywords contains s)) ^^ {
      Id(_)
    }
  val keywords: Set[String] = Set(
    "Infinity",
    "NaN",
    "true",
    "false",
    "undefined",
    "null",
    "absent",
    "typeof", /*"new",*/ "pop",
    "is-instance-of",
    "get-syntax",
    "contains",
  )

  // unary operators
  given uop: Parser[UOp] =
    "-" ^^^ ONeg | "!" ^^^ ONot | "~" ^^^ OBNot

  // binary operators
  given bop: Parser[BOp] =
    "+" ^^^ OPlus |
      "-" ^^^ OSub |
      "**" ^^^ OPow |
      "*" ^^^ OMul |
      "/" ^^^ ODiv |
      "%%" ^^^ OUMod |
      "%" ^^^ OMod |
      "==" ^^^ OEqual |
      "=" ^^^ OEq |
      "&&" ^^^ OAnd |
      "||" ^^^ OOr |
      "^^" ^^^ OXor |
      "&" ^^^ OBAnd |
      "|" ^^^ OBOr |
      "^" ^^^ OBXOr |
      "<<" ^^^ OLShift |
      "<" ^^^ OLt |
      ">>>" ^^^ OURShift |
      ">>" ^^^ OSRShift

  // convert operators
  given cop: Parser[COp] =
    "str2num" ^^^ CStrToNum |
      "str2bigint" ^^^ CStrToBigInt |
      "num2str" ^^^ CNumToStr |
      "num2int" ^^^ CNumToInt |
      "num2bigint" ^^^ CNumToBigInt |
      "bigint2num" ^^^ CBigIntToNum
}
