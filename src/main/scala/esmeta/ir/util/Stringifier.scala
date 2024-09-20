package esmeta.ir.util

import esmeta.LINE_SEP
import esmeta.ir.*
import esmeta.lang.Syntax
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import scala.collection.mutable.ListBuffer

/** stringifier for IR */
class Stringifier(detail: Boolean, location: Boolean) {
  import TyStringifier.{*, given}

  // elements
  given elemRule: Rule[IRElem] = (app, elem) =>
    elem match {
      case elem: Program  => programRule(app, elem)
      case elem: Func     => funcRule(app, elem)
      case elem: FuncKind => funcKindRule(app, elem)
      case elem: Param    => paramRule(app, elem)
      case elem: Inst     => instRule(app, elem)
      case elem: Expr     => exprRule(app, elem)
      case elem: UOp      => uopRule(app, elem)
      case elem: BOp      => bopRule(app, elem)
      case elem: VOp      => vopRule(app, elem)
      case elem: MOp      => mopRule(app, elem)
      case elem: COp      => copRule(app, elem)
      case elem: Ref      => refRule(app, elem)
      case elem: Type     => tyRule(app, elem)
    }

  // programs
  given programRule: Rule[Program] = (app, program) =>
    given Rule[Iterable[Func]] = iterableRule(sep = LINE_SEP)
    app >> program.funcs

  // functions
  given funcRule: Rule[Func] = (app, func) =>
    funcHeadRule(false)(app, func)
    app >> " = " >> func.body

  def funcHeadRule(inline: Boolean): Rule[Func] = (app, func) =>
    val Func(main, kind, name, params, retTy, body, _) = func
    app >> (if (main) "@main " else "") >> "def " >> kind
    app >> name
    if (inline)
      given Rule[List[Param]] = iterableRule("(", ", ", ")")
      app >> params
    else app.wrap("(", ")")(for (param <- params) app :> param >> ",")
    app >> ": " >> retTy

  // function kinds
  given funcKindRule: Rule[FuncKind] = (app, kind) =>
    import FuncKind.*
    app >> (kind match {
      case AbsOp        => ""
      case NumMeth      => "<NUM>:"
      case SynDirOp     => "<SYNTAX>:"
      case ConcMeth     => "<CONC>:"
      case InternalMeth => "<INTERNAL>:"
      case Builtin      => "<BUILTIN>:"
      case Clo          => "<CLO>:"
      case Cont         => "<CONT>:"
      case Aux          => "<AUX>:"
    })

  // function parameters
  given paramRule: Rule[Param] = (app, param) =>
    val Param(name, ty, optional, _) = param
    app >> name >> (if (optional) "?" else "") >> ": " >> ty

  // instructions
  given instRule: Rule[Inst] = withLoc { (app, inst) =>
    inst match
      case IExpr(expr) =>
        app >> expr
      case ILet(lhs, expr) =>
        app >> "let " >> lhs >> " = " >> expr
      case IAssign(ref, expr) =>
        app >> ref >> " = " >> expr
      case IExpand(base, expr) =>
        app >> "expand " >> Field(base, expr)
      case IDelete(base, expr) =>
        app >> "delete " >> Field(base, expr)
      case IPush(from, to, front) =>
        app >> "push "
        if (front) app >> from >> " > " >> to
        else app >> to >> " < " >> from
      case IPop(lhs, list, front) =>
        app >> "pop "
        if (front) app >> lhs >> " < " >> list
        else app >> list >> " > " >> lhs
      case IReturn(expr) =>
        app >> "return " >> expr
      case IAssert(expr) =>
        app >> "assert " >> expr
      case IPrint(expr) =>
        app >> "print " >> expr
      case INop() =>
        app >> "nop"
      case ISeq(insts) =>
        if (insts.isEmpty) app >> "{}"
        else if (detail) app.wrap(for { i <- insts } app :> i)
        else app >> "{ ... }"
      case IIf(cond, thenInst, elseInst) =>
        app >> "if " >> cond >> " " >> thenInst
        (thenInst, elseInst) match
          case (_, ISeq(List())) => app
          case (ISeq(_), _)      => app >> " else " >> elseInst
          case _                 => app :> "else " >> elseInst
      case IWhile(cond, body) =>
        app >> "while " >> cond >> " " >> body
      case ICall(lhs, fexpr, args) =>
        given Rule[List[Expr]] = iterableRule("(", ", ", ")")
        app >> "call " >> lhs >> " = " >> fexpr >> args
      case ISdoCall(lhs, ast, method, args) =>
        given Rule[List[Expr]] = iterableRule("(", ", ", ")")
        app >> "sdo-call " >> lhs >> " = "
        app >> ast >> "->" >> method >> args
  }

  // expressions
  given exprRule: Rule[Expr] = withLoc { (app, expr) =>
    expr match
      case EParse(code, rule) =>
        app >> "(parse " >> code >> " " >> rule >> ")"
      case EGrammarSymbol(name, params) =>
        app >> "(grammar-symbol |" >> name >> "|"
        given Rule[Boolean] = (app, bool) => app >> (if (bool) "T" else "F")
        given Rule[List[Boolean]] = iterableRule("[", "", "]")
        if (params.nonEmpty) app >> params
        app >> ")"
      case ESourceText(expr) =>
        app >> "(source-text " >> expr >> ")"
      case EYet(msg) =>
        app >> "(yet \"" >> normStr(msg) >> "\")"
      case EContains(list, elem) =>
        app >> "(contains " >> list >> " " >> elem >> ")"
      case ESubstring(expr, from, to) =>
        app >> "(substring " >> expr >> " " >> from
        to.map(app >> " " >> _)
        app >> ")"
      case ETrim(expr, isStarting) =>
        if (isStarting) app >> "(trim > " >> expr >> ")"
        else app >> "(trim " >> expr >> " <)"
      case ERef(ref) =>
        app >> ref
      case EUnary(uop, expr) =>
        app >> "(" >> uop >> " " >> expr >> ")"
      case EBinary(bop, left, right) =>
        app >> "(" >> bop >> " " >> left >> " " >> right >> ")"
      case EVariadic(vop, exprs) =>
        given Rule[Iterable[Expr]] = iterableRule(sep = " ")
        app >> "(" >> vop >> " " >> exprs >> ")"
      case EMathOp(mop, exprs) =>
        given Rule[Iterable[Expr]] = iterableRule(sep = " ")
        app >> "(" >> mop >> " " >> exprs >> ")"
      case EConvert(cop, expr) =>
        app >> "(" >> cop >> " " >> expr >> ")"
      case EExists(ref) =>
        app >> "(exists " >> ref >> ")"
      case ETypeOf(base) =>
        app >> "(typeof " >> base >> ")"
      case EInstanceOf(expr, target) =>
        app >> "(instanceof " >> expr >> " " >> target >> ")"
      case ETypeCheck(expr, ty) =>
        app >> "(? " >> expr >> ": " >> ty >> ")"
      case ESizeOf(expr) =>
        app >> "(sizeof " >> expr >> ")"
      case EClo(fname, captured) =>
        given Rule[Iterable[Name]] = iterableRule("[", ", ", "]")
        app >> "clo<" >> "\"" >> fname >> "\""
        if (!captured.isEmpty) app >> ", " >> captured
        app >> ">"
      case ECont(fname) =>
        app >> "cont<" >> "\"" >> fname >> "\"" >> ">"
      case EDebug(expr) =>
        app >> "(debug " >> expr >> ")"
      case expr: ERandom =>
        randRule(app, expr)
      case expr: AstExpr =>
        astExprRule(app, expr)
      case expr: AllocExpr =>
        allocExprRule(app, expr)
      case expr: LiteralExpr =>
        literalExprRule(app, expr)
  }

  // random number expressions
  lazy val randRule: Rule[ERandom] = (app, rand) => app >> "(random)"

  // abstract syntax tree (AST) expressions
  lazy val astExprRule: Rule[AstExpr] = (app, ast) =>
    ast match {
      case ESyntactic(name, args, rhsIdx, children) =>
        app >> "|" >> name >> "|"
        given Rule[Boolean] = (app, bool) => app >> (if (bool) "T" else "F")
        given Rule[List[Boolean]] = iterableRule("[", "", "]")
        if (!args.isEmpty) app >> args
        app >> "<" >> rhsIdx >> ">"
        given eo: Rule[Option[Expr]] = optionRule("")
        given el: Rule[List[Option[Expr]]] = iterableRule("(", ", ", ")")
        if (!children.isEmpty) app >> children
        app
      case ELexical(name, expr) =>
        app >> "|" >> name >> "|(" >> expr >> ")"
    }

  // allocation expressions
  lazy val allocExprRule: Rule[AllocExpr] = (app, expr) =>
    expr match {
      case ERecord(tname, fields) =>
        app >> "(record"
        if (tname.nonEmpty) app >> " [" >> tname >> "]"
        if (fields.nonEmpty) (app >> " ").wrap("{", "}")(for {
          (field, expr) <- fields
        } app :> "\"" >> field >> "\" : " >> expr >> ",")
        app >> ")"
      case EMap((kty, vyt), Nil) => app >> "(map[" >> kty >> ", " >> vyt >> "])"
      case EMap((kty, vty), pairs) =>
        app >> "(map [" >> kty >> ", " >> vty >> "] "
        app.wrap("{", "}")(for {
          (key, value) <- pairs
        } app :> key >> " -> " >> value >> ",") >> ")"
      case EList(exprs) =>
        given Rule[Iterable[Expr]] = iterableRule("[", ", ", "]")
        app >> "(list " >> exprs >> ")"
      case ECopy(obj) =>
        app >> "(copy " >> obj >> ")"
      case EKeys(map, intSorted) =>
        app >> "(keys" >> (if (intSorted) "-int" else "") >> " "
        app >> map >> ")"
    }
    if (expr.asite == -1) app
    else app >> "[#" >> expr.asite >> "]"

  // literals
  lazy val literalExprRule: Rule[LiteralExpr] = (app, lit) =>
    lit match {
      case EMath(n)     => app >> n
      case EInfinity(p) => app >> (if (p) "+" else "-") >> "INF"
      case ENumber(Double.PositiveInfinity) => app >> "+NUM_INF"
      case ENumber(Double.NegativeInfinity) => app >> "-NUM_INF"
      case ENumber(n) if n.isNaN            => app >> "NaN"
      case ENumber(n)                       => app >> n >> "f"
      case EBigInt(n)                       => app >> n >> "n"
      case EStr(str)    => app >> "\"" >> normStr(str) >> "\""
      case EBool(b)     => app >> b
      case EUndef()     => app >> "undefined"
      case ENull()      => app >> "null"
      case EEnum(name)  => app >> "~" >> name >> "~"
      case ECodeUnit(c) => app >> c.toInt >> "cu"
    }

  // unary operators
  given uopRule: Rule[UOp] = (app, uop) =>
    import UOp.*
    app >> (uop match {
      case Abs   => "abs"
      case Floor => "floor"
      case Neg   => "-"
      case Not   => "!"
      case BNot  => "~"
    })

  // binary operators
  given bopRule: Rule[BOp] = (app, bop) =>
    import BOp.*
    app >> (bop match
      case Add     => "+"
      case Sub     => "-"
      case Mul     => "*"
      case Pow     => "**"
      case Div     => "/"
      case UMod    => "%%"
      case Mod     => "%"
      case Eq      => "="
      case Equal   => "=="
      case And     => "&&"
      case Or      => "||"
      case Xor     => "^^"
      case BAnd    => "&"
      case BOr     => "|"
      case BXOr    => "^"
      case LShift  => "<<"
      case Lt      => "<"
      case URShift => ">>>"
      case SRShift => ">>"
    )

  // variadic operators
  given vopRule: Rule[VOp] = (app, vop) =>
    import VOp.*
    app >> (vop match
      case Min    => "min"
      case Max    => "max"
      case Concat => "concat"
    )

  // mathematical operators
  given mopRule: Rule[MOp] = (app, mop) =>
    import MOp.*
    app >> (mop match
      case Expm1 => "[math:expm1]"
      case Log10 => "[math:log10]"
      case Log2  => "[math:log2]"
      case Cos   => "[math:cos]"
      case Cbrt  => "[math:cbrt]"
      case Exp   => "[math:exp]"
      case Cosh  => "[math:cosh]"
      case Sinh  => "[math:sinh]"
      case Tanh  => "[math:tanh]"
      case Acos  => "[math:acos]"
      case Acosh => "[math:acosh]"
      case Asinh => "[math:asinh]"
      case Atanh => "[math:atanh]"
      case Asin  => "[math:asin]"
      case Atan2 => "[math:atan2]"
      case Atan  => "[math:atan]"
      case Log1p => "[math:log1p]"
      case Log   => "[math:log]"
      case Sin   => "[math:sin]"
      case Sqrt  => "[math:sqrt]"
      case Tan   => "[math:tan]"
    )

  // conversion operators
  given copRule: Rule[COp] = (app, cop) =>
    import COp.*
    cop match {
      case ToApproxNumber => app >> "[approx-number]"
      case ToNumber       => app >> "[number]"
      case ToBigInt       => app >> "[bigInt]"
      case ToMath         => app >> "[math]"
      case ToStr(radix) =>
        app >> "[str"
        radix.map(app >> " " >> _)
        app >> "]"
    }

  // references
  lazy val inlineField = "([_a-zA-Z][_a-zA-Z0-9]*)".r
  given refRule: Rule[Ref] = withLoc { (app, ref) =>
    ref match {
      case Field(base, EStr(inlineField(str))) => app >> base >> "." >> str
      case Field(base, expr) => app >> base >> "[" >> expr >> "]"
      case x: Var            => varRule(app, x)
    }
  }

  // identifiers
  given varRule: Rule[Var] = (app, x) =>
    x match {
      case Global(name) => app >> "@" >> name
      case Name(name)   => app >> name
      case Temp(idx)    => app >> "%" >> idx
    }

  // types
  given tyRule: Rule[Type] = (app, ty) => app >> ty.ty

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // append locations
  private def withLoc[T <: IRElem with LangEdge](rule: Rule[T]): Rule[T] =
    (app, elem) =>
      given Rule[Option[Syntax]] = (app, langOpt) =>
        for {
          lang <- langOpt
          loc <- lang.loc
        } app >> " @ " >> loc.toString
        app
      rule(app, elem)
      if (location) app >> elem.langOpt else app
}
