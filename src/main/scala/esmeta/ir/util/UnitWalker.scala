package esmeta.ir.util

import esmeta.ir.*
import esmeta.util.BasicUnitWalker

/** a unit walker for IR */
trait UnitWalker extends BasicUnitWalker {
  def walk(elem: IRElem): Unit = elem match {
    case elem: Program  => walk(elem)
    case elem: Func     => walk(elem)
    case elem: FuncKind => walk(elem)
    case elem: Param    => walk(elem)
    case elem: Inst     => walk(elem)
    case elem: Expr     => walk(elem)
    case elem: UOp      => walk(elem)
    case elem: BOp      => walk(elem)
    case elem: VOp      => walk(elem)
    case elem: MOp      => walk(elem)
    case elem: COp      => walk(elem)
    case elem: Ref      => walk(elem)
    case elem: Type     => walk(elem)
  }

  // programs
  def walk(program: Program): Unit = walkList(program.funcs, walk)

  // functions
  def walk(func: Func): Unit =
    val Func(main, kind, name, ps, rty, body, _) = func
    walk(main); walk(kind); walk(name);
    walkList(ps, walk); walk(rty); walk(func.body)

  // function kinds
  def walk(kind: FuncKind): Unit = {}

  // function parameters
  def walk(param: Param): Unit =
    val Param(name, opt, ty, _) = param
    walk(name); walk(opt); walk(ty)

  // instructions
  def walk(inst: Inst): Unit = inst match {
    case IExpr(expr)            => walk(expr)
    case ILet(lhs, expr)        => walk(lhs); walk(expr)
    case IAssign(ref, expr)     => walk(ref); walk(expr)
    case IDelete(ref)           => walk(ref)
    case IPush(from, to, front) => walk(from); walk(to); walk(front)
    case IPop(lhs, expr, front) => walk(lhs); walk(expr); walk(front)
    case IReturn(expr)          => walk(expr)
    case IAssert(expr)          => walk(expr)
    case IPrint(expr)           => walk(expr)
    case INop()                 =>
    case ISeq(insts)            => walkList(insts, walk)
    case IIf(c, t, e)           => walk(c); walk(t); walk(e)
    case IWhile(c, b)           => walk(c); walk(b)
    case ICall(l, f, as)        => walk(l); walk(f); walkList(as, walk)
    case ISdoCall(l, b, n, as)  => walk(l); walk(b); walk(n); walkList(as, walk)
  }

  // expressions
  def walk(expr: Expr): Unit = expr match {
    case EComp(tyExpr, valExpr, tgtExpr) =>
      walk(tyExpr); walk(valExpr); walk(tgtExpr)
    case EIsCompletion(expr) =>
      walk(expr)
    case EReturnIfAbrupt(expr, check) =>
      walk(expr); walk(check)
    case EParse(code, rule) =>
      walk(code); walk(rule)
    case ENt(name, params) =>
      walk(name); walkList(params, walk)
    case ESourceText(expr) =>
      walk(expr)
    case EYet(msg) =>
      walk(msg)
    case EContains(list, elem) =>
      walk(list); walk(elem)
    case ESubstring(expr, from, to) =>
      walk(expr); walk(from); walkOpt(to, walk)
    case ETrim(expr, isStarting) =>
      walk(expr); walk(isStarting)
    case ERef(ref) =>
      walk(ref)
    case EUnary(uop, expr) =>
      walk(uop); walk(expr)
    case EBinary(bop, left, right) =>
      walk(bop); walk(left); walk(right)
    case EMathOp(mop, exprs) =>
      walk(mop); walkList(exprs, walk)
    case EVariadic(vop, exprs) =>
      walk(vop); walkList(exprs, walk)
    case EConvert(cop, expr) =>
      walk(cop); walk(expr)
    case ETypeOf(base) =>
      walk(base)
    case ETypeCheck(expr, ty) =>
      walk(expr); walk(ty)
    case EClo(fname, captured) =>
      walk(fname); walkList(captured, walk)
    case ECont(fname) =>
      walk(fname)
    case EDebug(expr) =>
      walk(expr)
    case expr: ERandom     => walk(expr)
    case expr: AstExpr     => walk(expr)
    case expr: AllocExpr   => walk(expr)
    case expr: LiteralExpr => walk(expr)
  }

  // random number expressions
  def walk(rand: ERandom): Unit = {}

  // abstract syntax tree (AST) expressions
  def walk(ast: AstExpr): Unit = ast match {
    case ESyntactic(name, args, rhsIdx, children) =>
      walk(name)
      walkList(args, walk)
      walk(rhsIdx)
      walkList(children, walkOpt(_, walk))
    case ELexical(name, expr) =>
      walk(name); walk(expr)
  }

  // allocation expressions
  def walk(alloc: AllocExpr): Unit = alloc match {
    case ERecord(tnameOpt, fields) =>
      walkOpt(tnameOpt, walk)
      walkList(fields, { case (f, e) => (walk(f), walk(e)) })
    case EMap(pairs) =>
      walkList(pairs, { case (k, v) => (walk(k), walk(v)) })
    case EList(exprs) =>
      walkList(exprs, walk)
    case EListConcat(exprs) =>
      walkList(exprs, walk)
    case ECopy(obj) =>
      walk(obj)
    case EKeys(map, intSorted) =>
      walk(map); walk(intSorted)
    case EGetChildren(ast) =>
      walk(ast)
    case EGetItems(nt, ast) =>
      walk(nt); walk(ast)
  }

  // literals
  def walk(lit: LiteralExpr): Unit = {}

  // unary operators
  def walk(uop: UOp): Unit = {}

  // binary operators
  def walk(bop: BOp): Unit = {}

  // variadic operators
  def walk(vop: VOp): Unit = {}

  // mathematical operators
  def walk(mop: MOp): Unit = {}

  // conversion operators
  def walk(cop: COp): Unit = cop match {
    case COp.ToStr(radix) => walkOpt(radix, walk)
    case op               =>
  }

  // references
  def walk(ref: Ref): Unit = ref match {
    case Field(ref, expr) => walk(ref); walk(expr)
    case x: Var           => walk(x)
  }

  // identifiers
  def walk(x: Var): Unit = x match {
    case Global(x) => walk(x)
    case x: Name   => walk(x)
    case Temp(k)   => walk(k)
  }

  // named local identifiers
  def walk(x: Name): Unit = walk(x.name)

  // TODO types
  def walk(ty: Type): Unit = {}
}
