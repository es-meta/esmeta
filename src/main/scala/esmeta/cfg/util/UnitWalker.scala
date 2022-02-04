package esmeta.cfg.util

import esmeta.cfg.*
import esmeta.util.{BasicUnitWalker, Loc}

/** a unit walker for CFG */
trait UnitWalker extends BasicUnitWalker {
  def walk(elem: CFGElem): Unit = elem match {
    case elem: CFG         => walk(elem)
    case elem: Func        => walk(elem)
    case elem: Func.Kind   => walk(elem)
    case elem: Func.Param  => walk(elem)
    case elem: Node        => walk(elem)
    case elem: Branch.Kind => walk(elem)
    case elem: Inst        => walk(elem)
    case elem: Expr        => walk(elem)
    case elem: UOp         => walk(elem)
    case elem: BOp         => walk(elem)
    case elem: COp         => walk(elem)
    case elem: Ref         => walk(elem)
    case elem: Type        => walk(elem)
  }

  // control flow graphs (CFGs)
  def walk(cfg: CFG): Unit = walkIterable(cfg.funcs, walk)

  // functions
  def walk(func: Func): Unit =
    val Func(id, main, kind, name, params, _) = func
    walk(id)
    walk(main)
    walk(kind)
    walk(name)
    walkList(params, walk)
    walkSet(func.nodes, walk)

  // function kinds
  def walk(kind: Func.Kind): Unit = {}

  // function parameters
  def walk(param: Func.Param): Unit =
    val Func.Param(name, opt, ty) = param
    walk(name); walk(opt); walk(ty)

  // nodes
  def walk(node: Node): Unit = node match {
    case Block(id, insts, _) =>
      walk(id); walkIterable(insts, walk)
    case Call(id, lhs, fexpr, args, _) =>
      walk(id)
      walk(lhs)
      walk(fexpr)
      walkList(args, walk)
    case Branch(id, kind, cond, _, _) =>
      walk(id)
      walk(kind)
      walk(cond)
  }

  // branch kinds
  def walk(kind: Branch.Kind): Unit = {}

  // instructions
  def walk(inst: Inst): Unit = inst match {
    case IExpr(expr)            => walk(expr)
    case ILet(lhs, expr)        => walk(lhs); walk(expr)
    case IAssign(ref, expr)     => walk(ref); walk(expr)
    case IDelete(ref)           => walk(ref)
    case IPush(from, to, front) => walk(from); walk(to); walk(front)
    case IReturn(expr)          => walk(expr)
    case IAssert(expr)          => walk(expr)
    case IPrint(expr)           => walk(expr)
  }

  // expressions
  def walk(expr: Expr): Unit = expr match {
    case EComp(tyExpr, valExpr, tgtExpr) =>
      walk(tyExpr); walk(valExpr); walk(tgtExpr)
    case EIsCompletion(expr) =>
      walk(expr)
    case EReturnIfAbrupt(expr, check) =>
      walk(expr); walk(check)
    case EPop(list, front) =>
      walk(list); walk(front)
    case EParse(code, rule) =>
      walk(code); walk(rule)
    case EParseRule(name, params) =>
      walk(name); walkList(params, walk)
    case EYet(msg) =>
      walk(msg)
    case EContains(list, elem) =>
      walk(list); walk(elem)
    case ERef(ref) =>
      walk(ref)
    case EUnary(uop, expr) =>
      walk(uop); walk(expr)
    case EBinary(bop, left, right) =>
      walk(bop); walk(left); walk(right)
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
    case expr: AstExpr   => walk(expr)
    case expr: AllocExpr => walk(expr)
    case expr: Literal   => walk(expr)
  }

  // abstract syntax tree (AST) expressions
  def walk(ast: AstExpr): Unit = ast match {
    case ESyntactic(name, args, rhsIdx, bits, children) =>
      walk(name)
      walkList(args, walk)
      walk(rhsIdx)
      walk(bits)
      walkList(children, walk)
    case ELexical(name, expr) =>
      walk(name); walk(expr)
  }

  // allocation expressions
  def walk(alloc: AllocExpr): Unit = alloc match {
    case EMap(tname, fields, asite) =>
      walk(tname)
      walkList(fields, { case (p, e) => (walk(p), walk(e)) })
      walk(asite)
    case EList(exprs, asite) =>
      walkList(exprs, walk); walk(asite)
    case ESymbol(desc, asite) =>
      walk(desc); walk(asite)
    case ECopy(obj, asite) =>
      walk(obj); walk(asite)
    case EKeys(map, intSorted, asite) =>
      walk(map); walk(intSorted); walk(asite)
  }

  // literals
  def walk(lit: Literal): Unit = {}

  // unary operators
  def walk(uop: UOp): Unit = {}

  // binary operators
  def walk(bop: BOp): Unit = {}

  // variadic operators
  def walk(vop: VOp): Unit = {}

  // conversion operators
  def walk(cop: COp): Unit = cop match {
    case COp.ToStr(radix) => walkOpt(radix, walk)
    case op               =>
  }

  // references
  def walk(ref: Ref): Unit = ref match {
    case Prop(ref, expr) => walk(ref); walk(expr)
    case x: Id           => walk(x)
  }

  // identifiers
  def walk(x: Id): Unit = x match {
    case Global(x) => walk(x)
    case x: Name   => walk(x)
    case Temp(k)   => walk(k)
  }

  // named local identifiers
  def walk(x: Name): Unit = walk(x.name)

  // TODO types
  def walk(ty: Type): Unit = walk(ty.name)
}
