package esmeta.cfg

import esmeta.util.{BasicUnitWalker, Loc}

/** a unit walker for CFG */
trait UnitWalker extends BasicUnitWalker {
  def walk(elem: CFGElem): Unit = elem match {
    case elem: CFG         => walk(elem)
    case elem: Func        => walk(elem)
    case elem: Func.Kind   => walk(elem)
    case elem: Param       => walk(elem)
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
  def walk(cfg: CFG): Unit =
    val CFG(main, funcs) = cfg
    walk(main); walkList(funcs, walk)

  // functions
  def walk(func: Func): Unit =
    val Func(id, kind, name, params, entry, blocks, exit) = func
    walk(id)
    walk(kind)
    walk(name)
    walkList(params, walk)
    walk(entry)
    walkList(blocks, walk)
    walk(exit)

  // function kinds
  def walk(kind: Func.Kind): Unit = {}

  // function parameters
  def walk(param: Param): Unit =
    val Param(name, ty) = param
    walk(name); walk(ty)

  // nodes
  def walk(node: Node): Unit = node match {
    case node: Entry => walk(node)
    case node: Exit  => walk(node)
    case node: Block => walk(node)
  }

  // entry nodes
  def walk(entry: Entry): Unit =
    val Entry(id, next) = entry
    walk(id); walk(next)

  // exit nodes
  def walk(exit: Exit): Unit =
    val Exit(id) = exit
    walk(id)

  // block nodes
  def walk(block: Block): Unit = block match {
    case Linear(id, insts, next) =>
      walk(id); walkVector(insts, walk); walk(next)
    case Branch(id, kind, cond, loc, thenNode, elseNode) =>
      walk(id)
      walk(kind)
      walk(cond)
      walkOpt(loc, walk)
      walk(thenNode)
      walk(elseNode)
    case Call(id, lhs, fexpr, args, loc, next) =>
      walk(id)
      walk(lhs)
      walk(fexpr)
      walkList(args, walk)
      walkOpt(loc, walk)
      walk(next)
  }

  // branch kinds
  def walk(kind: Branch.Kind): Unit = {}

  // instructions
  def walk(inst: Inst): Unit = inst match {
    case ILet(lhs, expr, loc) =>
      walk(lhs); walk(expr); walkOpt(loc, walk)
    case IAssign(ref, expr, loc) =>
      walk(ref); walk(expr); walkOpt(loc, walk)
    case IDelete(ref, loc) =>
      walk(ref); walkOpt(loc, walk)
    case IPush(from, to, front, loc) =>
      walk(from); walk(to); walk(front); walkOpt(loc, walk)
    case IReturn(expr, loc) =>
      walk(expr); walkOpt(loc, walk)
    case IAssert(expr, loc) =>
      walk(expr); walkOpt(loc, walk)
    case IPrint(expr, loc) =>
      walk(expr); walkOpt(loc, walk)
  }

  // locations
  def walk(loc: Loc): Unit = {}

  // expressions
  def walk(expr: Expr): Unit = expr match {
    case EComp(tyExpr, tgtExpr, valExpr) =>
      walk(tyExpr); walk(tgtExpr); walk(valExpr)
    case EIsCompletion(expr) =>
      walk(expr)
    case EReturnIfAbrupt(expr, check) =>
      walk(expr); walk(check)
    case EPop(list, front) =>
      walk(list); walk(front)
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
    case EConvert(cop, expr) =>
      walk(cop); walk(expr)
    case ETypeOf(base) =>
      walk(base)
    case ETypeCheck(expr, ty) =>
      walk(expr); walk(ty)
    case EClo(fid, captured) =>
      walk(fid); walkList(captured, walk)
    case ECont(fid) =>
      walk(fid)
    case expr: AstExpr   => walk(expr)
    case expr: AllocExpr => walk(expr)
    case expr: Literal   => walk(expr)
  }

  // abstract syntax tree (AST) expressions
  def walk(ast: AstExpr): Unit = {
    val AstExpr(name, args, rhsIdx, bits, children) = ast
    walk(name)
    walkList(args, walk)
    walk(rhsIdx)
    walk(bits)
    walkList(children, walk)
  }

  // allocation expressions
  def walk(alloc: AllocExpr): Unit = alloc match {
    case EMap(tname, props, asite) =>
      walk(tname)
      walkList(props, { case (p, e) => (walk(p), walk(e)) })
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
    case x: Local  => walk(x)
    case Temp(k)   => walk(k)
  }

  // local identifiers
  def walk(x: Local): Unit = walk(x.name)

  // TODO types
  def walk(ty: Type): Unit = walk(ty.name)
}
