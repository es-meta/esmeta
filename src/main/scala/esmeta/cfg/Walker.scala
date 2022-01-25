package esmeta.cfg

import esmeta.util.{BasicWalker, Loc}

/** a walker for CFG */
trait Walker extends BasicWalker {
  def walk(elem: CFGElem): CFGElem = elem match {
    case elem: CFG         => walk(elem)
    case elem: Func        => walk(elem)
    case elem: Func.Kind   => walk(elem)
    case elem: Param       => walk(elem)
    case elem: Param.Kind  => walk(elem)
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
  def walk(cfg: CFG): CFG =
    val CFG(main, funcs) = cfg
    CFG(walk(main), walkList(funcs, walk))

  // functions
  def walk(func: Func): Func =
    val Func(id, kind, name, params, entry, blocks, exit) = func
    Func(
      walk(id),
      walk(kind),
      walk(name),
      walkList(params, walk),
      walk(entry),
      walkList(blocks, walk),
      walk(exit),
    )

  // function kinds
  def walk(kind: Func.Kind): Func.Kind = kind

  // function parameters
  def walk(param: Param): Param =
    val Param(name, kind, ty) = param
    Param(walk(name), walk(kind), walk(ty))

  // function parameter kinds
  def walk(kind: Param.Kind): Param.Kind = kind

  // nodes
  def walk(node: Node): Node = node match {
    case node: Entry => walk(node)
    case node: Exit  => walk(node)
    case node: Block => walk(node)
  }

  // entry nodes
  def walk(entry: Entry): Entry =
    val Entry(id, next) = entry
    Entry(walk(id), walk(next))

  // exit nodes
  def walk(exit: Exit): Exit =
    val Exit(id) = exit
    Exit(walk(id))

  // block nodes
  def walk(block: Block): Block = block match {
    case Linear(id, insts, next) =>
      Linear(walk(id), walkVector(insts, walk), walk(next))
    case Branch(id, kind, cond, loc, thenNode, elseNode) =>
      Branch(
        walk(id),
        walk(kind),
        walk(cond),
        walkOpt(loc, walk),
        walk(thenNode),
        walk(elseNode),
      )
    case Call(id, lhs, fexpr, args, loc, next) =>
      Call(
        walk(id),
        walk(lhs),
        walk(fexpr),
        walkList(args, walk),
        walkOpt(loc, walk),
        walk(next),
      )
  }

  // branch kinds
  def walk(kind: Branch.Kind): Branch.Kind = kind

  // instructions
  def walk(inst: Inst): Inst = inst match {
    case IExpr(expr, loc) =>
      IExpr(walk(expr), walkOpt(loc, walk))
    case ILet(lhs, expr, loc) =>
      ILet(walk(lhs), walk(expr), walkOpt(loc, walk))
    case IAssign(ref, expr, loc) =>
      IAssign(walk(ref), walk(expr), walkOpt(loc, walk))
    case IDelete(ref, loc) =>
      IDelete(walk(ref), walkOpt(loc, walk))
    case IPush(from, to, front, loc) =>
      IPush(walk(from), walk(to), walk(front), walkOpt(loc, walk))
    case IReturn(expr, loc) =>
      IReturn(walk(expr), walkOpt(loc, walk))
    case IAssert(expr, loc) =>
      IAssert(walk(expr), walkOpt(loc, walk))
    case IPrint(expr, loc) =>
      IPrint(walk(expr), walkOpt(loc, walk))
  }

  // locations
  def walk(loc: Loc): Loc = loc

  // expressions
  def walk(expr: Expr): Expr = expr match {
    case EComp(tyExpr, tgtExpr, valExpr) =>
      EComp(walk(tyExpr), walk(tgtExpr), walk(valExpr))
    case EIsCompletion(expr) =>
      EIsCompletion(walk(expr))
    case EReturnIfAbrupt(expr, check) =>
      EReturnIfAbrupt(walk(expr), walk(check))
    case EPop(list, front) =>
      EPop(walk(list), walk(front))
    case EYet(msg) =>
      EYet(walk(msg))
    case EContains(list, elem) =>
      EContains(walk(list), walk(elem))
    case ERef(ref) =>
      ERef(walk(ref))
    case EUnary(uop, expr) =>
      EUnary(walk(uop), walk(expr))
    case EBinary(bop, left, right) =>
      EBinary(walk(bop), walk(left), walk(right))
    case EConvert(cop, expr) =>
      EConvert(walk(cop), walk(expr))
    case ETypeOf(base) =>
      ETypeOf(walk(base))
    case ETypeCheck(expr, ty) =>
      ETypeCheck(walk(expr), walk(ty))
    case EClo(fid, captured) =>
      EClo(walk(fid), walkList(captured, walk))
    case ECont(fid) =>
      ECont(walk(fid))
    case expr: AstExpr   => walk(expr)
    case expr: AllocExpr => walk(expr)
    case lit: Literal    => walk(lit)
  }

  // abstract syntax tree (AST) expressions
  def walk(ast: AstExpr): AstExpr = ast match {
    case ESyntactic(name, args, rhsIdx, bits, children) =>
      ESyntactic(
        walk(name),
        walkList(args, walk),
        walk(rhsIdx),
        walk(bits),
        walkList(children, walk),
      )
    case ELexical(name, expr) =>
      ELexical(walk(name), walk(expr))
  }

  // allocation expressions
  def walk(alloc: AllocExpr): AllocExpr = alloc match {
    case EMap(tname, fields, asite) =>
      lazy val newFields =
        walkList(fields, { case (p, e) => (walk(p), walk(e)) })
      EMap(walk(tname), newFields, walk(asite))
    case EList(exprs, asite) =>
      EList(walkList(exprs, walk), walk(asite))
    case ESymbol(desc, asite) =>
      ESymbol(walk(desc), walk(asite))
    case ECopy(obj, asite) =>
      ECopy(walk(obj), walk(asite))
    case EKeys(map, intSorted, asite) =>
      EKeys(walk(map), walk(intSorted), walk(asite))
  }

  // literals
  def walk(lit: Literal): Literal = lit

  // unary operators
  def walk(uop: UOp): UOp = uop

  // binary operators
  def walk(bop: BOp): BOp = bop

  // conversion operators
  def walk(cop: COp): COp = cop match {
    case COp.ToStr(radix) => COp.ToStr(walkOpt(radix, walk))
    case op               => op
  }

  // references
  def walk(ref: Ref): Ref = ref match {
    case Prop(ref, expr) => Prop(walk(ref), walk(expr))
    case x: Id           => walk(x)
  }

  // identifiers
  def walk(x: Id): Id = x match {
    case Global(x) => Global(walk(x))
    case x: Name   => walk(x)
    case Temp(k)   => Temp(walk(k))
  }

  // named local identifiers
  def walk(x: Name): Name = Name(walk(x.name))

  // TODO types
  def walk(ty: Type): Type = Type(walk(ty.name))
}
