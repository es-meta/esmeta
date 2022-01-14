package esmeta.ir

import esmeta.util.BasicUnitWalker

/** a unit walker for IR */
trait UnitWalker extends BasicUnitWalker {
  def walk(node: IRElem): Unit = node match
    case prog: Program  => walk(prog)
    case inst: Inst     => walk(inst)
    case expr: Expr     => walk(expr)
    case ref: Ref       => walk(ref)
    case ty: Ty         => walk(ty)
    case id: Id         => walk(id)
    case uop: UOp       => walk(uop)
    case bop: BOp       => walk(bop)
    case cop: COp       => walk(cop)
    case st: State      => walk(st)
    case heap: Heap     => walk(heap)
    case obj: Obj       => walk(obj)
    case v: Value       => walk(v)
    case refV: RefValue => walk(refV)
    case ctxt: Context  => walk(ctxt)
    case cursor: Cursor => walk(cursor)

  // -----------------------------------------------------------------------------
  // Syntax
  // -----------------------------------------------------------------------------
  /** programs */
  def walk(program: Program): Unit = walkList[Inst](program.insts, walk)

  /** instructions */
  def walk(inst: Inst): Unit = inst match
    case IExpr(expr)          => walk(expr)
    case ILet(id, expr)       => walk(id); walk(expr)
    case IAssign(ref, expr)   => walk(ref); walk(expr)
    case IDelete(ref)         => walk(ref)
    case IAppend(expr, list)  => walk(expr); walk(list)
    case IPrepend(expr, list) => walk(expr); walk(list)
    case IReturn(expr)        => walk(expr)
    case IThrow(name)         =>
    case IIf(cond, thenInst, elseInst) =>
      walk(cond); walk(thenInst); walk(elseInst)
    case IWhile(cond, body) => walk(cond); walk(body)
    case ISeq(insts)        => walkList[Inst](insts, walk)
    case IAssert(expr)      => walk(expr)
    case IPrint(expr)       => walk(expr)
    case IApp(id, fexpr, args) =>
      walk(id); walk(fexpr); walkList[Expr](args, walk)
    case IWithCont(id, params, body) =>
      walk(id); walkList[Id](params, walk); walk(body)

  /** expressions */
  def walk(expr: Expr): Unit = expr match
    case ENum(_) | EINum(_) | EBigINum(_) | EStr(_) | EBool(_) | EUndef |
        ENull | EAbsent =>
    case EConst(name) =>
    case EClo(params, captured, body) =>
      walkList[Id](params, walk); walkList[Id](captured, walk); walk(body)
    case ECont(params, body)      => walkList[Id](params, walk); walk(body)
    case EComp(ty, value, target) => walk(ty); walk(value); walk(target)
    case EMap(ty, props) =>
      walk(ty);
      walkList[(Expr, Expr)](props, { case (x, y) => (walk(x), walk(y)) })
    case EList(exprs)              => walkList[Expr](exprs, walk)
    case ESymbol(desc)             => walk(desc)
    case EPop(list, idx)           => walk(list); walk(idx)
    case ERef(ref)                 => walk(ref)
    case EUOp(uop, expr)           => walk(uop); walk(expr)
    case EBOp(bop, left, right)    => walk(bop); walk(left); walk(right)
    case ETypeOf(expr)             => walk(expr)
    case EIsCompletion(expr)       => walk(expr)
    case EIsInstanceOf(base, name) => walk(base)
    case EGetElems(base, name)     => walk(base)
    case EGetSyntax(base)          => walk(base)
    case EParseSyntax(code, rule, parserParams) => walk(code); walk(rule)
    case EConvert(expr, cop, opt) =>
      walk(expr); walk(cop); walkOpt[Expr](opt, walk)
    case EContains(list, elem)        => walk(list); walk(elem)
    case EReturnIfAbrupt(expr, check) => walk(expr)
    case ECopy(obj)                   => walk(obj)
    case EKeys(obj, intSorted)        => walk(obj)
    case ENotSupported(msg)           =>

  /** references */
  def walk(ref: Ref): Unit = ref match
    case RefId(id)          => walk(id)
    case RefProp(ref, expr) => walk(ref); walk(expr)

  /** types */
  def walk(ty: Ty): Unit = {}

  /** identifiers */
  def walk(id: Id): Unit = {}

  /** unary operators */
  def walk(uop: UOp): Unit = {}

  /** binary operators */
  def walk(bop: BOp): Unit = {}

  /** convert operators */
  def walk(cop: COp): Unit = {}

  // -----------------------------------------------------------------------------
  // States
  // -----------------------------------------------------------------------------
  /** states */
  def walk(st: State): Unit =
    walk(st.context)
    walkList[Context](st.ctxtStack, walk)
    walkMMap[Id, Value](st.globals, walk, walk)
    walk(st.heap)

  /** context */
  def walk(ctxt: Context): Unit =
    walkOpt[Cursor](ctxt.cursorOpt, walk)
    walk(ctxt.retId)
    walkMMap[Id, Value](ctxt.locals, walk, walk)

  /** cursors */
  def walk(cursor: Cursor): Unit = cursor match
    case InstCursor(cur, rest) => walk(cur); walkList[Inst](rest, walk)

  /** heaps */
  def walk(heap: Heap): Unit = walkMMap[Addr, Obj](heap.map, walk, walk)

  /** objs */
  def walk(obj: Obj): Unit = obj match
    case IRSymbol(desc) => walk(desc)
    case IRList(values) => walkList[PureValue](values.toList, walk)
    case IRMap(ty, props, size) =>
      walk(ty)
      walkMMap[PureValue, IRMapValue](props, walk, mv => walk(mv.value))
    case IRNotSupported(tyname, msg) =>

  /** values */
  def walk(v: Value): Unit = v match
    case comp: CompValue => walk(comp)
    case pure: PureValue => walk(pure)

  /** completions */
  def walk(c: CompValue): Unit = c match
    case CompValue(ty, value, target) =>
      walk(ty); walk(value)

  /** pure values */
  def walk(v: PureValue): Unit = v match
    case Num(_) | INum(_) | BigINum(_) | Str(_) | Bool(_) | Undef | Null |
        Absent =>
    case const: Const => walk(const)
    case addr: Addr   => walk(addr)
    case clo: Clo     => walk(clo)
    case cont: Cont   => walk(cont)

  /** constants */
  def walk(c: Const): Unit = {}

  /** addresses */
  def walk(addr: Addr): Unit = {}

  /** closure */
  def walk(clo: Clo): Unit = clo match {
    case Clo(ctxtName, params, locals, cursor) =>
      walkList[Id](params, walk)
      walkMMap[Id, Value](locals, walk, walk)
      walkOpt[Cursor](cursor, walk)
  }

  /** continuation */
  def walk(cont: Cont): Unit = cont match
    case Cont(params, context, ctxtStack) =>
      walkList[Id](params, walk)
      walk(context)
      walkList[Context](ctxtStack, walk)

  /** reference values */
  def walk(refV: RefValue): Unit = refV match
    case RefValueId(id)           => walk(id)
    case RefValueProp(base, prop) => walk(base); walk(prop)
}
