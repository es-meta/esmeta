package esmeta.ir

import esmeta.util.BasicWalker

/** a walker for IR */
trait Walker extends BasicWalker {
  def walk(node: IRElem): IRElem = node match
    case prog: Program  => walk(prog)
    case inst: Inst     => walk(inst)
    case expr: Expr     => walk(expr)
    case ref: Ref       => walk(ref)
    case ty: Ty         => walk(ty)
    case id: Id         => walk(id)
    case uop: UOp       => walk(uop)
    case bop: BOp       => walk(bop)
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
  def walk(program: Program): Program = Program(
    walkList[Inst](program.insts, walk),
  )

  /** instructions */
  def walk(inst: Inst): Inst = inst match
    case IExpr(expr)          => IExpr(walk(expr))
    case ILet(id, expr)       => ILet(walk(id), walk(expr))
    case IAssign(ref, expr)   => IAssign(walk(ref), walk(expr))
    case IDelete(ref)         => IDelete(walk(ref))
    case IAppend(expr, list)  => IAppend(walk(expr), walk(list))
    case IPrepend(expr, list) => IPrepend(walk(expr), walk(list))
    case IReturn(expr)        => IReturn(walk(expr))
    case IThrow(name)         => IThrow(name)
    case IIf(cond, thenInst, elseInst) =>
      IIf(walk(cond), walk(thenInst), walk(elseInst))
    case IWhile(cond, body) => IWhile(walk(cond), walk(body))
    case ISeq(insts)        => ISeq(walkList[Inst](insts, walk))
    case IAssert(expr)      => IAssert(walk(expr))
    case IPrint(expr)       => IPrint(walk(expr))
    case IApp(id, fexpr, args) =>
      IApp(walk(id), walk(fexpr), walkList[Expr](args, walk))
    case IWithCont(id, params, body) =>
      IWithCont(walk(id), walkList[Id](params, walk), walk(body))

  /** expressions */
  def walk(expr: Expr): Expr = expr match
    case ENum(_) | EINum(_) | EBigINum(_) | EStr(_) | EBool(_) | EUndef |
        ENull | EAbsent =>
      expr
    case EConst(name) => EConst(name)
    case EClo(params, captured, body) =>
      EClo(
        walkList[Id](params, walk),
        walkList[Id](captured, walk),
        walk(body),
      )
    case ECont(params, body) => ECont(walkList[Id](params, walk), walk(body))
    case EComp(ty, value, target) => EComp(walk(ty), walk(value), walk(target))
    case EMap(ty, props) =>
      EMap(
        walk(ty),
        walkList[(Expr, Expr)](props, { case (x, y) => (walk(x), walk(y)) }),
      )
    case EList(exprs)              => EList(walkList[Expr](exprs, walk))
    case ESymbol(desc)             => ESymbol(walk(desc))
    case EPop(list, idx)           => EPop(walk(list), walk(idx))
    case ERef(ref)                 => ERef(walk(ref))
    case EUOp(uop, expr)           => EUOp(walk(uop), walk(expr))
    case EBOp(bop, left, right)    => EBOp(walk(bop), walk(left), walk(right))
    case ETypeOf(expr)             => ETypeOf(walk(expr))
    case EIsCompletion(expr)       => EIsCompletion(walk(expr))
    case EIsInstanceOf(base, name) => EIsInstanceOf(walk(base), name)
    case EGetElems(base, name)     => EGetElems(walk(base), name)
    case EGetSyntax(base)          => EGetSyntax(walk(base))
    case EParseSyntax(code, rule, parserParams) =>
      EParseSyntax(walk(code), walk(rule), parserParams)
    case EConvert(expr, cop, opt) =>
      EConvert(walk(expr), walk(cop), walkOpt[Expr](opt, walk))
    case EContains(list, elem)        => EContains(walk(list), walk(elem))
    case EReturnIfAbrupt(expr, check) => EReturnIfAbrupt(walk(expr), check)
    case ECopy(obj)                   => ECopy(walk(obj))
    case EKeys(obj, intSorted)        => EKeys(walk(obj), intSorted)
    case ENotSupported(msg)           => ENotSupported(msg)

  /** references */
  def walk(ref: Ref): Ref = ref match
    case RefId(id)        => RefId(walk(id))
    case RefProp(ref, id) => RefProp(walk(ref), walk(id))

  /** types */
  def walk(ty: Ty): Ty = ty

  /** identifiers */
  def walk(id: Id): Id = id

  /** unary operators */
  def walk(uop: UOp): UOp = uop

  /** binary operators */
  def walk(bop: BOp): BOp = bop

  /** convert operators */
  def walk(cop: COp): COp = cop

  // -----------------------------------------------------------------------------
  // States
  // -----------------------------------------------------------------------------
  /** state */
  def walk(st: State): State = State(
    st.cursorGen,
    walk(st.context),
    walkList[Context](st.ctxtStack, walk),
    walkMMap[Id, Value](st.globals, walk, walk),
    walk(st.heap),
  )

  /** contexts */
  def walk(ctxt: Context): Context = Context(
    walkOpt[Cursor](ctxt.cursorOpt, walk),
    walk(ctxt.retId),
    ctxt.name,
    walkMMap[Id, Value](ctxt.locals, walk, walk),
  )

  /** cursors */
  def walk(cursor: Cursor): Cursor = cursor match
    case InstCursor(cur, rest) =>
      InstCursor(walk(cur), walkList[Inst](rest, walk))

  /** heaps */
  def walk(heap: Heap): Heap = Heap(walkMMap[Addr, Obj](heap.map, walk, walk))

  /** objects */
  def walk(obj: Obj): Obj = obj match
    case IRSymbol(desc) => IRSymbol(walk(desc))
    case IRMap(ty, props, size) =>
      IRMap(
        walk(ty),
        walkMMap[PureValue, IRMapValue](
          props,
          walk,
          walk,
        ),
        size,
      )
    case IRList(values) =>
      IRList(
        walkList[PureValue](values.toList, walk).toVector,
      )
    case IRNotSupported(tyname, msg) => IRNotSupported(tyname, msg)

  /** ir map values */
  def walk(mv: IRMapValue): IRMapValue =
    IRMapValue(walk(mv.value), mv.creationTime)

  /** values */
  def walk(v: Value): Value = v match
    case comp: CompValue => walk(comp)
    case pure: PureValue => walk(pure)

  /** completions */
  def walk(c: CompValue): Unit = c match
    case CompValue(ty, value, target) =>
      CompValue(walk(ty), walk(value), target)

  /** values */
  def walk(value: PureValue): PureValue = value match
    case const: Const => walk(const)
    case addr: Addr   => walk(addr)
    case clo: Clo     => walk(clo)
    case cont: Cont   => walk(cont)
    case Num(_) | INum(_) | BigINum(_) | Str(_) | Bool(_) | Undef | Null |
        Absent =>
      value

  /** constants */
  def walk(c: Const): Const = c

  /** addresses */
  def walk(addr: Addr): Addr = addr

  /** closure */
  def walk(clo: Clo): Clo = clo match
    case Clo(ctxtName, params, locals, cursor) =>
      Clo(
        ctxtName,
        walkList[Id](params, walk),
        walkMMap[Id, Value](locals, walk, walk),
        walkOpt[Cursor](cursor, walk),
      )

  /** continuation */
  def walk(cont: Cont): Cont = cont match
    case Cont(params, context, ctxtStack) =>
      Cont(
        walkList[Id](params, walk),
        walk(context),
        walkList[Context](ctxtStack, walk),
      )

  /** reference values */
  def walk(refV: RefValue): RefValue = refV match
    case RefValueId(id)           => RefValueId(walk(id))
    case RefValueProp(base, prop) => RefValueProp(walk(base), walk(prop))
}
