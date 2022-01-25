package esmeta.interp

import esmeta.{TIMEOUT, TEST_MODE}
import esmeta.cfg.*
import esmeta.error.*
import esmeta.interp.Utils.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import scala.math.{BigInt => SBigInt}
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}

/** IR Interpreter */
class Interp(
  val st: State,
  val timeLimit: Option[Long] = Some(TIMEOUT),
) {
  import Interp.*

  /** special class for handle return */
  private case class ReturnValue(value: Value) extends Throwable

  /** control flow graphs */
  def cfg: CFG = st.cfg

  /** step */
  def step: Boolean =
    try interp(st.context.cur)
    catch case ReturnValue(value) => { setReturn(value); true }

  /** fixpoint */
  @tailrec
  final def fixpoint: State = if (step) fixpoint else st

  /** transition for nodes */
  def interp(node: Node): Boolean =
    var keep = true
    node match {
      case Entry(_, next) => moveTo(next)
      case Exit(_) =>
        st.callStack match
          case Nil =>
            st.context.retVal.map(st.globals += GLOBAL_RESULT -> _)
            keep = false
          case CallContext(retId, ctxt) :: rest =>
            val value = st.context.retVal.getOrElse(throw NoReturnValue)
            (value, setTypeMap.get(st.context.name)) match
              case (addr: Addr, Some(tname)) =>
                st.setType(addr, tname)
              case _ =>
            st.context = ctxt
            st.callStack = rest
            st.define(retId, value.wrapCompletion)
      case Linear(_, insts, next) =>
        for (inst <- insts) interp(inst); moveTo(next)
      case Branch(_, _, cond, _, thenNode, elseNode) =>
        moveTo(interp(cond).escaped match {
          case Bool(true)  => thenNode
          case Bool(false) => elseNode
          case v           => throw NoBoolean(cond, v)
        })
      case Call(_, lhs, fexpr, args, _, next) =>
        moveTo(next)
        call(lhs, fexpr, args)
    }
    keep

  /** transition for instructions */
  def interp(inst: Inst): Unit = inst match {
    case IExpr(expr, _)        => interp(expr)
    case ILet(lhs, expr, _)    => st.context.locals += lhs -> interp(expr)
    case IAssign(ref, expr, _) => st.update(interp(ref), interp(expr))
    case IDelete(ref, _)       => st.delete(interp(ref))
    case IPush(from, to, front, _) =>
      interp(to).escaped match {
        case (addr: Addr) =>
          if (front) st.prepend(addr, interp(from).escaped)
          else st.append(addr, interp(from).escaped)
        case v => throw NoAddr(to, v)
      }
    case IReturn(expr, _) => throw ReturnValue(interp(expr))
    case IAssert(expr, _) =>
      interp(expr).escaped match {
        case Bool(true) =>
        case v          => throw AssertionFail(expr)
      }
    case IPrint(expr, _) => {
      val v = interp(expr)
      if (!TEST_MODE) println(st.getString(v))
    }
  }

  /** transition for calls */
  def call(lhs: Id, fexpr: Expr, args: List[Expr]): Unit = fexpr match {
    case ERef(Global(name)) if simpleFuncs contains name =>
      val vs =
        if (name == "IsAbruptCompletion") args.map(interp)
        else args.map(interp(_).escaped)
      st.define(lhs, simpleFuncs(name)(st, vs))
    case _ =>
      interp(fexpr) match {
        case Clo(fid, captured) =>
          val func = cfg.funcMap(fid)
          val vs = args.map(interp)
          val newLocals = getLocals(func.params, vs) ++ captured
          st.callStack ::= CallContext(lhs, st.context)
          st.context = Context(func, newLocals)
        case Cont(fid, captured, callStack) => {
          val func = cfg.funcMap(fid)
          val vs = args.map(interp)
          val newLocals = getLocals(func.params, vs) ++ captured
          st.callStack = callStack.map(_.copied)
          st.context = Context(func, newLocals)
        }
        case v => throw NoFunc(fexpr, v)
      }
  }

  /** transition for expresssions */
  def interp(expr: Expr): Value = expr match {
    case EComp(tyExpr, tgtExpr, valExpr) =>
      val y = interp(tyExpr).escaped
      val t = interp(tgtExpr).escaped
      val v = interp(valExpr).escaped
      (y, t) match
        case (y: Const, Str(t))      => Comp(y, v, Some(t))
        case (y: Const, CONST_EMPTY) => Comp(y, v, None)
        case (y: Const, t)           => throw InvalidCompTarget(y)
        case (y, t)                  => throw InvalidCompType(t)
    case EIsCompletion(expr) =>
      Bool(interp(expr).isCompletion)
    case EReturnIfAbrupt(ERef(ref), check) =>
      val refV = interp(ref)
      val value = returnIfAbrupt(st(refV), check)
      st.update(refV, value)
      value
    case EReturnIfAbrupt(expr, check) =>
      returnIfAbrupt(interp(expr), check)
    case EPop(list, front) =>
      interp(list).escaped match
        case (addr: Addr) => st.pop(addr, front)
        case v            => throw NoAddr(list, v)
    case EYet(msg) =>
      throw NotSupported(msg)
    case EContains(list, elem) =>
      interp(list).escaped match
        case addr: Addr =>
          st(addr) match
            case ListObj(vs) => Bool(vs contains interp(elem).escaped)
            case obj         => throw NoList(list, obj)
        case v => throw NoAddr(list, v)
    case ERef(ref) =>
      st(interp(ref))
    case EUnary(uop, expr) =>
      val x = interp(expr).escaped
      Interp.interp(uop, x)
    case EBinary(BOp.And, left, right) => shortCircuit(BOp.And, left, right)
    case EBinary(BOp.Or, left, right)  => shortCircuit(BOp.Or, left, right)
    case EBinary(BOp.Eq, ERef(ref), EAbsent) => Bool(!st.exists(interp(ref)))
    case EBinary(bop, left, right) =>
      val l = interp(left).escaped
      val r = interp(right).escaped
      Interp.interp(bop, l, r)
    case EConvert(cop, expr) =>
      import COp.*
      (interp(expr).escaped, cop) match {
        case (Str(s), ToNumber) => ??? // TODO Number(ESValueParser.str2num(s))
        case (Str(s), ToBigInt) => ??? // TODO ESValueParser.str2bigint(s)
        // TODO other cases
        case (v, cop) => throw InvalidConversion(cop, expr, v)
      }
    case ETypeOf(base)        => ??? // TODO discuss about the type
    case ETypeCheck(expr, ty) => ??? // TODO discuss about the type
    case EClo(fid, captured) =>
      Clo(fid, Map.from(captured.map(x => x -> st(x))))
    case ECont(fid) =>
      val captured = st.context.locals.collect { case (x: Name, v) => x -> v }
      Cont(fid, Map.from(captured), st.callStack)
    case ESyntactic(name, args, rhsIdx, bits, children) =>
      val asts = children.map(child =>
        interp(child) match {
          case ast: Ast => ast
          case v        => throw NoAst(child, v)
        },
      )
      Syntactic(name, args, rhsIdx, bits, asts)
    case ELexical(name, expr) =>
      val str = interp(expr) match {
        case Str(str) => str
        case v        => throw NoString(expr, v)
      }
      Lexical(name, str)
    case EMap("Completion", props, asite) =>
      val map = (for {
        (kexpr, vexpr) <- props
        k = interp(kexpr).escaped
        v = interp(vexpr).escaped
      } yield k -> v).toMap
      (
        map.get(Str("Type")),
        map.get(Str("Value")),
        map.get(Str("Target")),
      ) match
        case (Some(ty: Const), Some(value), Some(target)) =>
          val targetOpt = target match
            case Str(target) => Some(target)
            case CONST_EMPTY => None
            case v           => throw InvalidCompTarget(v)
          Comp(ty, value, targetOpt)
        case _ => throw InvalidComp
    case EMap(ty, props, asite) =>
      val addr = st.allocMap(ty)
      for ((kexpr, vexpr) <- props)
        val k = interp(kexpr).escaped
        val v = interp(vexpr)
        st.update(addr, k, v)
      addr
    case EList(exprs, asite) =>
      st.allocList(exprs.map(expr => interp(expr).escaped))
    case ESymbol(desc, asite) =>
      interp(desc) match
        case (str: Str) => st.allocSymbol(str)
        case Undef      => st.allocSymbol(Undef)
        case v          => throw NoString(desc, v)
    case ECopy(obj, asite) =>
      interp(obj).escaped match
        case addr: Addr => st.copyObj(addr)
        case v          => throw NoAddr(obj, v)
    case EKeys(map, intSorted, asite) =>
      interp(map).escaped match
        case addr: Addr => st.keys(addr, intSorted)
        case v          => throw NoAddr(map, v)
    case EMathVal(n)           => Math(n)
    case ENumber(n) if n.isNaN => Number(Double.NaN)
    case ENumber(n)            => Number(n)
    case EBigInt(n)            => BigInt(n)
    case EStr(str)             => Str(str)
    case EBool(b)              => Bool(b)
    case EUndef                => Undef
    case ENull                 => Null
    case EAbsent               => Absent
    case EConst(name)          => Const(name)
  }

  /** short circuit evaluation */
  def shortCircuit(bop: BOp, left: Expr, right: Expr): Value =
    val l = interp(left).escaped
    (bop, l) match
      case (BOp.And, Bool(false)) => Bool(false)
      case (BOp.Or, Bool(true))   => Bool(true)
      case _ =>
        val r = interp(right).escaped
        Interp.interp(bop, l, r)

  /** get initial local variables */
  def getLocals(params: List[Param], args: List[Value]): MMap[Local, Value] = {
    val map = MMap[Local, Value]()
    @tailrec
    def aux(ps: List[Param], as: List[Value]): Unit = (ps, as) match {
      case (Nil, Nil) =>
      case (Param(lhs, kind, _) :: pl, Nil) =>
        import Param.Kind.*
        kind match
          case Normal => throw RemainingParams(ps)
          case Optional =>
            map += lhs -> Absent
            aux(pl, Nil)
      case (Nil, args) =>
        throw RemainingArgs(args)
      case (param :: pl, arg :: al) =>
        map += param.lhs -> arg
        aux(pl, al)
    }
    aux(params, args)
    map
  }

  /** helper for return-if-abrupt cases */
  def returnIfAbrupt(value: Value, check: Boolean): Value = value match
    case NormalComp(value) => value
    case comp: Comp =>
      if (check) throw ReturnValue(value)
      else throw UncheckedAbrupt(comp)
    case pure: PureValue => pure

  /** transition for references */
  def interp(ref: Ref): RefValue = ref match
    case x: Id => IdValue(x)
    case Prop(ref, expr) =>
      var base = st(interp(ref))
      val p = interp(expr).escaped
      PropValue(base, p)

  /** move to a node */
  def moveTo(nid: Int): Unit =
    st.context.cur = cfg.nodeMap.getOrElse(nid, throw InvalidNodeId(nid))

  /** set return value and move to the exit node */
  def setReturn(value: Value): Unit =
    st.context.retVal = Some(value)
    st.context.cur = st.func.exit
}

/** Interp object */
object Interp {

  /** run interp */
  def apply(
    cfg: CFG,
    st: State,
    timeLimit: Option[Long] = Some(TIMEOUT),
  ): State =
    val interp = new Interp(st)
    timeout(interp.fixpoint, timeLimit)
    st

  // type update algorithms
  val setTypeMap: Map[String, String] = Map(
    "OrdinaryFunctionCreate" -> "ECMAScriptFunctionObject",
    "ArrayCreate" -> "ArrayExoticObject",
  )

  // simple functions
  type SimpleFunc = PartialFunction[(State, List[Value]), Value]
  def arityCheck(pair: (String, SimpleFunc)): (String, SimpleFunc) =
    val (name, f) = pair
    name -> {
      case (st, args) =>
        optional(f(st, args)).getOrElse(throw InvalidArgs(name, args))
    }
  def mathBOp(
    name: String,
    op: (BigDecimal, BigDecimal) => BigDecimal,
  ): (String, SimpleFunc) =
    name -> {
      case (st, list @ _ :: _) =>
        val ds = list.map {
          case x: Math => x.n
          case v       => throw InvalidArgs(name, List(v))
        }
        Math(ds.reduce(op))
    }
  def mathUOp(
    name: String,
    op: BigDecimal => BigDecimal,
  ): (String, SimpleFunc) =
    name -> { case (st, List(Math(n))) => Math(op(n)) }
  val simpleFuncs: Map[String, SimpleFunc] = Map(
    arityCheck("GetArgument" -> {
      case (st, List(addr: Addr)) =>
        st(addr) match
          case list @ ListObj(vs) =>
            if (vs.isEmpty) Absent
            else list.pop(front = true)
          case _ => error(s"non-list @ GetArgument: $addr")
    }),
    arityCheck("IsDuplicate" -> {
      case (st, List(addr: Addr)) =>
        st(addr) match
          case ListObj(vs) => Bool(vs.toSet.size != vs.length)
          case _           => error(s"non-list @ IsDuplicate: $addr")
    }),
    arityCheck("IsArrayIndex" -> {
      case (st, List(Str(s))) =>
        // val d = ESValueParser.str2num(s)
        // val ds = toStringHelper(d)
        // val UPPER = (1L << 32) - 1
        // val l = d.toLong
        // Bool(ds == s && 0 <= l && d == l && l < UPPER)
        ???
      case (st, List(v)) => Bool(false)
    }),
    arityCheck(mathBOp("min", _ min _)),
    arityCheck(mathBOp("max", _ max _)),
    arityCheck(mathUOp("abs", _.abs)),
    arityCheck(mathUOp("floor", _.toDouble.floor)),
    arityCheck("fround" -> {
      case (st, List(Number(n))) => Number(n.toFloat.toDouble)
    }),
    arityCheck("ThrowCompletion" -> {
      case (st, List(value)) => value.wrapCompletion(CONST_THROW)
    }),
    arityCheck("NormalCompletion" -> {
      case (st, List(value)) => value.wrapCompletion
    }),
    arityCheck("IsAbruptCompletion" -> {
      case (st, List(value)) => Bool(value.isAbruptCompletion)
    }),
  )

  /** transition for unary opeartors */
  def interp(uop: UOp, operand: Value): Value =
    import UOp.*
    (uop, operand) match
      case (Neg, Number(n))  => Number(-n)
      case (Neg, Math(n))    => Math(-n)
      case (Neg, BigInt(b))  => BigInt(-b)
      case (Not, Bool(b))    => Bool(!b)
      case (BNot, Number(n)) => Math(~(n.toInt))
      case (BNot, Math(n))   => Math(~(n.toInt))
      case (BNot, BigInt(b)) => BigInt(~b)
      case (_, value) =>
        error(s"wrong type of value for the operator $uop: $value")

  /** transition for binary operators */
  def interp(bop: BOp, left: Value, right: Value): Value =
    import BOp.*
    (bop, left, right) match {
      // double operations
      case (Plus, Number(l), Number(r)) => Number(l + r)
      case (Sub, Number(l), Number(r))  => Number(l - r)
      case (Mul, Number(l), Number(r))  => Number(l * r)
      case (Pow, Number(l), Number(r))  => Number(math.pow(l, r))
      case (Div, Number(l), Number(r))  => Number(l / r)
      case (Mod, Number(l), Number(r))  => Number(l % r)
      case (UMod, Number(l), Number(r)) => Number(l %% r)
      case (Lt, Number(l), Number(r))   => Bool(l < r)

      // double with long operations
      case (Plus, Math(l), Number(r)) => Number(l.toDouble + r)
      case (Sub, Math(l), Number(r))  => Number(l.toDouble - r)
      case (Mul, Math(l), Number(r))  => Number(l.toDouble * r)
      case (Div, Math(l), Number(r))  => Number(l.toDouble / r)
      case (Mod, Math(l), Number(r))  => Number(l.toDouble % r)
      case (Pow, Math(l), Number(r))  => Number(scala.math.pow(l.toDouble, r))
      case (UMod, Math(l), Number(r)) => Number(l.toDouble %% r)
      case (Lt, Math(l), Number(r))   => Bool(l < r)
      case (Plus, Number(l), Math(r)) => Number(l + r.toDouble)
      case (Sub, Number(l), Math(r))  => Number(l - r.toDouble)
      case (Mul, Number(l), Math(r))  => Number(l * r.toDouble)
      case (Div, Number(l), Math(r))  => Number(l / r.toDouble)
      case (Mod, Number(l), Math(r))  => Number(l % r.toDouble)
      case (Pow, Number(l), Math(r))  => Number(math.pow(l, r.toDouble))
      case (UMod, Number(l), Math(r)) => Number(l %% r.toDouble)
      case (Lt, Number(l), Math(r))   => Bool(l < r)

      // string operations
      case (Plus, Str(l), Str(r)) => Str(l + r)
      case (Plus, Str(l), Number(r)) =>
        Str(l + Character.toChars(r.toInt).mkString(""))
      case (Sub, Str(l), Math(r)) => Str(l.dropRight(r.toInt))
      case (Lt, Str(l), Str(r))   => Bool(l < r)

      // long operations
      case (Plus, Math(l), Math(r)) => Math(l + r)
      case (Sub, Math(l), Math(r))  => Math(l - r)
      case (Mul, Math(l), Math(r))  => Math(l * r)
      case (Div, Math(l), Math(r))  => Math(l / r)
      case (Mod, Math(l), Math(r))  => Math(l % r)
      case (UMod, Math(l), Math(r)) => Math(l %% r)
      case (Pow, Math(l), Math(r)) =>
        Math(math.pow(l.toDouble, r.toDouble))
      case (Lt, Math(l), Math(r))   => Bool(l < r)
      case (BAnd, Math(l), Math(r)) => Math(l.toLong & r.toLong)
      case (BOr, Math(l), Math(r))  => Math(l.toLong | r.toLong)
      case (BXOr, Math(l), Math(r)) => Math(l.toLong ^ r.toLong)
      case (LShift, Math(l), Math(r)) =>
        Math((l.toInt << r.toInt).toLong)
      case (SRShift, Math(l), Math(r)) =>
        Math((l.toInt >> r.toInt).toLong)
      case (URShift, Math(l), Math(r)) =>
        Math((l.toLong >>> r.toLong) & 0xffffffff)

      // logical operations
      case (And, Bool(l), Bool(r)) => Bool(l && r)
      case (Or, Bool(l), Bool(r))  => Bool(l || r)
      case (Xor, Bool(l), Bool(r)) => Bool(l ^ r)

      // equality operations
      case (Eq, Math(l), Number(r))   => Bool(!(r equals -0.0) && l == r)
      case (Eq, Number(l), Math(r))   => Bool(!(l equals -0.0) && l == r)
      case (Eq, Number(l), Number(r)) => Bool(l equals r)
      case (Eq, Number(l), BigInt(r)) => Bool(l == r)
      case (Eq, BigInt(l), Number(r)) => Bool(l == r)
      case (Eq, Math(l), BigInt(r))   => Bool(l == r)
      case (Eq, BigInt(l), Math(r))   => Bool(l == r)
      case (Eq, l: Ast, r: Ast)       => Bool(l eq r)
      case (Eq, l, r)                 => Bool(l == r)

      // double equality operations
      case (Equal, Math(l), Number(r))   => Bool(l == r)
      case (Equal, Number(l), Math(r))   => Bool(l == r)
      case (Equal, Number(l), Number(r)) => Bool(l == r)
      case (Equal, l: Ast, r: Ast)       => Bool(l eq r)
      case (Equal, l, r)                 => Bool(l == r)

      // double with big integers
      case (Lt, BigInt(l), Number(r)) => Bool(BigDecimal(l) < BigDecimal(r))
      case (Lt, BigInt(l), Math(r))   => Bool(BigDecimal(l) < r)
      case (Lt, Number(l), BigInt(r)) => Bool(BigDecimal(l) < BigDecimal(r))
      case (Lt, Math(l), BigInt(r))   => Bool(l < BigDecimal(r))

      // big integers
      case (Plus, BigInt(l), BigInt(r))    => BigInt(l + r)
      case (LShift, BigInt(l), BigInt(r))  => BigInt(l << r.toInt)
      case (SRShift, BigInt(l), BigInt(r)) => BigInt(l >> r.toInt)
      case (Sub, BigInt(l), BigInt(r))     => BigInt(l - r)
      case (Sub, BigInt(l), Math(r))       => BigInt(l - r.toBigInt)
      case (Mul, BigInt(l), BigInt(r))     => BigInt(l * r)
      case (Div, BigInt(l), BigInt(r))     => BigInt(l / r)
      case (Mod, BigInt(l), BigInt(r))     => BigInt(l % r)
      case (UMod, BigInt(l), BigInt(r))    => BigInt(l %% r)
      case (UMod, BigInt(l), Math(r))      => BigInt(l %% r.toBigInt)
      case (Lt, BigInt(l), BigInt(r))      => Bool(l < r)
      case (BAnd, BigInt(l), BigInt(r))    => BigInt(l & r)
      case (BOr, BigInt(l), BigInt(r))     => BigInt(l | r)
      case (BXOr, BigInt(l), BigInt(r))    => BigInt(l ^ r)
      case (Pow, BigInt(l), BigInt(r))     => BigInt(l.pow(r.toInt))
      case (Pow, BigInt(l), Math(r))       => BigInt(l.pow(r.toInt))
      case (Pow, BigInt(l), Number(r)) =>
        if (r.toInt < 0) Number(math.pow(l.toDouble, r))
        else BigInt(l.pow(r.toInt))

      case (_, lval, rval) => error(s"wrong type: $lval $bop $rval")
    }
}
