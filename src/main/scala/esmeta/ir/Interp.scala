package esmeta.ir

import esmeta.error.*
import esmeta.ir.Utils.*
import esmeta.spec.Param
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{DEBUG, TIMEOUT}
import scala.annotation.{tailrec, targetName}
import scala.collection.mutable.{Map => MMap}
import scala.language.implicitConversions

/** IR Interpreter */
class Interp(val st: State) {
  import Interp.*

  val cursorGen: CursorGen[_ <: Cursor] = st.cursorGen

  /** special class for handle return */
  private case class ReturnValue(value: Value) extends Throwable

  /** step */
  final def step: Boolean = st.nextTarget match
    case None         => false
    case Some(cursor) =>
      // run single instruction
      try
        cursor match
          case InstCursor(inst, rest) => interp(inst, rest)

      // handle return instruction
      catch case ReturnValue(value) => st.doReturn(value)
      true

  /** fixpoint */
  @tailrec
  final def fixpoint: State = if step then fixpoint else st

  /** transition for instructions */
  def interp(inst: Inst, rest: List[Inst]): Unit = inst match
    case inst: ISeq       => interp(inst, rest)
    case inst: CondInst   => interp(inst, rest)
    case inst: CallInst   => interp(inst)
    case inst: ArrowInst  => interp(inst)
    case inst: NormalInst => interp(inst)

  /** transition for sequence instructions */
  def interp(inst: ISeq, rest: List[Inst]): Unit =
    st.context.cursorOpt = InstCursor.from(inst.insts ++ rest)

  /** transition for conditional instructions */
  def interp(inst: CondInst, rest: List[Inst]): Unit =
    st.context.cursorOpt = inst match
      case IIf(cond, thenInst, elseInst) =>
        interp(cond).escaped match
          case Bool(true)  => Some(InstCursor(thenInst, rest))
          case Bool(false) => Some(InstCursor(elseInst, rest))
          case v           => error(s"not a boolean: $v")
      case IWhile(cond, body) =>
        interp(cond).escaped match
          case Bool(true)  => Some(InstCursor(body, inst :: rest))
          case Bool(false) => InstCursor.from(rest)
          case v           => error(s"not a boolean: $v")

  /** transition for call instructions */
  def interp(inst: CallInst): Unit =
    st.moveNext
    inst match
      case IApp(id, fexpr, args) =>
        interp(fexpr).escaped match
          // closure
          case Clo(ctxtName, params, locals, cursorOpt) =>
            val vs = args.map(interp)
            val newLocals =
              locals ++ getLocals(params.map(x => Param(x.name)), vs)
            val context = Context(
              cursorOpt,
              id,
              ctxtName + ":closure",
              locals,
            )
            st.ctxtStack ::= st.context
            st.context = context

          // continuation
          case Cont(params, context, ctxtStack) =>
            val vs = args.map(interp)
            st.context = context.copied
            st.context.locals ++= params zip vs
            st.ctxtStack = ctxtStack.map(_.copied)
          case v => error(s"not a function: $fexpr -> $v")

  /** transition for normal instructions */
  def interp(inst: NormalInst): Unit =
    st.moveNext
    inst match
      case IExpr(expr)        => interp(expr)
      case ILet(id, expr)     => st.context.locals += id -> interp(expr)
      case IAssign(ref, expr) => st.update(interp(ref), interp(expr))
      case IDelete(ref)       => st.delete(interp(ref))
      case IAppend(expr, list) =>
        interp(list).escaped match
          case (addr: Addr) => st.append(addr, interp(expr).escaped)
          case v            => error(s"not an address: $v")
      case IPrepend(expr, list) =>
        interp(list).escaped match
          case (addr: Addr) => st.prepend(addr, interp(expr).escaped)
          case v            => error(s"not an address: $v")
      case IReturn(expr) => throw ReturnValue(interp(expr))
      case IThrow(name)  => ??? // TODO need error object modeling
      case IAssert(expr) =>
        interp(expr).escaped match
          case Bool(true) =>
          case v          => error(s"assertion failure: $expr")
      case IPrint(expr) => println(st.getString(interp(expr)))

  /** transition for arrow instructions */
  def interp(inst: ArrowInst): Unit =
    st.moveNext
    inst match
      case IWithCont(id, params, body) =>
        val State(_, context, ctxtStack, _, _, _, _) = st
        st.context = context.copied
        st.context.cursorOpt = cursorGen(body)
        st.context.locals += id -> Cont(params, context, ctxtStack)
        st.ctxtStack = ctxtStack.map(_.copied)

  /** transition for expresssions */
  def interp(expr: Expr): Value = expr match {
    // pure value
    case ENum(n)      => Num(n)
    case EINum(n)     => INum(n)
    case EBigINum(b)  => BigINum(b)
    case EStr(str)    => Str(str)
    case EBool(b)     => Bool(b)
    case EUndef       => Undef
    case ENull        => Null
    case EAbsent      => Absent
    case EConst(name) => Const(name)
    case EClo(params, captured, body) =>
      Clo(
        st.context.name,
        params,
        MMap.from(captured.map(x => x -> st(x))),
        cursorGen(body),
      )
    case ECont(params, body) =>
      val newCtxt = st.context.copied
      newCtxt.cursorOpt = cursorGen(body)
      Cont(
        params,
        newCtxt,
        st.ctxtStack.map(_.copied),
      )

    // completion
    case EComp(ty, value, target) =>
      val tyV = interp(ty).escaped
      val v = interp(value).escaped
      val targetV = interp(target).escaped
      (tyV, targetV) match
        case (y: Const, str: Str)    => CompValue(y, v, str)
        case (y: Const, CONST_EMPTY) => CompValue(y, v)
        case _                       => error("invalid completion")
    case EIsCompletion(expr) => Bool(interp(expr).isCompletion)
    case EReturnIfAbrupt(ERef(ref), check) =>
      val refV = interp(ref)
      val value = returnIfAbrupt(st(refV), check)
      st.update(refV, value)
      value
    case EReturnIfAbrupt(expr, check) => returnIfAbrupt(interp(expr), check)

    // IR objects
    case EMap(ty, props) =>
      val addr = st.allocMap(ty)
      for ((kexpr, vexpr) <- props)
        val k: PureValue = interp(kexpr).escaped
        val v = interp(vexpr)
        st.update(addr, k, v)
      addr
    case EList(exprs) => st.allocList(exprs.map(expr => interp(expr).escaped))
    case ESymbol(desc) =>
      interp(desc) match
        case (str: Str) => st.allocSymbol(str)
        case Undef      => st.allocSymbol(Undef)
        case v          => error(s"not a string: $v")
    case ENotSupported(msg) => throw NotSupported(msg)
    case EPop(list, idx) =>
      interp(list).escaped match
        case (addr: Addr) => st.pop(addr, interp(idx).escaped)
        case v            => error(s"not an address: $v")
    case EContains(list, elem) =>
      interp(list).escaped match
        case addr: Addr =>
          st(addr) match
            case IRList(vs) => Bool(vs contains interp(elem).escaped)
            case obj        => error(s"not a list: $obj")
        case v => error(s"not an address: $v")
    case ECopy(obj) =>
      interp(obj).escaped match
        case addr: Addr => ???
        // st.copyObj(addr)
        case v => error(s"not an address: $v")
    case EKeys(mobj, intSorted) =>
      interp(mobj).escaped match
        case addr: Addr => st.keys(addr, intSorted)
        case v          => error(s"not an address: $v")
    case EIsInstanceOf(base, name) =>
      val bv = interp(base)
      if (bv.isAbruptCompletion) Bool(false)
      else
        bv.escaped match
          // TODO handle AST value
          case Str(str) => Bool(str == name)
          case addr: Addr =>
            st(addr) match
              case IRMap(ty, _, _) => ??? // TODO need type modeling
              case _               => Bool(false)
          case _ => Bool(false)

    // etc
    case ERef(ref)                  => st(interp(ref))
    case EUOp(uop, expr)            => Interp.interp(uop, interp(expr).escaped)
    case EBOp(BOp.And, left, right) => shortCircuit(BOp.And, left, right)
    case EBOp(BOp.Or, left, right)  => shortCircuit(BOp.Or, left, right)
    case EBOp(BOp.Eq, ERef(ref), EAbsent) => Bool(!st.exists(interp(ref)))
    case EBOp(bop, left, right) =>
      val lv = interp(left).escaped
      val rv = interp(right).escaped
      Interp.interp(bop, lv, rv)
    case ETypeOf(expr) =>
      Str(
        interp(expr).escaped match
          case Const(const) => "Constant"
          case (addr: Addr) =>
            st(addr).ty.name match
              case name if name endsWith "Object" => "Object"
              case name                           => name
          case Num(_) | INum(_) => "Number"
          case BigINum(_)       => "BigInt"
          case Str(_)           => "String"
          case Bool(_)          => "Boolean"
          case Undef            => "Undefined"
          case Null             => "Null"
          case Absent           => "Absent"
          case Clo(_, _, _, _)  => "Closure"
          case Cont(_, _, _)    => "Continuation",
      )
    case EConvert(source, target, radixOpt) =>
      import COp.*
      interp(source).escaped match
        case Str(s) => ??? // TODO need ESValueParser
        case INum(n) =>
          target match
            case NumToStr => Str(toStringHelper(n.toDouble, getRadix(radixOpt)))
            case NumToInt => INum(n)
            case NumToBigInt => BigINum(BigInt(n))
            case _ => error(s"not convertable option: INum to $target")
        case Num(n) =>
          target match
            case NumToStr => Str(toStringHelper(n, getRadix(radixOpt)))
            case NumToInt =>
              INum((math.signum(n) * math.floor(math.abs(n))).toLong)
            case NumToBigInt =>
              BigINum(BigInt(new java.math.BigDecimal(n).toBigInteger))
            case _ => error(s"not convertable option: INum to $target")
        case BigINum(b) =>
          target match
            case NumToBigInt => BigINum(b)
            case NumToStr    => Str(b.toString)
            case BigIntToNum => Num(b.toDouble)
            case _ => error(s"not convertable option: BigINum to $target")
        case v => error(s"not an convertable value: $v")

    // TODO handle AST-related expressions
    case _: ASTExpr => ???
  }

  /** return if abrupt completion */
  def returnIfAbrupt(value: Value, check: Boolean): Value =
    value match
      case CompValue(CONST_NORMAL, value, CONST_EMPTY) => value
      case CompValue(_, _, _) =>
        if (check) throw ReturnValue(value)
        else error(s"unchecked abrupt completion: $value")
      case pv: PureValue => pv

  /** transition for references */
  def interp(ref: Ref): RefValue = ref match
    case RefId(id) => RefValueId(id)
    case RefProp(ref, expr) =>
      var base = st(interp(ref))
      val p = interp(expr).escaped
      RefValueProp(base, p)

  /** short circuit evaluation */
  def shortCircuit(bop: BOp, left: Expr, right: Expr): Value =
    import BOp.*
    val l = interp(left).escaped
    (bop, l) match
      case (And, Bool(false)) => Bool(false)
      case (Or, Bool(true))   => Bool(true)
      case _ =>
        val r = interp(right).escaped
        Interp.interp(bop, l, r)

  /** prepare initial local variables */
  def getLocals(params: List[Param], args: List[Value]): MMap[Id, Value] =
    val map = MMap[Id, Value]()

    @tailrec
    def aux(ps: List[Param], as: List[Value]): Unit = (ps, as) match
      case (Nil, Nil) =>

      // remaining parameters
      case (Param(name, kind, _) :: pl, Nil) =>
        kind match
          case Param.Kind.Normal => error(s"remaining parameter: $name")
          case _ =>
            map += Id(name) -> Absent
            aux(pl, Nil)

      // remaining arguments
      case (Nil, args) =>
        val argsStr = args.mkString("[", ", ", "]")
        error(s"remaining arguments: $argsStr")

      // map a parameter with an arugment
      case (param :: pl, arg :: al) =>
        map += Id(param.name) -> arg
        aux(pl, al)

    aux(params, args)
    map

  /** get radix for convert expression */
  def getRadix(radixOpt: Option[Expr]): Int =
    radixOpt.fold(10)(e =>
      interp(e).escaped match
        case INum(n) => n.toInt
        case Num(n)  => n.toInt
        case _       => error("radix is not int"),
    )
}

/** Interp object */
object Interp {

  /** run interp */
  def apply(
    st: State,
    timeLimit: Option[Long] = Some(TIMEOUT),
  ): State =
    val interp = new Interp(st)
    timeout(interp.fixpoint, timeLimit)
    st

  /** transition for unary opeartors */
  def interp(uop: UOp, operand: Value): Value =
    import UOp.*
    (uop, operand) match
      case (Neg, Num(n))     => Num(-n)
      case (Neg, INum(n))    => INum(-n)
      case (Neg, BigINum(b)) => BigINum(-b)
      case (Not, Bool(b))    => Bool(!b)
      case (Not, Num(n))     => INum(~(n.toInt))
      case (Not, INum(n))    => INum(~n)
      case (Not, BigINum(b)) => BigINum(~b)
      case (_, value) =>
        error(s"wrong type of value for the operator $uop: $value")

  /** transition for binary operators */
  def interp(bop: BOp, left: Value, right: Value): Value =
    import BOp.*
    given Conversion[Long, Double] = _.toDouble
    (bop, left, right) match {
      // double operations
      case (Plus, Num(l), Num(r)) => Num(l + r)
      case (Sub, Num(l), Num(r))  => Num(l - r)
      case (Mul, Num(l), Num(r))  => Num(l * r)
      case (Pow, Num(l), Num(r))  => Num(math.pow(l, r))
      case (Div, Num(l), Num(r))  => Num(l / r)
      case (Mod, Num(l), Num(r))  => Num(modulo(l, r))
      case (UMod, Num(l), Num(r)) => Num(unsigned_modulo(l, r))
      case (Lt, Num(l), Num(r))   => Bool(l < r)

      // double with long operations
      case (Plus, INum(l), Num(r)) => Num(l + r)
      case (Sub, INum(l), Num(r))  => Num(l - r)
      case (Mul, INum(l), Num(r))  => Num(l * r)
      case (Div, INum(l), Num(r))  => Num(l / r)
      case (Mod, INum(l), Num(r))  => Num(modulo(l, r))
      case (Pow, INum(l), Num(r))  => Num(scala.math.pow(l, r))
      case (UMod, INum(l), Num(r)) => Num(unsigned_modulo(l, r))
      case (Lt, INum(l), Num(r))   => Bool(l < r)
      case (Plus, Num(l), INum(r)) => Num(l + r)
      case (Sub, Num(l), INum(r))  => Num(l - r)
      case (Mul, Num(l), INum(r))  => Num(l * r)
      case (Div, Num(l), INum(r))  => Num(l / r)
      case (Mod, Num(l), INum(r))  => Num(modulo(l, r))
      case (Pow, Num(l), INum(r))  => Num(math.pow(l, r))
      case (UMod, Num(l), INum(r)) => Num(unsigned_modulo(l, r))
      case (Lt, Num(l), INum(r))   => Bool(l < r)

      // string operations
      case (Plus, Str(l), Str(r)) => Str(l + r)
      case (Plus, Str(l), Num(r)) =>
        Str(l + Character.toChars(r.toInt).mkString(""))
      case (Sub, Str(l), INum(r)) => Str(l.dropRight(r.toInt))
      case (Lt, Str(l), Str(r))   => Bool(l < r)

      // long operations
      case (Plus, INum(l), INum(r))    => INum(l + r)
      case (Sub, INum(l), INum(r))     => INum(l - r)
      case (Mul, INum(l), INum(r))     => INum(l * r)
      case (Div, INum(l), INum(r))     => Num(l / r)
      case (Mod, INum(l), INum(r))     => INum(unsigned_modulo(l, r).toLong)
      case (UMod, INum(l), INum(r))    => INum(modulo(l, r).toLong)
      case (Pow, INum(l), INum(r))     => number(math.pow(l, r))
      case (Lt, INum(l), INum(r))      => Bool(l < r)
      case (BAnd, INum(l), INum(r))    => INum(l & r)
      case (BOr, INum(l), INum(r))     => INum(l | r)
      case (BXOr, INum(l), INum(r))    => INum(l ^ r)
      case (LShift, INum(l), INum(r))  => INum((l.toInt << r.toInt).toLong)
      case (SRShift, INum(l), INum(r)) => INum((l.toInt >> r.toInt).toLong)
      case (URShift, INum(l), INum(r)) => INum(((l >>> r) & 0xffffffff).toLong)

      // logical operations
      case (And, Bool(l), Bool(r)) => Bool(l && r)
      case (Or, Bool(l), Bool(r))  => Bool(l || r)
      case (Xor, Bool(l), Bool(r)) => Bool(l ^ r)

      // equality operations
      case (Eq, INum(l), Num(r))     => Bool(!(r equals -0.0) && l == r)
      case (Eq, Num(l), INum(r))     => Bool(!(l equals -0.0) && l == r)
      case (Eq, Num(l), Num(r))      => Bool(l equals r)
      case (Eq, Num(l), BigINum(r))  => Bool(l == r)
      case (Eq, BigINum(l), Num(r))  => Bool(l == r)
      case (Eq, INum(l), BigINum(r)) => Bool(l == r)
      case (Eq, BigINum(l), INum(r)) => Bool(l == r)
      case (Eq, l, r)                => Bool(l == r)

      // double equality operations
      case (Equal, INum(l), Num(r)) => Bool(l == r)
      case (Equal, Num(l), INum(r)) => Bool(l == r)
      case (Equal, Num(l), Num(r))  => Bool(l == r)
      case (Equal, l, r)            => Bool(l == r)

      // double with big integers
      case (Lt, BigINum(l), Num(r)) =>
        Bool(
          new java.math.BigDecimal(l.bigInteger)
            .compareTo(new java.math.BigDecimal(r)) < 0,
        )
      case (Lt, BigINum(l), INum(r)) =>
        Bool(
          new java.math.BigDecimal(l.bigInteger)
            .compareTo(new java.math.BigDecimal(r)) < 0,
        )
      case (Lt, Num(l), BigINum(r)) =>
        Bool(
          new java.math.BigDecimal(l)
            .compareTo(new java.math.BigDecimal(r.bigInteger)) < 0,
        )
      case (Lt, INum(l), BigINum(r)) =>
        Bool(
          new java.math.BigDecimal(l)
            .compareTo(new java.math.BigDecimal(r.bigInteger)) < 0,
        )

      // big integers
      case (Plus, BigINum(l), BigINum(r))    => BigINum(l + r)
      case (LShift, BigINum(l), BigINum(r))  => BigINum(l << r.toInt)
      case (SRShift, BigINum(l), BigINum(r)) => BigINum(l >> r.toInt)
      case (Sub, BigINum(l), BigINum(r))     => BigINum(l - r)
      case (Sub, BigINum(l), INum(r))        => BigINum(l - r)
      case (Mul, BigINum(l), BigINum(r))     => BigINum(l * r)
      case (Div, BigINum(l), BigINum(r))     => BigINum(l / r)
      case (Mod, BigINum(l), BigINum(r))     => BigINum(modulo(l, r))
      case (UMod, BigINum(l), BigINum(r))    => BigINum(unsigned_modulo(l, r))
      case (UMod, BigINum(l), INum(r))       => BigINum(unsigned_modulo(l, r))
      case (Lt, BigINum(l), BigINum(r))      => Bool(l < r)
      case (And, BigINum(l), BigINum(r))     => BigINum(l & r)
      case (Or, BigINum(l), BigINum(r))      => BigINum(l | r)
      case (BXOr, BigINum(l), BigINum(r))    => BigINum(l ^ r)
      case (Pow, BigINum(l), BigINum(r))     => BigINum(l.pow(r.toInt))
      case (Pow, BigINum(l), INum(r))        => BigINum(l.pow(r.toInt))
      case (Pow, BigINum(l), Num(r)) =>
        if (r.toInt < 0) Num(math.pow(l.toDouble, r))
        else BigINum(l.pow(r.toInt))

      case (_, lval, rval) => error(s"wrong type: $lval $bop $rval")
    }
}
