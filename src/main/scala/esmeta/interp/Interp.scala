package esmeta.interp

import esmeta.cfg.*
import esmeta.error.*
import esmeta.interp.util.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.js.*
import esmeta.js.util.{Parser => JSParser, ESValueParser}
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{TIMEOUT, TEST_MODE, LOG}
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}
import scala.math.{BigInt => SBigInt}

/** CFG Interpreter */
class Interp(
  val st: State,
  val checkAfter: List[NormalInst],
) {
  import Interp.*

  /** JavaScript parser */
  lazy val jsParser: JSParser = cfg.jsParser

  /** special class for handle return */
  private case class ReturnValue(value: Value) extends Throwable

  /** control flow graphs */
  private given cfg: CFG = st.cfg

  /** type model */
  private def typeModel = cfg.typeModel

  /** grammar */
  private def grammar = cfg.grammar

  /** itereration count */
  private var iter = 0

  /** step */
  def step: Boolean =
    try {
      if (LOG) st.context.cursor match
        case NodeCursor(node) =>
          val func = cfg.funcOf(node)
          val irFunc = func.irFunc
          println(
            s"[${irFunc.kind}${irFunc.name}] ${node.toString(location = true)}",
          )
        case ExitCursor(func) =>
          val irFunc = func.irFunc
          println(s"[${irFunc.kind}${irFunc.name}] Exited")

      // garbage collection
      iter += 1
      if (iter % 100000 == 0) GC(st)

      interp(st.context.cursor)
    } catch case ReturnValue(value) => { setReturn(value); true }

  /** fixpoint */
  @tailrec
  final def fixpoint: State = if (step) fixpoint else st

  /** transition for cursors */
  def interp(cursor: Cursor): Boolean = cursor match {
    case NodeCursor(node) => interp(node); true
    case ExitCursor(func) =>
      st.callStack match
        case Nil =>
          st.context.retVal
            .map(v => st.globals += GLOBAL_RESULT -> v.wrapCompletion)
          for (assert <- checkAfter) interp(assert)
          false
        case CallContext(retId, ctxt) :: rest =>
          // XXX RequireInternalSlot has no explicit return step
          // It is fixed in the recent specification
          val value = st.context.retVal.getOrElse(Undef)
          // val value = st.context.retVal.getOrElse(throw NoReturnValue)
          (value, setTypeMap.get(st.context.name)) match
            case (addr: Addr, Some(tname)) =>
              st.setType(addr, tname)
            case _ =>
          st.context = ctxt
          st.callStack = rest
          st.define(retId, value.wrapCompletion)
          true
  }

  /** transition for nodes */
  def interp(node: Node): Unit =
    def moveTo(nodeOpt: Option[Node]): Unit = st.context.cursor =
      nodeOpt.fold(ExitCursor(cfg.funcOf(node)))(NodeCursor(_))
    node match {
      case Block(_, insts, next) =>
        for (inst <- insts) interp(inst); moveTo(next)
      case Branch(_, _, cond, thenNode, elseNode) =>
        moveTo(interp(cond).escaped match {
          case Bool(true)  => thenNode
          case Bool(false) => elseNode
          case v           => throw NoBoolean(cond, v)
        })
      case Call(_, lhs, fexpr, args, next) =>
        moveTo(next)
        call(lhs, fexpr, args)
    }

  /** transition for normal instructions */
  def interp(inst: NormalInst): Unit = inst match {
    case IExpr(expr)        => interp(expr)
    case ILet(lhs, expr)    => st.context.locals += lhs -> interp(expr)
    case IAssign(ref, expr) => st.update(interp(ref), interp(expr))
    case IDelete(ref)       => st.delete(interp(ref))
    case IPush(from, to, front) =>
      interp(to).escaped match {
        case (addr: Addr) =>
          if (front) st.prepend(addr, interp(from).escaped)
          else st.append(addr, interp(from).escaped)
        case v => throw NoAddr(to, v)
      }
    case IRemoveElem(list, elem) =>
      interp(list).escaped match
        case (addr: Addr) => st.remove(addr, interp(elem).escaped)
        case v            => throw NoAddr(list, v)
    case IReturn(expr) => throw ReturnValue(interp(expr))
    case IAssert(expr) =>
      interp(expr).escaped match {
        case Bool(true) =>
        case v          => throw AssertionFail(expr)
      }
    case IPrint(expr) => {
      val v = interp(expr)
      println(st.getString(v))
    }
    case INop() => /* do nothing */
  }

  /** transition for calls */
  def call(
    lhs: Id,
    fexpr: Expr,
    args: List[Expr],
  ): Unit = try {
    interp(fexpr).escaped match {
      case Clo(func, captured) =>
        val vs = args.map(interp)
        val newLocals = getLocals(func.irFunc.params, vs) ++ captured
        st.callStack ::= CallContext(lhs, st.context)
        st.context = Context(func, newLocals)
      case Cont(func, captured, callStack) => {
        // wrap completion for return to resumed context
        val vs = args.map(interp).map(_.wrapCompletion)
        val newLocals =
          getLocals(func.irFunc.params, vs, cont = true) ++ captured
        st.callStack = callStack.map(_.copied)
        st.context = Context(func, newLocals)
      }
      case v => throw NoFunc(fexpr, v)
    }
  } catch {
    case st.SyntacticCalled(ast, sdo) =>
      val vs = args.map(interp) match
        case h :: tail => AstValue(ast) :: tail // fix this param
        case _         => error("invalid SDO call")
      st.callStack ::= CallContext(lhs, st.context)
      st.context = Context(sdo, getLocals(sdo.irFunc.params, vs))
    case st.LexicalCalled(v) =>
      st.define(lhs, v.wrapCompletion)
  }

  /** transition for expresssions */
  def interp(expr: Expr): Value = expr match {
    case EComp(tyExpr, valExpr, tgtExpr) =>
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
    case EParse(code, rule) =>
      val (str, args) = interp(code).escaped match
        case Str(s) => (s, List())
        case AstValue(syn: Syntactic) =>
          (syn.toString(grammar = Some(grammar)), syn.args)
        case AstValue(lex: Lexical) => (lex.str, List())
        case v                      => throw InvalidParseSource(code, v)
      try {
        (str, interp(rule).escaped, st.sourceText, st.cachedAst) match
          // optimize the initial parsing using the given cached AST
          case (x, Grammar("Script", Nil), Some(y), Some(ast)) if x == y =>
            AstValue(ast)
          case (x, Grammar(name, params), _, _) =>
            AstValue(
              jsParser(name, if (params.isEmpty) args else params).from(x),
            )
          case (_, r, _, _) => throw NoGrammar(rule, r)
      } catch {
        case _: Throwable => st.allocList(Nil) // NOTE: throw a List of errors
      }
    case EGrammar(name, params) => Grammar(name, params)
    case ESourceText(expr) =>
      val ast = interp(expr).escaped.asAst
      // XXX fix last space in js stringifier
      Str(ast.toString(grammar = Some(grammar)).trim)
    case EGetChildren(kind, ast) =>
      val k = interp(kind).escaped match
        case Grammar(name, _) => name
        case v                => throw NoGrammar(kind, v)
      val a = interp(ast).escaped.asAst
      st.allocList(a.getChildren(k).map(AstValue(_)))
    case EYet(msg) =>
      throw NotSupported(msg)
    case EContains(list, elem) =>
      val l = interp(list).escaped.getList(list, st)
      Bool(l.values contains interp(elem).escaped)
    case ESubstring(expr, from, to) =>
      val s = interp(expr).escaped.asStr
      val f = interp(from).escaped.asInt
      val t = interp(to).escaped.asInt
      Str(s.substring(f, t))
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
    case EVariadic(vop, exprs) =>
      val vs = for (e <- exprs) yield interp(e).escaped
      Interp.interp(vop, vs)
    case EConvert(cop, expr) =>
      import COp.*
      (interp(expr).escaped, cop) match {
        case (Math(n), ToNumber) => Number(n.toDouble)
        case (Math(n), ToBigInt) => BigInt(n.toBigInt)
        case (Str(s), ToNumber)  => Number(ESValueParser.str2Number(s))
        case (Str(s), ToBigInt) => ESValueParser.str2bigint(s)
        case (POS_INF, ToMath) => POS_INF
        case (NEG_INF, ToMath) => NEG_INF
        case (Number(d), ToMath) => Math(BigDecimal.exact(d))
        case (CodeUnit(c), ToMath) => Math(BigDecimal.exact(c.toInt))
        case (BigInt(n), ToMath)   => Math(BigDecimal.exact(n))
        case (Number(d), ToStr(radixOpt)) =>
          val radix = radixOpt.fold(10)(e => interp(e).escaped.asInt)
          Str(toStringHelper(d, radix))
        // TODO other cases
        case (v, cop) => throw InvalidConversion(cop, expr, v)
      }
    case ETypeOf(base) =>
      Str(interp(base).escaped match
        case n: Number => "Number"
        case b: BigInt => "BigInt"
        case s: Str    => "String"
        case b: Bool   => "Boolean"
        case Undef     => "Undefined"
        case Null      => "Null"
        case addr: Addr =>
          st(addr) match
            case m: MapObj if typeModel.subType(m.ty, "Object") => "Object"
            case _: ListObj                                     => "List"
            case _: SymbolObj                                   => "Symbol"
            case v                                              => ???
        case v => ???,
      )
    case ETypeCheck(expr, ty) =>
      val v = interp(expr)
      if (v.isAbruptCompletion) Bool(false)
      else
        Bool(v.escaped match
          case _: Number => ty.name == "Number"
          case _: BigInt => ty.name == "BigInt"
          case _: Str    => ty.name == "String"
          case _: Bool   => ty.name == "Boolean"
          case _: Const  => ty.name == "Constant"
          case Undef     => ty.name == "Undefined"
          case Null      => ty.name == "Null"
          case AstValue(ast) =>
            ty.name == "ParseNode" || (ast.types contains ty.name)
          case _: Clo => ty.name == "AbstractClosure"
          case addr: Addr =>
            st(addr) match
              case m: MapObj    => typeModel.subType(m.ty, ty.name)
              case _: ListObj   => ty.name == "List"
              case _: SymbolObj => ty.name == "Symbol"
              case _            => ???
          case v => ???,
        )
    case EClo(fname, captured) =>
      val func = cfg.fnameMap.getOrElse(fname, error("invalid function name"))
      Clo(func, Map.from(captured.map(x => x -> st(x))))
    case ECont(fname) =>
      val func = cfg.fnameMap.getOrElse(fname, error("invalid function name"))
      val captured = st.context.locals.collect { case (x: Name, v) => x -> v }
      Cont(func, Map.from(captured), st.callStack)
    case ESyntactic(name, args, rhsIdx, children) =>
      val asts = children.map(childOpt =>
        childOpt.map(child =>
          interp(child) match {
            case AstValue(ast) => ast
            case v             => throw NoAst(child, v)
          },
        ),
      )
      AstValue(Syntactic(name, args, rhsIdx, asts))
    case ELexical(name, expr) =>
      val str = interp(expr).escaped.asStr
      AstValue(Lexical(name, str))
    case EMap(Type("Completion"), props) =>
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
    case EMap(ty, props) =>
      val addr = st.allocMap(ty)
      for ((kexpr, vexpr) <- props)
        val k = interp(kexpr).escaped
        val v = interp(vexpr)
        st.update(addr, k, v)
      addr
    case EList(exprs) =>
      st.allocList(exprs.map(expr => interp(expr).escaped))
    case EListConcat(exprs) =>
      val ls = exprs.map(e => interp(e).escaped.getList(e, st).values).flatten
      st.allocList(ls)
    case ESymbol(desc) =>
      interp(desc) match
        case (str: Str) => st.allocSymbol(str)
        case Undef      => st.allocSymbol(Undef)
        case v          => throw NoString(desc, v)
    case ECopy(obj) =>
      interp(obj).escaped match
        case addr: Addr => st.copyObj(addr)
        case v          => throw NoAddr(obj, v)
    case EKeys(map, intSorted) =>
      interp(map).escaped match
        case addr: Addr => st.keys(addr, intSorted)
        case v          => throw NoAddr(map, v)
    case EDuplicated(expr) =>
      val vs = interp(expr).escaped.getList(expr, st).values
      Bool(vs.toSet.size != vs.length)
    case EIsArrayIndex(expr) =>
      interp(expr).escaped match
        case Str(s) =>
          val d = ESValueParser.str2Number(s)
          val ds = toStringHelper(d)
          val UPPER = (1L << 32) - 1
          val l = d.toLong
          Bool(ds == s && 0 <= l && d == l && l < UPPER)
        case _ => Bool(false)
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
    case ECodeUnit(c)          => CodeUnit(c)
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
  import IRFunc.Param
  def getLocals(
    params: List[Param],
    args: List[Value],
    cont: Boolean = false,
  ): MMap[Local, Value] = {
    val map = MMap[Local, Value]()
    @tailrec
    def aux(ps: List[Param], as: List[Value]): Unit = (ps, as) match {
      case (Nil, Nil) =>
      case (Param(lhs, optional, _) :: pl, Nil) =>
        if (optional) {
          map += lhs -> Absent
          aux(pl, Nil)
        } else RemainingParams(ps)
      case (Nil, args) =>
        // XXX Handle GeneratorStart <-> GeneratorResume arith mismatch
        if (!cont) throw RemainingArgs(args)
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

  /** set return value and move to the exit node */
  def setReturn(value: Value): Unit =
    st.context.retVal = Some(value)
    st.context.cursor = ExitCursor(st.func)
}

/** Interp object */
object Interp {

  /** run interp */
  def apply(
    st: State,
    checkAfter: List[NormalInst] = Nil,
    timeLimit: Option[Long] = Some(TIMEOUT),
  ): State = timeout(new Interp(st, checkAfter).fixpoint, timeLimit)

  // type update algorithms
  val setTypeMap: Map[String, String] = Map(
    "OrdinaryFunctionCreate" -> "ECMAScriptFunctionObject",
    "ArrayCreate" -> "ArrayExoticObject",
  )

  /** transition for unary opeartors */
  def interp(uop: UOp, operand: Value): Value =
    import UOp.*
    (uop, operand) match
      // mathematic values
      case (Abs, Math(n))                    => Math(n.abs)
      case (Floor, Math(n)) if n.isValidLong => Math(n)
      case (Floor, Math(n)) => Math(n - (n % 1) - (if (n < 0) 1 else 0))
      // numeric values
      case (Neg, Number(n)) => Number(-n)
      case (Neg, Math(n))   => Math(-n)
      case (Neg, BigInt(b)) => BigInt(-b)
      // boolean
      case (Not, Bool(b)) => Bool(!b)
      // bitwise
      case (BNot, Math(n))   => Math(~(n.toInt))
      case (BNot, Number(n)) => Number(~(n.toInt))
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

      // mathematical value operations
      case (Plus, Math(l), Math(r)) => Math(l + r)
      case (Sub, Math(l), Math(r))  => Math(l - r)
      case (Mul, Math(l), Math(r))  => Math(l * r)
      case (Div, Math(l), Math(r))  => Math(l / r)
      case (Mod, Math(l), Math(r)) =>
        val m = l % r
        Math(if (m * r) < 0 then r + m else m)
      case (UMod, Math(l), Math(r)) => Math(l %% r)
      case (Pow, Math(l), Math(r)) =>
        Math(math.pow(l.toDouble, r.toDouble))
      // TODO consider 2's complement 32-bit strings
      case (BAnd, Math(l), Math(r)) => Math(l.toLong & r.toLong)
      case (BOr, Math(l), Math(r))  => Math(l.toLong | r.toLong)
      case (BXOr, Math(l), Math(r)) => Math(l.toLong ^ r.toLong)
      case (LShift, Math(l), Math(r)) =>
        Math((l.toInt << r.toInt).toLong)
      case (SRShift, Math(l), Math(r)) =>
        Math((l.toInt >> r.toInt).toLong)
      case (URShift, Math(l), Math(r)) =>
        Math((l.toLong << 32) >>> (32 + (r.toLong % 32)))
      case (Lt, Math(l), Math(r)) => Bool(l < r)

      // extended mathematical value operations
      case (Lt, POS_INF, Math(r)) => Bool(false)
      case (Lt, Math(r), POS_INF) => Bool(true)
      case (Lt, NEG_INF, Math(r)) => Bool(true)
      case (Lt, Math(r), NEG_INF) => Bool(false)

      // logical operations
      case (And, Bool(l), Bool(r)) => Bool(l && r)
      case (Or, Bool(l), Bool(r))  => Bool(l || r)
      case (Xor, Bool(l), Bool(r)) => Bool(l ^ r)

      // equality operations
      case (Eq, Number(l), Number(r))     => Bool(l equals r)
      case (Eq, AstValue(l), AstValue(r)) => Bool(l eq r)
      case (Eq, l, r)                     => Bool(l == r)

      // numeric equality operations
      case (Equal, Math(l), Math(r))     => Bool(l == r)
      case (Equal, Number(l), Number(r)) => Bool(l == r)
      case (Equal, BigInt(l), BigInt(r)) => Bool(l == r)

      // big integers
      case (Plus, BigInt(l), BigInt(r))    => BigInt(l + r)
      case (LShift, BigInt(l), BigInt(r))  => BigInt(l << r.toInt)
      case (SRShift, BigInt(l), BigInt(r)) => BigInt(l >> r.toInt)
      case (Sub, BigInt(l), BigInt(r))     => BigInt(l - r)
      case (Mul, BigInt(l), BigInt(r))     => BigInt(l * r)
      case (Div, BigInt(l), BigInt(r))     => BigInt(l / r)
      case (Mod, BigInt(l), BigInt(r))     => BigInt(l % r)
      case (UMod, BigInt(l), BigInt(r))    => BigInt(l %% r)
      case (Lt, BigInt(l), BigInt(r))      => Bool(l < r)
      case (BAnd, BigInt(l), BigInt(r))    => BigInt(l & r)
      case (BOr, BigInt(l), BigInt(r))     => BigInt(l | r)
      case (BXOr, BigInt(l), BigInt(r))    => BigInt(l ^ r)
      case (Pow, BigInt(l), BigInt(r))     => BigInt(l.pow(r.toInt))

      case (_, lval, rval) => error(s"wrong type: $lval $bop $rval")
    }

  /** transition for variadic operators */
  def interp(vop: VOp, vs: List[PureValue]): PureValue =
    import VOp.*
    if (vs.isEmpty) error(s"no arguments for: $vop")
    vop match
      case Min =>
        if (vs.contains(NEG_INF)) NEG_INF
        else {
          val filtered = vs.filter(_ != POS_INF)
          if (filtered.isEmpty) POS_INF
          else vopInterp(_.asMath, _ min _, Math(_), filtered)
        }
      case Max =>
        if (vs.contains(POS_INF)) POS_INF
        else {
          val filtered = vs.filter(_ != NEG_INF)
          if (filtered.isEmpty) NEG_INF
          else vopInterp(_.asMath, _ min _, Math(_), filtered)
        }
        vopInterp(_.asMath, _ max _, Math(_), vs)
      case Concat => vopInterp(_.asStr, _ + _, Str(_), vs)

  /** helpers for make transition for variadic operators */
  private def vopInterp[T](
    f: PureValue => T,
    op: (T, T) => T,
    g: T => PureValue,
    vs: List[PureValue],
  ) = g(vs.map(f).reduce(op))
}
