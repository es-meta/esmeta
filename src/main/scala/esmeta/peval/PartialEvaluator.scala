package esmeta.peval

import esmeta.{PEVAL_LOG_DIR, LINE_SEP}
import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.error.*
import esmeta.error.NotSupported.{*, given}
import esmeta.error.NotSupported.Category.{Type => _, *}
import esmeta.interpreter.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.es.*
import esmeta.parser.{ESParser, ESValueParser}
import esmeta.peval.pstate.*
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.BaseUtils.{error => _, *}
import esmeta.util.SystemUtils.*
import esmeta.TEST_MODE
import java.io.PrintWriter
import java.math.MathContext.DECIMAL128
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}
import scala.math.{BigInt => SBigInt}

/** extensible helper of IR interpreter with a CFG */
class PartialEvaluator(
  val cfg: CFG,
  val log: Boolean = false,
  val detail: Boolean = false,
  val logPW: Option[PrintWriter] = None,
  val timeLimit: Option[Int] = None,
) {
  import PartialEvaluator.*

  /** control flow graphs */
  given CFG = cfg

  def peval(ref: Ref, pst: PState): (Predict[RefTarget]) = ref match
    case x: Var => Known(VarTarget(x))
    case Field(ref, expr) =>
      val base = pst(peval(ref, pst))
      val (field, _) = peval(expr, pst)
      (base, field) match
        case (Known(b), Known(f)) => Known(FieldTarget(b, f))
        case _                    => Unknown

  def peval(expr: Expr, pst: PState): (Predict[Value], Expr) =
    val result = expr match
      case ERef(ref) => (pst(peval(ref, pst)), expr)
      case e: LiteralExpr =>
        e match
          case EMath(n)        => (Known(Math(n)), e)
          case EInfinity(pos)  => (Known(Infinity(pos)), e)
          case ENumber(double) => (Known(Number(double)), e)
          case EBigInt(bigInt) => (Known(BigInt(bigInt)), e)
          case EStr(str)       => (Known(Str(str)), e)
          case EBool(b)        => (Known(Bool(b)), e)
          case EUndef()        => (Known(Undef), e)
          case ENull()         => (Known(Null), e)
          case EEnum(name)     => (Known(Enum(name)), e)
          case ECodeUnit(c)    => (Known(CodeUnit(c)), e)
      // case EParse(code, rule)                       => ???
      // case EGrammarSymbol(name, params)             => ???
      // case ESourceText(expr)                        => ???
      // case EYet(msg)                                => ???
      // case EContains(list, expr)                    => ???
      // case ESubstring(expr, from, to)               => ???
      // case ETrim(expr, isStarting)                  => ???
      // case EUnary(uop, expr)                        => ???
      // case EBinary(bop, left, right)                => ???
      // case EVariadic(vop, exprs)                    => ???
      // case EMathOp(mop, args)                       => ???
      // case EConvert(cop, expr)                      => ???
      // case EExists(ref)                             => ???
      // case ETypeOf(base)                            => ???
      // case EInstanceOf(base, target)                => ???
      // case ETypeCheck(base, ty)                     => ???
      // case ESizeOf(base)                            => ???
      // case EClo(fname, captured)                    => ???
      // case ECont(fname)                             => ???
      // case EDebug(expr)                             => ???
      // case ERandom()                                => ???
      // case ESyntactic(name, args, rhsIdx, children) => ???
      // case ELexical(name, expr)                     => ???
      // case ERecord(tname, pairs)                    => ???
      // case EMap(ty, pairs)                          => ???
      // case EList(exprs)                             => ???
      // case ECopy(obj)                               => ???
      // case EKeys(map, intSorted)                    => ???
      case _ => (Unknown, expr)
    logging("expr", s"$expr -> $result")
    result

  def peval(inst: Inst, pst: PState): (Inst, PState) =
    logging("inst", inst.toString(detail = false))
    val newInst = inst match
      case ILet(lhs, expr) =>
        val (pv, _) = peval(expr, pst)
        pst.define(lhs, pv)
        (inst, pst)
      case ISeq(insts) =>
        val (newInsts, newPst) = insts.foldLeft((List[Inst](), pst)) {
          case ((acc, pst), inst) =>
            val (newInst, newPst) = peval(inst, pst)
            (acc :+ newInst, newPst)
        }
        (ISeq(newInsts), newPst)
      case ISdoCall(lhs, base, method, args) =>
        val (pv, newBase) = peval(base, pst)
        pv match
          // TODO: local variable inline: <varname>_<fid>_<ctxtcounter>
          // case Known(AstValue(ast)) =>
          case _ =>
            pst.define(lhs, Unknown)
            (inst, pst)
      case call @ ICall(lhs, fexpr, args) =>
        pst.define(lhs, Unknown)
        (inst, pst)
      case INop() => (ISeq(Nil), pst)
      // case IAssign(ref, expr)             => (inst)
      // case IExpand(base, expr)            => (inst)
      // case IDelete(base, expr)            => (inst)
      // case IPush(elem, list, front)       => (inst)
      // case IPop(lhs, list, front)         => (inst)
      // case IReturn(expr)                  => (inst)
      // case IAssert(expr)                  => (inst)
      // case IPrint(expr)                   => (inst)
      // case IIf(cond, thenInst, elseInst)  => (inst)
      // case IWhile(cond, body)             => (inst)
      // peval(fexpr) match
      // case Known(clo @ Clo(calleeFunc, captured)) -> _ => ???
      //   val vs = args.map(peval).map(_._1)
      //   val newLocals =
      //     getLocals(calleeFunc.irFunc.params, vs, clo) ++ (captured.map((k, v) => (k, Known(v)))) // XXX handle Unknown capture
      //   pst.callStack = ((pst.func, pst.locals) :: pst.callStack)
      //   pst.func = calleeFunc.irFunc
      //   pst.locals = newLocals
      //   call
      // case Known(cont @ Cont(func, captured, callStack)) -> _ => ???
      // case v => ??? // throw NoCallable(v)
      // peval(base).asKnown.asAst match
      //   case syn: Syntactic =>
      //     getSdo((syn, method)) match
      //       case Some((ast0, sdo)) =>
      //         val vs = args.map(peval).map(_._1)
      //         val newLocals = getLocals(
      //           sdo.irFunc.params,
      //           AstValue(ast0) :: vs,
      //           Clo(sdo, Map()),
      //         )
      //         st.callStack = CallContext(st.context, lhs)
      //         st.context = Context(sdo, newLocals)
      //       case None => throw InvalidAstField(syn, Str(method))
      // case lex: Lexical => ???
      // setCallResult(lhs, Interpreter.eval(lex, method))
      case _ => (inst, pst)
    logging("pst", pst)
    newInst

  /** final state */
  def run(inst: Inst, pst: PState): (Inst, PState) = timeout(
    {
      val result @ (newBody, newPst) = peval(inst, pst)
      logging("final", newBody)
      result
    },
    timeLimit,
  )

  /** ECMAScript parser */
  lazy val esParser: ESParser = cfg.esParser

  /** get initial local variables */
  def getLocals(
    params: List[Param],
    args: List[Predict[Value]],
    // caller: Call,
    callee: Callable,
  ): Map[Local, Predict[Value]] = {
    val func = callee.func
    @tailrec
    def aux(
      map: Map[Local, Predict[Value]],
    )(ps: List[Param], as: List[Predict[Value]]): Map[Local, Predict[Value]] =
      (ps, as) match {
        case (Nil, Nil) => map
        case (Param(lhs, ty, optional, _) :: pl, Nil) =>
          if (optional) aux(map)(pl, Nil)
          else throw RemainingParams(ps)
        case (Nil, args) =>
          // XXX Handle GeneratorStart <-> GeneratorResume arith mismatch
          callee match
            case _: Cont => map
            case _       => ??? // throw RemainingArgs(args)
        case (param :: pl, arg :: al) =>
          val newMap = map + (param.lhs -> arg)
          aux(newMap)(pl, al)
      }
    aux(Map.empty[Local, Predict[Value]])(params, args)
  }

  /** transition for references */
  def eval(ref: Ref): RefTarget = ref match
    case x: Var           => VarTarget(x)
    case Field(ref, expr) => ???
  // var base = pst(eval(ref))
  // val f = eval(expr)
  // FieldTarget(base, f)

  def logging(tag: String, data: Any): Unit = if (log)
    pw.println(s"[$tag] $data")
    pw.flush()

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------

  /** type model */
  private def tyModel = cfg.tyModel

  /** grammar */
  private def grammar = cfg.grammar

  /** itereration count */
  private var iter = 0

  /** logging */
  private lazy val pw: PrintWriter =
    logPW.getOrElse(getPrintWriter(s"$PEVAL_LOG_DIR/log"))

  /** cache to get syntax-directed operation (SDO) */
  private val getSdo = cached[(Ast, String), Option[(Ast, Func)]](_.getSdo(_))
}

/** IR PartialEvaluator with a CFG */
object PartialEvaluator {

  /** transition for lexical SDO */
  def eval(lex: Lexical, sdoName: String): Value = {
    import ESValueParser.*
    val Lexical(name, str) = lex
    (name, sdoName) match {
      case (_, "StringValue") if StringValue.of.contains(name) =>
        StringValue.of(name)(str)
      case (_, "NumericValue") if NumericValue.of.contains(name) =>
        NumericValue.of(name)(str)
      case (_, "MV") if MV.of.contains(name) =>
        MV.of(name)(str)
      case (_, "SV") if SV.of.contains(name) =>
        SV.of(name)(str)
      case (_, "TV") if TV.of.contains(name) =>
        TV.of(name)(str)
      case (_, "TRV") if TRV.of.contains(name) =>
        TRV.of(name)(str)
      case ("RegularExpressionLiteral", name) =>
        throw NotSupported(Feature)(List("RegExp"))
      case _ =>
        throw InvalidAstField(lex, Str(sdoName))
    }
  }

  /** transition for unary operators */
  def eval(uop: UOp, operand: Value): Value =
    import UOp.*
    (uop, operand) match
      // mathematic values
      case (Abs, m: Math)   => abs(m)
      case (Floor, m: Math) => floor(m)
      // numeric values
      case (Neg, Number(n)) => Number(-n)
      case (Neg, Math(n))   => Math(-n)
      case (Neg, POS_INF)   => NEG_INF
      case (Neg, NEG_INF)   => POS_INF
      case (Neg, BigInt(b)) => BigInt(-b)
      // boolean
      case (Not, Bool(b)) => Bool(!b)
      // bitwise
      case (BNot, Math(n))   => Math(~(n.toInt))
      case (BNot, Number(n)) => Number(~(n.toInt))
      case (BNot, BigInt(b)) => BigInt(~b)
      case (_, value)        => throw InvalidUnaryOp(uop, value)

  /** transition for binary operators */
  def eval(bop: BOp, left: Value, right: Value): Value =
    import BOp.*
    (bop, left, right) match {
      // double operations
      case (Add, Number(l), Number(r))  => Number(l + r)
      case (Sub, Number(l), Number(r))  => Number(l - r)
      case (Mul, Number(l), Number(r))  => Number(l * r)
      case (Pow, Number(l), Number(r))  => Number(math.pow(l, r))
      case (Div, Number(l), Number(r))  => Number(l / r)
      case (Mod, Number(l), Number(r))  => Number(l % r)
      case (UMod, Number(l), Number(r)) => Number(l %% r)
      case (Lt, Number(l), Number(r)) if (l equals -0.0) && (r equals 0.0) =>
        Bool(true)
      case (Lt, Number(l), Number(r)) => Bool(l < r)

      // mathematical value operations
      case (Add, Math(l), Math(r)) => Math(l + r)
      case (Sub, Math(l), Math(r)) => Math(l - r)
      case (Mul, Math(l), Math(r)) => Math(l * r)
      case (Div, Math(l), Math(r)) =>
        // XXX rounded by DECIMAL128 to handle non-terminating decimal
        // expansion. For example, 1 / 3 = 1.3333...
        Math(l(DECIMAL128) / r(DECIMAL128))
      case (Mod, Math(l), Math(r)) =>
        val m = l % r
        Math(if (m * r) < 0 then r + m else m)
      case (UMod, Math(l), Math(r)) => Math(l %% r)
      case (Pow, Math(l), Math(r)) if r.isValidInt && r >= 0 =>
        Math(l.pow(r.toInt))
      case (Pow, Math(l), Math(r)) => Math(math.pow(l.toDouble, r.toDouble))
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
      case (Add, POS_INF, Math(_))          => POS_INF
      case (Add, Math(_), POS_INF)          => POS_INF
      case (Add, POS_INF, POS_INF)          => POS_INF
      case (Add, NEG_INF, Math(_))          => NEG_INF
      case (Add, Math(_), NEG_INF)          => NEG_INF
      case (Add, NEG_INF, NEG_INF)          => NEG_INF
      case (Sub, POS_INF, Math(_))          => POS_INF
      case (Sub, POS_INF, NEG_INF)          => POS_INF
      case (Sub, Math(_), POS_INF)          => NEG_INF
      case (Sub, NEG_INF, Math(_))          => NEG_INF
      case (Sub, NEG_INF, POS_INF)          => NEG_INF
      case (Sub, Math(_), NEG_INF)          => POS_INF
      case (Mul, POS_INF, Math(r)) if r > 0 => POS_INF
      case (Mul, POS_INF, Math(r)) if r < 0 => NEG_INF
      case (Mul, Math(l), POS_INF) if l > 0 => POS_INF
      case (Mul, Math(l), POS_INF) if l < 0 => NEG_INF
      case (Mul, POS_INF, POS_INF)          => POS_INF
      case (Mul, POS_INF, NEG_INF)          => NEG_INF
      case (Mul, NEG_INF, POS_INF)          => NEG_INF
      case (Mul, NEG_INF, Math(r)) if r > 0 => NEG_INF
      case (Mul, NEG_INF, Math(r)) if r < 0 => POS_INF
      case (Mul, Math(l), NEG_INF) if l > 0 => NEG_INF
      case (Mul, Math(l), NEG_INF) if l < 0 => POS_INF
      case (Mul, NEG_INF, NEG_INF)          => POS_INF
      case (Lt, POS_INF, Math(_))           => Bool(false)
      case (Lt, Math(_), POS_INF)           => Bool(true)
      case (Lt, NEG_INF, Math(_))           => Bool(true)
      case (Lt, Math(_), NEG_INF)           => Bool(false)
      case (Lt, NEG_INF, POS_INF)           => Bool(true)
      case (Lt, POS_INF, NEG_INF)           => Bool(false)

      // logical operations
      case (And, Bool(l), Bool(r)) => Bool(l && r)
      case (Or, Bool(l), Bool(r))  => Bool(l || r)
      case (Xor, Bool(l), Bool(r)) => Bool(l ^ r)

      // equality operations
      case (Eq, Number(l), Number(r))     => Bool(l equals r)
      case (Eq, AstValue(l), AstValue(r)) => Bool(l eq r)
      case (Eq, l, r)                     => Bool(l == r)

      // numeric equality operations
      case (Equal, Math(l), Math(r))         => Bool(l == r)
      case (Equal, Infinity(l), Infinity(r)) => Bool(l == r)
      case (Equal, Number(l), Number(r))     => Bool(l == r)
      case (Equal, BigInt(l), BigInt(r))     => Bool(l == r)
      case (Equal, POS_INF, Math(_))         => Bool(false)
      case (Equal, Math(_), POS_INF)         => Bool(false)
      case (Equal, NEG_INF, Math(_))         => Bool(false)
      case (Equal, Math(_), NEG_INF)         => Bool(false)

      // big integers
      case (Add, BigInt(l), BigInt(r))     => BigInt(l + r)
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

      case (_, lval, rval) => throw InvalidBinaryOp(bop, lval, rval)
    }

  /** transition for variadic operators */
  def eval(vop: VOp, vs: List[Value]): Value =
    import VOp.*
    if (vs.isEmpty) throw InvalidVariadicOp(vop)
    vop match
      case Min =>
        if (vs.contains(NEG_INF)) NEG_INF
        else {
          val filtered = vs.filter(_ != POS_INF)
          if (filtered.isEmpty) POS_INF
          else vopEval(_.asMath, _ min _, Math(_), filtered)
        }
      case Max =>
        if (vs.contains(POS_INF)) POS_INF
        else {
          val filtered = vs.filter(_ != NEG_INF)
          if (filtered.isEmpty) NEG_INF
          else vopEval(_.asMath, _ min _, Math(_), filtered)
        }
        vopEval(_.asMath, _ max _, Math(_), vs)
      case Concat =>
        def toString(v: Value): String = v match
          case Str(s)      => s
          case CodeUnit(c) => c.toString
          case v           => throw NoString(v)
        vopEval(toString, _ + _, Str(_), vs)

  /** transition for mathematical operators */
  def eval(mop: MOp, vs: List[Value]): Value =
    import math.*
    (mop, vs) match
      case (MOp.Expm1, List(Math(x))) => Math(expm1(x.toDouble))
      case (MOp.Log10, List(Math(x))) => Math(log10(x.toDouble))
      case (MOp.Log2, List(Math(x)))  => Math(log(x.toDouble) / log(2))
      case (MOp.Cos, List(Math(x)))   => Math(cos(x.toDouble))
      case (MOp.Cbrt, List(Math(x)))  => Math(cbrt(x.toDouble))
      case (MOp.Exp, List(Math(x)))   => Math(exp(x.toDouble))
      case (MOp.Cosh, List(Math(x)))  => Math(cosh(x.toDouble))
      case (MOp.Sinh, List(Math(x)))  => Math(sinh(x.toDouble))
      case (MOp.Tanh, List(Math(x)))  => Math(tanh(x.toDouble))
      case (MOp.Acos, List(Math(x)))  => Math(acos(x.toDouble))
      case (MOp.Acosh, List(Math(x))) =>
        throw NotSupported(Metalanguage)("acosh")
      case (MOp.Asinh, List(Math(x))) =>
        throw NotSupported(Metalanguage)("asinh")
      case (MOp.Atanh, List(Math(x))) =>
        throw NotSupported(Metalanguage)("atanh")
      case (MOp.Asin, List(Math(x))) => Math(asin(x.toDouble))
      case (MOp.Atan2, List(Math(x), Math(y))) =>
        Math(atan2(x.toDouble, y.toDouble))
      case (MOp.Atan, List(Math(x)))  => Math(atan(x.toDouble))
      case (MOp.Log1p, List(Math(x))) => Math(log1p(x.toDouble))
      case (MOp.Log, List(Math(x)))   => Math(log(x.toDouble))
      case (MOp.Sin, List(Math(x)))   => Math(sin(x.toDouble))
      case (MOp.Sqrt, List(Math(x)))  => Math(sqrt(x.toDouble))
      case (MOp.Tan, List(Math(x)))   => Math(tan(x.toDouble))
      case _                          => throw InvalidMathOp(mop, vs)

  /** the absolute value operation for mathematical values */
  def abs(m: Math): Math = Math(m.decimal.abs)

  /** the floor operation for mathematical values */
  def floor(m: Math): Math =
    val Math(d) = m
    if (d.isWhole) m
    else Math(d - (d % 1) - (if (d < 0) 1 else 0))

  /** helpers for make transition for variadic operators */
  def vopEval[T](
    f: Value => T,
    op: (T, T) => T,
    g: T => Value,
    vs: List[Value],
  ) = g(vs.map(f).reduce(op))
}
