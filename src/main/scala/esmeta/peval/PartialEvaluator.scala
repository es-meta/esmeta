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
import esmeta.peval.util.*
import esmeta.state.{BigInt, *}
import esmeta.ty.*
import esmeta.util.BaseUtils.{error => _, *}
import esmeta.util.SystemUtils.*
import esmeta.TEST_MODE
import java.io.PrintWriter
import java.math.MathContext.DECIMAL128
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}
import scala.math.{BigInt => SBigInt}

import scala.util.{Try}

/** extensible helper of IR interpreter with a CFG */
class PartialEvaluator(
  val cfg: CFG,
  val log: Boolean = false,
  val detail: Boolean = false,
  val simplifyLevel: Int = 1,
  val logPW: Option[PrintWriter] = None,
  val timeLimit: Option[Int] = None,
  val renamer: Renamer,
  // NOTE : renamer should NOT be copied when copying PState - renamer is, specializer-level global state.
) {
  import PartialEvaluator.*

  /** control flow graphs */
  given CFG = cfg

  def peval(ref: Ref, pst: PState): (Predict[RefTarget], Ref) = ref match

    case x: Var =>
      val newVar = renamer.get(x, pst.context)
      (Known(VarTarget(newVar)), newVar)
    case Field(ref, expr) =>
      val (base, newBase) =
        val (tgt, newRef) = peval(ref, pst);
        (pst(tgt), newRef)
      val (field, newField) = peval(expr, pst)
      val tgt = (base, field) match
        case (Known(b), Known(f)) => Known(FieldTarget(b, f))
        case _                    => (Unknown)
      (tgt, Field(newBase, newField))

  def peval(expr: Expr, pst: PState): (Predict[Value], Expr) =
    val result = expr match
      case ERef(ref) =>
        val (tgt, newRef) = peval(ref, pst);
        pst(tgt) match
          case Known(v) if v.isPrintable => (pst(tgt), v.toExpr)
          case _                         => (pst(tgt), ERef(newRef))
      // TODO : how to properly handle control flow?

      // case EClo(fname, captured) =>
      // val func = cfg.getFunc(fname)
      // (PClo(func, captured.map(x => x -> pst(x)).toMap), expr)

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
      case EList(exprs) =>
        val vs = exprs.map(expr => peval(expr, pst))
        val addr = renamer.newAddr
        pst.allocList(addr, vs.map(_._1).toVector)
        (Known(addr), EList(vs.map(_._2)))
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
    logging(
      s"inst @ ${pst.context.func.name} = cs[${pst.callStack.size}]",
      inst.toString(detail = false),
    )
    val (newInst, newPst) = inst match
      case ILet(lhs, expr) =>
        val newLhs = renamer.get(lhs, pst.context)
        val (pv, newExpr) = peval(expr, pst)
        pst.define(newLhs, pv)
        (ILet(newLhs, newExpr), pst)
      case ISeq(insts) =>
        val (newInsts, newPst) = insts.foldLeft((List[Inst](), pst)) {
          case ((acc, pst), inst) =>
            val (newInst, newPst) = peval(inst, pst)
            (acc :+ newInst, newPst)
        }
        (ISeq(newInsts), newPst)

      case ISdoCall(lhs, base, method, args) =>
        logCallStack(
          s"ISdoCall : ${pst.callStack.size}",
          s"Found ISdoCall, inst is =${ISdoCall(lhs, base, method, args)}...",
        )
        logCallStack(
          s"ISdoCall : ${pst.callStack.size}",
          s"let's evaluate base=${base}...",
        )
        logCallStack(
          s"ISdoCall : ${pst.callStack.size}",
          s"just before that.. this is ctxt.locals=${pst.context.locals}, and ctxt.sensitivity=${pst.context.sensitivity}",
        )
        val newCallCount = renamer.newCallCount
        val newLhs = renamer.get(lhs, pst.context)
        val (pv, newBase) = peval(base, pst)
        pv match
          case Unknown =>
            logCallStack(
              s"ISdoCall : ${pst.callStack.size}",
              s"(pv ~> ???) : UNKNOWN CALL",
            )
            // we can skip evaluating args
            pst.define(newLhs, Unknown)
            (ISdoCall(newLhs, newBase, method, /* unused */ args), pst)
          // TODO: local variable inline: <varname>_<fid>_<ctxtcounter>
          // case Known(AstValue(ast)) =>
          case Known(value) =>
            logCallStack(
              s"ISdoCall : ${pst.callStack.size}",
              s"(pv ~> ${value})",
            )
            value.asAst match {
              case syn: Syntactic =>
                logCallStack(
                  s"ISdoCall : ${pst.callStack.size}",
                  s"(pv ~> ${value}) : syntactic",
                )
                getSdo((syn, method)) match
                  case None => throw InvalidAstField(syn, Str(method))
                  case Some((ast0, sdo)) => {
                    val vs = args.map((e) => peval(e, pst))
                    val newContext = PContext(
                      func = sdo.irFunc,
                      locals = MMap.empty,
                      sensitivity = newCallCount,
                      ret = None,
                      pathCondition = Nil,
                      // empty path condition for 'new' function context
                    );
                    val allocations = setLocals(
                      at = newContext,
                      params = sdo.irFunc.params.map(p =>
                        Param(
                          renamer.get(p.lhs, newContext),
                          p.ty,
                          p.optional,
                          p.specParam,
                        ),
                      ),
                      // TODO : There is no way to print ast0 as expression this should be removed somehow
                      /* Ad-hoc fix */
                      args = Known(AstValue(ast0)) -> ERef(Temp(-1)) :: vs,
                      func = sdo,
                      isCont = false,
                    )

                    pst.callStack ::= PCallContext(pst.context, newLhs)
                    pst.context = newContext
                    logCallStack("call:sdo - this", AstValue(ast0))
                    logCallStack("call:sdo - func", sdo.irFunc.name)
                    logCallStack("call:sdo - (new) context", newContext)
                    val (body, newPst) = peval(sdo.irFunc.body, pst)
                    newPst.callStack match
                      case Nil => /* this branch should never happen */ ???
                      case (PCallContext(callerContext, _)) :: rest =>
                        val calleeContext = newPst.context
                        newPst.callStack = rest
                        newPst.context = callerContext
                        pst.define(
                          newLhs,
                          calleeContext.ret.getOrElse(throw NoReturnValue),
                        )
                        (ISeq(List(allocations, body)), newPst)
                  }
              case lex: Lexical =>
                logCallStack(
                  s"ISdoCall : ${pst.callStack.size}",
                  s"(pv ~> ${value}) : lexical",
                )
                val v = PartialEvaluator.eval(lex, method);
                pst.define(newLhs, Known(v))
                (IAssign(newLhs, v.toExprOpt.getOrElse(???)), pst)
            }
      case call @ ICall(lhs, fexpr, args) =>
        logCallStack(s"ICall : ${pst.callStack.size}", s"${call}")
        val newLhs = renamer.get(lhs, pst.context)
        val (f, newFexpr) = peval(fexpr, pst)
        f match
          case Known(value) => // TODO
          case Unknown      => // TODO
        pst.define(newLhs, Unknown)
        (ICall(newLhs, newFexpr, args), pst)

      case INop() => (ISeq(Nil), pst)
      case IAssign(ref, expr) =>
        ref match
          case x: Var =>
            val newVar = renamer.get(x, pst.context);
            val (pv, newExpr) = peval(expr, pst)
            pst.update(newVar, pv)
            (IAssign(newVar, newExpr), pst)
          case Field(_, _) =>
            (inst, pst) // TODO

      case IPop(lhs, list, front) =>
        val newLhs = renamer.get(lhs, pst.context);
        val (pv, newListExpr) = peval(list, pst);
        pv match
          case Known(_) => ??? // TODO : modify heap
          case Unknown  => ??? // TODO : killall heap (일단 지금은 없음)
        pst.define(newLhs, Unknown)
        (IPop(newLhs, newListExpr, front), pst)

      case IReturn(expr) =>
        logging("tring return, pst is : ", pst)
        val (pv, newExpr) = peval(expr, pst)
        pst.callStack match
          case head :: _ =>
            pst.context.ret = Some(pv)
            (IAssign(head.retId, newExpr), pst)
          case Nil =>
            pst.context.ret = Some(pv)
            (IReturn(newExpr), pst)

      // case IIf(cond, thenInst, elseInst) =>
      // val (pv, newCond) = peval(cond, pst)
      // pv match
      //   case Known(Bool(true))  => peval(thenInst, pst)
      //   case Known(Bool(false)) => peval(elseInst, pst)
      //   case Known(value)       => throw NoBoolean(value)
      //   case Unknown => {
      //     ???
      //     /*
      //     val (thenPst, elsePst) = (pst.copied, pst.copied)

      //     thenPst.context.pathCondition ::= newCond
      //     elsePst.context.pathCondition ::= EUnary(UOp.Not, newCond);

      //     val (newThen, newThenPst) = peval(thenInst, thenPst)
      //     val (newElse, newElsePst) = peval(elseInst, elsePst)
      //     (newThenPst.context.ret, newElsePst.context.ret) match
      //       case (None, None) =>
      //         (
      //           IIf(newCond, newThen, newElse),
      //           newThenPst.join(elsePst),
      //         ) /* TODO : Join */
      //       case (_, _) =>
      //         /* Handle Return */
      //         ???
      //      */
      //   }
      // case IExpand(base, expr)            => (inst)
      // case IDelete(base, expr)            => (inst)
      // case IPush(elem, list, front)       => (inst)
      // case IAssert(expr)                  => (inst)
      // case IPrint(expr)                   => (inst)
      // case IWhile(cond, body)             => (inst)

      case _ => (inst, pst)
    logging("pst", pst)
    logging(
      s"inst @ ${pst.context.func.name} = cs[${pst.callStack.size}]",
      s"${inst.toString(detail = false)} -> ${newInst.toString(detail = false)}\n",
    )
    (newInst, newPst)

  /** final state */
  def run(func: IRFunc, pst: PState): (Inst, PState) = timeout(
    {
      val inst = func.body
      val result @ (newBody, newPst) = peval(inst, pst)

      if (log) then
        val writer = getPrintWriter(s"$PEVAL_LOG_DIR/result.ir")
        writer.print(
          IRFunc(
            func.main,
            func.kind,
            s"${func.name}PEvaled",
            func.params,
            func.retTy,
            simplifyLevel match
              case 0 => newBody
              case 1 => InstFlattener(newBody)
              case _ => InstFlattener(newBody) // TODO : add usedef
            ,
            func.algo,
          ),
        );
        writer.flush();
      result
    },
    timeLimit,
  )

  /** ECMAScript parser */
  lazy val esParser: ESParser = cfg.esParser

  /** get initial local variables */
  def setLocals(
    at: PContext,
    params: List[Param],
    args: List[(Predict[Value], Expr)],
    func: Func,
    isCont: Boolean,
  ): Inst = {
    val map = at.locals;
    @tailrec
    def aux(
      evalArgs: List[Inst],
    )(ps: List[Param], as: List[(Predict[Value], Expr)]): List[Inst] =
      (ps, as) match {
        case (Nil, Nil) => (evalArgs)
        case (Param(lhs, ty, optional, _) :: pl, Nil) =>
          if (optional) aux(evalArgs)(pl, Nil)
          else throw RemainingParams(ps)
        case (Nil, args) =>
          // XXX Handle GeneratorStart <-> GeneratorResume arith mismatch
          if (isCont) then (evalArgs) else ??? // throw RemainingArgs(args)
        case (param :: pl, (arg, argExpr) :: al) =>
          map += (param.lhs -> arg)
          aux(
            IAssign(param.lhs, argExpr)
              .addComment("NOTE : should be removed") :: evalArgs,
          )(
            pl,
            al,
          )
      }
    // reverse needed to keep order
    ISeq(aux(Nil)(params, args).reverse.toList)
  }

  def buildLogger = (writer: PrintWriter) =>
    (tag: String, data: Any) =>
      if (log)
        writer.println(s"[$tag] $data")
        writer.flush()

  lazy val logging = buildLogger(pw)
  lazy val logCallStack = buildLogger(
    getPrintWriter(s"$PEVAL_LOG_DIR/callstack"),
  )

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

  extension (v: Value) {

    def toExpr: Expr = v match
      case Math(decimal)  => EMath(decimal)
      case Infinity(pos)  => EInfinity(pos)
      case Enum(name)     => EEnum(name)
      case CodeUnit(c)    => ECodeUnit(c)
      case Number(double) => ENumber(double)
      case BigInt(bigInt) => EBigInt(bigInt)
      case Str(str)       => EStr(str)
      case Bool(bool)     => EBool(bool)
      case Undef          => EUndef()
      case Null           => ENull()
      case _              => throw new NonLiteral(v)

    def toExprOpt: Option[Expr] = Try(v.toExpr).toOption

    def isPrintable: Boolean = toExprOpt.isDefined
  }

  /** transition for lexical SDO - copied from Interpreter.scala */
  def eval(lex: Lexical, sdoName: String): Str | Numeric | Math | Undef = {
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
