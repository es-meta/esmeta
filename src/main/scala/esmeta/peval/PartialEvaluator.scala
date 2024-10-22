package esmeta.peval

import esmeta.{PEVAL_LOG_DIR, LINE_SEP}
import esmeta.analyzer.*
import esmeta.error.*
import esmeta.error.NotSupported.{*, given}
import esmeta.error.NotSupported.Category.{Type => _, *}
import esmeta.interpreter.*
import esmeta.ir.*
import esmeta.es.*
import esmeta.parser.{ESParser, ESValueParser}
import esmeta.peval.pstate.*
import esmeta.peval.util.*
import esmeta.peval.simplifier.*
import esmeta.state.{BigInt, *}
import esmeta.spec.{Spec}
import esmeta.util.BaseUtils.{error => _, *}
import esmeta.util.SystemUtils.*
import esmeta.TEST_MODE
import java.io.PrintWriter
import java.math.MathContext.DECIMAL128
import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.mutable.{Map as MMap, Set as MSet}
import scala.math.{BigInt as SBigInt}
import scala.util.{Try, Success}

/** extensible helper of IR interpreter with a CFG */
class PartialEvaluator(
  val program: Program,
  val log: Boolean = false,
  val detail: Boolean = false,
  val simplifyLevel: Int = 1,
  val logPW: Option[PrintWriter] = None,
  val timeLimit: Option[Int] = None,
  val renamer: Renamer,
  // NOTE : renamer should NOT be copied when copying PState - renamer is, specializer-level global state.
) {
  import PartialEvaluator.*

  /** given spec */
  given Spec = program.spec

  lazy val funcMap = Map.from(program.funcs.map(p => p.name -> p))
  lazy val getFunc = (fname: String) =>
    funcMap.getOrElse(fname, throw UnknownFunc(fname))

  def peval(ref: Ref, pst: PState)(using
    MSet[(Var, Var)],
  ): (Predict[RefTarget], Ref) = ref match

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

  def peval(expr: Expr, pst: PState)(using
    // ad-hoc fix for EClo
    captures: MSet[(Var, Var)],
  ): (Predict[Value], Expr) =
    val result = expr match
      case ERef(ref) =>
        val (tgt, newRef) = peval(ref, pst);
        pst(tgt) match
          case Known(v) if v.isLiteralValue => (pst(tgt), v.toExpr)
          case _                            => (pst(tgt), ERef(newRef))
      // TODO : how to properly handle control flow?

      // case EClo(fname, captured) =>
      // val func = cfg.getFunc(fname)
      // (PClo(func, captured.map(x => x -> pst(x)).toMap), expr)

      case EClo(fname, captured) =>
        val func = getFunc(fname)
        // keep original name `x`
        val v = PClo(
          func,
          captured.map(x => x -> pst(renamer.get(x, pst.context))).toMap,
        )
        captures ++= captured.map(old => old -> renamer.get(old, pst.context))
        (Known(v), EClo(fname, captured))

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

      case ERecord(tname, pairs) =>
        val addr = renamer.newAddr
        val pvs = for {
          (f, expr) <- pairs
          (pv, newExpr) = peval(expr, pst)
        } yield (f -> pv, f -> newExpr)
        pst.allocRecord(
          addr,
          tname,
          pvs.map(_._1),
        )
        (Known(addr), ERecord(tname, pvs.map(_._2)))
      case ESizeOf(expr) =>
        val (pv, newE) = peval(expr, pst)
        pv match
          case Unknown => (Unknown, ESizeOf(newE))
          case Known(v) =>
            v match
              case addr: Addr =>
                pst(addr) match
                  case Unknown => (Unknown, ESizeOf(newE))
                  case Known(po) =>
                    val ret = Math(po.size)
                    (Known(ret), ret.toExpr)
              case Str(s) =>
                val ret = Math(s.length)
                (Known(ret), ret.toExpr)
              case AstValue(ast) =>
                val ret = Math(ast.children.size)
                (Known(ret), ret.toExpr)
              case v => throw InvalidSizeOf(v)

      case EUnary(uop, expr) =>
        val (pv, newExpr) = peval(expr, pst)
        pv match
          case Known(v) =>
            val result = Interpreter.eval(uop, v)
            (
              Known(result),
              result.toExprOpt match
                case None        => EUnary(uop, newExpr)
                case Some(value) => value,
            )
          case Unknown => (Unknown, EUnary(uop, newExpr))

      case EBinary(BOp.And, left, right) =>
        val (lv, newLeft) = peval(left, pst)
        lv match
          case Known(Bool(false)) => Known(Bool(false)) -> EBool(false)
          case _ =>
            val (rv, newRight) = peval(right, pst)
            (lv, rv) match
              case (Known(lvv), Known(rvv)) =>
                val result = Interpreter.eval(BOp.And, lvv, rvv)
                Known(result) ->
                result.toExprOpt.getOrElse(EBinary(BOp.And, newLeft, newRight))
              case _ =>
                (Unknown, EBinary(BOp.And, newLeft, newRight))

      case EBinary(BOp.Or, left, right) =>
        val (lv, newLeft) = peval(left, pst)
        lv match
          case Known(Bool(true)) => Known(Bool(true)) -> EBool(true)
          case _ =>
            val (rv, newRight) = peval(right, pst)
            (lv, rv) match
              case (Known(lvv), Known(rvv)) =>
                val result = Interpreter.eval(BOp.Or, lvv, rvv)
                Known(result) ->
                result.toExprOpt.getOrElse(EBinary(BOp.Or, newLeft, newRight))
              case _ =>
                (Unknown, EBinary(BOp.Or, newLeft, newRight))
      case EBinary(bop, left, right) =>
        val (lv, newLeft) = peval(left, pst)
        val (rv, newRight) = peval(right, pst)
        (lv, rv) match
          case (Known(v1), Known(v2)) =>
            val result = Interpreter.eval(bop, v1, v2)
            (
              Known(result),
              result.toExprOpt.getOrElse(EBinary(bop, newLeft, newRight)),
            )
          case _ => (Unknown, EBinary(bop, newLeft, newRight))

      case ETypeCheck(expr, ty) =>
        val (pv, newExpr) = peval(expr, pst);
        val ret = ty.ty.contains(pv, pst).map(Bool.apply)
        (ret, ret.map((_: Value).toExpr).getOrElse(ETypeCheck(newExpr, ty)))

      case EContains(list, elem) =>
        val (l, newL) = peval(list, pst)
        val (e, newE) = peval(elem, pst) // TODO is this order okay
        l match
          case Unknown => (Unknown, EContains(newL, newE))
          case Known(addr: Addr) =>
            pst(addr) match
              case Unknown => (Unknown, EContains(newL, newE))
              // TODO make two branch linear
              case Known(lobj: PListObj) if (lobj.values.contains(Unknown)) =>
                (Unknown, EContains(newL, newE))
              case Known(lobj: PListObj) =>
                e match
                  case Unknown => (Unknown, EContains(newL, newE))
                  case Known(ev) =>
                    val ret = Bool(lobj.values.contains(Known(ev)))
                    (Known(ret), ret.toExpr)
              case Known(o) =>
                throw UnexpectedKnown(o, "not a list for EContains")
          case Known(v) => throw UnexpectedKnown(v, "not an addr for EContains")

      // case EParse(code, rule)                       => ???
      // case EGrammarSymbol(name, params)             => ???
      // case ESourceText(expr)                        => ???
      // case EYet(msg)                                => ???
      // case ESubstring(expr, from, to)               => ???
      // case ETrim(expr, isStarting)                  => ???

      // case EVariadic(vop, exprs)                    => ???
      // case EMathOp(mop, args)                       => ???
      // case EConvert(cop, expr)                      => ???
      // case EExists(ref)                             => ???
      // case ETypeOf(base)                            => ???
      // case EInstanceOf(base, target)                => ???
      // case ETypeCheck(base, ty)                     => ???

      // case ECont(fname)                             => ???
      // case EDebug(expr)                             => ???
      // case ERandom()                                => ???
      // case ESyntactic(name, args, rhsIdx, children) => ???
      // case ELexical(name, expr)                     => ???
      // case ERecord(tname, pairs)                    => ???
      // case EMap(ty, pairs)                          => ???
      // case ECopy(obj)                               => ???
      // case EKeys(map, intSorted)                    => ???

      case _ =>
        logging("expr", s"NOT SUPPORTED EXPR! $expr")
        (Unknown, RenameWalker(expr)(using renamer, pst))
    logging("expr", s"$expr -> $result")
    val (v, newE) = result
    if (detail && !v.isKnownLiteral) then
      (v, newE.addCmt(v.toString(detail = true)))
    else (v, newE)

  def peval(inst: Inst, pst: PState)(using
    captures: MSet[(Var, Var)],
  ): (Inst, PState) =
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
        val newCallCount = renamer.newCallCount
        val newLhs = renamer.get(lhs, pst.context)
        val (pv, newBase) = peval(base, pst)
        val vs = args.map((e) => peval(e, pst)) // TODO check order
        pv match
          case Unknown =>
            pst.define(newLhs, Unknown)
            (ISdoCall(newLhs, newBase, method, vs.map(_._2)), pst)
          case Known(ast: AstValue) if vs.map(_._1).exists(_.isEmpty) =>
            pst.define(newLhs, Unknown)
            (ISdoCall(newLhs, newBase, method, vs.map(_._2)), pst)
          // TODO: local variable inline: <varname>_<fid>_<ctxtcounter>
          // case Known(AstValue(ast)) =>
          case Known(value) =>
            value.asAst match {
              case syn: Syntactic => {
                val (ast0, sdo) = getSdo((syn, method)).getOrElse(
                  throw InvalidAstField(syn, Str(method)),
                )

                val calleeCtx = PContext(
                  func = sdo,
                  locals = MMap.empty,
                  sensitivity = newCallCount,
                  ret = None,
                );

                val allocations = setLocals(
                  at = calleeCtx,
                  params = sdo.params.map(p =>
                    Param(
                      renamer.get(p.lhs, calleeCtx),
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

                val calleePst = {
                  val fresh = pst.copied;
                  fresh.callStack ::= PCallContext(pst.context, newLhs)
                  fresh.context = calleeCtx
                  fresh
                }

                Try {
                  val (body, after) = peval(sdo.body, calleePst)
                  after.callStack match
                    case Nil => /* never */ ???
                    case callerCtx :: rest =>
                      val calleeCtx = after.context;
                      after.callStack = rest
                      after.context = callerCtx.ctxt
                      after.define(
                        newLhs,
                        calleeCtx.ret.getOrElse(Unknown),
                      )
                      (ISeq(List(allocations, body)), after)
                }.recoverWith {
                  case NoMoreInline() =>
                    pst.define(newLhs, Unknown)
                    pst.heap.clear(vs.map(_._1)) // ...
                    Success(
                      (ISdoCall(newLhs, newBase, method, vs.map(_._2)), pst),
                    )
                }.get
              }

              case lex: Lexical =>
                val v = Interpreter.eval(lex, method);
                pst.define(newLhs, Known(v))
                (IAssign(newLhs, v.toExpr), pst)
            }
      case call @ ICall(lhs, fexpr, args) =>
        val newCallCount = renamer.newCallCount
        val newLhs = renamer.get(lhs, pst.context)
        val (f, newFexpr) = peval(fexpr, pst)
        val vs = args.map(e => peval(e, pst)) // TODO check order
        f match
          case Known(pclo @ PClo(callee, captured))
              if vs.map(_._1).forall(_.isDefined) =>
            val calleeCtx = PContext(
              func = callee,
              locals = MMap.empty,
              sensitivity = newCallCount,
              ret = None,
            );

            val allocations = setLocals(
              at = calleeCtx,
              params = callee.params.map(p =>
                Param(
                  renamer.get(p.lhs, calleeCtx),
                  p.ty,
                  p.optional,
                  p.specParam,
                ),
              ),

              // TODO : There is no way to print ast0 as expression this should be removed somehow
              /* Ad-hoc fix */
              args = vs,
              func = callee,
              isCont = false,
            )

            val calleePst = {
              val fresh = pst.copied;
              fresh.callStack ::= PCallContext(pst.context, newLhs)
              fresh.context = calleeCtx
              fresh
            }

            Try {
              val (body, after) = peval(callee.body, calleePst)
              after.callStack match
                case Nil => /* never */ ???
                case callerCtx :: rest =>
                  val calleeCtx = after.context;
                  after.callStack = rest
                  after.context = callerCtx.ctxt
                  val retVal = calleeCtx.ret.getOrElse(throw NoReturnValue)
                  after.define(newLhs, retVal)
                  (ISeq(List(allocations, body)), after)
            }.recoverWith {
              case NoMoreInline() =>
                pst.define(newLhs, Unknown)
                pst.heap.clear(vs.map(_._1))
                Success(ICall(newLhs, newFexpr, vs.map(_._2)), pst)
            }.get

          case Known(_: PClo) /* with Unknown argument */ =>
            pst.define(newLhs, Unknown)
            pst.heap.clear(vs.map(_._1))
            (ICall(newLhs, newFexpr, vs.map(_._2)), pst)
          case Known(_: PCont) => throwPeval"not yet supported"
          case Known(v)        => throw NoCallable(v)
          case Unknown =>
            pst.define(newLhs, Unknown)
            pst.heap.clear(vs.map(_._1))
            (ICall(newLhs, newFexpr, vs.map(_._2)), pst)

      case INop() => (INop(), pst)
      case IAssign(ref, expr) =>
        ref match
          case x: Var =>
            val newVar = renamer.get(x, pst.context);
            val (pv, newExpr) = peval(expr, pst)
            pst.update(newVar, pv)
            (IAssign(newVar, newExpr), pst)
          case Field(_, _) =>
            pst.heap.clear
            (RenameWalker(inst)(using renamer, pst), pst) // TODO

      case IPop(lhs, list, front) =>
        val newLhs = renamer.get(lhs, pst.context);
        val (pv, newListExpr) = peval(list, pst);
        pv match
          case Known(_) => pst.heap.clear // TODO : modify heap
          case Unknown  => pst.heap.clear // TODO : kill heap
        pst.define(newLhs, Unknown)
        (IPop(newLhs, newListExpr, front), pst)

      case IPush(elem, list, front) =>
        val (value, newElem) = peval(elem, pst)
        val (pv, newListExpr) = peval(list, pst)
        pv match
          case Known(addr: Addr) => pst.heap.push(addr, value, front)
          case Unknown           => pst.heap.clear // TODO : kill heap
          case _                 => throwPeval"not an address"
        (IPush(newElem, newListExpr, front), pst)

      case IReturn(expr) =>
        val (pv, newExpr) = peval(expr, pst)
        pst.callStack match
          case head :: _ =>
            pst.context.ret = Some(pv)
            (
              IAssign(head.retId, newExpr)
                .addCmt(s"<~ IReturn @ ${pst.context.func.name}"),
              pst,
            )
          case Nil =>
            pst.context.ret = Some(pv)
            (IReturn(newExpr), pst)

      case iif @ IIf(cond, thenInst, elseInst) =>
        val (pv, newCond) = peval(cond, pst)
        pv match
          case Known(Bool(true))                 => peval(thenInst, pst)
          case Known(Bool(false))                => peval(elseInst, pst)
          case Known(v)                          => throw NoBoolean(v)
          case Unknown if pst.callStack.size > 0 => throw NoMoreInline()
          case Unknown /* pst.callStack.size == 0 */ => {

            val (thenPst, elsePst) = (pst.copied, pst.copied)
            val (newThen, newThenPst) = peval(thenInst, thenPst)
            val (newElse, newElsePst) = peval(elseInst, elsePst)
            val newPst = newThenPst.join(elsePst)
            (IIf(newCond, newThen, newElse), newPst)

            /* Handle Return */
          }
      case IExpr(expr) => (IExpr(peval(expr, pst)._2), pst)
      case IExpand(_, _) | IDelete(_, _) => /* heap modifying insts */
        val newInst = RenameWalker(inst)(using renamer, pst)
        pst.heap.clear
        (newInst.addCmt("not supported yet"), pst)

      case IAssert(expr) =>
        val (pv, newExpr) = peval(expr, pst);
        val newInst = IAssert(newExpr);
        pv match
          case Known(v) =>
            v.asBool match
              case true  => (INop(), pst)
              case false => throw AssertionFail(expr)
          case Unknown =>
            (newInst.addCmt("will be checked at runtime."), pst)

      case IPrint(expr) =>
        val (_, newExpr) = peval(expr, pst);
        val newInst = IAssert(newExpr);
        (newInst, pst)

      case IWhile(cond, body) =>
        val (cv, _) = peval(cond, pst)
        cv match
          case Known(Bool(false)) => (INop(), pst)
          case Known(Bool(true)) =>
            val (newBody, newPst) = peval(body, pst)
            val (rest, newPst2) = peval(inst, newPst)
            (ISeq(newBody :: rest :: Nil), newPst2)
          case Known(v) => throw NoBoolean(v)
          case Unknown =>
            val affectedLocals =
              LocalImpact(body)(using renamer, pst.context)
            for { l <- affectedLocals } pst.define(l, Unknown)
            pst.heap.clear
            (
              RenameWalker(inst)(using renamer, pst).addCmt(
                "unknown condition : heap is cleared",
              ),
              pst,
            )

    logging("pst", pst)
    logging(
      s"inst @ ${pst.context.func.name} = cs[${pst.callStack.size}]",
      s"${inst.toString(detail = false)} -> ${newInst.toString(detail = true).replace("\n", " ")}\n",
    )
    captures.size match
      case 0 => (newInst, newPst)
      case _ =>
        val assigns = ISeq(captures.toList.map {
          case (old -> newV) => IAssign(old, ERef(newV))
        })
        captures.clear
        (ISeq(assigns :: newInst :: Nil), newPst)

  /** final state */
  def run(
    func: Func,
    pst: PState,
    newName: Option[String] = None,
  ): (Func, PState) = timeout(
    {
      val inst = func.body
      val newParams = func.params.map {
        case Param(lhs, ty, optional, specParam) =>
          Param(renamer.get(lhs, pst.context), ty, optional, specParam)
      }
      val result @ (newBody, newPst) = peval(inst, pst)(using MSet.empty)
      val simplifiedNewBody = simplifyLevel match
        case 0 => newBody
        case 1 => InstFlattener(newBody)
        case 2 => InstFlattener(NoLiterals(newBody))
        case 3 => ??? // InstFlattener(NoLiterals(newBody)) // TODO

      (
        Func(
          func.main,
          func.kind,
          newName.getOrElse(func.name),
          newParams,
          func.retTy,
          simplifiedNewBody,
          func.overloads,
          func.algo,
        ),
        newPst,
      )
    },
    timeLimit,
  )

  /** ECMAScript parser */
  lazy val esParser: ESParser = program.esParser

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
            IAssign(param.lhs, argExpr) :: evalArgs,
          )(
            pl,
            al,
          )
      }
    // reverse needed to keep order
    ISeq(aux(Nil)(params, args).reverse.toList.filter { /* adhoc */
      case IAssign(_, ERef(Temp(-1))) => false
      case _                          => true
    })
  }

  private def buildLogger = (writer: PrintWriter) =>
    (tag: String, data: Any) =>
      if (log)
        writer.println(s"[$tag] $data")
        writer.flush()

  lazy val logging = buildLogger(pw)

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------

  /** type model */
  private def tyModel = program.tyModel

  /** spec */
  private def spec = program.spec

  /** itereration count */
  private var iter = 0

  /** logging */
  private lazy val pw: PrintWriter =
    logPW.getOrElse(getPrintWriter(s"$PEVAL_LOG_DIR/log"))

  /** cache to get syntax-directed operation (SDO) */
  private val getSdo =
    cached[(Ast, String), Option[(Ast, Func)]](
      _.getSdo[Func](_)(using spec, funcMap),
    )

}

/** IR PartialEvaluator with a CFG */
object PartialEvaluator {

  /** prepare new Renamer and PState for partial evaluation */
  def prepare(func: Func): (Renamer, PState) = {
    val renamer = Renamer()
    val thisCallCount = renamer.newCallCount
    val pst = PState.empty(PContext.empty(func, sensitivity = thisCallCount))
    (renamer, pst)
  }

  object ForECMAScript {

    /** create new PState for p-evaluating `FunctionDeclarationInstantation`
      *
      * @param func
      *   ir function, must be `FunctionDeclarationInstantation`.
      * @param esFuncDecl
      *   es function declaration to use as an argument.
      * @return
      *   (renamer, pst)
      */
    def prepareForFDI(func: Func, esFuncDecl: Ast) = {

      if (func.name != FUNC_DECL_INSTANT) {
        throw PartialEvaluatorError(
          s"`preparePst` is only callable with  ${FUNC_DECL_INSTANT}",
        )
      }

      val (renamer, pst) = prepare(func)

      val addr_func_obj_record = renamer.newAddr

      pst.allocRecord(
        addr_func_obj_record,
        "ECMAScriptFunctionObject",
        List(
          FORMAL_PARAMS -> Known(
            AstValue(AstHelper.getChildByName(esFuncDecl, FORMAL_PARAMS)),
          ),
          ECMASCRIPT_CODE -> Known(
            AstValue(AstHelper.getChildByName(esFuncDecl, FUNC_BODY)),
          ),
          "ThisMode" -> Known(ENUM_STRICT),
          "Strict" -> Known(Bool(true)), // ESMeta is always strict
        ),
      )

      pst.define(
        renamer.get(Name("func"), pst.context),
        Known(addr_func_obj_record),
      )
      pst.define(
        renamer.get(Name("argumentsList"), pst.context),
        Unknown,
      );

      (renamer, pst)
    }

    /** overload FDI of Program in a immutable way */
    def overloadFDI(
      program: Program,
      overloads: List[(Func, Ast)],
    ): Program = {
      val funcs = program.funcs.map {
        case f if f.name != FUNC_DECL_INSTANT => f
        case f                                =>
          // TODO : optimize finding matching overloads
          val overloadsMap = {
            val astOfOverloads = overloads.map {
              case (func, decl) => {
                val formalParamsOfDecl =
                  AstHelper
                    .getAllChildrenByName(decl, FORMAL_PARAMS)
                    .headOption
                    .map(AstValue.apply)
                    .getOrElse(throwPeval"formalParams not found")
                val ecmaScriptCodeOfDecl =
                  AstHelper
                    .getAllChildrenByName(decl, FUNC_BODY)
                    .headOption
                    .map(AstValue.apply)
                    .getOrElse(throwPeval"ecmaScriptCode not found")
                (func, formalParamsOfDecl, ecmaScriptCodeOfDecl)
              }
            }
            HashMap.from(astOfOverloads.map {
              case (ol, fpOfDecl, escOfDecl) => (fpOfDecl, escOfDecl) -> ol.name
            })
          }

          val go =
            (args: Iterable[Value], st: State) =>
              for {
                addr <- args.headOption.flatMap {
                  case addr: Addr => Some(addr)
                  case _          => None
                }
                record <- st(addr) match
                  case r: RecordObj => Some(r)
                  case _            => None
                asts <- record
                  .get(Str(FORMAL_PARAMS))
                  .zip(record.get(Str(ECMASCRIPT_CODE)))
                  .flatMap {
                    case (v1: AstValue, v2: AstValue) => Some((v1, v2))
                    case _                            => None
                  }
                fname <- overloadsMap.get(asts)
              } yield fname
          Func(
            f.main,
            f.kind,
            f.name,
            f.params,
            f.retTy,
            f.body,
            GetOverloads(go),
            f.algo,
          )
      }
      Program(
        List.from(overloads.map(_._1)) ::: funcs,
        program.spec,
      )
    }

    import esmeta.cfg.CFG

    /** overload FDI of CFG using muation */
    def overloadFDIofCfg(
      cfg: CFG,
      overloads: List[(Func, Ast)],
    ): Unit = {
      val funcs = cfg.funcs.foreach {
        case f if f.name != FUNC_DECL_INSTANT =>
        case f                                =>
          // TODO : optimize finding matching overloads
          val overloadsMap = {
            val astOfOverloads = overloads.map {
              case (func, decl) => {
                val formalParamsOfDecl =
                  AstHelper
                    .getAllChildrenByName(decl, FORMAL_PARAMS)
                    .headOption
                    .map(AstValue.apply)
                    .getOrElse(throwPeval"formalParams not found")
                val ecmaScriptCodeOfDecl =
                  AstHelper
                    .getAllChildrenByName(decl, FUNC_BODY)
                    .headOption
                    .map(AstValue.apply)
                    .getOrElse(throwPeval"ecmaScriptCode not found")
                (func, formalParamsOfDecl, ecmaScriptCodeOfDecl)
              }
            }
            HashMap.from(astOfOverloads.map {
              case (ol, fpOfDecl, escOfDecl) => (fpOfDecl, escOfDecl) -> ol.name
            })
          }

          val go =
            (args: Iterable[Value], st: State) =>
              for {
                addr <- args.headOption.flatMap {
                  case addr: Addr => Some(addr)
                  case _          => None
                }
                record <- st(addr) match
                  case r: RecordObj => Some(r)
                  case _            => None
                asts <- record
                  .get(Str(FORMAL_PARAMS))
                  .zip(record.get(Str(ECMASCRIPT_CODE)))
                  .flatMap {
                    case (v1: AstValue, v2: AstValue) => Some((v1, v2))
                    case _                            => None
                  }
                fname <- overloadsMap.get(asts)
              } yield fname
          f.irFunc.overloads = GetOverloads(go)
      }
    }
  }
}
