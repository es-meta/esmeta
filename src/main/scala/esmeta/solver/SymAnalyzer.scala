package esmeta.solver

import esmeta.analyzer.tychecker.TyChecker
import esmeta.cfg.*
import esmeta.ir.{Func => _, *}
import esmeta.spec.{BuiltinHead, ParamKind}
import esmeta.ty.*
import scala.collection.mutable.{Map => MMap}

/** infer function summaries and collect symbolic builtin returns */
class SymAnalyzer(cfg: CFG) extends TyChecker(cfg, silent = true) {
  import SymTy.*

  val builtinResults = MMap.empty[InternalReturnPoint, (AbsValue, AbsState)]

  private def builtinHead(func: Func): Option[BuiltinHead] =
    func.head.collect { case head: BuiltinHead if func.isBuiltin => head }

  override def getCalleeState(
    callerSt: AbsState,
    locals: List[(Local, AbsValue)],
    callee: Func,
  ): AbsState = builtinHead(callee) match
    case Some(h) =>
      import ParamKind.*
      // environment for built-in functions
      var locals = Map[Local, AbsValue](
        NAME_THIS -> AbsValue(SThis),
        NAME_ARGS_LIST -> AbsValue(SArgs),
        NAME_NEW_TARGET -> AbsValue(SNewTarget),
        NAME_ARGS -> AbsValue(RecordT("")), // argument-presence record
      )
      val ps = h.params.zipWithIndex
      for {
        (p, i) <- ps
        sty = if (p.kind == Variadic) SArgs else SSym(i)
      } { locals += Name(p.name) -> AbsValue(sty) }
      // symbolic environment for built-in functions
      val symEnv = Map(
        SThis.sym -> ESValueT,
        SArgs.sym -> ListT(ESValueT),
        SNewTarget.sym -> (ConstructorT || UndefT),
      ) ++ (for ((p, i) <- ps if p.kind != Variadic) yield { i -> ESValueT })
      AbsState(true, locals, symEnv, TypeProp.Top, Effect.Bot)
    case _ => super.getCalleeState(callerSt, locals, callee)

  /** bind call arguments to the symbols used by a callee summary */
  def bindArgs(
    callee: Func,
    vs: List[AbsValue],
    ret: AbsRet,
  )(using np: NodePoint[Call], st: AbsState): Map[Sym, AbsValue] =
    builtinHead(callee) match {
      case None => vs.zipWithIndex.map((v, i) => i -> v).toMap
      case Some(head) =>
        val receiver = vs.headOption.getOrElse(AbsValue.Bot)
        val argsList = vs.lift(1).getOrElse(AbsValue.Bot)
        val newTarget = vs.lift(2).getOrElse(AbsValue.Bot)
        val elemTy = argsList.ty.list.elem
        var remaining = np.node.callInst match {
          case ICall(_, _, _ :: EList(elems) :: _) =>
            Some(elems.map(expr => transfer.transfer(expr)(st)._1))
          case _ if !argsList.ty.list.isBottom && elemTy.isBottom => Some(Nil)
          case _                                                  => None
        }
        var required = head.params.count(_.kind == ParamKind.Normal)
        var rest: Option[List[AbsValue]] = None
        var bindings = Map(
          SThis.sym -> receiver,
          SNewTarget.sym -> newTarget,
        )
        for ((param, i) <- head.params.zipWithIndex) {
          if (param.kind == ParamKind.Variadic) {
            rest = remaining.map(xs => xs.take((xs.size - required).max(0)))
            remaining = remaining.map(_.takeRight(required))
          } else {
            if (param.kind == ParamKind.Normal) required -= 1
            bindings += i -> remaining.fold(AbsValue(elemTy || UndefT))(
              _.headOption.getOrElse(AbsValue(UndefT)),
            )
            remaining = remaining.map(_.drop(1))
          }
        }
        if (!head.params.exists(_.kind == ParamKind.Variadic)) rest = remaining
        val restTy = rest.fold(ListT(elemTy)) { xs =>
          ListT(xs.foldLeft(BotT)((ty, v) => ty || v.ty))
        }
        bindings += SArgs.sym -> (
          if (head.params.forall(_.kind == ParamKind.Variadic)) argsList
          else AbsValue(restTy)
        )
        val symbols = (ret.noSym :: ret.syms.values.toList)
          .flatMap { (value, constr) => value.bases ++ constr.bases }
          .collect { case s: Sym if variadicIdxOf(s).isDefined => s }
          .toSet
        for (sym <- symbols; i <- variadicIdxOf(sym))
          bindings += sym -> rest.fold(AbsValue(elemTy))(
            _.lift(i).getOrElse(AbsValue.Bot),
          )
        bindings
    }

  override val transfer: AbsTransfer = new AbsTransfer {
    override def apply(np: NodePoint[?]): Unit =
      np.func.builtinEntry match
        case Some(entry) if np.node == np.func.entry =>
          SymAnalyzer.this += np.copy(node = entry) -> getResult(np)
        case _ => super.apply(np)

    override def doReturn(
      inst: Return,
      st: AbsState,
      value: AbsValue,
      effect: Effect,
    )(using np: NodePoint[Node]): Unit = {
      if (curCp.nonEmpty) {
        if (
          builtinHead(np.func).nonEmpty && !st.isBottom &&
          (value.ty(using st) overlaps NormalT)
        )
          val irp = InternalReturnPoint(np.func, np.node, inst)
          builtinResults(irp) = (value, st)
        super.doReturn(inst, st, value, effect)
      }
    }

    override def propagate(rp: ReturnPoint, callerNp: NodePoint[Call]): Unit =
      if (!canUseReturnTy(rp.func)) {
        given NodePoint[Call] = callerNp
        given callerSt: AbsState = callInfo(callerNp)
        val ret = getResult(rp)
        val args = bindArgs(rp.func, argsInfo(callerNp), ret)
        for {
          next <- getAfterCallNp(callerNp)
          (value, constr) <- ret.noSym :: ret.syms.values.toList
          if !value.isBottom
          refined = refine(instantiate(constr, args))(callerSt)
          if !refined.isBottom
          result = instantiate(value, args)(using refined)
            .bind(using refined)
            .refine(rp.func.retTy.ty.toValue)(using refined)
          if !result.isBottom
        } {
          val nextSt =
            refined.weaken(ret.effect).define(callerNp.node.lhs, result)
          SymAnalyzer.this += next -> nextSt
        }
      }

    override def apply(rp: ReturnPoint): Unit =
      for (caller <- retEdges.getOrElse(rp, Set())) propagate(rp, caller)
  }
}
