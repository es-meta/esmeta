package esmeta.analyzer

import esmeta.analyzer.domain.*
import esmeta.analyzer.util.*
import esmeta.ir.{Func => _, *}
import esmeta.ty.*

/** helper functions for pruning/refinement */
trait PruneHelper { this: AbsTransfer =>

  /** loading monads */
  import AbsState.monad.*

  /** prune condition */
  def prune(
    cond: Expr,
    positive: Boolean,
  )(using cp: NodePoint[_]): Updater = cond match {
    case _ if !USE_REFINE   => st => st
    case EUnary(UOp.Not, e) => prune(e, !positive)
    case EBinary(BOp.Eq, ERef(ref: Local), target) =>
      for {
        rv <- transfer(ref)
        tv <- transfer(target)
        _ <- modify(pruneValue(rv, tv, positive))
      } yield ()
    case EBinary(BOp.Eq, ERef(Prop(ref: Local, EStr("Value"))), target) =>
      AbsValue match
        case domain.value.TypeDomain =>
          for {
            rv <- transfer(ref)
            base <- transfer(rv)
            tv <- transfer(target)
            ty = base.ty
            normal = ty.normal.prune(tv.ty.pureValue, positive)
            newV = AbsValue(ty.copy(comp = CompTy(normal, ty.abrupt)))
            _ <- modify(_.update(rv, newV))
          } yield ()
        case _ => st => st
    case ETypeCheck(ERef(ref: Local), tyExpr) =>
      for {
        rv <- transfer(ref)
        tv <- transfer(tyExpr)
        _ <- modify(pruneTypeCheck(rv, tv, positive))
      } yield ()
    case EBinary(BOp.Eq, ETypeOf(ERef(ref: Local)), tyRef: ERef) =>
      for {
        rv <- transfer(ref)
        tv <- transfer(tyRef)
        _ <- modify(pruneType(rv, tv, positive))
      } yield ()
    case EBinary(BOp.Or, l, r) =>
      st =>
        lazy val ltst = prune(l, true)(st)
        lazy val lfst = prune(l, false)(st)
        val rst = prune(r, positive)(lfst)
        if (positive) ltst ⊔ rst else lfst ⊓ rst
    case EBinary(BOp.And, l, r) =>
      st =>
        lazy val ltst = prune(l, true)(st)
        lazy val lfst = prune(l, false)(st)
        val rst = prune(r, positive)(ltst)
        if (positive) ltst ⊓ rst else lfst ⊔ rst
    case _ => st => st
  }

  /** prune value */
  def pruneValue(
    l: AbsRefValue,
    r: AbsValue,
    positive: Boolean,
  )(using cp: NodePoint[_]): Updater = for {
    lv <- transfer(l)
    st <- get
    prunedV = lv.pruneValue(r, positive)
    _ <- modify(_.update(l, prunedV))
  } yield ()

  /** prune type */
  def pruneType(
    l: AbsRefValue,
    r: AbsValue,
    positive: Boolean,
  )(using cp: NodePoint[_]): Updater = for {
    lv <- transfer(l)
    st <- get
    prunedV = lv.pruneType(r, positive)
    _ <- modify(_.update(l, prunedV))
  } yield ()

  /** prune type check */
  def pruneTypeCheck(
    l: AbsRefValue,
    r: AbsValue,
    positive: Boolean,
  )(using cp: NodePoint[_]): Updater =
    for {
      lv <- transfer(l)
      st <- get
      prunedV = lv.pruneTypeCheck(r, positive)
      _ <- modify(_.update(l, prunedV))
    } yield ()
}
