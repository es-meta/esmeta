package esmeta.analyzer

import esmeta.analyzer.domain.*
import esmeta.analyzer.util.*
import esmeta.cfg.Node
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
  )(using np: NodePoint[Node]): Updater = cond match {
    case _ if !USE_REFINE => st => st
    // prune values
    case EBinary(BOp.Eq, ERef(ref: Local), target) =>
      for {
        l <- transfer(ref)
        r <- transfer(target)
        lv <- transfer(l)
        prunedV = lv.pruneValue(r, positive)
        _ <- modify(_.update(l, prunedV))
      } yield ()
    // prune fields
    case EBinary(BOp.Eq, ERef(Prop(ref: Local, EStr(field))), target) =>
      for {
        l <- transfer(ref)
        r <- transfer(target)
        lv <- transfer(l)
        prunedV = lv.pruneField(field, r, positive)
        _ <- modify(_.update(l, prunedV))
      } yield ()
    // prune types
    case EBinary(BOp.Eq, ETypeOf(ERef(ref: Local)), tyRef: ERef) =>
      for {
        l <- transfer(ref)
        r <- transfer(tyRef)
        lv <- transfer(l)
        prunedV = lv.pruneType(r, positive)
        _ <- modify(_.update(l, prunedV))
      } yield ()
    // prune type checks
    case ETypeCheck(ERef(ref: Local), tyExpr) =>
      for {
        l <- transfer(ref)
        r <- transfer(tyExpr)
        lv <- transfer(l)
        prunedV = lv.pruneTypeCheck(r, positive)
        _ <- modify(_.update(l, prunedV))
      } yield ()
    case EUnary(UOp.Not, e) => prune(e, !positive)
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
}
