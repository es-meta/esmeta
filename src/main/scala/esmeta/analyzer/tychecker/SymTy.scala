package esmeta.analyzer.tychecker

import esmeta.cfg.*
import esmeta.interpreter.Interpreter
import esmeta.ir.{Name, BOp, COp, VOp, MOp, UOp, Local, IRElem}
import esmeta.state.*
import esmeta.ty.{*, given}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

trait SymTyDecl { self: TyChecker =>
  import tyStringifier.given

  enum SymTy extends SymTyLike {
    case STy(ty: ValueTy)
    case SRef(ref: SymRef)
    case SNormal(symty: SymTy)

    def isBottom: Boolean = this match
      case STy(ty)        => ty.isBottom
      case SRef(ref)      => false
      case SNormal(symty) => symty.isBottom

    def isSingle(using st: AbsState): Boolean = this.ty.getSingle match
      case One(_) => true
      case _      => false

    def ty(using st: AbsState): ValueTy = this match
      case STy(ty)        => ty
      case SRef(ref)      => st.getTy(ref)
      case SNormal(symty) => NormalT(symty.ty)

    def has(base: SymBase): Boolean = this match
      case STy(ty)        => false
      case SRef(ref)      => ref.has(base)
      case SNormal(symty) => symty.has(base)

    def bases: Set[SymBase] = this match
      case STy(ty)        => Set()
      case SRef(ref)      => ref.bases
      case SNormal(symty) => symty.bases

    def kill(bases: Set[SymBase]): Option[SymTy] = this match
      case STy(ty)        => Some(this)
      case SRef(ref)      => ref.kill(bases).map(SRef(_))
      case SNormal(symty) => symty.kill(bases).map(SNormal(_))

    /** partial order in same state */
    def ⊑(that: SymTy)(using st: AbsState): Boolean =
      (this ⊑ that)(st, st)

    /** partial order in different state */
    def ⊑(that: SymTy)(lst: AbsState, rst: AbsState): Boolean =
      (this, that) match
        case (STy(lty), STy(rty))           => lty ⊑ rty
        case (l, STy(rty))                  => l.ty(using lst) ⊑ rty
        case (l, r) if l.isBottom || l == r => true
        case _                              => false

    /** not partial order */
    def !⊑(that: SymTy)(using AbsState): Boolean = !(this ⊑ that)

    /** join operator in same state */
    def ⊔(that: SymTy)(using st: AbsState): SymTy =
      (this ⊔ that)(st, st)

    /** join operator in different state */
    def ⊔(that: SymTy)(lst: AbsState, rst: AbsState): SymTy =
      (this, that) match
        case (l, r) if l.isBottom || l == r => r
        case (l, r) if r.isBottom           => l
        case (l, r) => STy(l.ty(using lst) || r.ty(using rst))

    /** meet operator in same state */
    def ⊓(that: SymTy)(using st: AbsState): SymTy =
      (this ⊓ that)(st, st)

    /** meet operator in different state */
    def ⊓(that: SymTy)(lst: AbsState, rst: AbsState): SymTy =
      (this, that) match
        case (l, r) if l.isBottom || r.isBottom => SymTy.Bot
        case (l, r) if l == r                   => l
        case (l, r) => STy(l.ty(using lst) && r.ty(using rst))

    /** prune operator in same state */
    def --(that: SymTy)(using st: AbsState): SymTy =
      (this -- that)(st, st)

    /** prune operator in different state */
    def --(that: SymTy)(lst: AbsState, rst: AbsState): SymTy =
      (this, that) match
        case (l, r) if r.isBottom => l
        case (l, r)               => STy(l.ty(using lst) -- r.ty(using rst))

    def getString = s"${this}"
  }
  object SymTy extends DomainLike[SymTy] {
    override def Top: SymTy = STy(ValueTy.Top)

    override def Bot: SymTy = STy(ValueTy.Bot)

    given rule: Rule[SymTy] = (app, elem) =>
      elem match {
        case STy(ty)        => app >> ty
        case SRef(ref)      => app >> ref
        case SNormal(symty) => app >> "Normal[" >> symty >> "]"
      }
  }
}
