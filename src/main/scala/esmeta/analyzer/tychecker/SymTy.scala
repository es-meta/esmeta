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

  import SymTy.*

  type Sym = Int
  type Base = Sym | Local
  type SymBase = SSym | SVar
  type SymRef = SSym | SVar | SField

  enum SymTy {
    case STy(ty: ValueTy)
    case SVar(x: Local)
    case SSym(sym: Sym)
    case SField(base: SymRef, field: SymTy)
    case SNormal(symty: SymTy)

    def isBottom: Boolean = this match
      case STy(ty)        => ty.isBottom
      case SNormal(symty) => symty.isBottom
      case _              => false

    def isSingle(using st: AbsState): Boolean = this.upper.getSingle match
      case One(_) => true
      case _      => false

    /* Evaluation of the Symbolic type */
    def upper(using st: AbsState): ValueTy = this match
      case STy(ty)             => ty
      case SVar(x)             => st.get(x).ty
      case SSym(sym)           => st.get(sym)
      case SField(base, field) => st.get(base.upper, field.upper)
      case SNormal(symty)      => NormalT(symty.upper)

    def has(base: Base): Boolean = this match
      case STy(ty)        => false
      case SVar(x)        => base == SVar(x)
      case SSym(sym)      => base == SSym(sym)
      case SField(b, f)   => b.has(base) || f.has(base)
      case SNormal(symty) => symty.has(base)

    def bases: Set[Base] = this match
      case STy(ty)             => Set()
      case SVar(x)             => Set(x)
      case SSym(sym)           => Set(sym)
      case SField(base, field) => base.bases ++ field.bases
      case SNormal(symty)      => symty.bases

    def weaken(bases: Set[Base], update: Boolean): Option[SymTy] = this match
      case t: SymRef      => weakenRef(t, bases, update)
      case STy(ty)        => Some(STy(ty))
      case SNormal(symty) => symty.weaken(bases, update).map(SNormal(_))

    def weakenRef(
      ref: SymRef,
      bases: Set[Base],
      update: Boolean,
    ): Option[SymRef] = ref match
      case SVar(x)   => if (bases contains x) None else Some(SVar(x))
      case SSym(sym) => if (bases contains sym) None else Some(SSym(sym))
      case SField(b, f) =>
        for {
          b <- weakenRef(b, bases, update)
          f <- f.weaken(bases, update)
        } yield SField(b, f)

    def isSymbolic: Boolean = this match
      case STy(_) => false
      case _      => true

    /** partial order in same state */
    def ⊑(that: SymTy)(using st: AbsState): Boolean =
      (this ⊑ that)(st, st)

    /** partial order in different state */
    def ⊑(that: SymTy)(lst: AbsState, rst: AbsState): Boolean =
      (this, that) match
        case (STy(lty), STy(rty))           => lty ⊑ rty
        case (l, STy(rty))                  => l.upper(using lst) ⊑ rty
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
        case (l, r) => STy(l.upper(using lst) || r.upper(using rst))

    /** meet operator in same state */
    def ⊓(that: SymTy)(using st: AbsState): SymTy =
      (this ⊓ that)(st, st)

    /** meet operator in different state */
    def ⊓(that: SymTy)(lst: AbsState, rst: AbsState): SymTy =
      (this, that) match
        case (l, r) if l.isBottom || r.isBottom => SymTy.Bot
        case (l, r) if l == r                   => l
        case (l, r) => STy(l.upper(using lst) && r.upper(using rst))

    /** prune operator in same state */
    def --(that: SymTy)(using st: AbsState): SymTy =
      (this -- that)(st, st)

    /** prune operator in different state */
    def --(that: SymTy)(lst: AbsState, rst: AbsState): SymTy =
      (this, that) match
        case (l, r) if r.isBottom => l
        case (l, r)               => STy(l.upper(using lst) -- r.upper(using rst))

    def refine(ty: ValueTy)(using st: AbsState): SymTy =
      if (this ⊑ STy(ty)) this
      else STy(this.upper ⊓ ty)

    def getString = s"${this}"
  }
  object SymTy extends DomainLike[SymTy] {
    override def Top: SymTy = STy(ValueTy.Top)
    override def Bot: SymTy = STy(ValueTy.Bot)

    given rule: Rule[SymTy] = (app, elem) =>
      elem match {
        case STy(ty)   => app >> ty
        case SVar(x)   => app >> x.toString
        case SSym(sym) => app >> "#" >> sym.toString
        case SField(base, STy(x)) if x.isBottom =>
          x.getSingle match
            case One(f: String) => app >> base >> "." >> f
            case _              => app >> base >> "[" >> x >> "]"
        case SField(base, field) => app >> base >> "[" >> field >> "]"
        case SNormal(symty)      => app >> "Normal[" >> symty >> "]"
      }
    given Ordering[SymTy] = Ordering.by(_.toString)

    given Rule[Base] = (app, elem) =>
      elem match
        case x: Local => app >> x.toString
        case x: Sym   => app >> "#" >> x.toString
    given Ordering[Base] = Ordering.by(_.toString)
  }

  extension (sb: SymBase) {
    def toBase: Base = sb match
      case SVar(x) => x
      case SSym(s) => s
  }
}
