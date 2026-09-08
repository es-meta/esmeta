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

  import SymTy.*, Property.*

  type Sym = Int
  type Base = Sym | Local
  type SymBase = SSym | SVar
  type SymRef = SSym | SVar | SField | SProp | SCall | SConstruct

  lazy val SThis: SSym = SSym(-1)
  lazy val SArgs: SSym = SSym(-2)
  lazy val SNewTarget: SSym = SSym(-3)

  def SVariadicIdx(k: Int): SSym = SSym(-4 - k)
  def variadicIdxOf(sym: Sym): Option[Int] =
    if (sym <= -4) Some(-4 - sym) else None

  enum SymTy {
    case STy(ty: ValueTy)
    case SVar(x: Local)
    case SSym(sym: Sym)
    case SField(base: SymRef, field: SymTy)
    case SProp(base: SymRef, prop: Property)
    case SCall(base: SymRef)
    case SConstruct(base: SymRef)
    case SRecord(base: ValueTy, fields: Map[String, SymTy])

    def isBottom: Boolean = this match
      case STy(ty) => ty.isBottom
      case SRecord(base, fields) =>
        base.isBottom || fields.values.exists(_.isBottom)
      case _ => false

    def isSingle(using st: AbsState): Boolean = this.ty.getSingle match
      case One(_) => true
      case _      => false

    /* Evaluation of the Symbolic type */
    def ty(using st: AbsState): ValueTy = this match
      case STy(ty)             => ty
      case SVar(x)             => st.get(x).ty
      case SSym(sym)           => st.get(sym)
      case SField(base, field) => st.get(base.ty, field.ty)
      case SProp(base, prop)   => base.ty.record(prop).getTy
      case SCall(base)         => base.ty.record.call.getTy
      case SConstruct(base)    => base.ty.record.construct.getTy
      case SRecord(base, fields) =>
        fields.foldLeft(base) {
          case (ty, (field, symty)) =>
            ty.copied(record =
              ty.record.update(field, symty.ty, refine = false),
            )
        }

    def has(base: Base): Boolean = this match
      case STy(ty)            => false
      case SVar(x)            => base == SVar(x)
      case SSym(sym)          => base == SSym(sym)
      case SField(b, f)       => b.has(base) || f.has(base)
      case SProp(b, _)        => b.has(base)
      case SCall(b)           => b.has(base)
      case SConstruct(b)      => b.has(base)
      case SRecord(_, fields) => fields.values.exists(_.has(base))

    def hasLocal: Boolean = this match
      case STy(ty)            => false
      case SVar(_)            => true
      case SSym(_)            => false
      case SField(b, f)       => b.hasLocal || f.hasLocal
      case SProp(b, _)        => b.hasLocal
      case SCall(b)           => b.hasLocal
      case SConstruct(b)      => b.hasLocal
      case SRecord(_, fields) => fields.values.exists(_.hasLocal)

    def hasSym: Boolean = this match
      case STy(ty)            => false
      case SVar(_)            => false
      case SSym(_)            => true
      case SField(b, f)       => b.hasSym || f.hasSym
      case SProp(b, _)        => b.hasSym
      case SCall(b)           => b.hasSym
      case SConstruct(b)      => b.hasSym
      case SRecord(_, fields) => fields.values.exists(_.hasSym)

    def bases: Set[Base] = this match
      case STy(ty)             => Set()
      case SVar(x)             => Set(x)
      case SSym(sym)           => Set(sym)
      case SField(base, field) => base.bases ++ field.bases
      case SProp(base, _)      => base.bases
      case SCall(base)         => base.bases
      case SConstruct(base)    => base.bases
      case SRecord(_, fields)  => fields.values.flatMap(_.bases).toSet

    def weaken(bases: Set[Base], update: Boolean): Option[SymTy] = this match
      case t: SymRef => weakenRef(t, bases, update)
      case STy(ty)   => Some(STy(ty))
      case SRecord(base, fields) =>
        val next = fields.flatMap { (field, symty) =>
          symty.weaken(bases, update).map(field -> _)
        }
        Some(if (next.isEmpty) STy(base) else SRecord(base, next))

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
      case SProp(b, prop) =>
        for {
          b <- weakenRef(b, bases, update)
        } yield SProp(b, prop)
      case SCall(base) =>
        for {
          b <- weakenRef(base, bases, update)
        } yield SCall(b)
      case SConstruct(base) =>
        for {
          b <- weakenRef(base, bases, update)
        } yield SConstruct(b)

    def isSymbolic: Boolean = this match
      case STy(_) => false
      case _      => true

    def upper(using st: AbsState): ValueTy = ty

    def weaken(effect: Effect)(using st: AbsState): SymTy = this match
      case STy(ty)       => STy(effect(ty))
      case SVar(x)       => this
      case SSym(sym)     => this
      case SField(b, f)  => SField(weakenRef(b, effect), f.weaken(effect))
      case SProp(b, p)   => SProp(weakenRef(b, effect), p)
      case SCall(b)      => SCall(weakenRef(b, effect))
      case SConstruct(b) => SConstruct(weakenRef(b, effect))
      case record @ SRecord(_, fields) =>
        val upper = record.ty
        val weakened = effect(upper)
        val next = fields.flatMap { (field, symty) =>
          Option.unless(effect.mayUpdate(upper, field))(
            field -> symty.weaken(effect),
          )
        }
        if (next.isEmpty) STy(weakened) else SRecord(weakened, next)

    private def weakenRef(ref: SymRef, effect: Effect)(using AbsState): SymRef =
      (ref: SymTy).weaken(effect).asInstanceOf[SymRef]

    def refine(ty: ValueTy)(using st: AbsState): SymTy =
      if (this ⊑ STy(ty)) this
      else STy(this.ty && ty)

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
    def ⊔(that: SymTy)(lst: AbsState, rst: AbsState): SymTy = (this, that) match
      case (l, r) if l.isBottom || l == r => r
      case (l, r) if r.isBottom           => l
      case (l @ SRecord(_, lfields), r @ SRecord(_, rfields)) =>
        val upper = l.ty(using lst) || r.ty(using rst)
        val fields = (lfields.keySet intersect rfields.keySet).flatMap {
          field =>
            val joined = (lfields(field) ⊔ rfields(field))(lst, rst)
            Option.when(joined.isSymbolic)(field -> joined)
        }.toMap
        if (fields.isEmpty) STy(upper) else SRecord(upper, fields)
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

    override def toString: String = stringify(this)
  }
  object SymTy extends DomainLike[SymTy] {
    override def Top: SymTy = STy(ValueTy.Top)
    override def Bot: SymTy = STy(ValueTy.Bot)

    def record(
      base: ValueTy,
      fields: Iterable[(String, SymTy)],
      captureUpper: Boolean = true,
    )(using AbsState): SymTy =
      val givenFields = fields.toMap
      val upper = givenFields.foldLeft(base) {
        case (ty, (field, symty)) if captureUpper =>
          ty.copied(record = ty.record.update(field, symty.ty, refine = false))
        case (ty, (field, STy(fieldTy))) =>
          ty.copied(record = ty.record.update(field, fieldTy, refine = false))
        case (ty, _) => ty
      }
      val all = givenFields.map { (field, symty) =>
        val locals: Set[Base] =
          symty.bases.collect { case local: Local => local: Base }
        val noLocals =
          if (locals.isEmpty) symty
          else
            symty
              .weaken(locals, update = false)
              .getOrElse(STy(upper.record(field).value))
        field -> noLocals
      }
      val symbolic = all.filter((_, symty) => symty.isSymbolic)
      if (symbolic.isEmpty) STy(upper) else SRecord(upper, symbolic)

    given rule: Rule[SymTy] = (app, elem) =>
      elem match {
        case STy(ty)   => app >> ty
        case SVar(x)   => app >> x.toString
        case SSym(sym) => app >> sym
        case SField(base, STy(x)) =>
          x.getSingle match
            case One(Str(f)) => app >> base >> "." >> f
            case _           => app >> base >> "[" >> x >> "]"
        case SField(base, field)   => app >> base >> "[" >> field >> "]"
        case SProp(base, prop)     => app >> base >> "[[" >> prop >> "]]"
        case SCall(base)           => app >> base >> ".call(...)"
        case SConstruct(base)      => app >> base >> ".construct(...)"
        case SRecord(base, fields) => app >> "SRecord[" >> base >> "]" >> fields
      }
    given Ordering[SymTy] = Ordering.by(_.toString)

    given Rule[Base] = (app, elem) =>
      elem match
        case x: Local => app >> x.toString
        case -1       => app >> "#THIS"
        case -2       => app >> "#ARGS"
        case -3       => app >> "#NEW_TARGET"
        case x: Sym =>
          variadicIdxOf(x) match
            case Some(k) => app >> "#VAR[" >> k.toString >> "]"
            case None    => app >> "#" >> x.toString
    given Ordering[Base] = Ordering.by(_.toString)
  }

  extension (sb: SymBase) {
    def toBase: Base = sb match
      case SVar(x) => x
      case SSym(s) => s
  }
}
