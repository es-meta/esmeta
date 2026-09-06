package esmeta.analyzer.tychecker

import esmeta.cfg.*
import esmeta.ir.{Func => _, *}
import esmeta.ty.*
import esmeta.util.{*, given}
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.exists

/** type guards */
trait TypeGuardDecl { self: TyChecker =>

  /** type guard */
  case class TypeGuard(map: Map[TargetType, TypeProp] = Map()) {
    def isEmpty: Boolean = map.isEmpty
    def nonEmpty: Boolean = !isEmpty
    def dtys: Set[TargetType] = map.keySet

    def apply(dty: TargetType): TypeProp =
      map.getOrElse(dty, TypeProp.Top)

    def bases: Set[Base] = map.values.flatMap(_.bases).toSet

    def weaken(bases: Set[Base])(using AbsState): TypeGuard = TypeGuard(for {
      (dty, constr) <- map
      newConstr = constr.weaken(bases)
      if !newConstr.isTop
    } yield dty -> newConstr)

    def weaken(effect: Effect): TypeGuard = TypeGuard(for {
      (dty, constr) <- map
      newConstr = constr.weaken(effect)
      if !newConstr.isTop
    } yield dty -> newConstr)

    def lookup(ty: ValueTy): TypeProp =
      if (map.isEmpty) TypeProp.Top
      else
        var acc: TypeProp = null
        for ((dty, c) <- map if ty <= dty.ty)
          acc = if (acc eq null) c else acc && c
        if (acc eq null) TypeProp.Top else acc
    def apply(ty: ValueTy): TypeProp = lookup(ty)

    def update(ty: ValueTy, constr: TypeProp): TypeGuard = TypeGuard(
      (for dty <- TargetType.allTargets yield dty -> {
        val c = map.getOrElse(dty, TypeProp.Top)
        if (ty <= dty.ty) constr && c else c
      }).toMap.filter { case (_, c) => !c.isTop },
    )

    def refine(ty: ValueTy): TypeGuard =
      if (map.isEmpty) this
      else
        TypeGuard(for {
          (dty, _) <- map
          if ty overlaps dty.ty
        } yield dty -> lookup(dty.ty))

    def fieldLookup(fld: String): TypeGuard =
      val m = for {
        (dty, c) <- map
        ity = dty.ty.record(fld).value
        if TargetType.set.contains(ity)
      } yield TargetType(ity) -> c
      m.foldLeft(TypeGuard.Empty) {
        case (acc, (dty, c)) => acc.update(dty.ty, c)
      }

    def fieldUpdate(fld: String, ty: ValueTy): TypeGuard =
      map.foldLeft(TypeGuard.Empty) {
        case (acc, (dty, c)) => acc.update(dty.ty, c.fieldUpdate(fld, ty))
      }

    def filter(ty: ValueTy): TypeGuard =
      TypeGuard(map.filter { (dty, _) => dty.ty overlap ty })

    def has(x: Base): Boolean = map.values.exists(_.has(x))

    def derive(fromTy: ValueTy, toTy: ValueTy): TypeProp =
      val ty = fromTy && toTy
      map
        .collect { case (dty, constr) if ty <= dty.ty => constr }
        .foldLeft(TypeProp.Top)(_ && _)

    def hasLocal: Boolean = map.values.exists(_.hasLocal)

    def hasSym: Boolean = map.values.exists(_.hasSym)

    def onlySym: TypeGuard = TypeGuard(
      map.map { (dty, constr) => dty -> constr.onlySym },
    )

    def normalized(upper: ValueTy): TypeGuard = TypeGuard(
      map.filter((dty, constr) => (dty.ty overlap upper) && !constr.isTop),
    )

    override def toString: String = (new Appender >> this).toString
  }
  object TypeGuard {
    val Empty: TypeGuard = TypeGuard()
    def apply(ps: (TargetType, TypeProp)*): TypeGuard = TypeGuard(
      ps.toMap,
    )
  }
  extension (lpair: (ValueTy, TypeGuard)) {
    def <=(rpair: (ValueTy, TypeGuard)): Boolean = {
      val (luty, lguard) = lpair
      val (ruty, rguard) = rpair
      luty <= ruty &&
      rguard.map.forall { (dty, constr) =>
        (luty distinct dty.ty) || lguard(dty) <= constr
      }
    }
    def ||(rpair: (ValueTy, TypeGuard)): TypeGuard = {
      val (luty, lguard) = lpair
      val (ruty, rguard) = rpair
      val ty = luty || ruty
      TypeGuard(
        (for {
          dty <- (lguard.dtys ++ rguard.dtys).toList
          constr = {
            (if (dty.ty overlap luty) lguard(dty) else TypeProp.Bot) ||
            (if (dty.ty overlap ruty) rguard(dty) else TypeProp.Bot)
          }
          if !constr.isTop
        } yield dty -> constr).toMap,
      )
    }
    def &&(rpair: (ValueTy, TypeGuard)): TypeGuard = {
      val (luty, lguard) = lpair
      val (ruty, rguard) = rpair
      val ty = luty && ruty
      TypeGuard(
        (for {
          dty <- (lguard.dtys ++ rguard.dtys).toList
          if dty.ty overlap ty
          constr = lguard(dty) && rguard(dty)
        } yield dty -> constr).toMap,
      )
    }
    def add(constr: TypeProp): TypeGuard =
      val (uty, guard) = lpair
      TypeGuard(
        TargetType.from(uty).map(dty => dty -> (guard(dty) && constr)).toMap,
      )
  }

  case class TargetType(ty: ValueTy)

  object TargetType {
    val all: List[ValueTy] = List(
      TrueT,
      FalseT,
      NormalT,
      AbruptT,
      NormalT(TrueT),
      NormalT(FalseT),
      ENUMT_SYNC,
      ENUMT_ASYNC,
    )
    val set: Set[ValueTy] = all.toSet

    val allTargets: List[TargetType] = all.map(TargetType(_))

    def from(givenTy: ValueTy): List[TargetType] =
      allTargets.filter(dty => givenTy overlaps dty.ty)
  }

  /** type constraints */
  enum TypeProp {
    case Bot
    case Elem(map: Map[Base, ValueTy])

    import TypeProp.*
    def isTop: Boolean = this == Top
    def isBottom: Boolean = this == Bot

    def get(x: Base): ValueTy = this match
      case Bot       => BotT
      case Elem(map) => map.getOrElse(x, AnyT)

    def map(f: Map[Base, ValueTy] => Map[Base, ValueTy]): TypeProp =
      this match
        case Bot       => Bot
        case Elem(map) => Elem(f(map))

    def fold[T](default: => T)(f: Map[Base, ValueTy] => T): T = this match
      case Bot       => default
      case Elem(map) => f(map)

    def forall(f: Map[Base, ValueTy] => Boolean): Boolean = this match
      case Bot       => true
      case Elem(map) => f(map)

    def exists(f: Map[Base, ValueTy] => Boolean): Boolean = this match
      case Bot       => false
      case Elem(map) => f(map)

    def <=(that: TypeProp): Boolean = (this, that) match
      case (Bot, _) => true
      case (_, Bot) => true
      case (Elem(lmap), Elem(rmap)) =>
        rmap.forall { case (r, rty) => lmap.get(r).fold(false) { _ <= rty } }

    def ||(that: TypeProp): TypeProp = (this, that) match
      case (Bot, _) => that
      case (_, Bot) => this
      case (Elem(lmap), Elem(rmap)) =>
        Elem((for {
          x <- (lmap.keySet intersect rmap.keySet).toList
          lty = lmap(x)
          rty = rmap(x)
          pair = {
            if (lty <= rty) rty
            else if (rty <= lty) lty
            else lty || rty
          }
        } yield x -> pair).toMap)

    def &&(that: TypeProp): TypeProp = (this, that) match
      case (Bot, _) | (_, Bot) => Bot
      // an absent binding stands for the top type, which the meet keeps
      case (Elem(lmap), Elem(rmap)) if rmap.isEmpty => this
      case (Elem(lmap), Elem(rmap)) if lmap.isEmpty => that
      case (Elem(lmap), Elem(rmap)) =>
        Elem((for {
          x <- (lmap.keySet ++ rmap.keySet).toList
          lty = lmap.getOrElse(x, AnyT)
          rty = rmap.getOrElse(x, AnyT)
          pair = {
            if (lty <= rty) lty
            else if (rty <= lty) rty
            else lty && rty
          }
        } yield x -> pair).toMap)

    def has(x: Base): Boolean = exists(_.contains(x))

    def bases: Set[Base] = this match
      case Bot       => Set()
      case Elem(map) => map.keySet.collect { case s: Sym => s }

    def weaken(bases: Set[Base])(using AbsState): TypeProp =
      map(_.filter { case (x, _) => !bases.contains(x) })

    def nonTop: Boolean = !isTop

    def weaken(effect: Effect): TypeProp =
      map(_.map { case (x, ty) => x -> effect(ty) })

    def fieldUpdate(fld: String, ty: ValueTy): TypeProp =
      map(_.map {
        case (x, oty) =>
          x -> oty.copied(record = oty.record.update(fld, ty, refine = false))
      })

    def bind(using st: AbsState): TypeProp = this && st.constr

    def hasLocal: Boolean = exists(_.keySet.exists {
      case _: Local => true
      case _        => false
    })

    def hasSym: Boolean = exists(_.keySet.exists {
      case s: Sym => true
      case _      => false
    })

    def onlySym: TypeProp =
      map(_.collect { case (x: Sym, ty) => x -> ty })

    override def toString: String = (new Appender >> this).toString
  }
  object TypeProp {
    val Top: TypeProp = Elem(Map())
    def apply(pairs: (Base, ValueTy)*): TypeProp = Elem(pairs.toMap)
  }
  // -----------------------------------------------------------------------------
  // helpers
  // -----------------------------------------------------------------------------
  import tyStringifier.given

  /** TypeGuard */
  given Rule[TypeGuard] = (app, guard) =>
    given Ordering[TargetType] = Ordering.by(_.toString)
    given Rule[TargetType] = (app, dty) => app >> dty.ty
    given Rule[Map[TargetType, TypeProp]] = sortedMapRule("{", "}", " => ")
    app >> guard.map

  /** TypeProp */
  given Rule[TypeProp] = (app, constr) =>
    import TypeProp.*
    import SymTy.given
    given Rule[Map[Base, ValueTy]] = sortedMapRule(sep = ": ")
    constr match
      case Bot => app >> "⊥"
      case Elem(map) =>
        if (map.nonEmpty) app >> map
        app

}
