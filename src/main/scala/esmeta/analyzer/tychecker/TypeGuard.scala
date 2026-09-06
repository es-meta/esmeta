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
    case Elem(map: Map[Base, ValueTy], expr: Option[SymExpr])

    import TypeProp.*
    def isTop: Boolean = this == Top
    def isBottom: Boolean = this == Bot

    def get(x: Base): ValueTy = this match
      case Bot          => BotT
      case Elem(map, _) => map.getOrElse(x, AnyT)

    def map(f: Map[Base, ValueTy] => Map[Base, ValueTy]): TypeProp =
      this match
        case Bot              => Bot
        case Elem(map, sexpr) => Elem(f(map), sexpr)

    def fold[T](default: => T)(f: Map[Base, ValueTy] => T): T = this match
      case Bot          => default
      case Elem(map, _) => f(map)

    def forall(f: Map[Base, ValueTy] => Boolean): Boolean = this match
      case Bot          => true
      case Elem(map, _) => f(map)

    def exists(f: Map[Base, ValueTy] => Boolean): Boolean = this match
      case Bot          => false
      case Elem(map, _) => f(map)

    def <=(that: TypeProp): Boolean = (this, that) match
      case (Bot, _) => true
      case (_, Bot) => true
      case (Elem(lmap, lexpr), Elem(rmap, rexpr)) =>
        rmap.forall { case (r, rty) => lmap.get(r).fold(false) { _ <= rty } } &&
        lexpr == rexpr

    def ||(that: TypeProp): TypeProp = (this, that) match
      case (Bot, _) => that
      case (_, Bot) => this
      case (Elem(lmap, lexpr), Elem(rmap, rexpr)) =>
        Elem(
          (for {
            x <- (lmap.keySet intersect rmap.keySet).toList
            lty = lmap(x)
            rty = rmap(x)
            pair = {
              if (lty <= rty) rty
              else if (rty <= lty) lty
              else lty || rty
            }
          } yield x -> pair).toMap,
          lexpr || rexpr,
        )

    def &&(that: TypeProp): TypeProp = (this, that) match
      case (Bot, _) | (_, Bot) => Bot
      // an absent binding stands for the top type, which the meet keeps
      case (Elem(lmap, lexpr), Elem(rmap, rexpr)) =>
        val sexpr = lexpr && rexpr
        if (rmap.isEmpty) Elem(lmap, sexpr)
        else if (lmap.isEmpty) Elem(rmap, sexpr)
        else
          Elem(
            (for {
              x <- (lmap.keySet ++ rmap.keySet).toList
              lty = lmap.getOrElse(x, AnyT)
              rty = rmap.getOrElse(x, AnyT)
              pair = {
                if (lty <= rty) lty
                else if (rty <= lty) rty
                else lty && rty
              }
            } yield x -> pair).toMap,
            sexpr,
          )

    def sexpr: Option[SymExpr] = this match
      case Bot           => None
      case Elem(_, expr) => expr

    def has(x: Base): Boolean =
      exists(_.contains(x)) || sexpr.exists(_.has(x))

    def bases: Set[Base] = this match
      case Bot              => Set()
      case Elem(map, sexpr) => map.keySet ++ sexpr.fold(Set[Base]())(_.bases)

    def weaken(bases: Set[Base])(using AbsState): TypeProp = this match
      case Bot => Bot
      case Elem(map, sexpr) =>
        Elem(
          map.filter { case (x, _) => !bases.contains(x) },
          sexpr.flatMap(_.weaken(bases)),
        )

    def nonTop: Boolean = !isTop

    def weaken(effect: Effect): TypeProp = this match
      case Bot => Bot
      case Elem(map, _) =>
        Elem(map.map { case (x, ty) => x -> effect(ty) }, None)

    def fieldUpdate(fld: String, ty: ValueTy): TypeProp = this match
      case Bot => Bot
      case Elem(map, _) =>
        Elem(
          map.map {
            case (x, oty) =>
              x -> oty.copied(record =
                oty.record.update(fld, ty, refine = false),
              )
          },
          None,
        )

    def bind(using st: AbsState): TypeProp = this && st.constr

    def hasLocal: Boolean = bases.exists {
      case _: Local => true
      case _        => false
    }

    def hasSym: Boolean = bases.exists {
      case _: Sym => true
      case _      => false
    }

    def onlySym: TypeProp = this match
      case Bot => Bot
      case Elem(map, _) =>
        Elem(map.collect { case (x: Sym, ty) => x -> ty }, None)

    override def toString: String = (new Appender >> this).toString
  }
  object TypeProp {
    val Top: TypeProp = Elem(Map(), None)
    def apply(pairs: (Base, ValueTy)*): TypeProp = Elem(pairs.toMap, None)
    def apply(sexpr: SymExpr): TypeProp = Elem(Map(), Some(sexpr))
  }

  enum SymExpr {
    case SEBool(b: Boolean)
    case SERef(ref: SymRef)
    case SEExists(ref: SymRef)
    case SETypeCheck(base: SymExpr, ty: ValueTy)
    case SETypeOf(base: SymExpr)
    case SEEq(left: SymExpr, right: SymExpr)
    def ||(that: SymExpr): SymExpr = (this, that) match
      case _ if this == that  => this
      case (SEBool(false), _) => that
      case (_, SEBool(false)) => this
      case _                  => SEBool(true)
    def &&(that: SymExpr): SymExpr = (this, that) match
      case _ if this == that                       => this
      case (SEBool(true), _)                       => that
      case (_, SEBool(true))                       => this
      case (SEBool(false), _) | (_, SEBool(false)) => SEBool(false)
      case _                                       => SEBool(true)
    def has(x: Base): Boolean = this match
      case SEBool(_)            => false
      case SERef(ref)           => ref.has(x)
      case SEExists(ref)        => ref.has(x)
      case SETypeCheck(base, _) => base.has(x)
      case SETypeOf(base)       => base.has(x)
      case SEEq(left, right)    => left.has(x) || right.has(x)
    def bases: Set[Base] = this match
      case SEBool(_)            => Set()
      case SERef(ref)           => ref.bases
      case SEExists(ref)        => ref.bases
      case SETypeCheck(base, _) => base.bases
      case SETypeOf(base)       => base.bases
      case SEEq(left, right)    => left.bases ++ right.bases
    def weaken(bases: Set[Base]): Option[SymExpr] = this match
      case SEBool(_)     => Some(this)
      case SERef(ref)    => ref.weakenRef(ref, bases, true).map(SERef(_))
      case SEExists(ref) => ref.weakenRef(ref, bases, true).map(SEExists(_))
      case SETypeCheck(base, ty) =>
        base.weaken(bases).map(SETypeCheck(_, ty))
      case SETypeOf(base) => base.weaken(bases).map(SETypeOf(_))
      case SEEq(left, right) =>
        for {
          l <- left.weaken(bases)
          r <- right.weaken(bases)
        } yield SEEq(l, r)
    override def toString: String = (new Appender >> this).toString
  }
  object SymExpr {
    extension (l: Option[SymExpr])
      def &&(r: Option[SymExpr]): Option[SymExpr] = (l, r) match
        case (Some(le), Some(re)) => Some(le && re)
        case (Some(_), None)      => l
        case (None, Some(_))      => r
        case _                    => None
      def ||(r: Option[SymExpr]): Option[SymExpr] = (l, r) match
        case (Some(le), Some(re)) => Some(le || re)
        case _                    => None
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

  /** SymExpr */
  given Rule[SymExpr] = symExprRule
  private def symExprRule(app: Appender, expr: SymExpr): Appender =
    import SymExpr.*, SymTy.given
    expr match
      case SEBool(bool)  => app >> bool
      case SERef(ref)    => app >> ref
      case SEExists(ref) => app >> "(exists " >> ref >> ")"
      case SETypeCheck(e, ty) =>
        symExprRule(app >> "(? ", e) >> ": " >> ty >> ")"
      case SETypeOf(base) => symExprRule(app >> "(typeof ", base) >> ")"
      case SEEq(left, right) =>
        val a = symExprRule(app >> "(= ", left)
        symExprRule(a >> " ", right) >> ")"

  /** TypeProp */
  given Rule[TypeProp] = (app, constr) =>
    import TypeProp.*
    import SymTy.given
    given Rule[Map[Base, ValueTy]] = sortedMapRule(sep = ": ")
    constr match
      case Bot => app >> "⊥"
      case Elem(map, sexpr) =>
        if (map.nonEmpty) app >> map
        sexpr.fold(app)(app >> _)

}
