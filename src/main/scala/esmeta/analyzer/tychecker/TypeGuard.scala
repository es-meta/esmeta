package esmeta.analyzer.tychecker

import esmeta.cfg.*
import esmeta.ir.{Func => _, *}
import esmeta.ty.*
import esmeta.util.{*, given}
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** type guards */
trait TypeGuardDecl { self: TyChecker =>

  /** type guard */
  case class TypeGuard(map: Map[DemandType, TypeConstr] = Map()) {
    def isEmpty: Boolean = map.isEmpty
    def nonEmpty: Boolean = !isEmpty
    def dtys: Set[DemandType] = map.keySet
    def get(dty: DemandType): Option[TypeConstr] = map.get(dty)

    def apply(dty: DemandType): TypeConstr =
      map.getOrElse(dty, TypeConstr())

    def bases: Set[Base] = map.values.flatMap(_.bases).toSet

    def kill(bases: Set[Base])(using AbsState): TypeGuard = TypeGuard(for {
      (dty, constr) <- map
      newConstr = constr.kill(bases)
      if newConstr.nonTop
    } yield dty -> newConstr)

    def forReturn(symEnv: Map[Sym, ValueTy]): TypeGuard = TypeGuard(for {
      (dty, constr) <- map
      newConstr = constr.forReturn(symEnv)
      if newConstr.nonTop
    } yield dty -> newConstr)

    def filter(ty: ValueTy): TypeGuard =
      TypeGuard(map.filter { (dty, _) => !(dty.ty && ty).isBottom })

    def lift(ty: ValueTy = ValueTy.Top)(using st: AbsState): TypeGuard = 
      this && TypeGuard((for {
        kind <- DemandType.from(ty).toList
        constr = TypeConstr().lift
        if constr.nonTop
      } yield kind -> constr).toMap)

    def has(x: Base): Boolean = map.values.exists(_.has(x))

    def <=(that: TypeGuard): Boolean = that.map.forall { (dty, r) =>
      this.map.get(dty) match
        case Some(l) => l <= r
        case None    => false
    }

    def ||(that: TypeGuard)(lty: ValueTy, rty: ValueTy): TypeGuard =
      val (ldtys, rdtys) = (this.dtys, that.dtys)
      val dtys =
        ldtys.filter(k => (k.ty && rty).isBottom || rdtys.contains(k)) ++
        rdtys.filter(k => (k.ty && lty).isBottom || ldtys.contains(k))
      TypeGuard((for {
        dty <- dtys.toList
        ty = lty || rty
        constr = (this.evaluate(ty, dty.ty), that.evaluate(ty, dty.ty)) match
          case (l, r) if (lty && dty.ty).isBottom => r
          case (l, r) if (rty && dty.ty).isBottom => l
          case (l, r)                             => l || r
        if !constr.isTop
      } yield dty -> constr).toMap)

    def &&(that: TypeGuard): TypeGuard = TypeGuard((for {
      dty <- (this.dtys ++ that.dtys).toList
      constr = this(dty) && that(dty)
      if !constr.isTop
    } yield dty -> constr).toMap)

    def evaluate(lty: ValueTy, rty: ValueTy): TypeConstr =
      if (lty && rty).isBottom then TypeConstr()
      else {
        val constrs = for {
          (dty, constr) <- map
          if rty <= dty.ty
        } yield constr
        if constrs.isEmpty then TypeConstr()
        else constrs.reduce(_ && _)
      }

    override def toString: String = (new Appender >> this).toString
  }
  object TypeGuard {
    val Empty: TypeGuard = TypeGuard()
    def apply(ps: (DemandType, TypeConstr)*): TypeGuard = TypeGuard(
      ps.toMap,
    )
  }

  /** type refinement target */
  enum RefinementTarget:
    case BranchTarget(branch: Branch, isTrue: Boolean)
    case AssertTarget(block: Block, idx: Int)
    def node: Node = this match
      case BranchTarget(branch, _) => branch
      case AssertTarget(block, _)  => block
    def func: Func = cfg.funcOf(node)

  case class DemandType(private val _ty: ValueTy) {
    def ty: ValueTy = _ty
  }

  object DemandType {
    val set: Set[ValueTy] =
      Set(
        TrueT,
        FalseT,
        NormalT,
        AbruptT,
        NormalT(TrueT),
        NormalT(FalseT),
        ENUMT_SYNC,
        ENUMT_ASYNC,
      )

    def apply(ty: ValueTy): DemandType =
      if (DemandType.set.contains(ty)) new DemandType(ty)
      else throw notSupported(s"Unsupported DemandType: $ty")

    def from(givenTy: ValueTy): Set[DemandType] =
      DemandType.set
        .filter(ty => !(givenTy && ty).isBottom)
        .map(DemandType(_))
  }

  case class Provenance(map: Map[Func, List[Call]] = Map()) {
    def depth: Int = map.values.map(_.length).max
    def <=(that: Provenance): Boolean =
      this.map.forall {
        case (lfunc, lcalls) =>
          that.map.get(lfunc).fold(false) { rcalls => lcalls == rcalls }
      }
    def join(that: Provenance): Provenance = Provenance((for {
      key <- (this.map.keySet union that.map.keySet).toList
      calls <- (this.map.get(key), that.map.get(key)) match
        case (Some(lcalls), Some(rcalls)) =>
          Some(if (lcalls.length < rcalls.length) lcalls else rcalls)
        case (Some(lcalls), None) => Some(lcalls)
        case (None, Some(rcalls)) => Some(rcalls)
        case (None, None)         => None
    } yield key -> calls).toMap)
    def forReturn(call: Call): Provenance = Provenance(
      map.map { case (key, calls) => key -> (call :: calls) },
    )
    override def toString: String = (new Appender >> this).toString
  }
  object Provenance {
    def apply(funcs: Func*): Provenance =
      Provenance(funcs.map(_ -> Nil).toMap)
  }

  /** type constraints */
  case class TypeConstr(
    map: Map[Base, (ValueTy, Provenance)] = Map(),
    sexpr: Option[(SymExpr, Provenance)] = None,
  ) {
    def isTop: Boolean = map.isEmpty && sexpr.isEmpty
    def nonTop: Boolean = !isTop
    def <=(that: TypeConstr): Boolean =
      that.map.forall {
        case (x, (rty, rprov)) =>
          this.map.get(x).fold(false) {
            case (lty, lprov) =>
              if (lty == rty) lprov <= rprov
              else lty <= rty
          }
      } && (this.sexpr == that.sexpr)
    def ||(that: TypeConstr): TypeConstr = TypeConstr(
      map = (for {
        x <- (this.map.keySet intersect that.map.keySet).toList
        (lty, lprov) = this.map(x)
        (rty, rprov) = that.map(x)
        ty = lty || rty
        prov = lprov join rprov
      } yield x -> (ty, prov)).toMap,
      sexpr = this.sexpr || that.sexpr,
    )
    def &&(that: TypeConstr): TypeConstr = TypeConstr(
      map = (for {
        x <- (this.map.keySet ++ that.map.keySet).toList
        (lty, lprov) = this.map.getOrElse(x, (AnyT, Provenance()))
        (rty, rprov) = that.map.getOrElse(x, (AnyT, Provenance()))
        ty = lty && rty
        prov = lprov join rprov
      } yield x -> (ty, prov)).toMap,
      sexpr = this.sexpr && that.sexpr,
    )
    def has(x: Base): Boolean =
      map.contains(x) || sexpr.fold(false) { case (sexpr, _) => sexpr.has(x) }
    def bases: Set[Base] =
      map.keySet.collect { case s: Sym => s } ++
      sexpr.fold(Set[Base]()) { (sexpr, _) => sexpr.bases }
    def kill(bases: Set[Base])(using AbsState): TypeConstr =
      this.copy(
        map.filter { case (x, _) => !bases.contains(x) },
        sexpr.fold(None)((e, p) => e.kill(bases).map(_ -> p)),
      )
    def forReturn(symEnv: Map[Sym, ValueTy]): TypeConstr = TypeConstr(
      map = for {
        case (x: Sym, (ty, prov)) <- map
        origTy = symEnv.getOrElse(x, BotT)
      } yield x -> (origTy && ty, prov),
      sexpr = None,
    )
    def depth: Int =
      val provs = map.values.map(_._2).toList
      sexpr.fold(provs)(_._2 :: provs).map(_.depth).max

    def lift(using st: AbsState): TypeConstr =
      this && st.constr

    override def toString: String = (new Appender >> this).toString
  }
  object TypeConstr {
    def apply(pair: (SymExpr, Provenance)): TypeConstr =
      TypeConstr(sexpr = Some(pair))
    def apply(pairs: (Base, (ValueTy, Provenance))*): TypeConstr =
      TypeConstr(pairs.toMap, None)
    def apply(base: Base, ty: ValueTy): TypeConstr = TypeConstr(
      base -> (ty, Provenance()),
    )
  }

  /** symbolic expressions */
  enum SymExpr {
    // case SEBool(b: Boolean)
    case SERef(ref: SymRef)
    case SEExists(ref: SymRef)
    case SETypeCheck(base: SymExpr, ty: ValueTy)
    case SETypeOf(base: SymExpr)
    case SEEq(left: SymExpr, right: SymExpr)
    // case SEOr(left: SymExpr, right: SymExpr)
    // case SEAnd(left: SymExpr, right: SymExpr)
    // case SENot(expr: SymExpr)
    def ||(that: SymExpr): SymExpr = (this, that) match
      case _ if this == that => this
      case _                 => ???
    // case (SEBool(false), _)                    => that
    // case (_, SEBool(false))                    => this
    // case (SEBool(true), _) | (_, SEBool(true)) => SEBool(true)
    // case _                                     => SEOr(this, that)
    def &&(that: SymExpr): SymExpr = (this, that) match
      case _ if this == that => this
      case _                 => ???
    // case (SEBool(true), _)                       => that
    // case (_, SEBool(true))                       => this
    // case (SEBool(false), _) | (_, SEBool(false)) => SEBool(false)
    // case _                                       => SEAnd(this, that)
    def has(x: Base): Boolean = this match
      // case SEBool(b)             => false
      case SERef(ref)            => ref.has(x)
      case SEExists(ref)         => ref.has(x)
      case SETypeCheck(base, ty) => base.has(x)
      case SETypeOf(base)        => base.has(x)
      case SEEq(left, right)     => left.has(x) || right.has(x)
    // case SEOr(left, right)     => left.has(x) || right.has(x)
    // case SEAnd(left, right)    => left.has(x) || right.has(x)
    // case SENot(expr)           => expr.has(x)
    def bases: Set[Base] = this match
      // case SEBool(b)             => Set()
      case SERef(ref)            => ref.bases
      case SEExists(ref)         => ref.bases
      case SETypeCheck(base, ty) => base.bases
      case SETypeOf(base)        => base.bases
      case SEEq(left, right)     => left.bases ++ right.bases
    // case SEOr(left, right)     => left.bases ++ right.bases
    // case SEAnd(left, right)    => left.bases ++ right.bases
    // case SENot(expr)           => expr.bases
    def kill(bases: Set[Base]): Option[SymExpr] = this match
      // case SEBool(b) => Some(this)
      case SERef(ref) =>
        ref.killRef(ref, bases, true).map(SERef(_)) // FIXME: check later
      case SEExists(ref) => ref.killRef(ref, bases, true).map(SEExists(_))
      case SETypeCheck(base, ty) => base.kill(bases).map(SETypeCheck(_, ty))
      case SETypeOf(base)        => base.kill(bases).map(SETypeOf(_))
      case SEEq(left, right) =>
        for {
          l <- left.kill(bases)
          r <- right.kill(bases)
        } yield SEEq(l, r)
    // case SEOr(left, right) =>
    //   for {
    //     l <- left.kill(bases)
    //     r <- right.kill(bases)
    //   } yield SEOr(l, r)
    // case SEAnd(left, right) =>
    //   (left.kill(bases), right.kill(bases)) match
    //     case (Some(l), Some(r)) => Some(l && r)
    //     case (Some(l), None)    => Some(l)
    //     case (None, Some(r))    => Some(r)
    //     case _                  => None
    // case SENot(expr) => expr.kill(bases).map(SENot(_))
    override def toString: String = (new Appender >> this).toString
  }
  object SymExpr {
    // val T: SymExpr = SEBool(true)
    // val F: SymExpr = SEBool(false)
    extension (l: Option[(SymExpr, Provenance)])
      def &&(
        r: Option[(SymExpr, Provenance)],
      ): Option[(SymExpr, Provenance)] = (l, r) match
        case (Some(le, lp), Some(re, rp)) => Some(le && re, lp join rp)
        case (Some(l), None)              => Some(l)
        case (None, Some(r))              => Some(r)
        case _                            => None
      def ||(
        r: Option[(SymExpr, Provenance)],
      ): Option[(SymExpr, Provenance)] = (l, r) match
        case (Some(le, lp), Some(re, rp)) => Some(le || re, lp join rp)
        case _                            => None
  }

  // -----------------------------------------------------------------------------
  // helpers
  // -----------------------------------------------------------------------------
  import tyStringifier.given

  /** SymExpr */
  given Rule[SymExpr] = (app, expr) =>
    import SymExpr.*
    expr match
      // case SEBool(bool)  => app >> bool
      case SERef(ref)    => app >> ref
      case SEExists(ref) => app >> "(exists " >> ref >> ")"
      case SETypeCheck(expr, ty) =>
        app >> "(? " >> expr >> ": " >> ty >> ")"
      case SETypeOf(base) =>
        app >> "(typeof " >> base >> ")"
      case SEEq(left, right) =>
        app >> "(=" >> " " >> left >> " " >> right >> ")"
  // case SEOr(left, right) =>
  //   app >> "(|| " >> left >> " " >> right >> ")"
  // case SEAnd(left, right) =>
  //   app >> "(&& " >> left >> " " >> right >> ")"
  // case SENot(expr) =>
  //   app >> "(! " >> expr >> ")"

  /** Provenance */
  given Rule[Provenance] = (app, prov) =>
    val Provenance(map) = prov
    if (map.nonEmpty) (app >> " <from> ").wrap("{", "}") {
      for ((func, calls) <- map.toList.sortBy(_._1)) {
        app :> func.nameWithId
        if (calls.nonEmpty) {
          for (call <- calls.reverse)
            app :> "<- " >> call.name >> " @ " >> cfg.funcOf(call).nameWithId
        }
      }
    }
    app

  /** TypeConstr */
  given Rule[TypeConstr] = (app, constr) =>
    import TypeConstr.*
    given Rule[(ValueTy, Provenance)] =
      case (app, (ty, prov)) => app >> ty >> prov
    import SymTy.given
    given Rule[Map[Base, (ValueTy, Provenance)]] = sortedMapRule(sep = ": ")
    if (constr.map.nonEmpty) app >> constr.map
    constr.sexpr.fold(app) { (sexpr, prov) => app >> sexpr >> prov }

  /** TypeGuard */
  given Rule[TypeGuard] = (app, guard) =>
    given Ordering[DemandType] = Ordering.by(_.toString)
    given Rule[DemandType] = (app, dty) => app >> dty.ty
    given Rule[Map[DemandType, TypeConstr]] =
      sortedMapRule("{", "}", " => ")
    app >> guard.map

  /** RefinementTarget */
  given Rule[RefinementTarget] = (app, target) =>
    import RefinementTarget.*
    val node = target.node
    val func = target.func
    app >> func.nameWithId >> ":" >> node.name >> ":"
    target match
      case BranchTarget(branch, isTrue) =>
        app >> (if (isTrue) "T" else "F")
      case AssertTarget(block, idx) =>
        app >> idx
  given Ordering[RefinementTarget] = Ordering.by { target =>
    import RefinementTarget.*
    target match
      case BranchTarget(branch, isTrue) => (branch.id, if (isTrue) 1 else 0)
      case AssertTarget(block, idx)     => (block.id, idx)
  }
}
