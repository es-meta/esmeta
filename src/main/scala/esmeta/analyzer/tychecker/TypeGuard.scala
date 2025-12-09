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
  case class TypeGuard(map: Map[DemandType, TypeProp] = Map()) {
    def isEmpty: Boolean = map.isEmpty
    def nonEmpty: Boolean = !isEmpty
    def dtys: Set[DemandType] = map.keySet
    def get(dty: DemandType): Option[TypeProp] = map.get(dty)

    def apply(dty: DemandType): TypeProp =
      map.getOrElse(dty, TypeProp())

    def bases: Set[Base] = map.values.flatMap(_.bases).toSet

    def kill(bases: Set[Base])(using AbsState): TypeGuard = TypeGuard(for {
      (dty, prop) <- map
      newProp = prop.kill(bases)
      if newProp.nonTop
    } yield dty -> newProp)

    def forReturn(symEnv: Map[Sym, ValueTy]): TypeGuard = TypeGuard(for {
      (dty, prop) <- map
      newProp = prop.forReturn(symEnv)
      if newProp.nonTop
    } yield dty -> newProp)

    def filter(ty: ValueTy): TypeGuard =
      TypeGuard(map.filter { (dty, _) => !(dty.ty && ty).isBottom })

    def lift(ty: ValueTy = ValueTy.Top)(using st: AbsState): TypeGuard =
      this && TypeGuard((for {
        kind <- DemandType.from(ty).toList
        prop = TypeProp().lift
        if prop.nonTop
      } yield kind -> prop).toMap)

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
        prop = (this.evaluate(ty, dty.ty), that.evaluate(ty, dty.ty)) match
          case (l, r) if (lty && dty.ty).isBottom => r
          case (l, r) if (rty && dty.ty).isBottom => l
          case (l, r)                             => l || r
        if !prop.isTop
      } yield dty -> prop).toMap)

    def &&(that: TypeGuard): TypeGuard = TypeGuard((for {
      dty <- (this.dtys ++ that.dtys).toList
      prop = this(dty) && that(dty)
      if !prop.isTop
    } yield dty -> prop).toMap)

    def evaluate(lty: ValueTy, rty: ValueTy): TypeProp =
      if (lty && rty).isBottom then TypeProp()
      else {
        val props = for {
          (dty, prop) <- map
          if rty <= dty.ty
        } yield prop
        if props.isEmpty then TypeProp()
        else props.reduce(_ && _)
      }

    def simple: TypeGuard =
      TypeGuard(map.filterNot { (dty, prop) =>
        map.exists { (tdty, tconstr) =>
          (dty.ty != tdty.ty && dty.ty <= tdty.ty && tconstr <= prop)
        }
      })

    override def toString: String = (new Appender >> this).toString
  }
  object TypeGuard {
    val Empty: TypeGuard = TypeGuard()
    def apply(ps: (DemandType, TypeProp)*): TypeGuard = TypeGuard(
      ps.toMap,
    )
  }

  /** type refinement target */
  enum RefinementTarget:
    case BranchTarget(branch: Branch, isTrue: Boolean)
    case AssertTarget(block: Block, idx: Int)
    case NodeTarget(nd: Node)
    def node: Node = this match
      case BranchTarget(branch, _) => branch
      case AssertTarget(block, _)  => block
      case NodeTarget(nd)          => nd
    def func: Func = cfg.funcOf(node)

  case class DemandType(private val _ty: ValueTy) {
    def ty: ValueTy = _ty
  }

  object DemandType {
    val set: Set[ValueTy] =
      if (useBooleanGuard) Set(TrueT, FalseT)
      else
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
      else {
        Thread.dumpStack()
        throw notSupported(s"Unsupported DemandType: $ty")
      }

    def from(givenTy: ValueTy): Set[DemandType] =
      DemandType.set
        .filter(ty => !(givenTy && ty).isBottom)
        .map(DemandType(_))
  }

  /** type propositions */
  case class TypeProp(
    map: Map[Base, (ValueTy, Provenance)] = Map(),
    sexpr: Option[SymExpr] = None,
  ) {
    def isTop: Boolean = map.isEmpty && sexpr.isEmpty

    def nonTop: Boolean = !isTop

    def <=(that: TypeProp): Boolean =
      that.map.forall {
        case (x, (rty, rprov)) =>
          this.map.get(x).fold(false) {
            case (lty, lprov) =>
              if (lty == rty) lprov <= rprov
              else lty <= rty
          }
      } && (this.sexpr == that.sexpr)

    def ||(that: TypeProp): TypeProp =
      TypeProp(
        map = (for {
          x <- (this.map.keySet intersect that.map.keySet).toList
          (lty, lprov) = this.map(x)
          (rty, rprov) = that.map(x)
          pair = {
            if (lty <= rty) (rty, rprov)
            else if (rty <= lty) (lty, lprov)
            else (lty || rty, lprov || rprov)
          }
        } yield x -> pair).toMap,
        sexpr = this.sexpr || that.sexpr,
      )

    def &&(that: TypeProp): TypeProp =
      TypeProp(
        map = (for {
          x <- (this.map.keySet ++ that.map.keySet).toList
          (lty, lprov) = this.map.getOrElse(x, (ValueTy.Top, Provenance.Top))
          (rty, rprov) = that.map.getOrElse(x, (ValueTy.Top, Provenance.Top))
          pair = {
            if (lty <= rty) (lty, lprov)
            else if (rty <= lty) (rty, rprov)
            else (lty && rty, lprov && rprov)
          }
        } yield x -> pair).toMap,
        sexpr = this.sexpr && that.sexpr,
      )

    def has(x: Base): Boolean =
      map.contains(x) || sexpr.fold(false)(_.has(x))

    def bases: Set[Base] =
      map.keySet.collect { case s: Sym => s } ++
      sexpr.fold(Set[Base]())(_.bases)

    def kill(bases: Set[Base])(using AbsState): TypeProp =
      this.copy(
        map.filter { case (x, _) => !bases.contains(x) },
        sexpr.fold(None)(_.kill(bases)),
      )

    def forReturn(symEnv: Map[Sym, ValueTy]): TypeProp = TypeProp(
      map = for {
        case (x: Sym, (ty, prov)) <- map
        origTy = symEnv.getOrElse(x, BotT)
      } yield x -> (origTy && ty, prov),
      sexpr = None,
    )

    def depth: Int = map.values.map(_._2).map(_.depth).max

    def lift(using st: AbsState): TypeProp =
      this && st.prop

    override def toString: String = (new Appender >> this).toString
  }
  object TypeProp {
    def apply(sexpr: SymExpr): TypeProp =
      TypeProp(sexpr = Some(sexpr))
    def apply(pairs: (Base, (ValueTy, Provenance))*): TypeProp =
      TypeProp(pairs.toMap, None)
  }

  /** symbolic expressions */
  enum SymExpr {
    case SEBool(b: Boolean)
    case SERef(ref: SymRef)
    case SEExists(ref: SymRef)
    case SETypeCheck(base: SymExpr, ty: ValueTy)
    case SETypeOf(base: SymExpr)
    case SEEq(left: SymExpr, right: SymExpr)
    def ||(that: SymExpr): SymExpr = (this, that) match
      case _ if this == that                     => this
      case (SEBool(false), _)                    => that
      case (_, SEBool(false))                    => this
      case (SEBool(true), _) | (_, SEBool(true)) => SEBool(true)
      case _                                     => SEBool(true)
    def &&(that: SymExpr): SymExpr = (this, that) match
      case _ if this == that                       => this
      case (SEBool(true), _)                       => that
      case (_, SEBool(true))                       => this
      case (SEBool(false), _) | (_, SEBool(false)) => SEBool(false)
      case _                                       => SEBool(true)
    def has(x: Base): Boolean = this match
      case SEBool(b)             => false
      case SERef(ref)            => ref.has(x)
      case SEExists(ref)         => ref.has(x)
      case SETypeCheck(base, ty) => base.has(x)
      case SETypeOf(base)        => base.has(x)
      case SEEq(left, right)     => left.has(x) || right.has(x)
    def bases: Set[Base] = this match
      case SEBool(b)             => Set()
      case SERef(ref)            => ref.bases
      case SEExists(ref)         => ref.bases
      case SETypeCheck(base, ty) => base.bases
      case SETypeOf(base)        => base.bases
      case SEEq(left, right)     => left.bases ++ right.bases
    def kill(bases: Set[Base]): Option[SymExpr] = this match
      case SEBool(b) => Some(this)
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
    override def toString: String = (new Appender >> this).toString
  }
  object SymExpr {
    // val T: SymExpr = SEBool(true)
    // val F: SymExpr = SEBool(false)
    extension (l: Option[SymExpr])
      def &&(
        r: Option[SymExpr],
      ): Option[SymExpr] = (l, r) match
        case (Some(le), Some(re)) => Some(le && re)
        case (Some(l), None)      => Some(l)
        case (None, Some(r))      => Some(r)
        case _                    => None
      def ||(
        r: Option[SymExpr],
      ): Option[SymExpr] = (l, r) match
        case (Some(le), Some(re)) => Some(le || re)
        case _                    => None
  }

  /** Provenance */

  sealed trait Provenance {
    import Provenance.*

    def ty: ValueTy
    def size: Int
    def depth: Int
    def leafCnt: Int

    // Infer a consistent boolean truth value from leaves, when available.
    // Returns Some(true/false) only if all annotated leaves agree; otherwise None.
    final def truthOpt: Option[Boolean] = this match
      case Leaf(_, _, _, truth)  => truth
      case CallPath(_, _, child) => child.truthOpt
      case Join(children) =>
        val ts = children.flatMap(_.truthOpt)
        if (ts.isEmpty) None
        else if (ts.forall(_ == ts.head)) Some(ts.head)
        else None
      case Meet(children) =>
        val ts = children.flatMap(_.truthOpt)
        if (ts.isEmpty) None
        else if (ts.forall(_ == ts.head)) Some(ts.head)
        else None
      case RefinePoint(_, child, _, _, _, _) => child.truthOpt
      case Provenance.Bot | Provenance.Top   => None
      case Placeholder(_)                    => ???

    // simple explanation is bigger (imprecise)
    def <=(that: Provenance): Boolean = {
      if this == Bot then true
      else if that == Bot then false
      else if this == Top then false
      else if that == Top then true
      else // this and that are not Bot or Top
        (this, that) match
          case (Leaf(lnode, _, lty, _), Leaf(rnode, _, rty, _)) =>
            if lty == rty then lnode == rnode
            else lty <= rty
          case (CallPath(lcall, lty, lchild), CallPath(rcall, rty, rchild)) =>
            if lty == rty then lcall == rcall && lchild <= rchild
            else lty <= rty
          case (l @ Join(lchild), r @ Join(rchild)) =>
            if l.ty == r.ty then rchild.forall(l => lchild.exists(l <= _))
            else l.ty <= r.ty
          case (l @ Meet(lchild), r @ Meet(rchild)) =>
            if l.ty == r.ty then rchild.forall(l => lchild.exists(l <= _))
            else l.ty <= r.ty
          case _ => false
    }

    def ||(that: Provenance): Provenance = {
      if this == Bot then return that
      else if that == Bot then return this
      else if this == Top || that == Top then return Top
      else // this and that are not Bot or Top
      if this.ty <= that.ty then return that
      else if that.ty <= this.ty then return this

      // this and that are not subsumption of each other
      (this, that) match
        case (
              Leaf(lnode, lbase, lty, ltruth),
              Leaf(rnode, rbase, rty, rtruth),
            ) =>
          if lnode == rnode then
            val base = if (lbase == rbase) lbase else None
            val truth = if (ltruth == rtruth) ltruth else None
            Leaf(lnode, base, lty || rty, truth)
          else Join(Set(this, that)).canonical
        case (CallPath(lcall, lty, lchild), CallPath(rcall, rty, rchild)) =>
          if lcall == rcall then CallPath(lcall, lty || rty, lchild || rchild)
          else Join(Set(this, that)).canonical
        case (prov: (Leaf | CallPath), Join(child)) =>
          Join(child + prov).canonical
        case (Join(child), prov: (Leaf | CallPath)) =>
          Join(child + prov).canonical
        case (Join(lchild), Join(rchild)) =>
          Join(lchild ++ rchild).canonical
        case _ => Join(Set(this, that)).canonical
    }

    def &&(that: Provenance): Provenance = {
      if this == Bot || that == Bot then return Bot
      else if this == Top then return that
      else if that == Top then return this
      else // this and that are not Bot or Top
      if this.ty <= that.ty then return this
      else if that.ty <= this.ty then return that

      // this and that are not subsumption of each other
      (this, that) match
        case (
              Leaf(lnode, lbase, lty, ltruth),
              Leaf(rnode, rbase, rty, rtruth),
            ) =>
          if lnode == rnode then
            val base = if (lbase == rbase) lbase else None
            val truth = if (ltruth == rtruth) ltruth else None
            Leaf(lnode, base, lty && rty, truth)
          else Meet(Set(this, that)).canonical
        case (CallPath(lcall, lty, lchild), CallPath(rcall, rty, rchild)) =>
          if lcall == rcall then CallPath(lcall, lty && rty, lchild && rchild)
          else Meet(Set(this, that)).canonical
        case (prov: (Leaf | CallPath), Meet(child)) =>
          Meet(child + prov).canonical
        case (Meet(child), prov: (Leaf | CallPath)) =>
          Meet(child + prov).canonical
        case (Meet(lchild), Meet(rchild)) =>
          Meet(lchild ++ rchild).canonical
        case _ => Meet(Set(this, that)).canonical
    }

    def existsDuplicateCall(c: Call): Boolean =
      this match
        case CallPath(call, _, child) =>
          if call == c then true
          else child.existsDuplicateCall(c)
        case Join(child) => child.exists(_.existsDuplicateCall(c))
        case Meet(child) => child.exists(_.existsDuplicateCall(c))
        case _           => false

    def replaceCall(
      call: Call,
      ty: ValueTy,
    ): Provenance = // ONLY WORKS WITH NO VIEW
      this match
        case CallPath(c, _, child) =>
          if c == call then CallPath(c, ty, child)
          else CallPath(c, ty, child.replaceCall(call, ty))
        case Join(child) => Join(child.map(_.replaceCall(call, ty)))
        case Meet(child) => Meet(child.map(_.replaceCall(call, ty)))
        case _           => this

    def canonical: Provenance =
      this match
        case Join(set) =>
          val group = set
            .groupBy(_.ty)
            .map {
              case (ty, dupl) =>
                ty -> dupl.minBy(_.depth)
            }
            .toMap
          Join(
            group
              .filterNot {
                case (ty, _) =>
                  group.keySet.exists(otherTy =>
                    (ty <= otherTy && otherTy != ty),
                  )
              }
              .values
              .toSet,
          )
        case Meet(set) =>
          val group = set
            .groupBy(_.ty)
            .map {
              case (ty, dupl) =>
                ty -> dupl.minBy(_.depth)
            }
            .toMap
          Meet(
            group
              .filterNot {
                case (ty, _) =>
                  group.keySet.exists(otherTy =>
                    (otherTy <= ty && otherTy != ty),
                  )
              }
              .values
              .toSet,
          )
        case _ => this

    def forReturn(call: Call, ty: ValueTy): Provenance =
      if this == Bot then Bot
      else if this == Top then Top
      else if this.existsDuplicateCall(call) then this.replaceCall(call, ty)
      else CallPath(call, ty, this)

    def usedForRefine(
      target: RefinementTarget,
      base: Base,
      refinedTo: ValueTy,
      refinedVarTy: ValueTy,
    ): Provenance =
      val branch: Option[Boolean] = target match
        case RefinementTarget.BranchTarget(_, isTrue) => Some(isTrue)
        case _                                        => None
      RefinePoint(
        target = target,
        child = this,
        base = Some(base),
        branch = branch,
        refined = Some(refinedTo),
        varRefined = Some(refinedVarTy),
      )

    def toTree(indent: Int): String
    def toTree: String = toTree(0)
  }

  // Provenance placeholder used to mask complex functions like GetFunctionRealm
  case class Placeholder(name: String) extends Provenance {
    lazy val ty: ValueTy = BotT
    def size: Int = 0
    def depth: Int = 0
    def leafCnt: Int = 0
    override def toString: String =
      s"$name: No explanation provided (too complex)"
    def toTree(indent: Int): String = s"${"  " * indent}$toString"
  }

  // Leaf provenance: attach base and node to recover spec text later
  case class Leaf(
    node: Node,
    base: Option[Base],
    ty: ValueTy,
    truth: Option[Boolean] = None,
  ) extends Provenance {
    def size: Int = 0
    def depth: Int = 0
    def leafCnt: Int = 1

    override def toString =
      s"Leaf(${cfg.funcOf(node).nameWithId}:${node.id}, $ty)"
    def toTree(indent: Int): String = s"${"  " * indent}$this"
  }
  case class CallPath(call: Call, ty: ValueTy, child: Provenance)
    extends Provenance {
    def size: Int = child.size + 1
    def depth: Int = child.depth + 1
    def leafCnt: Int = child.leafCnt

    override def toString =
      s"CallPath(${cfg.funcOf(call).nameWithId}:${call.id}, $ty, ${child.toString})"
    def toTree(indent: Int): String =
      s"${"  " * indent}CallPath(${cfg.funcOf(call).nameWithId}:${call.id}, $ty)\n${child
        .toTree(indent + 1)}"
  }
  case class Join(child: Set[Provenance]) extends Provenance {
    lazy val ty: ValueTy = child.toList.map(_.ty).reduce(_ || _)
    def size: Int = child.toList.map(_.size).sum + 1
    def depth: Int = child.toList.map(_.depth).max + 1
    def leafCnt: Int = child.toList.map(_.leafCnt).sum

    override def toString = s"Join(${child.toList.map(_.toString)})"
    def toTree(indent: Int): String =
      s"${"  " * indent}Join($ty)\n${child.toList.map(_.toTree(indent + 1)).mkString("\n")}"
  }

  case class Meet(child: Set[Provenance]) extends Provenance {
    lazy val ty: ValueTy = child.toList.map(_.ty).reduce(_ && _)
    def size: Int = child.toList.map(_.size).sum + 1
    def depth: Int = child.toList.map(_.depth).max + 1
    def leafCnt: Int = child.toList.map(_.leafCnt).sum

    override def toString = s"Meet(${child.toList.map(_.toString)})"
    def toTree(indent: Int): String =
      s"${"  " * indent}Meet($ty)\n${child.toList.map(_.toTree(indent + 1)).mkString("\n")}"
  }

  case class RefinePoint(
    target: RefinementTarget,
    child: Provenance,
    base: Option[Base] = None,
    branch: Option[Boolean] = None,
    refined: Option[ValueTy] = None, // e.g., True/False for branch condition
    varRefined: Option[ValueTy] =
      None, // refined variable type to show in header
  ) extends Provenance {
    lazy val ty: ValueTy = child.ty
    def size: Int = child.size
    def depth: Int = child.depth
    def leafCnt: Int = child.leafCnt

    override def toString = s"RefinePoint($target, $child)"
    def toTree(indent: Int): String =
      s"${"  " * indent}RefinePoint($target)\n${child.toTree(indent + 1)}"
  }
  object Provenance {
    case object Bot extends Provenance {
      lazy val ty: ValueTy = BotT
      val INF = 1e8.toInt
      def size: Int = INF
      def depth: Int = INF
      def leafCnt: Int = INF

      override def toString = "Bot"
      def toTree(indent: Int): String = s"${"  " * indent}$this"
    }
    case object Top extends Provenance {
      lazy val ty: ValueTy = BotT
      def size: Int = 0
      def depth: Int = 0
      def leafCnt: Int = 0

      override def toString = "Top"
      def toTree(indent: Int): String = s"${"  " * indent}$this"
    }

    // Create a leaf provenance without base information
    def apply(ty: ValueTy)(using nd: Node): Provenance =
      if useProvenance then Leaf(nd, None, ty, None) else Bot

    // Overloaded constructor that requires a base
    def apply(
      base: Base,
      ty: ValueTy,
      truth: Option[Boolean] = None,
    )(using nd: Node): Provenance =
      if useProvenance then Leaf(nd, Some(base), ty, truth) else Bot

    // Check whether a provenance tree involves a function by name
    def involvesFunc(prov: Provenance, fname: String): Boolean = prov match
      case Leaf(node, _, _, _) =>
        val f = cfg.funcOf(node)
        f.name == fname || f.irFunc.name == fname
      case CallPath(call, _, child) =>
        // Detect direct calls to a target algorithm by closure name or SDO method
        val direct = call.callInst match
          case ICall(_, fexpr, _) =>
            fexpr match
              case EClo(fn, _) => fn == fname
              case ECont(_)    => false
              case _           => false
          case ISdoCall(_, _, method, _) => method == fname
        direct || involvesFunc(child, fname)
      case Join(children) => children.exists(involvesFunc(_, fname))
      case Meet(children) => children.exists(involvesFunc(_, fname))
      case RefinePoint(target, child, _, _, _, _) =>
        val f = target.func
        f.name == fname || f.irFunc.name == fname || involvesFunc(child, fname)
      case Bot | Top      => false
      case Placeholder(_) => ???

    // Mask provenance tree with a placeholder if it involves an exception function
    private val ExceptionFuncs: Set[String] = Set("GetFunctionRealm")
    def isExceptionName(name: String): Boolean = ExceptionFuncs.contains(name)
    def maskExceptions(prov: Provenance): Provenance =
      ExceptionFuncs.collectFirst { case n if involvesFunc(prov, n) => n } match
        case Some(name) => Placeholder(name)
        case None       => prov
  }
  // -----------------------------------------------------------------------------
  // helpers
  // -----------------------------------------------------------------------------
  import tyStringifier.given

  /** SymExpr */
  private def symExprRule(app: Appender, expr: SymExpr): Appender =
    import SymExpr.*
    expr match
      case SEBool(bool)  => app >> bool
      case SERef(ref)    => app >> ref
      case SEExists(ref) => app >> "(exists " >> ref >> ")"
      case SETypeCheck(e, ty) =>
        symExprRule(app >> "(? ", e) >> ": " >> ty >> ")"
      case SETypeOf(base) =>
        symExprRule(app >> "(typeof ", base) >> ")"
      case SEEq(left, right) =>
        val a = symExprRule(app >> "(= ", left)
        symExprRule(a >> " ", right) >> ")"

  // Expose the helper as the given instance, avoiding self-referential implicit search issues.
  given Rule[SymExpr] = symExprRule

  /** Provenance pretty printer (spec-like) */
  given Rule[Provenance] = (app, prov) => ProvTextPrinter.print(app, prov)

  /** TypeConstr */
  given Rule[TypeProp] = (app, prop) =>
    import TypeProp.*
    given Rule[(ValueTy, Provenance)] =
      case (app, (ty, prov)) if useProvenance => app >> ty >> " ~~\n" >> prov
      case (app, (ty, prov))                  => app >> ty
    import SymTy.given
    given Rule[Map[Base, (ValueTy, Provenance)]] = sortedMapRule(sep = ": ")
    if (prop.map.nonEmpty) app >> prop.map
    prop.sexpr.fold(app) { app >> _ }

  /** TypeGuard */
  given Rule[TypeGuard] = (app, guard) =>
    given Ordering[DemandType] = Ordering.by(_.toString)
    given Rule[DemandType] = (app, dty) => app >> dty.ty
    given Rule[Map[DemandType, TypeProp]] =
      sortedMapRule("{", "}", " => ")
    app >> guard.simple.map

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
      case NodeTarget(nd) => app
  given Ordering[RefinementTarget] = Ordering.by { target =>
    import RefinementTarget.*
    target match
      case BranchTarget(branch, isTrue) => (branch.id, if (isTrue) 1 else 0)
      case AssertTarget(block, idx)     => (block.id, idx)
      case NodeTarget(nd)               => (nd.id, 0)
  }

  // ---------------------------------------------------------------------------
  // Provenance Text Printer
  // ---------------------------------------------------------------------------
  object ProvTextPrinter {
    import Provenance.*
    import RefinementTarget.*
    import SymTy.*

    private def humanizeRecordNames(s: String): String =
      s.replaceAll("""\bRecord\[([A-Za-z0-9_]+)\]""", "$1")
    private def showTy(ty: ValueTy): String = humanizeRecordNames(ty.toString)

    // -----------------------------------------------------------------------
    // Ordering helpers for consistent printing
    // -----------------------------------------------------------------------
    private def stepSeqOf(node: Node): Option[List[Int]] = node.loc.map(_.steps)
    private def stepSeqOf(prov: Provenance): Option[List[Int]] = prov match
      case RefinePoint(target, child, _, _, _, _) => stepSeqOf(target.node)
      case Leaf(node, _, _, _)                    => stepSeqOf(node)
      case CallPath(call, _, child) => stepSeqOf(call).orElse(stepSeqOf(child))
      case Join(children) =>
        val seqs = children.toList.flatMap(stepSeqOf)
        if seqs.isEmpty then None else Some(seqs.minBy(stepsCode))
      case Meet(children) =>
        val seqs = children.toList.flatMap(stepSeqOf)
        if seqs.isEmpty then None else Some(seqs.minBy(stepsCode))
      case Provenance.Bot | Provenance.Top => None
      case Placeholder(_)                  => ???
    private def stepsCode(steps: List[Int]): BigInt =
      steps.foldLeft(BigInt(0))((acc, s) => acc * 1000 + BigInt(s))
    private def stepRankOf(prov: Provenance): BigInt =
      stepSeqOf(prov).map(stepsCode).getOrElse(BigInt(Long.MaxValue))

    def print(app: Appender, prov: Provenance): Appender =
      // Short-circuit for masked exception functions
      prov match
        case Placeholder(name) =>
          app :> s"$name: No explanation provided (too complex)"
        case rp: RefinePoint if containsPlaceholder(rp) =>
          // If inner provenance was masked, print placeholder only
          findPlaceholderName(rp).foreach { name =>
            app :> s"$name: No explanation provided (too complex)"
          }
        case rp: RefinePoint =>
          val func = cfg.funcOf(rp.target.node)
          val params = func.irFunc.params.map(_.lhs.toString).mkString(", ")
          app :> s"${func.irFunc.name} ( $params )"
          render(app, prov, 1)
        case _ =>
          // Unexpected, but render without header to be robust
          render(app, prov, 0)
      app

    private def indent(depth: Int): String =
      if depth <= 0 then "" else ("| " * depth)

    private def prettyBase(base: Base, node: Node): String = base match
      case x: Local => x.toString
      case s: Sym =>
        val func = cfg.funcOf(node)
        val entrySt = getResult(NodePoint(func, func.entry, emptyView))
        val opt = entrySt.locals.collectFirst {
          case (lx, v) if (v.symty match
                case SSym(ss) if ss == s => true
                case _                   => false
              ) =>
            lx
        }
        opt.map(_.toString).getOrElse("#" + s.toString)

    // Optionally carry header (node, base) to render concise summaries
    // inside conjunction blocks.
    private def render(
      app: Appender,
      prov: Provenance,
      depth: Int,
      baseCtx: Option[(Node, Base)] = None,
    ): Unit =
      prov match
        case Placeholder(name) =>
          app :> s"${indent(depth)}$name: No explanation provided (too complex)"
        case RefinePoint(
              target,
              child,
              baseOpt,
              branchOpt,
              refinedOpt,
              varRefinedOpt,
            ) =>
          val baseStr =
            baseOpt.map(b => s"`${prettyBase(b, target.node)}`").getOrElse("<>")
          val refinedStr =
            varRefinedOpt
              .orElse(refinedOpt)
              .fold(showTy(child.ty))(showTy)
          val branchStr = branchOpt.map(b => if b then "true" else "false")
          val stepStr = target.node.loc.map(_.stepString)
          val head =
            (indent(depth) + s"$baseStr is $refinedStr" +
            stepStr
              .map(s =>
                s" in the ${branchStr.getOrElse("?")} branch of step $s",
              )
              .getOrElse(""))
          app :> head
          // Print spec excerpt as a comment
          specLine(target.node)
            .map { line =>
              app :> s"${indent(depth)}  // $line"
            }
            .getOrElse(())
          // Pass base context down so Meet can show concise summaries
          render(app, child, depth, baseOpt.map(b => (target.node, b)))

        case Meet(children) =>
          app :> s"${indent(depth)}All the following statements hold:"
          val ordered = children.toList.sortBy(stepRankOf)
          ordered.foreach(render(app, _, depth, baseCtx))

        case Join(children) =>
          app :> s"${indent(depth)}Either of the following statements hold:"
          val ordered = children.toList.sortBy(stepRankOf)
          ordered.foreach(render(app, _, depth, baseCtx))

        case cp @ CallPath(call, cty, child) =>
          baseCtx.foreach { (nd, base) =>
            cp.truthOpt.foreach { b =>
              val baseStr = s"`${prettyBase(base, nd)}`"
              val truthWord = if b then "True" else "False"
              app :> s"${indent(depth)}* $baseStr is ${showTy(cty)} because `${callSig(call)}` is $truthWord"
              specLine(call)
                .map { line =>
                  app :> s"${indent(depth)}  // $line"
                }
                .getOrElse(())
            }
          }
          val callLine = callSig(call)
          app :> s"${indent(depth)}  $callLine"
          render(app, child, depth + 1, baseCtx)

        case Leaf(node, baseOpt, ty, truthOpt) =>
          val baseStr =
            baseOpt.map(b => s"`${prettyBase(b, node)}`").getOrElse("<>")
          val line = specLine(node)
          val reason = originSnippet(node)
            .orElse(
              line.flatMap(extractCond).orElse(line.map(Loc.dropStepPrefix)),
            )
          val suffix = truthOpt
            .map(b => if b then " is True" else " is False")
            .getOrElse("")
          val because = reason.map(c => s" because `$c`$suffix").getOrElse("")
          app :> s"${indent(depth)}* [ORIGIN] $baseStr is ${showTy(ty)}$because"
          // spec comment
          line
            .map { line =>
              app :> s"${indent(depth)}  // $line"
            }
            .getOrElse(())

        case _ => ()

    private def containsPlaceholder(prov: Provenance): Boolean = prov match
      case Placeholder(_)                    => true
      case RefinePoint(_, child, _, _, _, _) => containsPlaceholder(child)
      case CallPath(_, _, child)             => containsPlaceholder(child)
      case Join(children) => children.exists(containsPlaceholder)
      case Meet(children) => children.exists(containsPlaceholder)
      case _              => false

    private def findPlaceholderName(prov: Provenance): Option[String] =
      prov match
        case Placeholder(name)                 => Some(name)
        case RefinePoint(_, child, _, _, _, _) => findPlaceholderName(child)
        case CallPath(_, _, child)             => findPlaceholderName(child)
        case Join(children) =>
          children.toList.flatMap(findPlaceholderName).headOption
        case Meet(children) =>
          children.toList.flatMap(findPlaceholderName).headOption
        case _ => None

    private def algoCode(func: Func): Option[String] =
      // Prefer raw spec text to preserve wording like "If ... then".
      cfg.spec.fnameMap
        .get(func.irFunc.name)
        .map(_.code)
        .orElse(func.irFunc.algo.map(_.code))

    // Remove ecmarkup decorations (e.g., [id="..."], <emu-...> wrappers) while preserving inner text
    private def sanitizeSpec(s: String): String =
      var t = s
      // Remove bracketed id labels like [id="..."]
      t = t.replaceAll("""\s*\[id\s*=\s*(['"]).*?\1\s*]""", "")
      // Unwrap <emu-...>...</emu-...> while keeping inner text
      t = t.replaceAll(
        """<emu-[A-Za-z0-9\-]+(?:\s[^>]*?)?>(.*?)</emu-[A-Za-z0-9\-]+>""",
        "$1",
      )
      // Remove self-closing or stray emu tags
      t = t.replaceAll("""<emu-[A-Za-z0-9\-]+(?:\s[^>]*?)?/>""", "")
      t = t.replaceAll("""</?emu-[A-Za-z0-9\-]+(?:\s[^>]*?)?>""", "")
      // Remove empty references like "(see )" possibly left by empty xrefs
      t = t.replaceAll("""\(\s*see\s*\)""", "")
      // Normalize whitespace and punctuation spacing
      t = t.replaceAll("""\s+([.,;:])""", "$1").replaceAll("""\s+""", " ").trim
      t

    private def specLine(node: Node): Option[String] = for {
      loc <- node.loc
      func = cfg.funcOf(node)
      code <- algoCode(func)
      line <- loc.findStepFullLine(code)
    } yield sanitizeSpec(line)

    private def originSnippet(node: Node): Option[String] = for {
      loc <- node.loc
      func = cfg.funcOf(node)
      code <- algoCode(func)
    } yield sanitizeSpec(Loc.oneLine(loc.getString(code)))

    // Try to extract the condition from an "If <cond>, ..." line
    private def extractCond(s: String): Option[String] =
      val IfCond = "(?is).*?If\\s+(.+?)(?:,\\s*(?:then|return)|\\.)".r
      s match
        case IfCond(cond) => Some(cond.trim)
        case _            => None

    // Show a concise call signature from a call node
    private def callSig(call: Call): String = call.callInst match
      case ICall(_, fexpr, args) =>
        val as = args.map(_.toString).mkString(", ")
        // Prefer readable function name for closures/continuations
        val fname = fexpr match
          case EClo(fn, _) => fn
          case ECont(fn)   => fn
          case _           => fexpr.toString
        s"$fname ( $as )"
      case ISdoCall(_, base, method, args) =>
        val as = (base :: args).map(_.toString).mkString(", ")
        s"$method ( $as )"
  }
}
