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

    def kill(effect: Effect): TypeGuard = TypeGuard(for {
      (dty, constr) <- map
      newConstr = constr.kill(effect)
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

    def simple: TypeGuard =
      TypeGuard(map.filterNot { (dty, constr) =>
        map.exists { (tdty, tconstr) =>
          (dty.ty != tdty.ty && dty.ty <= tdty.ty && tconstr <= constr)
        }
      })

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

  /** type constraints */
  case class TypeConstr(
    map: Map[Base, (ValueTy, Provenance)] = Map(),
    sexpr: Option[SymExpr] = None,
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

    def ||(that: TypeConstr): TypeConstr =
      TypeConstr(
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

    def &&(that: TypeConstr): TypeConstr =
      TypeConstr(
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

    def kill(bases: Set[Base])(using AbsState): TypeConstr =
      this.copy(
        map.filter { case (x, _) => !bases.contains(x) },
        sexpr.fold(None)(_.kill(bases)),
      )

    def kill(effect: Effect): TypeConstr =
      import TypeGuardDecl.this.Effect.*
      TypeConstr(map.map {
        case (x, (ty, prov)) =>
          x -> (effect(ty), prov)
      })

    def forReturn(symEnv: Map[Sym, ValueTy]): TypeConstr = TypeConstr(
      map = for {
        case (x: Sym, (ty, prov)) <- map
        origTy = symEnv.getOrElse(x, BotT)
      } yield x -> (origTy && ty, prov),
      sexpr = None,
    )

    def depth: Int = map.values.map(_._2).map(_.depth).max

    def lift(using st: AbsState): TypeConstr =
      this && st.constr

    override def toString: String = (new Appender >> this).toString
  }
  object TypeConstr {
    def apply(sexpr: SymExpr): TypeConstr =
      TypeConstr(sexpr = Some(sexpr))
    def apply(pairs: (Base, (ValueTy, Provenance))*): TypeConstr =
      TypeConstr(pairs.toMap, None)
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

    // simple explanation is bigger (imprecise)
    def <=(that: Provenance): Boolean = {
      if this == Bot then true
      else if that == Bot then false
      else if this == Top then false
      else if that == Top then true
      else // this and that are not Bot or Top
        (this, that) match
          case (Leaf(lnode, lty), Leaf(rnode, rty)) =>
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
        case (Leaf(lnode, lty), Leaf(rnode, rty)) =>
          if lnode == rnode then Leaf(lnode, lty || rty)
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
        case (Leaf(lnode, lty), Leaf(rnode, rty)) =>
          if lnode == rnode then Leaf(lnode, lty && rty)
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

    def usedForRefine(target: RefinementTarget): Provenance =
      RefinePoint(target, this)

    def toTree(indent: Int): String
    def toTree: String = toTree(0)
  }

  case class Leaf(node: Node, ty: ValueTy) extends Provenance { // TODO: change to expression & node
    def size: Int = 1
    def depth: Int = 1

    override def toString =
      s"Leaf(${cfg.funcOf(node).nameWithId}:${node.id}, $ty)"
    def toTree(indent: Int): String = s"${"  " * indent}$this"
  }
  case class CallPath(call: Call, ty: ValueTy, child: Provenance)
    extends Provenance {
    def size: Int = child.size + 1
    def depth: Int = child.depth + 1

    override def toString =
      s"CallPath(${cfg.funcOf(call).nameWithId}:${call.id}, $ty, ${child.toString})"
    def toTree(indent: Int): String =
      s"${"  " * indent}CallPath(${cfg.funcOf(call).nameWithId}:${call.id}, $ty)\n${child
        .toTree(indent + 1)}"
  }
  case class Join(child: Set[Provenance]) extends Provenance {
    lazy val ty: ValueTy = child.map(_.ty).reduce(_ || _)
    def size: Int = child.map(_.size).sum + 1
    def depth: Int = child.map(_.depth).max + 1

    override def toString = s"Join(${child.map(_.toString)})"
    def toTree(indent: Int): String =
      s"${"  " * indent}Join($ty)\n${child.map(_.toTree(indent + 1)).mkString("\n")}"
  }

  case class Meet(child: Set[Provenance]) extends Provenance {
    lazy val ty: ValueTy = child.map(_.ty).reduce(_ && _)
    def size: Int = child.map(_.size).sum + 1
    def depth: Int = child.map(_.depth).max + 1

    override def toString = s"Meet(${child.map(_.toString)})"
    def toTree(indent: Int): String =
      s"${"  " * indent}Meet($ty)\n${child.map(_.toTree(indent + 1)).mkString("\n")}"
  }

  case class RefinePoint(target: RefinementTarget, child: Provenance)
    extends Provenance {
    lazy val ty: ValueTy = child.ty
    def size: Int = child.size + 1
    def depth: Int = child.depth + 1

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

      override def toString = "Bot"
      def toTree(indent: Int): String = s"${"  " * indent}$this"
    }
    case object Top extends Provenance {
      lazy val ty: ValueTy = BotT
      def size: Int = 0
      def depth: Int = 0

      override def toString = "Top"
      def toTree(indent: Int): String = s"${"  " * indent}$this"
    }

    def apply(ty: ValueTy)(using nd: Node): Provenance =
      if useProvenance then Leaf(nd, ty) else Bot
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
    import Provenance.*
    if useProvenance then app >> prov.toTree else app

  /** TypeConstr */
  given Rule[TypeConstr] = (app, constr) =>
    import TypeConstr.*
    given Rule[(ValueTy, Provenance)] =
      case (app, (ty, prov)) if useProvenance => app >> ty >> " ~~\n" >> prov
      case (app, (ty, prov))                  => app >> ty
    import SymTy.given
    given Rule[Map[Base, (ValueTy, Provenance)]] = sortedMapRule(sep = ": ")
    if (constr.map.nonEmpty) app >> constr.map
    constr.sexpr.fold(app) { app >> _ }

  /** TypeGuard */
  given Rule[TypeGuard] = (app, guard) =>
    given Ordering[DemandType] = Ordering.by(_.toString)
    given Rule[DemandType] = (app, dty) => app >> dty.ty
    given Rule[Map[DemandType, TypeConstr]] =
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
  given Ordering[RefinementTarget] = Ordering.by { target =>
    import RefinementTarget.*
    target match
      case BranchTarget(branch, isTrue) => (branch.id, if (isTrue) 1 else 0)
      case AssertTarget(block, idx)     => (block.id, idx)
  }

  object ProvPrinter {
    def getId(prov: Provenance): String =
      prov match
        case Leaf(node, ty)             => norm(s"Leaf${node.id}")
        case CallPath(call, ty, child)  => norm(s"call${call.id}")
        case Join(child)                => norm(s"join${child.hashCode()}")
        case Meet(child)                => norm(s"meet${child.hashCode()}")
        case RefinePoint(target, child) => norm(s"refine${target.node.id}")
        case Provenance.Bot             => ???
        case Provenance.Top             => ???

    /** Colors */
    val NODE_COLOR = """"black""""
    val BG_COLOR = """"white""""
    val EDGE_COLOR = """"black""""

    def draw(prov: Provenance): String =
      given app: Appender = new Appender
      (app >> "digraph").wrap {
        app :> """graph [fontname = "Consolas"]"""
        app :> """node [fontname = "Consolas"]"""
        app :> """edge [fontname = "Consolas"]"""
        drawProvenance(prov)
      }
      app.toString

    def drawProvenance(prov: Provenance)(using app: Appender): Unit =
      drawProvenanceNode(prov)
      prov match
        case CallPath(call, ty, child) => drawProvenance(child)
        case Join(child) =>
          child.foreach(drawProvenance)
        case RefinePoint(target, child) => drawProvenance(child)
        case _                          => ()

    def drawProvenanceNode(prov: Provenance)(using Appender): Unit =
      prov match
        case p @ Leaf(node, ty) =>
          drawNaming(getId(p), NODE_COLOR, node.name)
          drawNode(getId(p), "box", NODE_COLOR, BG_COLOR, Some(ty.toString))
        case p @ CallPath(call, ty, child) =>
          drawNaming(getId(p), NODE_COLOR, call.name)
          drawNode(getId(p), "box", NODE_COLOR, BG_COLOR, Some(ty.toString))
          drawEdge(getId(p), getId(child), EDGE_COLOR, None)
        case p @ Join(child) =>
          drawNode(
            getId(p),
            "ellipse",
            NODE_COLOR,
            BG_COLOR,
            Some(p.ty.toString),
          )
          child.foreach { c =>
            drawEdge(getId(p), getId(c), EDGE_COLOR, None)
          }
        case p @ Meet(child) =>
          drawNode(
            getId(p),
            "ellipse",
            NODE_COLOR,
            BG_COLOR,
            Some(p.ty.toString),
          )
          child.foreach { c =>
            drawEdge(getId(p), getId(c), EDGE_COLOR, None)
          }
        case p @ RefinePoint(target, child) =>
          drawNaming(getId(p), NODE_COLOR, target.node.name)
          drawEdge(getId(p), getId(child), EDGE_COLOR, Some(p.ty.toString))
        case Provenance.Bot => ()
        case Provenance.Top => ()

    def drawNode(
      dotId: String,
      shape: String,
      color: String,
      bgColor: String,
      labelOpt: Option[String],
    )(using app: Appender): Unit = labelOpt match
      case Some(label) =>
        app :> s"""$dotId [shape=$shape, label=<<font color=$color>$label</font>> color=$color fillcolor=$bgColor, style=filled]"""
      case None =>
        app :> s"""$dotId [shape=$shape label=" " color=$color fillcolor=$bgColor style=filled]"""

    def drawEdge(
      fid: String,
      tid: String,
      color: String,
      labelOpt: Option[String],
    )(using app: Appender): Unit =
      app :> s"$fid -> $tid ["
      labelOpt.map { label =>
        app >> s"label=<<font color=$color>$label</font>> "
      }
      app >> s"color=$color]"

    def drawNaming(
      id: String,
      color: String,
      name: String,
    )(using app: Appender): Unit =
      app :> id >> "_name [shape=none, "
      app >> "label=<<font color=" >> color >> ">" >> name >> "</font>>]"
      app :> id >> "_name -> " >> id
      app >> " [arrowhead=none, color=" >> color >> ", style=dashed]"

    def norm(str: String): String =
      val s = HtmlUtils.escapeHtml(str).replaceAll("\u0000", "U+0000")
      s.replaceAll("[^a-zA-Z0-9]", "")
    def norm(node: IRElem): String =
      norm(node.toString(detail = false, location = false))
    protected def norm(insts: Iterable[Inst]): String = (for {
      (inst, idx) <- insts.zipWithIndex
      str = norm(inst)
    } yield s"""[$idx] $str<BR ALIGN="LEFT"/>""").mkString
  }
}
