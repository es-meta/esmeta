package esmeta.ty

import esmeta.ir.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** type guard */
case class TypeGuard(map: Map[RefinementKind, SymPred] = Map()) {
  def isEmpty: Boolean = map.isEmpty
  def nonEmpty: Boolean = !isEmpty
  def kinds: Set[RefinementKind] = map.keySet
  def get(kind: RefinementKind): Option[SymPred] = map.get(kind)
  def apply(kind: RefinementKind): SymPred = map.getOrElse(kind, SymPred())
  def getSyms: Set[Sym] =
    map.values.flatMap(_.getSyms).collect { case s: Sym => s }.toSet
  def forReturn: TypeGuard =
    TypeGuard(map.map { (kind, pred) => kind -> pred.forReturn })
  def <=(that: TypeGuard): Boolean = that.map.forall { (kind, r) =>
    this.map.get(kind) match
      case Some(l) => l == r
      case None    => false
  }
  def ||(that: TypeGuard)(lty: ValueTy, rty: ValueTy): TypeGuard =
    val (lkinds, rkinds) = (this.kinds, that.kinds)
    val kinds =
      lkinds.filter(k => (k.ty && rty).isBottom || rkinds.contains(k)) ++
      rkinds.filter(k => (k.ty && lty).isBottom || lkinds.contains(k))
    TypeGuard((for {
      kind <- kinds.toList
      pred = (this.get(kind), that.get(kind)) match
        case (Some(l), Some(r)) => l || r
        case (Some(l), None)    => l
        case (None, Some(r))    => r
        case _                  => SymPred()
      if !pred.isTop
    } yield kind -> pred).toMap)
  def &&(that: TypeGuard): TypeGuard = TypeGuard((for {
    kind <- (this.kinds ++ that.kinds).toList
    pred = this(kind) && that(kind)
    if !pred.isTop
  } yield kind -> pred).toMap)
  override def toString: String = (new Appender >> this).toString
}
object TypeGuard {
  val Empty: TypeGuard = TypeGuard()
  def apply(ps: (RefinementKind, SymPred)*): TypeGuard = TypeGuard(ps.toMap)
}

/** type refinement kinds */
enum RefinementKind {
  case True, False, Normal, Abrupt, NormalTrue, NormalFalse
  lazy val ty: ValueTy = this match
    case True        => TrueT
    case False       => FalseT
    case Normal      => NormalT
    case Abrupt      => AbruptT
    case NormalTrue  => NormalT(TrueT)
    case NormalFalse => NormalT(FalseT)
  override def toString: String = (new Appender >> this).toString
}
object RefinementKind {
  val compKinds: List[RefinementKind] =
    List(NormalTrue, NormalFalse, Normal, Abrupt) // order is important
}

/** Symbol */
type Sym = Int

/** symbolic predicates */
case class SymPred(
  map: Map[SymBase, ValueTy] = Map(),
  expr: Option[SymExpr] = None,
) {
  def isTop: Boolean = map.isEmpty && expr.isEmpty
  def nonTop: Boolean = !isTop
  def ||(that: SymPred): SymPred = SymPred(
    (for {
      x <- (this.map.keySet intersect that.map.keySet).toList
      ty = this.map(x) || that.map(x)
    } yield x -> ty).toMap,
    this.expr || that.expr,
  )
  def &&(that: SymPred): SymPred = SymPred(
    (for {
      x <- (this.map.keySet ++ that.map.keySet).toList
      ty = this.map.getOrElse(x, AnyT) && that.map.getOrElse(x, AnyT)
    } yield x -> ty).toMap,
    this.expr && that.expr,
  )
  def getSyms: Set[Sym] =
    map.keySet.collect { case s: Sym => s } ++
    expr.fold(Set[Sym]())(_.getSyms)
  def forReturn: SymPred = SymPred(
    map.collect { case (x: Sym, ty) => x -> ty },
    None, // TODO
  )
  def kill(x: Local): SymPred = SymPred(
    for {
      (y, ty) <- map
      if y != x
    } yield y -> ty,
    expr.flatMap(_.kill(x)),
  )
  override def toString: String = (new Appender >> this).toString
}

/** symbolic bases */
type SymBase = Sym | Local

/** symbolic expressions */
enum SymExpr {
  case SEBool(b: Boolean)
  case SEStr(s: String)
  case SERef(ref: SymRef)
  case SEExists(ref: SymRef)
  case SETypeCheck(base: SymExpr, ty: ValueTy)
  case SETypeOf(base: SymExpr)
  case SEBinary(bop: BOp, left: SymExpr, right: SymExpr)
  case SEUnary(uop: UOp, expr: SymExpr)
  def &&(that: SymExpr): SymExpr = (this, that) match
    case _ if this == that                       => this
    case (SEBool(true), _)                       => that
    case (_, SEBool(true))                       => this
    case (SEBool(false), _) | (_, SEBool(false)) => SEBool(false)
    case _ => SEBinary(BOp.And, this, that)
  def ||(that: SymExpr): SymExpr = (this, that) match
    case _ if this == that                     => this
    case (SEBool(false), _)                    => that
    case (_, SEBool(false))                    => this
    case (SEBool(true), _) | (_, SEBool(true)) => SEBool(true)
    case _                                     => SEBinary(BOp.Or, this, that)
  def has(sym: Sym): Boolean = this match
    case SEBool(b)                  => false
    case SEStr(s)                   => false
    case SERef(ref)                 => ref.has(sym)
    case SEExists(ref)              => ref.has(sym)
    case SETypeCheck(base, ty)      => base.has(sym)
    case SETypeOf(base)             => base.has(sym)
    case SEBinary(bop, left, right) => left.has(sym) || right.has(sym)
    case SEUnary(uop, expr)         => expr.has(sym)
  def getSyms: Set[Sym] = this match
    case SEBool(b)                  => Set()
    case SEStr(s)                   => Set()
    case SERef(ref)                 => ref.getSyms
    case SEExists(ref)              => ref.getSyms
    case SETypeCheck(base, ty)      => base.getSyms
    case SETypeOf(base)             => base.getSyms
    case SEBinary(bop, left, right) => left.getSyms ++ right.getSyms
    case SEUnary(uop, expr)         => expr.getSyms
  def kill(x: Local): Option[SymExpr] = this match
    case SEBool(b)             => Some(this)
    case SEStr(s)              => Some(this)
    case SERef(ref)            => ref.kill(x).map(SERef(_))
    case SEExists(ref)         => ref.kill(x).map(SEExists(_))
    case SETypeCheck(base, ty) => base.kill(x).map(SETypeCheck(_, ty))
    case SETypeOf(base)        => base.kill(x).map(SETypeOf(_))
    case SEBinary(BOp.And, left, right) =>
      (left.kill(x), right.kill(x)) match
        case (Some(l), Some(r)) => Some(l && r)
        case (Some(l), None)    => Some(l)
        case (None, Some(r))    => Some(r)
        case _                  => None
    case SEBinary(bop, left, right) =>
      for {
        l <- left.kill(x)
        r <- right.kill(x)
      } yield SEBinary(bop, l, r)
    case SEUnary(uop, expr) => expr.kill(x).map(SEUnary(uop, _))
  override def toString: String = (new Appender >> this).toString
}
object SymExpr {
  val T: SymExpr = SEBool(true)
  val F: SymExpr = SEBool(false)
  extension (l: Option[SymExpr])
    def &&(r: Option[SymExpr]): Option[SymExpr] = (l, r) match
      case (Some(l), Some(r)) => Some(l && r)
      case (Some(l), None)    => Some(l)
      case (None, Some(r))    => Some(r)
      case _                  => None
    def ||(r: Option[SymExpr]): Option[SymExpr] = (l, r) match
      case (Some(l), Some(r)) => Some(l || r)
      case _                  => None
}

/** symbolic references */
enum SymRef {
  case SBase(base: SymBase)
  case SField(base: SymRef, field: SymExpr)
  def getBase: SymBase = this match
    case SBase(s)        => s
    case SField(base, f) => base.getBase
  def has(sym: Sym): Boolean = this match
    case SBase(s)        => s == sym
    case SField(base, f) => base.has(sym) || f.has(sym)
  def getSyms: Set[Sym] = this match
    case SBase(s: Sym)   => Set(s)
    case SBase(_)        => Set()
    case SField(base, f) => base.getSyms ++ f.getSyms
  def kill(x: Local): Option[SymRef] = this match
    case SBase(y) => if (x == y) None else Some(this)
    case SField(base, f) =>
      for {
        b <- base.kill(x)
        f <- f.kill(x)
      } yield SField(b, f)
  override def toString: String = (new Appender >> this).toString
}

// -----------------------------------------------------------------------------
// helpers
// -----------------------------------------------------------------------------
import TyStringifier.given
val irStringifier = IRElem.getStringifier(true, false)
import irStringifier.given
given Rule[SymBase] = (app, base) =>
  base match
    case sym: Sym     => app >> "#" >> sym.toString
    case local: Local => app >> local.toString
given Ordering[SymBase] = Ordering.by(_.toString)
given Rule[SymExpr] = (app, expr) =>
  import SymExpr.*
  expr match
    case SEBool(bool)  => app >> bool
    case SEStr(str)    => app >> "\"" >> normStr(str) >> "\""
    case SERef(ref)    => app >> ref
    case SEExists(ref) => app >> "(exists " >> ref >> ")"
    case SETypeCheck(expr, ty) =>
      app >> "(? " >> expr >> ": " >> ty >> ")"
    case SETypeOf(base) =>
      app >> "(typeof " >> base >> ")"
    case SEBinary(bop, left, right) =>
      app >> "(" >> bop >> " " >> left >> " " >> right >> ")"
    case SEUnary(uop, expr) =>
      app >> "(" >> uop >> " " >> expr >> ")"
given Rule[SymRef] = (app, ref) =>
  import SymExpr.*
  lazy val inlineField = "([_a-zA-Z][_a-zA-Z0-9]*)".r
  import SymRef.*
  ref match
    case SBase(x)                            => app >> x
    case SField(base, SEStr(inlineField(f))) => app >> base >> "." >> f
    case SField(base, field) => app >> base >> "[" >> field >> "]"
given Rule[SymPred] = (app, pred) =>
  import SymPred.*
  given Rule[Map[SymBase, ValueTy]] = sortedMapRule(sep = " <: ")
  if (pred.map.nonEmpty) app >> pred.map
  pred.expr.fold(app)(app >> "[" >> _ >> "]")
given Rule[TypeGuard] = (app, guard) =>
  given Rule[Map[RefinementKind, SymPred]] = sortedMapRule("{", "}", " => ")
  app >> guard.map
given Rule[RefinementKind] = (app, kind) =>
  import RefinementKind.*
  kind match
    case True        => app >> "True"
    case False       => app >> "False"
    case Normal      => app >> "Normal"
    case Abrupt      => app >> "Abrupt"
    case NormalTrue  => app >> "Normal[True]"
    case NormalFalse => app >> "Normal[False]"
given Ordering[RefinementKind] = Ordering.by(_.toString)
