package esmeta.ty

import esmeta.cfg.Node
import esmeta.state.*
import esmeta.ir.*
import esmeta.ty.util.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** type elements */
trait TyElem {
  override def toString: String =
    import Stringifier.elemRule
    stringify(this)
}

/** Symbol */
type Sym = Int

/** symbolic expressions */
enum SymExpr:
  case SEBool(b: Boolean)
  case SEStr(s: String)
  case SERef(ref: SymRef)
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
    case SETypeCheck(base, ty)      => base.has(sym)
    case SETypeOf(base)             => base.has(sym)
    case SEBinary(bop, left, right) => left.has(sym) || right.has(sym)
    case SEUnary(uop, expr)         => expr.has(sym)
  def kill(x: Local): Option[SymExpr] = this match
    case SEBool(b)             => Some(this)
    case SEStr(s)              => Some(this)
    case SERef(ref)            => ref.kill(x).map(SERef(_))
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
object SymExpr:
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

/** symbolic bases */
type SymBase = Sym | Local

/** symbolic references */
enum SymRef:
  case SBase(base: SymBase)
  case SField(base: SymRef, field: SymExpr)
  def getBase: SymBase = this match
    case SBase(s)        => s
    case SField(base, f) => base.getBase
  def has(sym: Sym): Boolean = this match
    case SBase(s)        => s == sym
    case SField(base, f) => base.has(sym) || f.has(sym)
  def kill(x: Local): Option[SymRef] = this match
    case SBase(y) => if (x == y) None else Some(this)
    case SField(base, f) =>
      for {
        b <- base.kill(x)
        f <- f.kill(x)
      } yield SField(b, f)
  override def toString: String = (new Appender >> this).toString

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
  def kill(x: Local): SymPred = SymPred(
    for {
      (y, ty) <- map
      if y != x
    } yield y -> ty,
    expr.flatMap(_.kill(x)),
  )
  override def toString: String = (new Appender >> this).toString
}

/** type guard */
case class TypeGuard(map: Map[RefinementKind, SymPred] = Map()) {
  def isEmpty: Boolean = map.isEmpty
  def nonEmpty: Boolean = !isEmpty
  def kinds: Set[RefinementKind] = map.keySet
  def get(kind: RefinementKind): Option[SymPred] = map.get(kind)
  def apply(kind: RefinementKind): SymPred = map.getOrElse(kind, SymPred())
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
}
object TypeGuard {
  val Empty: TypeGuard = TypeGuard()
  def apply(ps: (RefinementKind, SymPred)*): TypeGuard = TypeGuard(ps.toMap)
}

/** type refinement kinds */
enum RefinementKind:
  case True, False, Normal, Abrupt, NormalTrue, NormalFalse
  lazy val ty: ValueTy = this match
    case True        => TrueT
    case False       => FalseT
    case Normal      => NormalT
    case Abrupt      => AbruptT
    case NormalTrue  => NormalT(TrueT)
    case NormalFalse => NormalT(FalseT)

// -----------------------------------------------------------------------------
// helpers
// -----------------------------------------------------------------------------
lazy val AnyT: ValueTy = ValueTy.Top
lazy val CompT: ValueTy = ValueTy(record = RecordTy("CompletionRecord"))
lazy val AbruptT: ValueTy = ValueTy(record = RecordTy("AbruptCompletion"))
def AbruptT(xs: String*): ValueTy = AbruptT(xs.toSet)
def AbruptT(xs: Set[String]): ValueTy =
  ValueTy(record = RecordTy("AbruptCompletion", Map("Type" -> EnumT(xs.toSet))))
lazy val ThrowT: ValueTy = RecordT("ThrowCompletion")
lazy val ReturnT: ValueTy = RecordT("ReturnCompletion")
lazy val NormalT: ValueTy = ValueTy(record = RecordTy("NormalCompletion"))
def NormalT(value: ValueTy): ValueTy =
  if (value.isBottom) BotT
  else ValueTy(record = RecordTy("NormalCompletion", Map("Value" -> value)))
def MapT: ValueTy = ValueTy(map = MapTy.Top)
def MapT(key: ValueTy, value: ValueTy): ValueTy =
  if (key.isBottom || value.isBottom) BotT
  else ValueTy(map = MapTy(key, value))
lazy val CloT: ValueTy = ValueTy(clo = Inf)
def CloT(names: String*): ValueTy = CloT(names.toSet)
def CloT(names: Set[String]): ValueTy =
  if (names.isEmpty) BotT
  else ValueTy(clo = Fin(names))
lazy val ContT: ValueTy = ValueTy(cont = Inf)
def ContT(nids: Int*): ValueTy =
  if (nids.isEmpty) BotT
  else ValueTy(cont = Fin(nids.toSet))
lazy val ObjectT: ValueTy = RecordT("Object")
lazy val FunctionT: ValueTy = RecordT("FunctionObject")
lazy val ConstructorT: ValueTy = RecordT("Constructor")
lazy val ArrayT: ValueTy = RecordT("Array")
lazy val TypedArrayT: ValueTy = RecordT("TypedArray")
lazy val RegExpT: ValueTy = RecordT("RegExp")
lazy val ESPrimT: ValueTy = ValueTy(
  record = RecordTy("Symbol"),
  number = NumberTy.Top,
  bigInt = true,
  str = Inf,
  bool = BoolTy.Top,
  undef = true,
  nullv = true,
)
lazy val ESValueT: ValueTy = ObjectT || ESPrimT
lazy val RealmT: ValueTy = RecordT("RealmRecord")
lazy val RecordT: ValueTy = ValueTy(record = RecordTy.Top)
def RecordT(names: Set[String]): ValueTy = ValueTy(record = RecordTy(names))
def RecordT(names: String*): ValueTy = RecordT(names.toSet)
def RecordT(name: String, fields: Map[String, ValueTy]): ValueTy =
  ValueTy(record = RecordTy(name, fields))
def RecordT(name: String, fieldMap: FieldMap): ValueTy =
  ValueTy(record = RecordTy(name, fieldMap))
def RecordT(map: Map[String, FieldMap]): ValueTy =
  ValueTy(record = RecordTy(map))
def NilT: ValueTy = ValueTy(list = ListTy.Nil)
def ListT: ValueTy = ValueTy(list = ListTy.Top)
def ListT(ty: ValueTy): ValueTy = ValueTy(list = ListTy(ty))
lazy val SymbolT: ValueTy = RecordT("Symbol")
lazy val AstT: ValueTy = ValueTy(ast = AstTy.Top)
def AstT(xs: Set[String]): ValueTy =
  if (xs.isEmpty) BotT
  else ValueTy(ast = AstTy.Simple(xs.toSet))
def AstT(xs: String*): ValueTy = AstT(xs.toSet)
def AstT(name: String, idx: Int): ValueTy =
  ValueTy(ast = AstTy.Detail(name, idx))
def GrammarSymbolT: ValueTy = ValueTy(grammarSymbol = Inf)
def GrammarSymbolT(xs: GrammarSymbol*): ValueTy =
  if (xs.isEmpty) BotT
  else ValueTy(grammarSymbol = Fin(xs.toSet))
lazy val CodeUnitT: ValueTy = ValueTy(codeUnit = true)
def EnumT: ValueTy = ValueTy(enumv = Inf)
def EnumT(set: Set[String]): ValueTy =
  if (set.isEmpty) BotT
  else ValueTy(enumv = Fin(set))
def EnumT(xs: String*): ValueTy = EnumT(xs.toSet)
lazy val MathT: ValueTy = ValueTy(math = MathTy.Top)
lazy val ExtMathT: ValueTy = MathT || InfinityT
lazy val IntT: ValueTy = ValueTy(math = IntTy)
lazy val NonPosIntT: ValueTy = ValueTy(math = NonPosIntTy)
lazy val NonNegIntT: ValueTy = ValueTy(math = NonNegIntTy)
lazy val NegIntT: ValueTy = ValueTy(math = NegIntTy)
lazy val PosIntT: ValueTy = ValueTy(math = PosIntTy)
def MathT(ds: BigDecimal*): ValueTy =
  if (ds.isEmpty) BotT
  else ValueTy(math = MathSetTy(ds.toSet.map(Math(_))))
lazy val InfinityT: ValueTy = ValueTy(infinity = InfinityTy.Top)
lazy val NegInfinityT: ValueTy = ValueTy(infinity = InfinityTy.Neg)
lazy val PosInfinityT: ValueTy = ValueTy(infinity = InfinityTy.Pos)
def InfinityT(ps: Boolean*): ValueTy =
  if (ps.isEmpty) BotT
  else ValueTy(infinity = InfinityTy(ps.toSet))
lazy val NumericT: ValueTy = NumberT || BigIntT
lazy val NumberT: ValueTy = ValueTy(number = NumberTy.Top)
lazy val NumberIntT: ValueTy = ValueTy(number = NumberTy.Int)
def NumberT(ns: Number*): ValueTy =
  if (ns.isEmpty) BotT
  else ValueTy(number = NumberSetTy(ns.toSet))
lazy val BigIntT: ValueTy = ValueTy(bigInt = true)
lazy val StrT: ValueTy = ValueTy(str = Inf)
def StrT(set: Set[String]): ValueTy =
  if (set.isEmpty) BotT
  else ValueTy(str = Fin(set))
def StrT(xs: String*): ValueTy =
  if (xs.isEmpty) BotT
  else ValueTy(str = Fin(xs.toSet))
def BoolT(set: Set[Boolean]): ValueTy =
  if (set.isEmpty) BotT
  else ValueTy(bool = BoolTy(set))
def BoolT(seq: Boolean*): ValueTy =
  if (seq.isEmpty) BotT
  else ValueTy(bool = BoolTy(seq.toSet))
lazy val BoolT: ValueTy = BoolT(true, false)
lazy val TrueT: ValueTy = BoolT(true)
lazy val FalseT: ValueTy = BoolT(false)
lazy val UndefT: ValueTy = ValueTy(undef = true)
lazy val NullT: ValueTy = ValueTy(nullv = true)
lazy val BotT: ValueTy = ValueTy.Bot

/** predefined enum types */
val ENUMT_EMPTY = EnumT("empty")
val ENUMT_UNRESOLVABLE = EnumT("unresolvable")
val ENUMT_LEXICAL = EnumT("lexical")
val ENUMT_INITIALIZED = EnumT("initialized")
val ENUMT_UNINITIALIZED = EnumT("uninitialized")
val ENUMT_BASE = EnumT("base")
val ENUMT_DERIVED = EnumT("derived")
val ENUMT_STRICT = EnumT("strict")
val ENUMT_GLOBAL = EnumT("global")
val ENUMT_UNLINKED = EnumT("unlinked")
val ENUMT_LINKING = EnumT("linking")
val ENUMT_LINKED = EnumT("linked")
val ENUMT_EVALUATING = EnumT("evaluating")
val ENUMT_EVALUATED = EnumT("evaluated")
val ENUMT_NUMBER = EnumT("Number")
val ENUMT_BIGINT = EnumT("BigInt")
val ENUMT_NORMAL = EnumT("normal")
val ENUMT_BREAK = EnumT("break")
val ENUMT_CONTINUE = EnumT("continue")
val ENUMT_RETURN = EnumT("return")
val ENUMT_THROW = EnumT("throw")
val ENUMT_SUSPENDED_START = EnumT("suspendedStart")
val ENUMT_SUSPENDED_YIELD = EnumT("suspendedYield")
val ENUMT_EXECUTING = EnumT("executing")
val ENUMT_AWAITING_RETURN = EnumT("awaitingDASHreturn")
val ENUMT_COMPLETED = EnumT("completed")
val ENUMT_PENDING = EnumT("pending")
val ENUMT_FULFILLED = EnumT("fulfilled")
val ENUMT_REJECTED = EnumT("rejected")
val ENUMT_FULFILL = EnumT("Fulfill")
val ENUMT_REJECT = EnumT("Reject")

extension (elem: Boolean) {
  inline def isTop: Boolean = elem == true
  inline def isBottom: Boolean = elem == false
  inline def --(that: Boolean): Boolean = elem && !that
}

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
    case SEBool(bool) => app >> bool
    case SEStr(str)   => app >> "\"" >> normStr(str) >> "\""
    case SERef(ref)   => app >> ref
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
given Rule[RefinementKind] = (app, kind) => app >> kind.toString
given Ordering[RefinementKind] = Ordering.by(_.toString)
