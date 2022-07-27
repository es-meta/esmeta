package esmeta.analyzer.domain

import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.interp.*
import esmeta.ir.Name
import esmeta.js.Ast
import esmeta.util.BaseUtils.*

/** values for analysis */
sealed trait AValue extends AnalyzerElem
object AValue {

  /** from original concrete values */
  def from(value: Value): AValue = value match
    case Comp(ty, value, targetOpt) =>
      AComp(
        AConst(ty.name),
        from(value),
        targetOpt.fold[AValue](AConst("empty"))(str => ASimple(Str(str))),
      )
    case addr: Addr    => Loc.from(addr)
    case AstValue(ast) => AAst(ast)
    case Clo(func, captured) =>
      val newCaptured = captured.map { case (x, v) => x -> AbsValue(v) }.toMap
      AClo(func, newCaptured)
    case Math(n)           => AMath(n)
    case Const(c)          => AConst(c)
    case CodeUnit(cu)      => ACodeUnit(cu)
    case Grammar(name, ps) => AGrammar(name, ps)
    case sv: SimpleValue   => ASimple(sv)
    case _                 => error(s"impossible to convert to AValue: $value")

  /** from singleton types */
  def from(ty: SingleT): AValue = ty match
    case CodeUnitT(cu)         => ACodeUnit(cu)
    case ConstT(c)             => AConst(c)
    case MathSingleT(n)        => AMath(n)
    case NumberSingleT(n)      => ASimple(Number(n))
    case BigIntSingleT(bigint) => ASimple(BigInt(bigint))
    case StrSingleT(str)       => ASimple(Str(str))
    case BoolSingleT(bool)     => ASimple(Bool(bool))
    case UndefT                => ASimple(Undef)
    case NullT                 => ASimple(Null)
    case AbsentT               => ASimple(Absent)
    case NilT                  => ???
}

/** completions values */
case class AComp(ty: AConst, value: AValue, target: AValue) extends AValue

/** abstract locations for addresses */
sealed trait Loc extends AValue {

  /** check named locations */
  def isNamed: Boolean = this match
    case NamedLoc(_) | SubMapLoc(NamedLoc(_)) => true
    case _                                    => false

  /** get base locations */
  def base: BaseLoc = this match
    case base: BaseLoc   => base
    case SubMapLoc(base) => base
}
object Loc {
  // from original concrete addresses
  private val subMapPattern = "(.+).SubMap".r
  def from(addr: Addr): Loc = addr match
    case NamedAddr(name) =>
      name match
        case subMapPattern(base) => SubMapLoc(NamedLoc(base))
        case name                => NamedLoc(name)
    case _ => error(s"impossible to convert to Loc: $addr")
}
sealed trait BaseLoc extends Loc
case class NamedLoc(name: String) extends BaseLoc
case class AllocSite(k: Int, view: View) extends BaseLoc
case class SubMapLoc(baseLoc: BaseLoc) extends Loc

/** closures */
case class AClo(
  func: Func,
  captured: Map[Name, AbsValue],
) extends AValue

/** continuations */
case class ACont(
  target: NodePoint[Node],
  captured: Map[Name, AbsValue],
) extends AValue

/** AST values */
case class AAst(ast: Ast) extends AValue

/** grammar */
case class AGrammar(name: String, params: List[Boolean]) extends AValue

/** math values */
case class AMath(n: BigDecimal) extends AValue

/** constants */
case class AConst(name: String) extends AValue

/** code units */
case class ACodeUnit(c: Char) extends AValue

/** simple values */
case class ASimple(value: SimpleValue) extends AValue
