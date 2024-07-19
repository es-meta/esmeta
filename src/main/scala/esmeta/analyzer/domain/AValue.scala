package esmeta.analyzer.domain

import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.state.*
import esmeta.ir.Name
import esmeta.es.Ast
import esmeta.util.BaseUtils.*

trait AValueDecl { self: Self =>

  /** values for analysis */
  type AValue = AComp | APureValue
  object AValue:
    /** from original values */
    def from(value: Value): AValue = value match
      case comp: Comp           => AComp.from(comp)
      case pureValue: PureValue => APureValue.from(pureValue)

  /** completion values for analysis */
  case class AComp(ty: Enum, value: APureValue, target: Option[String])
  object AComp:
    /** from original completions */
    def from(comp: Comp): AComp =
      val Comp(ty, value, target) = comp
      AComp(ty, APureValue.from(value), target)

  /** pure values for analysis */
  type APureValue = Part | AClo | ACont | AstValue | Nt | Math | Infinity |
    Enum | CodeUnit | SimpleValue
  object APureValue:
    /** from original pure values */
    def from(pureValue: PureValue): APureValue = pureValue match
      case addr: Addr               => Part.from(addr)
      case clo: Clo                 => AClo.from(clo)
      case cont: Cont               => ACont.from(cont)
      case astValue: AstValue       => astValue
      case nt: Nt                   => nt
      case math: Math               => math
      case infinity: Infinity       => infinity
      case enumv: Enum              => enumv
      case codeUnit: CodeUnit       => codeUnit
      case simpleValue: SimpleValue => simpleValue

  /** address partitions */
  sealed trait Part:
    /** check named elements */
    def isNamed: Boolean = this match
      case Named(_) | SubMap(Named(_)) => true
      case _                           => false

    /** get base elements */
    def base: Base
  object Part:
    /** from original addresses */
    def from(addr: Addr): Part = addr match
      case NamedAddr(name) =>
        name match
          case subMapPattern(base) => SubMap(Named(base))
          case name                => Named(name)
      case _ => error(s"impossible to convert to Loc: $addr")
    private val subMapPattern = "(.+).SubMap".r
  sealed trait Base extends Part { def base = this }
  case class Named(name: String) extends Base
  case class AllocSite(k: Int, view: View) extends Base
  case class SubMap(base: Base) extends Part

  /** closures */
  case class AClo(func: Func, captured: Map[Name, AbsValue])
  object AClo:
    /** from original closures */
    def from(clo: Clo): AClo =
      val Clo(func, captured) = clo
      val newCaptured = captured.map((x, v) => x -> AbsValue(v)).toMap
      AClo(func, newCaptured)

  /** continuations */
  case class ACont(target: NodePoint[Node], captured: Map[Name, AbsValue])
  object ACont:
    /** from original continuations */
    def from(cont: Cont): ACont =
      error(s"impossible to convert continuations: $cont")
}
