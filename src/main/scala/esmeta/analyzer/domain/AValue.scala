package esmeta.analyzer.domain

import esmeta.analyzer.*
import esmeta.cfg.*
import esmeta.state.*
import esmeta.ir.Name
import esmeta.es.Ast
import esmeta.util.BaseUtils.*

trait AValueDecl { self: Self =>

  /** pure values for analysis */
  type AValue = Part | AClo | ACont | AstValue | GrammarSymbol | Math |
    Infinity | Enum | CodeUnit | SimpleValue
  object AValue:
    /** from original pure values */
    def from(value: Value): AValue = value match
      case addr: Addr                   => Part.from(addr)
      case clo: Clo                     => AClo.from(clo)
      case cont: Cont                   => ACont.from(cont)
      case astValue: AstValue           => astValue
      case grammarSymbol: GrammarSymbol => grammarSymbol
      case math: Math                   => math
      case infinity: Infinity           => infinity
      case enumv: Enum                  => enumv
      case codeUnit: CodeUnit           => codeUnit
      case simpleValue: SimpleValue     => simpleValue
      case pclo: PClo                   => ???
      case pcont: PCont                 => ???

  /** address partitions */
  sealed trait Part:
    /** check named elements */
    def isNamed: Boolean = this match
      case Named(_) | InnerMap(Named(_)) => true
      case _                             => false

    /** get base elements */
    def base: Base
  object Part:
    /** from original addresses */
    def from(addr: Addr): Part = addr match
      case NamedAddr(name) =>
        name match
          case mapPattern(base) => InnerMap(Named(base))
          case name             => Named(name)
      case _ => error(s"impossible to convert to Loc: $addr")
    private val mapPattern = "(.+).Map".r
  sealed trait Base extends Part { def base = this }
  case class Named(name: String) extends Base
  case class AllocSite(k: Int, view: View) extends Base
  // TODO remove
  case class InnerMap(base: Base) extends Part

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
