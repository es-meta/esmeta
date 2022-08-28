package esmeta.ty.util

import esmeta.ty.*
import esmeta.util.BaseUtils.*
import esmeta.util.BasicParsers

/** metalanguage parser */
object Parser extends Parsers
trait Parsers extends BasicParsers {
  // types
  given ty: Parser[Ty] = {
    "" ^^ { _ => ??? }
  }.named("ty.Ty")

  lazy val unknownTy: Parser[UnknownTy] = {
    "" ^^ { _ => ??? }
  }.named("ty.UnknownTy")

  lazy val valueTy: Parser[ValueTy] = {
    "" ^^ { _ => ??? }
  }.named("ty.ValueTy")

  /** completion record types */
  lazy val compTy: Parser[CompTy] = {
    "" ^^ { _ => ??? }
  }.named("ty.CompTy")

  /** list types */
  lazy val listTy: Parser[ListTy] = {
    "" ^^ { _ => ??? }
  }.named("ty.ListTy")

  /** pure value types (non-completion record types) */
  lazy val pureValueTy: Parser[PureValueTy] = {
    "" ^^ { _ => ??? }
  }.named("ty.PureValueTy")

  /** record types */
  lazy val recordTy: Parser[RecordTy] = {
    "" ^^ { _ => ??? }
  }.named("ty.RecordTy")

  /** sub map types */
  lazy val subMapTy: Parser[SubMapTy] = {
    "" ^^ { _ => ??? }
  }.named("ty.SubMapTy")
}
