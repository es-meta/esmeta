package esmeta.spec

import esmeta.spec.util.*

/** alternatives or right-hand-sides (RHSs) of productions */
case class Rhs(
  condition: Option[RhsCond],
  symbols: List[Symbol],
  id: Option[String],
) extends SpecElem {

  /** get RHS all names */
  def allNames: List[String] =
    symbols.foldLeft(List[String]("")) {
      case (names, Terminal(term)) => names.map(_ + term)
      case (names, Nonterminal(name, _, optional)) =>
        names.flatMap(x => {
          if (optional) List(x, x + name) else List(x + name)
        })
      case (names, ButNot(base, _)) =>
        names.map(_ + base.name)
      case (names, ButOnlyIf(base, _, _)) =>
        names.map(_ + base.name)
      case (names, _) => names
    }

  /** get non-terminals in an RHS */
  def nts: List[Nonterminal] = symbols.flatMap(_.getNt)

  /** check if empty */
  def isEmpty: Boolean = symbols match
    case Empty :: Nil => true
    case _            => false

  /** get index of non-terminal */
  def getNtIndex(ntName: String): Option[Int] =
    val filtered = nts.zipWithIndex.filter { case (nt, _) => nt.name == ntName }
    filtered match
      case (_, idx) :: rest => Some(idx)
      case _                => None

  /** get parameters from RHSs */
  // TODO give more precise type
  def params: List[Param] =
    nts.map(nt => Param(nt.name, Param.Kind.Normal, UnknownType))
}
object Rhs extends Parser.From[Rhs] {

  /** conditions for RHSs */
  case class Condition(name: String, pass: Boolean) extends SpecElem
  object Condition extends Parser.From[Condition]
}

/** helpers for nonterminal arguments */
type RhsCond = Rhs.Condition
val RhsCond = Rhs.Condition
