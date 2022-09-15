package esmeta.spec

import esmeta.lang.*
import esmeta.spec.util.*
import esmeta.ty.*
import esmeta.util.BaseUtils.cached

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
  lazy val nts: List[Nonterminal] = symbols.flatMap(_.getNt)
  lazy val ts: List[Terminal] = symbols.flatMap(_.getT)
  lazy val getNts = cached[Int, List[Option[String]]] { subIdx =>
    val binStr = subIdx.toBinaryString
    val optCount = nts.count(_.optional)
    var flags = (("0" * (optCount - binStr.length)) + binStr).map(_ == '1')
    nts.map(nt =>
      if (nt.optional) {
        val present = flags.head
        flags = flags.tail
        if (present) Some(nt.name) else None
      } else Some(nt.name),
    )
  }

  /** count sub production */
  lazy val countSubs: Int = scala.math.pow(2, nts.count(_.optional)).toInt

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
  def params: List[Param] =
    nts.map(nt => Param(nt.name, Type(AstT(nt.name))))
}
object Rhs extends Parser.From(Parser.rhs) {

  /** conditions for RHSs */
  case class Condition(name: String, pass: Boolean) extends SpecElem
  object Condition extends Parser.From(Parser.rhsCond)
}

/** helpers for nonterminal arguments */
type RhsCond = Rhs.Condition
val RhsCond = Rhs.Condition
