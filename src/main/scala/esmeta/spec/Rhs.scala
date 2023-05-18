package esmeta.spec

import esmeta.lang.*
import esmeta.spec.util.*
import esmeta.ty.*
import esmeta.util.BaseUtils.cached

/** alternatives or right-hand-sides (RHSs) of productions */
case class Rhs(
  conditions: List[RhsCond],
  symbols: List[Symbol],
  id: Option[String],
) extends SpecElem {

  /** get RHS all names */
  def allNames: List[String] =
    symbols.foldLeft(List[String]("")) {
      case (names, Terminal(term)) =>
        names.map(_ + term)
      case (names, s: Optional) =>
        s.getName.fold(names)(name => names.flatMap(x => List(x, x + name)))
      case (names, s) =>
        s.getName.fold(names)(name => names.map(_ + name))
    }

  /** optional symbols */
  lazy val optionals = symbols.collect { case opt: Optional => opt }

  lazy val nts: List[Nonterminal] = for {
    symbol <- symbols
    nt <- symbol.getNt
  } yield nt

  /** get non-terminals with whether it is optional in an RHS */
  lazy val ntsWithOptional: List[(Nonterminal, Boolean)] = for {
    symbol <- symbols
    nt <- symbol.getNt
  } yield (nt, symbol.isInstanceOf[Optional])

  /** get terminals in an RHS */
  lazy val ts: List[Terminal] = symbols.flatMap(_.getT)

  /** get terminals in an RHS */
  lazy val getNts = cached[Int, List[Option[String]]] { subIdx =>
    val binStr = subIdx.toBinaryString
    val optCount = optionals.size
    var flags = (("0" * (optCount - binStr.length)) + binStr).map(_ == '1')
    for {
      symbol <- symbols
      nt <- symbol.getNt
    } yield symbol match
      case _: Optional =>
        val present = flags.head
        flags = flags.tail
        if (present) Some(nt.name) else None
      case _ => Some(nt.name)
  }

  /** count sub production */
  lazy val countSubs: Int = scala.math.pow(2, optionals.size).toInt

  /** check if empty */
  def isEmpty: Boolean = symbols match
    case Empty :: Nil => true
    case _            => false

  /** get index of non-terminal */
  def getNtIndex(ntName: String): Option[Int] =
    nts.zipWithIndex.find(_.head.name == ntName).map(_.last)

  /** get parameters from RHSs */
  def params: List[Param] = nts.map(nt => Param(nt.name, Type(AstT(nt.name))))

  /** check whether the RHS is available */
  def available(argsSet: Set[String]): Boolean = conditions.forall {
    case RhsCond(name, pass) => (argsSet contains name) == pass
  }

  /** check whether the RHS is available */
  def available(argsMap: Map[String, Boolean]): Boolean =
    conditions.forall(cond => argsMap(cond.name) == cond.pass)
}
object Rhs extends Parser.From(Parser.rhs) {

  /** conditions for RHSs */
  case class Condition(name: String, pass: Boolean) extends SpecElem
  object Condition extends Parser.From(Parser.rhsCond)
}

/** helpers for nonterminal arguments */
type RhsCond = Rhs.Condition
val RhsCond = Rhs.Condition
