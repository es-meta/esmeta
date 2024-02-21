package esmeta.spec

import esmeta.spec.util.Parser

/** productions for ECMAScript grammars */
case class Production(
  lhs: Lhs,
  kind: ProductionKind,
  oneof: Boolean,
  rhsVec: Vector[Rhs],
) extends SpecElem {

  /** get name */
  lazy val name: String = lhs.name

  /** get the index mapping for productions */
  lazy val idxMap: Map[String, (Int, Int)] = (for {
    (rhs, i) <- rhsVec.zipWithIndex
    (name, j) <- rhs.allNames.zipWithIndex
  } yield lhs.name + ":" + name -> (i, j)).toMap

  /** get non-terminals with whether it is optional in an RHS */
  lazy val nts: List[Nonterminal] = for {
    rhs <- rhsVec.toList
    nt <- rhs.nts
  } yield nt

  /** get non-terminals with whether it is optional in an RHS */
  lazy val ntsWithOptional: List[(Nonterminal, Boolean)] = for {
    rhs <- rhsVec.toList
    pair <- rhs.ntsWithOptional
  } yield pair

  /** get terminals in RHSs */
  lazy val ts: List[Terminal] = for {
    rhs <- rhsVec.toList
    t <- rhs.ts
  } yield t
}

object Production extends Parser.From(Parser.prod)
enum ProductionKind extends SpecElem:
  case Syntactic, Lexical, NumericString
object ProductionKind extends Parser.From(Parser.prodKind)

/** ordering of productions */
given Ordering[Production] =
  Ordering.by(prod => (prod.kind.ordinal, prod.lhs.name))
