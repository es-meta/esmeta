package esmeta.es.util.mutator

import esmeta.es.util.mutator.GrammarNode.isValid

import scala.collection.mutable.Map as MMap

class GrammarNode(
  val name: String,
  val rhsSymbols: List[Option[List[Option[String]]]],
) {
  var parents: List[GrammarNode] = List.empty
  var score: Option[Int] = None
  val symbolScores: MMap[String, Option[Int]] =
    MMap.empty.withDefaultValue(None)
  var rhsScores: List[Option[Int]] = List.empty

  private def maxIfValid(list: List[Option[Int]]): Option[Int] = {
    if isValid(list) then Some(list.flatten.max) else None
  }

  private def minIfValid(list: List[Option[Int]]): Option[Int] = {
    if isValid(list) then Some(list.flatten.min) else None
  }

  private def updateParents(): Unit =
    parents.foreach(parent => score.map(parent.updateChildScore(_, name)))

  /** update if rhsSymbolScores has changed */
  def update(): Unit = {
    rhsScores = rhsSymbols.map(
      _.flatMap(symbols =>
        maxIfValid(symbols.flatMap(s => s.map(symbolScores(_)))),
      ),
    )
    val oldScore = score
    score =
      if rhsScores.flatten.nonEmpty then Some(rhsScores.flatten.min + 1)
      else None
    if (oldScore != score)
      updateParents()
  }

  def setScore(newScore: Int): Unit = {
    score = Some(newScore); updateParents()
  }

  def updateChildScore(newScore: Int, childName: String): Unit = {
    symbolScores(childName) = Some(newScore)
    update()
  }

  def simplestRhsIdx: Option[Integer] =
    val validRhsScoresWithIndex = rhsScores.zipWithIndex.flatMap {
      case (scoreOpt, idx) => scoreOpt.map((_, idx))
    }
    if validRhsScoresWithIndex.nonEmpty then
      Some(validRhsScoresWithIndex.min._2)
    else None
}

object GrammarNode {
  def isValid[T](list: List[Option[T]]): Boolean = list.foldLeft(true) {
    case (_, None)       => false
    case (bool, Some(_)) => bool
  } && list.nonEmpty
}
