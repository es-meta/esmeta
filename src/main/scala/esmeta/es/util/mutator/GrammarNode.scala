package esmeta.es.util.mutator

import scala.collection.mutable.{Map => MMap}

class GrammarNode(name: String, rhsSymbols: List[List[String]]) {
  var parents: List[GrammarNode] = List.empty
  var score: Option[Int] = None
  val symbolScores: MMap[String, Option[Int]] =
    MMap.empty.withDefaultValue(None)
  var rhsScores: List[Option[Int]] = List.empty

  private def isValid[T](list: List[Option[T]]): Boolean = list.foldLeft(true) {
    case (_, None)       => false
    case (bool, Some(_)) => bool
  } && list.nonEmpty

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
    rhsScores = rhsSymbols.map(s => maxIfValid(s.map(symbolScores(_))))
    val oldScore = score
    score = minIfValid(rhsScores).map(_ + 1)
    if (oldScore != score)
      updateParents()
  }

  def setScore(newScore: Int): Unit = { score = Some(newScore) }

  def updateChildScore(newScore: Int, childName: String): Unit = {
    symbolScores(childName) = Some(newScore)
    update()
  }
}
