package esmeta.es.util.mutator

import esmeta.es.util.mutator.LexicalNode.isValid

import scala.collection.mutable
import scala.collection.mutable.Map as MMap

/** a class for find the shallowest string of a lexical production */
class LexicalNode(
  val name: String,
  val rhsSymbols: List[Option[List[Option[String]]]],
) {
  var parents: List[LexicalNode] = List.empty
  var depth: Option[Int] = None
  val symbolDepths: MMap[String, Option[Int]] =
    MMap.empty.withDefaultValue(None)
  var rhsDepths: List[Option[Int]] = List.empty

  private def maxIfValid(list: List[Option[Int]]): Option[Int] = {
    if isValid(list) then Some(list.flatten.max) else None
  }

  def updateParents(): Unit =
    parents.foreach(parent => {
      depth.map(parent.updateChildDepth(_, name))
    })

  /** update if rhsSymbolScores has changed */
  def update(): Unit = {
    rhsDepths = rhsSymbols.map(
      _.flatMap(symbols =>
        maxIfValid(symbols.flatMap(s => s.map(symbolDepths(_)))),
      ),
    )
    val oldDepth = depth
    depth =
      if rhsDepths.flatten.nonEmpty then Some(rhsDepths.flatten.min + 1)
      else None
    if (oldDepth != depth)
      updateParents()
  }

  def setDepth(newDepth: Int): Unit = {
    depth = Some(newDepth); updateParents()
  }

  def updateChildDepth(newDepth: Int, childName: String): Unit = {
    symbolDepths(childName) = Some(newDepth)
    update()
  }

  def simplestRhsIdx: Option[Integer] =
    val validRhsDepthsWithIndex = rhsDepths.zipWithIndex.flatMap {
      case (depthOpt, idx) => depthOpt.map((_, idx))
    }
    if validRhsDepthsWithIndex.nonEmpty then
      Some(validRhsDepthsWithIndex.min._2)
    else None
}

object LexicalNode {
  def isValid[T](list: Iterable[Option[T]]): Boolean =
    list.foldLeft(true) {
      case (_, None)       => false
      case (bool, Some(_)) => bool
    } && list.nonEmpty
}
