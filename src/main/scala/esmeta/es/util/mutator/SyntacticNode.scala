package esmeta.es.util.mutator

import esmeta.es.util.mutator.GrammarNode.isValid
import esmeta.spec.*

import scala.collection.mutable.Map as MMap

class SyntacticNode(
  val name: String,
  val rhsSymbols: List[Option[List[Option[String]]]],
  val rhsConditions: List[Option[RhsCond]] = Nil,
) {
  var parents: List[SyntacticNode] = List.empty
  var length: Option[Int] = None
  val symbolLengths: MMap[String, Option[Int]] =
    MMap.empty.withDefaultValue(None)
  var rhsLengths: List[Option[Int]] = List.empty

  private def sumIfValid(list: List[Option[Int]]): Option[Int] = {
    if isValid(list) then Some(list.flatten.sum) else None
  }

  private def minIfValid(list: List[Option[Int]]): Option[Int] = {
    if isValid(list) then Some(list.flatten.min) else None
  }

  def updateParents(): Unit =
    parents.foreach(parent => {
      length.map(parent.updateChildLength(_, name))
    })

  /** update if rhsSymbolLengths has changed */
  def update(): Unit = {
    rhsLengths = rhsSymbols.map(
      _.flatMap(symbols =>
        sumIfValid(symbols.flatMap(s => s.map(symbolLengths(_)))),
      ),
    )
    val oldLength = length
    length = if rhsLengths.flatten.nonEmpty then {
      Some(rhsLengths.flatten.min)
    } else None
    if (oldLength != length)
//      println(s"$name has been updated from $oldLength to $length.");
      updateParents()
  }

  def setLength(newLength: Int): Unit = {
    length = Some(newLength); updateParents()
//    println(s"$name has been set to $length.")
  }

  def updateChildLength(newLength: Int, childName: String): Unit = {
    symbolLengths(childName) = Some(newLength)
    update()
  }

  def simplestRhsIdx: Option[Integer] =
    val validRhsLengthsWithIndex = rhsLengths.zipWithIndex.flatMap {
      case (lengthOpt, idx) => lengthOpt.map((_, idx))
    }
    if validRhsLengthsWithIndex.nonEmpty then
      Some(validRhsLengthsWithIndex.min._2)
    else None

  def simplestRhsIdx(argsMap: Map[String, Boolean]) = {
    val validRhsLengthsWithIndex =
      (rhsLengths zip rhsConditions).zipWithIndex.flatMap {
        case ((lengthOpt, conditionOpt), idx) =>
          conditionOpt match {
            case Some(RhsCond(condName, pass))
                if (argsMap(condName) && !pass) || (!argsMap(
                  condName,
                ) && pass) =>
              None
            case _ => lengthOpt.map((_, idx))
          }
      }
    if validRhsLengthsWithIndex.nonEmpty then
      Some(validRhsLengthsWithIndex.min._2)
    else None

  }
}
