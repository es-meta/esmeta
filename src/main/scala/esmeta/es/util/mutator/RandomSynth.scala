package esmeta.es.util.mutator

import scala.collection.mutable.{Stack, Map as MMap, Set as MSet}
import esmeta.spec.*
import esmeta.es.*

import scala.collection.mutable
import esmeta.util.*

/** ast synthesizer for random mutation */
class RandomSynth(grammar: Grammar) {
  var fromNonterminal: String = "AssignmentExpression"
  var toNonterminal: String = "RelationalExpression"

  /** a mapping from names to productions */
  lazy val nameMap: Map[String, Production] = grammar.nameMap

  /** a mapping from rhs to lhs */
  lazy val reversedMap: MMap[String, List[String]] = {
    val revMap: MMap[String, List[String]] =
      MMap.empty.withDefaultValue(List.empty)
    for {
      prod <- grammar.prods
      rhs <- prod.rhsList
      symbol <- rhs.symbols
    } yield {
      symbol match {
        case Terminal(term)          => revMap(term) :+= prod.lhs.name
        case Nonterminal(name, _, _) => revMap(name) :+= prod.lhs.name
        case _                       =>
      }
    }
    revMap
  }

  case class GrammarState(
    stack: List[String],
    routes: List[List[String]],
    touched: Set[String],
  )

//  val grammarMonad: StateMonad[GrammarState] =
//    StateMonad[GrammarState]()

  def searchRoute(state: GrammarState): GrammarState = {
    reversedMap(state.stack.head).foldLeft(state) {
      case (GrammarState(stack, routes, touched), producer) =>
        if (producer == fromNonterminal)
          GrammarState(
            stack,
            (producer :: stack) :: routes,
            touched + producer,
          )
        else if (touched.contains(producer))
          GrammarState(stack, routes, touched)
        else
          searchRoute(
            GrammarState(producer :: stack, routes, touched + producer),
          ).copy(stack = stack)
    }
  }
  def getRouteParam(route: List[String]): List[String] = {
    route match {
      case head :: next => {
        val prod = nameMap(head)
        prod.rhsList

        ???
      }
      case Nil => Nil
    }
  }

  def synthesize(from: String, to: String): Ast => Ast = {
    fromNonterminal = from
    toNonterminal = to

    val routes = searchRoute(
      GrammarState(List(toNonterminal), List.empty, Set.empty),
    ).routes

    routes.foreach(_.foreach(println(_)))
    x => x
  }
}
