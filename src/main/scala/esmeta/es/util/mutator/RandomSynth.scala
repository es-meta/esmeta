package esmeta.es.util.mutator

import com.sun.xml.internal.bind.v2.TODO

import scala.collection.mutable.{Stack, Map as MMap, Set as MSet}
import esmeta.spec.*
import esmeta.es.*

import scala.collection.mutable
import esmeta.util.*

import scala.annotation.tailrec

/** ast synthesizer for random mutation */
class RandomSynth(grammar: Grammar) {

  var startNonterminal: String = "AssignmentExpression"
  var endNonterminal: String = "RelationalExpression"

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
  case class IncompleteAstState(
    route: List[String],
    incAstList: List[IncompleteAst],
  )
  type ParamInstance = Map[String, Boolean]
//  case class ParamInstance(values: List[Boolean], names: List[String])

//  val grammarMonad: StateMonad[GrammarState] =
//    StateMonad[GrammarState]()

  def searchRoute(state: GrammarState): GrammarState = {
    reversedMap(state.stack.head).foldLeft(state) {
      case (GrammarState(stack, routes, touched), producer) =>
        if (producer == startNonterminal)
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
  def enumerateBool(enumNum: Int): List[List[Boolean]] = {
    @tailrec
    def helper(n: Int, en: List[List[Boolean]]): List[List[Boolean]] =
      if n == 0 then en
      else helper(n - 1, en.flatMap(e => List(true :: e, false :: e)))
    helper(enumNum, List.empty)
  }
  def allParamInstances(paramNames: List[String]): List[ParamInstance] = {
    for (boolInstance <- enumerateBool(paramNames.length))
      yield (paramNames zip boolInstance).toMap
  }
  def arePossibleArgs(
    paramInstance: ParamInstance,
    symbolArgs: List[NonterminalArgument],
    args: List[Boolean],
  ): Boolean = {
    symbolArgs.zipWithIndex.foldLeft(true) {
      case (possible, (ntArg, idx)) =>
        if possible then
          ntArg.kind match {
            case NonterminalArgumentKind.True  => args(idx)
            case NonterminalArgumentKind.False => !args(idx)
            case NonterminalArgumentKind.Pass  => true
          }
        else false
    }
  }

//  def getIncompleteAst(state: IncompleteAstState): IncompleteAstState = {
//    // TODO: consider parameter constraint of startNonterminal and endNonterminal
//    state.route match {
//      case head :: Nil => {
//        val prod = nameMap(head)
//        val rhsList = prod.rhsList
//        ???
//      }
//      case head :: tail => {
//        val prod = nameMap(head)
////        prod.kind match { }
//        val paramNames = prod.lhs.params
//        for (paramInstance <- allParamInstances(paramNames)) yield {
//          getIncompleteAst(state.copy(route = tail)).incAstList.map {
//            case IncSyntactic(name, args, rhsIdx, children) => {
////              if (arePossibleArgs(paramInstance, prod.rhsList(rhsIdx).symbols))
//              ???
//            }
//            case IncLexical(name, str)            => ???
//            case IncDesired(name, args, rhsIdx)   => ???
//            case IncUndesired(name, args, rhsIdx) => ???
//          }
//        }
//        ???
//      }
//      case _ => ???
//    }
//  }

  def synthesize(from: String, to: String): Ast => Ast = {
    startNonterminal = from
    endNonterminal = to

    val routes = searchRoute(
      GrammarState(List(endNonterminal), List.empty, Set.empty),
    ).routes

    routes.foreach(_.foreach(println(_)))
    x => x
  }
}
