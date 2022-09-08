package esmeta.es.util.mutator

import scala.collection.mutable.{Stack, Map as MMap, Set as MSet}
import esmeta.spec.*
import esmeta.es.*

import scala.collection.mutable
import esmeta.util.*

import scala.annotation.tailrec

/** ast synthesizer for random mutation */
class RandomSynth(grammar: Grammar) {

  val generator: SimpleAstGenerator = SimpleAstGenerator(grammar)

  /** a mapping from names to productions */
  lazy val nameMap: Map[String, Production] = grammar.nameMap

  val synthBreakpoints: Set[String] = Set(
    "AssignmentExpression",
    "PrimaryExpression",
    "Statement",
    "VariableDeclaration",
  )
  def synthesizeNonterminalOptOpt(
    name: String,
    args: List[Boolean],
    argsMap: Map[String, Boolean],
    ntName: String,
    ntArgs: List[NonterminalArgument],
    optional: Boolean,
  ): Option[Option[Ast]] = {
    val makeOrNot = !optional || BaseUtils.randBool
//    val makeOrNot = false
    if makeOrNot then {
      // TODO: Duplicated code
      val newArgs = ntArgs.flatMap {
        case NonterminalArgument(kind, ntArgName) =>
          kind match {
            case NonterminalArgumentKind.Pass  => argsMap.get(ntArgName)
            case NonterminalArgumentKind.True  => Some(true)
            case NonterminalArgumentKind.False => Some(false)
          }
      }
      //      val simpleOrNot = BaseUtils.randBool || synthBreakpoints.contains(name)
      val simpleOrNot = true
      if simpleOrNot then Some(generator.generate(ntName, newArgs))
      else
        println(s"name: $name")
        println(s"argsmap: $argsMap")
//        println(s"validRhsList: $validRhsList")
        println(s"ntName: $ntName")
        synthesize(ntName, newArgs).map(Some(_))
    } else Some(None)
  }

  def synthesize(name: String, args: List[Boolean]): Option[Ast] = {
    nameMap
      .get(name)
      .flatMap(prod =>
        prod.kind match {
          case ProductionKind.Syntactic =>
            val argsMap = (prod.lhs.params zip args).toMap
            val validRhsList = prod.rhsList.filter(rhs =>
              rhs.condition match {
                case None                          => true
                case Some(RhsCond(condName, pass)) =>
//                  println(s"name: $name")
//                  println(s"argsmap: $argsMap")
//                  println(s"condName: $condName")
                  (argsMap(condName) && pass) || (!argsMap(condName) && !pass)

              },
            )
            val (rhs, rhsIdx) = BaseUtils.chooseWithIndex(validRhsList)
            val childrenOpt = rhs.symbols.map {
              case Terminal(term) => Some(Some(Lexical(term, term)))
              case Nonterminal(ntName, ntArgs, optional) =>
                synthesizeNonterminalOptOpt(
                  name,
                  args,
                  argsMap,
                  ntName,
                  ntArgs,
                  optional,
                )
              case ButNot(Nonterminal(ntName, ntArgs, optional), _) =>
                synthesizeNonterminalOptOpt(
                  name,
                  args,
                  argsMap,
                  ntName,
                  ntArgs,
                  optional,
                )
              case _ => None // TODO: handle other symbols?
            }
//            println(s"childrenOpt: $childrenOpt")
            if childrenOpt.forall(_.isDefined) then
              Some(Syntactic(name, args, rhsIdx, childrenOpt.flatten))
            else None
          case _ => generator.generate(name)
        },
      )
  }
}
