package esmeta.es.util.mutator

import scala.collection.mutable.{Stack, Map as MMap, Set as MSet}
import esmeta.spec.*
import esmeta.es.*
import esmeta.parser.ESParser

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
//        println(s"name: $name")
//        println(s"argsmap: $argsMap")
//        println(s"validRhsList: $validRhsList")
//        println(s"ntName: $ntName")
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
            val activeRhsList = prod.rhsList.map(rhs =>
              if rhs.condition match {
                  case None                          => true
                  case Some(RhsCond(condName, pass)) =>
//                    println(s"condName: $condName")
                    (argsMap(condName) && pass) || (!argsMap(condName) && !pass)

                }
              then Some(rhs)
              else None,
            )
            val (rhs, rhsIdx) =
              BaseUtils.choose(activeRhsList.zipWithIndex.flatMap {
                case (Some(r), i) => Some(r, i)
                case _            => None
              })
            val childrenOpt = rhs.symbols.filter(_.getNt.isDefined).map {
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
//            println(s"name: $name")
//            println(s"argsmap: $argsMap")
//            println(s"activeRhsList: $activeRhsList")
//            println(s"rhs, rhsIdx: $rhs, $rhsIdx")
//            println(s"childrenOpt: $childrenOpt")
            if childrenOpt.forall(_.isDefined) then
              Some(Syntactic(name, args, rhsIdx, childrenOpt.flatten))
            else None
          case _ => generator.generate(name)
        },
      )
  }

  def test(): Unit = {
    val syntacticNameMap: Map[String, Production] =
      nameMap.filter(_._2.kind == ProductionKind.Syntactic)
    var counter = 0
    var curStr = ""
    var cur: Ast = Lexical("", "")
    for ((name, prod) <- syntacticNameMap) {
      if (!RandomMutation.skippingProductions.contains(name)) {
        try {
          val argLen = prod.lhs.params.length
          val randArg = List.fill(argLen)(BaseUtils.randBool)
          val resultOpt = synthesize(name, randArg)
          resultOpt match {
            case None =>
              println(s"synthesis fail: $name"); counter += 1
            case Some(result) => {
              cur = result
              curStr = result.toString(grammar = Some(grammar))
              val newCur = ESParser(grammar)(name, randArg)
                .from(curStr)
                .toString(grammar = Some(grammar))
              val validity = newCur == curStr
              if (!validity) {
                println(s"synthesis is invalid: $name")
                println(curStr)
                println(cur)
                println(newCur)
                counter += 1
              }
            }
          }
        } catch {
          case s: Throwable => {
            println(s"synthesis parsing fail: $name"); counter += 1
            println(s)
            println(curStr)
            println(cur)
          }
        }
      }
    }
    println(s"total $counter failures.")
  }
}
