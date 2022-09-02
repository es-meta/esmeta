package esmeta.es.util.mutator

import scala.collection.mutable.Map as MMap
import esmeta.spec.*
import esmeta.es.*

class SimpleAstGenerator(grammar: Grammar) {

  /** a mapping from names to productions */
  lazy val nameMap: Map[String, Production] = grammar.nameMap
  lazy val lexicalNameMap: Map[String, Production] =
    nameMap.filter(_._2.kind == ProductionKind.Lexical)
  lazy val syntacticNameMap: Map[String, Production] =
    nameMap.filter(_._2.kind == ProductionKind.Syntactic)

  /** a mapping from rhs to lhs */
  val reversedMap: MMap[String, Set[String]] =
    MMap.empty.withDefaultValue(Set.empty)

  var grammarNodeMap: Map[String, GrammarNode] = Map.empty
  var lexicalNodeMap: Map[String, GrammarNode] = Map.empty

  var terminals: Set[String] = Set.empty
  val topLevelTerminals: Set[String] = grammar.topLevelTerminals
  val topLevelLexicals: Set[String] =
    grammar.topLevelLexicals ++ topLevelTerminals

  // Initialization
  {
    // initialize reversedMap, terminals
    for {
      prod <- grammar.prods
      rhsOpt <- prod.nonRecursiveRhsList
      symbols = rhsOpt.map(_.symbols).getOrElse(Nil)
      symbol <- symbols
    } yield {
      symbol match {
        case Terminal(term) => {
          reversedMap(term) += prod.lhs.name
          terminals += term
          lexicalNodeMap += (term -> GrammarNode(term, List.empty))
        }
        case Nonterminal(name, _, _) => reversedMap(name) += prod.lhs.name
        case _                       => // TODO: ButNot, ButOnlyIf, Lookahead
      }
    }
    // initialize grammarNodeMap and lexicalNodeMap for non-terminals
    topLevelTerminals.foreach(term =>
      grammarNodeMap += (term -> GrammarNode(term, List.empty)),
    )
    for (prod <- grammar.prods) yield {
      val rhsSymbols = prod.nonRecursiveRhsList.map(_.map(_.symbols.map {
        case Terminal(term) => Some(term)
        case Nonterminal(name, _, optional) =>
          if optional then None else Some(name)
        case _ => None // TODO: ButNot, ButOnlyIf, Lookahead
      }))
      prod.kind match {
        case ProductionKind.Syntactic =>
          grammarNodeMap += (prod.name -> GrammarNode(prod.name, rhsSymbols))
        case _ =>
          lexicalNodeMap += (prod.name -> GrammarNode(prod.name, rhsSymbols))
          if topLevelLexicals.contains(prod.name) then
            grammarNodeMap += (prod.name -> GrammarNode(prod.name, List.empty))
      }
    }
    // initialize parents for grammar elements
    for ((child, parents) <- reversedMap) yield {
      grammarNodeMap
        .get(child)
        .foreach(_.parents = parents.toList.flatMap(grammarNodeMap.get(_)))

      lexicalNodeMap
        .get(child)
        .foreach(node =>
          node.parents = parents.toList.flatMap(lexicalNodeMap.get(_)),
        )
    }
    // initialize lexicalNodeMap for terminals
    for (term <- terminals) yield {
      lexicalNodeMap(term).setScore(0)
    }

    // initialize grammarNodeMap for terminals(top level lexicals)
    for (term <- topLevelLexicals) yield {
      grammarNodeMap(term).setScore(0)
    }

    val lexicalComplexityScoreMap: Map[String, Option[Int]] =
      lexicalNodeMap.map {
        case (name, node) => name -> node.score
      }

    val syntacticComplexityScoreMap: Map[String, Option[Int]] =
      grammarNodeMap.map {
        case (name, node) => name -> node.score
      }
  }

  def generateLexicalHelper(name: String): Option[String] = {
    if terminals.contains(name) then {
//      println(lexicalNodeMap(name).parents.map(_.name));
//      println(reversedMap(name));
      Some(name)
    } else {
      val prod = nameMap(name)
      val lexicalNode = lexicalNodeMap(name)
//      print("LexicalNode.rhsSymbols: "); println(lexicalNode.rhsSymbols)
//      print("LexicalNode.symbolScores: "); println(lexicalNode.symbolScores)
//      print("LexicalNode.score: "); println(lexicalNode.score)
//      print("LexicalNode.rhsScores: "); println(lexicalNode.rhsScores)
      val simplestRhs =
        lexicalNode.simplestRhsIdx.flatMap(prod.nonRecursiveRhsList(_))
      val instance = simplestRhs.map(_.symbols.map {
        case Terminal(term) => generateLexicalHelper(term)
        case Nonterminal(name, _, optional) =>
          if optional then Some("") else generateLexicalHelper(name)
        case _ => Some("")
      })
//      print("instance: "); println(instance)
      val ret = instance
        .flatMap(list =>
          if GrammarNode.isValid(list) then Some(list.flatten) else None,
        )
        .map(_.mkString)
//      print("returning: "); println(a)
      ret
    }
  }

  def generateLexical(name: String): Option[Lexical] =
    generateLexicalHelper(name).map(Lexical(name, _))

  def generate(
    name: String,
    args: List[Boolean] = Nil,
  ): Option[Ast] = {
    nameMap.get(name).flatMap { prod =>
      prod.kind match {
        case ProductionKind.Syntactic => {
          val syntacticNode = grammarNodeMap(name)
          val simplestRhsIdxOpt = syntacticNode.simplestRhsIdx
          val simplestRhs =
            simplestRhsIdxOpt.flatMap(prod.nonRecursiveRhsList(_))
          val instance = simplestRhs.map(_.symbols.flatMap {
            case Terminal(term) => Some(generateLexical(term))
            case Nonterminal(ntName, ntArgs, optional) =>
              if optional then None
              else {
                val newArgs = ntArgs.flatMap {
                  case NonterminalArgument(kind, ntArgName) =>
                    (prod.lhs.params zip args).toMap
                      .get(ntArgName)
                      .map(arg =>
                        kind match {
                          case NonterminalArgumentKind.Pass  => arg
                          case NonterminalArgumentKind.True  => true
                          case NonterminalArgumentKind.False => false
                        },
                      )
                }
                Some(generate(ntName, newArgs))
              }
            case _ => None
          })
          for {
            child <- instance
            simplestRhsIdx <- simplestRhsIdxOpt
          } yield Syntactic(name, args, simplestRhsIdx, child)
        }
        case _ => generateLexical(prod.name)
      }
    }
  }
}
