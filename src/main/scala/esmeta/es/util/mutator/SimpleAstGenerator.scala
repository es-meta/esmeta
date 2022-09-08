package esmeta.es.util.mutator

import scala.collection.mutable.Map as MMap
import esmeta.spec.*
import esmeta.es.*
import esmeta.spec

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

  var syntacticNodeMap: Map[String, SyntacticNode] = Map.empty
  var lexicalNodeMap: Map[String, GrammarNode] = Map.empty

  var terminals: Set[String] = Set.empty
  val topLevelTerminals: Set[String] = grammar.topLevelTerminals
  val topLevelLexicals: Set[String] =
    grammar.topLevelLexicals ++ topLevelTerminals

  var cache: MMap[String, Ast] = MMap.empty
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
        case ButNot(Nonterminal(baseName, _, _), _) =>
          reversedMap(baseName) += prod.lhs.name // TODO: handle cases
        case _ => // TODO: ButNot, ButOnlyIf, Lookahead
      }
    }
    // initialize grammarNodeMap and lexicalNodeMap for non-terminals
    topLevelTerminals.foreach(term =>
      syntacticNodeMap += (term -> SyntacticNode(term, List.empty)),
    )
    for (prod <- grammar.prods) yield {
      val rhsSymbols = prod.nonRecursiveRhsList.map(_.map(_.symbols.map {
        case Terminal(term) => Some(term)
        case Nonterminal(name, _, optional) =>
          if optional then None else Some(name)
        case ButNot(Nonterminal(name, _, optional), _) =>
          if optional then None else Some(name) // TODO: handle cases
        case _ => None // TODO: ButNot, ButOnlyIf, Lookahead
      }))
      prod.kind match {
        case ProductionKind.Syntactic =>
          syntacticNodeMap += (prod.name -> SyntacticNode(
            prod.name,
            rhsSymbols,
            prod.rhsList.map(_.condition),
          ))
        case _ =>
          lexicalNodeMap += (prod.name -> GrammarNode(prod.name, rhsSymbols))
          if topLevelLexicals.contains(prod.name) then
            syntacticNodeMap += (prod.name -> SyntacticNode(
              prod.name,
              List.empty,
            ))
      }
    }
    // initialize parents for grammar elements
    for ((child, parents) <- reversedMap) yield {
      syntacticNodeMap
        .get(child)
        .foreach(_.parents = parents.toList.flatMap(syntacticNodeMap.get(_)))

      lexicalNodeMap
        .get(child)
        .foreach(node =>
          node.parents = parents.toList.flatMap(lexicalNodeMap.get(_)),
        )
    }
    // initialize lexicalNodeMap for terminals
    for (term <- terminals) yield {
      lexicalNodeMap(term).setDepth(0)
    }

    val lexicalDepthMap: Map[String, Option[Int]] =
      lexicalNodeMap.map {
        case (name, node) => name -> node.depth
      }

    // calculate lengths of lexical productions and terminals
    val lexicalStrMap: Map[String, Option[String]] =
      lexicalNameMap.keys
        .map(lexicalName =>
          lexicalName -> generateLexical(lexicalName).map(_.str),
        )
        .toMap ++ terminals.map(term => term -> Some(term)).toMap
    cache ++= lexicalStrMap.flatMap {
      case (name, strOpt) => strOpt.map(name -> Lexical(name, _))
    }

    // initialize SyntacticNodeMap for terminals in syntactic production view(top level lexicals)
    for (term <- topLevelLexicals) yield {
      for {
        lexicalStrOpt <- lexicalStrMap.get(term)
        lexicalStr <- lexicalStrOpt
      } yield syntacticNodeMap(term).setLength(lexicalStr.length)
    }

    val syntacticLengthMap: Map[String, Option[Int]] =
      syntacticNodeMap.map {
        case (name, node) => name -> node.length
      }
  }

  def generateLexicalHelper(name: String): Option[String] = {
    if terminals.contains(name) then {
      Some(name)
    } else {
      val prod = nameMap(name)
      val lexicalNode = lexicalNodeMap(name)
      val simplestRhs =
        lexicalNode.simplestRhsIdx.flatMap(prod.nonRecursiveRhsList(_))
      val instance = simplestRhs.map(_.symbols.map {
        case Terminal(term) => generateLexicalHelper(term)
        case Nonterminal(name, _, optional) =>
          if optional then Some("") else generateLexicalHelper(name)
        case ButNot(Nonterminal(name, _, optional), _) =>
          if optional then Some("") else generateLexicalHelper(name)
        case _ => Some("")
      })
      val ret = instance
        .flatMap(list =>
          if GrammarNode.isValid(list) then Some(list.flatten) else None,
        )
        .map(_.mkString)
      ret
    }
  }

  def generateLexical(name: String): Option[Lexical] =
    generateLexicalHelper(name).map(Lexical(name, _))

  def generateNonterminalOptOpt(
    name: String,
    args: List[Boolean],
    argsMap: Map[String, Boolean],
    ntName: String,
    ntArgs: List[NonterminalArgument],
    optional: Boolean,
  ): Option[Option[Ast]] = {
    if optional then Some(None)
    else {
      val newArgs = ntArgs.flatMap {
        case NonterminalArgument(kind, ntArgName) =>
          kind match {
            case NonterminalArgumentKind.Pass  => argsMap.get(ntArgName)
            case NonterminalArgumentKind.True  => Some(true)
            case NonterminalArgumentKind.False => Some(false)
          }
      }
      Some(generate(ntName, newArgs))
    }
  }
  def generate(
    name: String,
    args: List[Boolean] = Nil,
  ): Option[Ast] = {
    cache.get(name) match {
      case None =>
        val ret = nameMap.get(name).flatMap { prod =>
          prod.kind match {
            case ProductionKind.Syntactic =>
              val syntacticNode = syntacticNodeMap(name)
//              println(s"syntacticNode $name")
//              print("rhsSymbols: "); println(syntacticNode.rhsSymbols)
//              print("rhsConditions: "); println(syntacticNode.rhsConditions)
//              print("length: "); println(syntacticNode.length)
//              print("rhsLengths: "); println(syntacticNode.rhsLengths)
//              print("symbolLengths: "); println(syntacticNode.symbolLengths)
              val simplestRhsIdxOpt =
                syntacticNode.simplestRhsIdx((prod.lhs.params zip args).toMap)
//              print("simplestRhsIdxOpt: ")
//              println(simplestRhsIdxOpt)
              val simplestRhs =
                simplestRhsIdxOpt.flatMap(prod.nonRecursiveRhsList(_))
              val instance = simplestRhs.map(_.symbols.flatMap {
//                No need to make terminal for symbolic ast
//                case Terminal(term) => Some(generateLexical(term))
                case Nonterminal(ntName, ntArgs, optional) =>
                  generateNonterminalOptOpt(
                    ntName,
                    args,
                    (prod.lhs.params zip args).toMap,
                    ntName,
                    ntArgs,
                    optional,
                  )
                case ButNot(Nonterminal(ntName, ntArgs, optional), _) =>
                  generateNonterminalOptOpt(
                    ntName,
                    args,
                    (prod.lhs.params zip args).toMap,
                    ntName,
                    ntArgs,
                    optional,
                  )
                case _ => None
              })
              for {
                child <- instance
                simplestRhsIdx <- simplestRhsIdxOpt
              } yield Syntactic(name, args, simplestRhsIdx, child)
            case _ => generateLexical(prod.name)
          }
        }
        ret.foreach(cache += name -> _)
        ret
      case x => x
    }
  }

  def debug() = println(syntacticNodeMap("CallExpression").length)
}
