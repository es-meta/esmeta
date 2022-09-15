package esmeta.es.util.mutator

import scala.collection.mutable.Map as MMap
import esmeta.spec.*
import esmeta.es.*
import esmeta.spec
import esmeta.util.BaseUtils
import esmeta.parser.ESParser

class SimpleAstGenerator(grammar: Grammar) {

  /** a mapping from names to productions */
  lazy val nameMap: Map[String, Production] = grammar.nameMap

  /** a mapping from names to lexical productions */
  lazy val lexicalNameMap: Map[String, Production] =
    nameMap.filter(_._2.kind == ProductionKind.Lexical)

  /** a mapping from names to syntactic productions */
  lazy val syntacticNameMap: Map[String, Production] =
    nameMap.filter(_._2.kind == ProductionKind.Syntactic)

  /** a mapping from rhs to lhs */
  val reversedMap: MMap[String, Set[String]] =
    MMap.empty.withDefaultValue(Set.empty)

  /** a mapping from production name to SyntacticNode */
  var syntacticNodeMap: Map[String, SyntacticNode] = Map.empty

  /** a mapping from production name to LexicalNode */
  var lexicalNodeMap: Map[String, LexicalNode] = Map.empty

  /** a set of terminals in the grammar */
  var terminals: Set[String] = Set.empty

  /** a set of terminals used in syntactic productions */
  val topLevelTerminals: Set[String] = grammar.topLevelTerminals

  /** a set of lexicals used in syntactic productions */
  val topLevelLexicals: Set[String] =
    grammar.topLevelLexicals ++ topLevelTerminals

  /** ast generation cache */
  var cache: MMap[String, Ast] = MMap.empty

  val reservedLexicals: Map[String, String] = Map(
    "IdentifierName" -> "x",
    "NullLiteral" -> "null",
    "BooleanLiteral" -> "true",
    "NumericLiteral" -> "42",
    "StringLiteral" -> "''",
    "NoSubstitutionTemplate" -> "``",
    "TemplateHead" -> "`${",
    "TemplateMiddle" -> "}${",
    "TemplateTail" -> "}`",
    "RegularExpressionLiteral" -> "/a/",
    "PrivateIdentifier" -> "#x",
  )

  // Initialization
  {
    // warning if all top level lexicals (not terminals) are not reserved
    if (
      !LexicalNode.isValid(
        grammar.topLevelLexicals.map(reservedLexicals.get),
      )
    ) {
      println("Warning: there is an undefined top level lexical")
    }
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
          lexicalNodeMap += (term -> LexicalNode(term, List.empty))
        }
        case Nonterminal(name, _, _) => reversedMap(name) += prod.lhs.name
        case ButNot(Nonterminal(baseName, _, _), _) =>
          reversedMap(baseName) += prod.lhs.name // TODO: handle cases
        case _ => // TODO: ButOnlyIf, Lookahead
      }
    }
    // initialize syntacticNodeMap and lexicalNodeMap for non-terminals
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
        case _ => None // TODO: ButOnlyIf, Lookahead
      }))
      prod.kind match {
        case ProductionKind.Syntactic =>
          syntacticNodeMap += (prod.name -> SyntacticNode(
            prod.name,
            rhsSymbols,
            prod.rhsList.map(_.condition),
          ))
        case _ =>
          lexicalNodeMap += (prod.name -> LexicalNode(prod.name, rhsSymbols))
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

  /** generate shortest string for lexical production */
  def generateLexicalHelper(name: String): Option[String] = {
    if terminals.contains(name) then {
      Some(name)
    } else if reservedLexicals.contains(name) then Some(reservedLexicals(name))
    else {
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
          if LexicalNode.isValid(list) then Some(list.flatten) else None,
        )
        .map(_.mkString)
      ret
    }
  }

  /** generate shortest lexical production */
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

  /** generate shortest production */
  def generate(
    name: String,
    args: List[Boolean] = Nil,
  ): Option[Ast] = {
    cache.get(name.concat(args.mkString)) match {
      case None =>
        val ret = nameMap.get(name).flatMap { prod =>
          prod.kind match {
            case ProductionKind.Syntactic =>
              val syntacticNode = syntacticNodeMap(name)
              val simplestRhsIdxOpt =
                syntacticNode.simplestRhsIdx((prod.lhs.params zip args).toMap)
              val simplestRhs =
                simplestRhsIdxOpt.flatMap(prod.nonRecursiveRhsList(_))
//              println(simplestRhs)
              val instance = simplestRhs.map(_.symbols.flatMap {
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

//              println(s"syntacticNode $name")
//              print("rhsSymbols: "); println(syntacticNode.rhsSymbols)
//              print("rhsConditions: "); println(syntacticNode.rhsConditions)
//              print("length: "); println(syntacticNode.length)
//              print("rhsLengths: "); println(syntacticNode.rhsLengths)
//              print("symbolLengths: "); println(syntacticNode.symbolLengths)
//              print("simplestRhsIdxOpt: "); println(simplestRhsIdxOpt)
//              println(s"instance: $instance")
              for {
                child <- instance
                simplestRhsIdx <- simplestRhsIdxOpt
              } yield Syntactic(name, args, simplestRhsIdx, child)
            case _ => generateLexical(prod.name)
          }
        }
        ret.foreach(cache += name.concat(args.mkString) -> _)
        ret
      case x => x
    }
  }

  def test(): Unit = {
    var counter = 0
    var curStr = ""
    var cur: Ast = Lexical("", "")
    for ((name, prod) <- syntacticNameMap) {
      if (!RandomMutation.skippingProductions.contains(name)) {
        try {
          val argLen = prod.lhs.params.length
          val randArg = List.fill(argLen)(BaseUtils.randBool)
          val resultOpt = generate(name, randArg)
          resultOpt match {
            case None =>
              println(s"simple generation fail: $name"); counter += 1
            case Some(result) => {
              cur = result
              curStr = result.toString(grammar = Some(grammar))
              val newCur = ESParser(grammar)(name, randArg)
                .from(curStr)
                .toString(grammar = Some(grammar))
              val validity = newCur == curStr
              if (!validity) {
                println(s"simple generation is invalid: $name")
                println(curStr)
                println(cur)
                println(newCur)
                counter += 1
              }
            }
          }
        } catch {
          case s: Throwable => {
            println(s"simple generation parsing fail: $name"); counter += 1
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
