package esmeta.synthesizer

import esmeta.es.*
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.spec.util.GrammarGraph

// TODO refactoring
/** A simple ECMAScript AST synthesizer */
class SimpleSynthesizer(
  grammar: Grammar,
) extends Synthesizer {

  val graph = GrammarGraph(grammar)
  import graph.*

  /** synthesizer name */
  def name: String = "SimpleSynthesizer"

  /** get script */
  def script: String = choose(initPool)

  /** get initial pool */
  lazy val initPool: Vector[String] =
    lazy val pool = (for {
      (node, scripts) <- scriptCovered.toList.sortBy(_._1.id)
      ast <- scripts
      code = handleInvalid(ast.toString(grammar = Some(grammar)).trim)
    } yield code).toSet.toVector.sortBy(_.length)
    pool

  /** for syntactic production */
  def apply(name: String, args: List[Boolean]): Syntactic =
    val (ast, _) = cache(getSyn(name, args))
    ast.asInstanceOf[Syntactic]

  /** for lexical production */
  def apply(name: String): Lexical = Lexical(name, reservedLexicals(name))

  /** reserved lexicals */
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

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // cache for shortest AST for each grammar node
  private lazy val cache: Map[Node, (Ast, String)] =
    fixpoint(Map(), topological, auxNode)

  private lazy val scriptCovered = getCoveredFrom(getSyn("Script", Nil))

  // lexicographical ordering for code length and code string
  private given Ordering[(Ast, String)] = Ordering.by {
    case (_, code) => (code.length, code)
  }

  private def auxNode(
    map: Map[Node, (Ast, String)],
    node: Node,
  ): (Ast, String) = node match
    case synNode: SynNode => auxSyn(map, synNode)
    case lexNode: LexNode => auxLex(map, lexNode)
    case rhsNode: RhsNode => auxRhs(map, rhsNode)

  private def auxSyn(
    map: Map[Node, (Ast, String)],
    synNode: SynNode,
  ): (Ast, String) =
    val SynNode(_, name, args) = synNode
    val pairs = for {
      rhsNode <- synEdges.getOrElse(synNode, Set())
      pair @ (_, code) <- map.get(rhsNode)
    } yield pair
    pairs.min

  private def auxLex(
    map: Map[Node, (Ast, String)],
    lexNode: LexNode,
  ): (Ast, String) =
    val LexNode(_, name) = lexNode
    val code = reservedLexicals(name)
    (Lexical(name, code), code)

  private def auxRhs(
    map: Map[Node, (Ast, String)],
    rhsNode: RhsNode,
  ): (Ast, String) =
    val RhsNode(_, name, args, rhsIdx) = rhsNode
    val children = for {
      symbol <- rhsNode.rhs.symbols
      child = symbol.match
        case (nt: Nonterminal) =>
          Some(auxSymbol(map, rhsNode.prod, nt, rhsNode.argMap)._2)
        case ButNot(base, _) =>
          Some(auxSymbol(map, rhsNode.prod, base, rhsNode.argMap)._2)
        case ButOnlyIf(base, _, _) =>
          Some(auxSymbol(map, rhsNode.prod, base, rhsNode.argMap)._2)
        case _ => None
    } yield child
    val ast = Syntactic(name, args, rhsIdx, children.toVector)
    val code = ast.toString(grammar = Some(grammar))
    (ast, code)

  private def auxSymbol(
    map: Map[Node, (Ast, String)],
    prod: Production,
    nt: Nonterminal,
    argMap: Map[String, Boolean],
  ): (ProdNode, Ast) =
    val Nonterminal(name, args) = nt
    val prodNode = getProd(nt, argMap)
    (
      prodNode,
      prodNode match
        case lexNode: LexNode => Lexical(name, reservedLexicals(name))
        case synNode: SynNode =>
          val (ast, _) = map(synNode)
          ast,
    )

  private def getCoveredFrom(node: SynNode): Map[RhsNode, Vector[Ast]] = {
    val worklist: Worklist[(SynNode, Int, Syntactic => Syntactic)] =
      QueueWorklist(List((node, 0, (x: Syntactic) => x)))
    var map: Map[RhsNode, Vector[Ast]] = Map()
    var minSizes: Map[SynNode, Int] = Map()

    def aux(
      rhsNode: RhsNode,
      rhs: Rhs,
      argMap: Map[String, Boolean],
      astF: Syntactic => Syntactic,
    ): Unit = {
      val RhsNode(_, name, args, rhsIdx) = rhsNode
      var scripts = Vector[Ast]()
      val nts = rhs.symbols.flatMap(_.getNt).toVector
      val opts = rhs.symbols.zipWithIndex.collect { case (Optional(_), i) => i }
      // added scripts for current RHS node
      val (prodNodes, children) = (for {
        nt <- nts
        (prodNode, child) = auxSymbol(cache, rhsNode.prod, nt, argMap)
      } yield (prodNode, Some(child))).unzip
      def create(children: Vector[Option[Ast]]): Syntactic =
        astF(Syntactic(name, args, rhsIdx, children))
      for {
        removed <- opts.toSet.subsets
        newChildren = for {
          (child, idx) <- children.zipWithIndex
        } yield if (removed contains idx) None else child
        ast = create(newChildren)
      } scripts :+= ast
      map += rhsNode -> scripts
      // propagate to symbols
      val minChildren = children.zipWithIndex.map {
        case (child, i) =>
          if (opts.contains(i)) None else child
      }
      val size = create(minChildren).toString(grammar = Some(grammar)).length
      for {
        case (synNode @ SynNode(_, _, _), idx) <- prodNodes.zipWithIndex
        newAstF = (ast: Ast) => create(minChildren.updated(idx, Some(ast)))
      } minSizes.get(synNode) match
        case Some(origSize) if origSize <= size =>
        case _ =>
          minSizes += synNode -> size
          worklist += ((synNode, size, newAstF))
    }

    while (
      worklist.next match
        case Some((synNode @ SynNode(_, name, args), _, astF)) =>
          for {
            rhsNode <- synEdges.getOrElse(synNode, Set())
          } aux(rhsNode, rhsNode.rhs, rhsNode.argMap, astF)
          true
        case None => false
    ) {}

    map
  }

  // handle invalid code
  private def handleInvalid(code: String): String =
    if (code.startsWith("{") && code.endsWith("} ;")) "var x = " + code
    else code
}
