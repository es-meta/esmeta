package esmeta.spec.util

import esmeta.spec.*
import esmeta.util.*
import scala.collection.mutable.{Map => MMap, Queue}
import math.Ordering.Implicits.infixOrderingOps

/** graph representation for syntactic grammar */
case class GrammarGraph(grammar: Grammar) {
  import GrammarGraph.*

  /** get a production node */
  def getProd(nt: Nonterminal, argMap: Map[String, Boolean]): ProdNode =
    import ProductionKind.*, NonterminalArgumentKind.*
    val Nonterminal(name, args) = nt
    val prod = nameMap(name)
    prod.kind match
      case Syntactic =>
        val newArgs = for (arg <- args) yield arg.kind match
          case True  => true
          case False => false
          case Pass  => argMap(arg.name)
        getSyn(name, newArgs)
      case _ => getLex(name)

  /** get a syntactic production node */
  def getSyn(name: String, args: List[Boolean]): SynNode = synMap(name, args)

  /** get a lexical production node */
  def getLex(name: String): LexNode = lexMap(name)

  /** get a RHS node */
  def getRhs(name: String, args: List[Boolean], idx: Int): RhsNode =
    rhsMap(name, args, idx)
  def getRhs(synNode: SynNode, idx: Int): RhsNode =
    getRhs(synNode.name, synNode.args, idx)

  lazy val (
    /** getter for syntactic production nodes */
    synMap: MMap[(String, List[Boolean]), SynNode],
    /** getter for lexical production nodes */
    lexMap: MMap[String, LexNode],
    /** getter for RHS nodes */
    rhsMap: MMap[(String, List[Boolean], Int), RhsNode],
    /** all nodes */
    nodes: Set[Node],
    /** edges from syntactic productions to RHSs */
    synEdges: Map[SynNode, Set[RhsNode]],
    /** must edges from RHSs to productions */
    rhsMustEdges: Map[RhsNode, Set[ProdNode]],
    /** may edges from RHSs to productions */
    rhsMayEdges: Map[RhsNode, Set[ProdNode]],
  ) = {
    import ProductionKind.*, NonterminalArgumentKind.*
    val synMap: MMap[(String, List[Boolean]), SynNode] = MMap()
    val lexMap: MMap[String, LexNode] = MMap()
    val rhsMap: MMap[(String, List[Boolean], Int), RhsNode] = MMap()
    var nodes: Set[Node] = Set()
    val synEdges: MMap[SynNode, Set[RhsNode]] = MMap()
    val rhsMustEdges: MMap[RhsNode, Set[ProdNode]] = MMap()
    val rhsMayEdges: MMap[RhsNode, Set[ProdNode]] = MMap()

    // get the corresponding node or create a new one otherwise
    def add[T <: Node](node: T): T = { nodes += node; node }
    def get[K, V <: Node](map: MMap[K, V], key: K, builder: Int => V): V =
      map.getOrElse(
        key, {
          val node = builder(nodes.size)
          map += key -> node
          nodes += node
          node
        },
      )
    def getSyn(name: String, args: List[Boolean]): SynNode =
      get(synMap, (name, args), SynNode(_, name, args))
    def getLex(name: String): LexNode =
      get(lexMap, name, LexNode(_, name))
    def getRhs(name: String, args: List[Boolean], idx: Int): RhsNode =
      get(rhsMap, (name, args, idx), RhsNode(_, name, args, idx))

    // aux for syntactic productions
    def auxSyn(
      synNode: SynNode,
      prod: Production,
      argMap: Map[String, Boolean],
    ): Unit = for {
      (rhs, idx) <- prod.rhsVec.zipWithIndex
      if (rhs.conditions.forall(condition =>
        argMap.get(condition.name).fold(false)(condition.pass == _),
      ))
      rhsNode = getRhs(synNode.name, synNode.args, idx)
      _ = update(synEdges, synNode, rhsNode)
    } auxRhs(rhsNode, rhs, argMap)

    // aux for RHs
    def auxRhs(
      rhsNode: RhsNode,
      rhs: Rhs,
      argMap: Map[String, Boolean],
    ): Unit = for {
      (nt, isOptional) <- rhs.ntsWithOptional
      must = !isOptional
      name = nt.name
      prod <- grammar.nameMap.get(name)
      prodNode <- prod.kind match
        case Syntactic =>
          val args = for (arg <- nt.args) yield arg.kind match
            case True  => true
            case False => false
            case Pass  => argMap(arg.name)
          Some(synMap.getOrElse((name, args), getSyn(name, args)))
        case Lexical       => Some(getLex(name))
        case NumericString => None
      _ = update(rhsMayEdges, rhsNode, prodNode)
      _ = if (must) update(rhsMustEdges, rhsNode, prodNode)
    } ()

    for {
      prod <- grammar.prods
      if prod.kind == Syntactic
      name = prod.name
      params = prod.lhs.params
      selected <- params.toSet.subsets
      args = params.map(selected contains _)
      argMap = (params zip args).toMap
      synNode = getSyn(name, args)
    } auxSyn(synNode, prod, argMap)

    (
      synMap,
      lexMap,
      rhsMap,
      nodes,
      synEdges.toMap,
      rhsMustEdges.toMap,
      rhsMayEdges.toMap,
    )
  }

  /** all production nodes */
  lazy val prodNodes: Set[ProdNode] =
    nodes.collect { case node: ProdNode => node }

  /** all syntactic production nodes */
  lazy val synNodes: Set[SynNode] =
    nodes.collect { case node: SynNode => node }

  /** all lexical production nodes */
  lazy val lexNodes: Set[LexNode] =
    nodes.collect { case node: LexNode => node }

  /** all RHS nodes */
  lazy val rhsNodes: Set[RhsNode] =
    nodes.collect { case node: RhsNode => node }

  /** RHSs that must use given productions */
  lazy val mustUsedIn: Map[ProdNode, Set[RhsNode]] = reverse(rhsMustEdges)

  /** RHSs that may use given productions */
  lazy val mayUsedIn: Map[ProdNode, Set[RhsNode]] = reverse(rhsMayEdges)

  /** entry syntactic productions */
  lazy val entries: Set[SynNode] =
    for (synNode <- synNodes if !mustUsedIn.contains(synNode)) yield synNode

  /** bottom nodes */
  lazy val bottoms: Set[Node] = lexNodes ++ (rhsNodes -- rhsMustEdges.keySet)

  /** production nodes sorted in a topological order */
  lazy val topological: Vector[Node] =
    var mustCounter: Map[RhsNode, Int] =
      for ((rhsNode, prodNodes) <- rhsMustEdges) yield rhsNode -> prodNodes.size
    var visited: Set[SynNode] = Set()
    var topological: Vector[Node] = Vector()
    val worklist: Worklist[Node] = QueueWorklist(bottoms)
    while (
      worklist.next match
        case Some(node) =>
          topological :+= node
          node match
            case prodNode: ProdNode =>
              for {
                rhsNode <- mustUsedIn.getOrElse(prodNode, Set())
                count <- mustCounter.get(rhsNode)
              }
                if (count == 1) { worklist += rhsNode; mustCounter -= rhsNode }
                else mustCounter += rhsNode -> (count - 1)
            case RhsNode(_, name, args, idx) =>
              val synNode = getSyn(name, args)
              if (!visited.contains(synNode))
                visited += synNode
                worklist += synNode
          true
        case None => false
    ) {}
    topological

  // compute fixpoint of mapping from nodes to data
  def fixpoint[T](
    init: Map[Node, T],
    touched: Iterable[Node],
    f: (Map[Node, T], Node) => T,
  )(using Ordering[T]): Map[Node, T] = {
    var map: Map[Node, T] = init
    var worklist = QueueWorklist(touched)
    while (
      worklist.next match
        case Some(node) =>
          val newT = f(map, node)
          map.get(node) match
            case Some(origT) if origT <= newT =>
            case _                            =>
              // update the current node with new value
              map += node -> newT
              // propagate to the current node's parent nodes
              node match
                case prodNode: ProdNode =>
                  worklist ++= mayUsedIn.getOrElse(prodNode, Set())
                case RhsNode(_, name, args, _) =>
                  worklist += getSyn(name, args)
          true
        case None => false
    ) {}
    map
  }

  /** grammar nodes */
  sealed trait Node extends UId {
    def name: String
    lazy val prod: Production = nameMap(name)
    override def toString: String =
      def str(args: List[Boolean]): String =
        args.map(if (_) "T" else "F").mkString
      this match
        case SynNode(id, name, args)      => s"[$id] $name[${str(args)}]"
        case LexNode(id, name)            => s"[$id] $name"
        case RhsNode(id, name, args, idx) => s"[$id] $name[${str(args)}]:$idx"
  }

  /** production nodes */
  sealed trait ProdNode extends Node

  /** syntactic production nodes */
  case class SynNode private[GrammarGraph] (
    id: Int,
    name: String,
    args: List[Boolean],
  ) extends ProdNode

  /** lexical production nodes */
  case class LexNode private[GrammarGraph] (
    id: Int,
    name: String,
  ) extends ProdNode

  /** RHS nodes */
  case class RhsNode private[GrammarGraph] (
    id: Int,
    name: String,
    args: List[Boolean],
    idx: Int,
  ) extends Node {
    lazy val rhs: Rhs = prod.rhsVec(idx)
    lazy val argMap: Map[String, Boolean] = (prod.lhs.params zip args).toMap
  }

  /** ordering of grammar nodes */
  given Ordering[Node] = Ordering.by(_.id)

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // update a mapping form keys to set of values
  private def update[K, V](map: MMap[K, Set[V]], key: K, value: V): Unit =
    map += key -> (map.getOrElse(key, Set()) + value)

  // reverse a mapping from keys to set of values
  private def reverse[K, V](map: Map[K, Set[V]]): Map[V, Set[K]] =
    val revMap: MMap[V, Set[K]] = MMap()
    for {
      (key, values) <- map
      value <- values
    } update(revMap, value, key)
    revMap.toMap

  // name map for productions
  private lazy val nameMap = grammar.nameMap
}
