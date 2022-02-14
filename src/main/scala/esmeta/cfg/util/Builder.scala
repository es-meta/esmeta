package esmeta.cfg.util

import esmeta.util.BaseUtils.*
import esmeta.cfg.*
import scala.collection.mutable.{ListBuffer, Map => MMap}

/** CFG builder */
class Builder {

  /** get CFG */
  lazy val cfg: CFG = CFG(main, funcs)

  /** function builder */
  case class FuncBuilder(
    val main: Boolean,
    val kind: Func.Kind,
    val name: String,
    val params: List[Func.Param],
  ) {
    // function id
    private val fid: Int = nextFId

    // entry node
    private var entry: Option[Node] = None

    // previous edges
    private type Edge = (Node, Boolean)
    private var prev: List[Edge] = Nil
    private var labelMap = MMap[String, List[Edge]]().withDefaultValue(Nil)
    private var loops: List[(Branch, Option[String])] = List()

    // temporal identifier id counter
    private def nextTId: Int = { val tid = tidCount; tidCount += 1; tid }
    private var tidCount: Int = 0

    /** get next temporal identifier */
    def newTId: Temp = Temp(nextTId)
    def newTIdWithExpr: (Temp, Expr) = { val x = newTId; (x, ERef(x)) }

    /** get next allocation site */
    def newSite: Int = nextSite

    /** get function */
    lazy val func =
      // XXX handle next of loop end
      loops.foreach { case (branch, bid) => connect(Nil, branch, bid) }
      val func = Func(fid, main, kind, name, params, entry)
      funcs += func
      func

    def addEdge(next: Option[String], edge: Edge): Unit = next match
      case Some(next) => prev = Nil; labelMap(next) ::= edge
      case None       => prev = List(edge)

    /** add instructions */
    def addInst(insts: Inst*): Unit = addInsts(insts)
    def addInsts(
      insts: Iterable[Inst],
      next: Option[String] = None,
      label: Option[String] = None,
    ): Unit = prev match
      case List((block: Block, _)) => block.insts ++= insts
      case _ =>
        val block = Block(nextNId)
        connect(prev, block, label)
        block.insts ++= insts
        addEdge(next, (block, true))

    /** add call nodes */
    def addCall(
      lhs: Id,
      fexpr: Expr,
      args: List[Expr],
      next: Option[String] = None,
      label: Option[String] = None,
    ): Unit =
      val call = Call(nextNId, lhs, fexpr, args)
      connect(prev, call, label)
      addEdge(next, (call, true))

    /** add branch nodes */
    def addBranch(
      kind: Branch.Kind,
      cond: Expr,
      thenF: => Unit,
      elseF: => Unit = {},
      loop: Boolean = false,
      label: Option[String] = None,
    ): Unit =
      val branch = Branch(nextNId, kind, cond)
      connect(prev, branch, label)
      val thenPrev = { prev = List((branch, true)); thenF; prev }
      val elsePrev = { prev = List((branch, false)); elseF; prev }
      prev = elsePrev ++ (if (loop) { connect(thenPrev, branch); Nil }
                          else thenPrev)

    /** add branch nodes with id */
    def addBranchWithLabel(
      kind: Branch.Kind,
      cond: Expr,
      thenId: Option[String],
      elseId: Option[String],
      label: Option[String] = None,
    ): Unit =
      val branch = Branch(nextNId, kind, cond)
      connect(prev, branch, label)
      thenId.map(labelMap(_) ::= (branch, true))
      elseId.map(labelMap(_) ::= (branch, false))

      // XXX handle next of loop end
      kind match
        case Branch.Kind.Loop(_) => loops ::= (branch, label)
        case _                   => None

    // connect previous edges to
    private def connect(
      directPrev: List[Edge],
      node: Node,
      label: Option[String] = None,
    ): Unit =
      val prev = directPrev ++ label.fold(Nil)(labelMap(_))
      if (prev.isEmpty) entry = Some(node)
      prev foreach {
        case (block: Block, _)       => block.next = Some(node)
        case (call: Call, _)         => call.next = Some(node)
        case (branch: Branch, true)  => branch.thenNode = Some(node)
        case (branch: Branch, false) => branch.elseNode = Some(node)
      }
  }

  // ---------------------------------------------------------------------------
  // Private Helpers
  // ---------------------------------------------------------------------------
  // get the main function
  private def main: Func = funcs.filter(_.main) match {
    case ListBuffer()     => error("no main function")
    case ListBuffer(main) => main
    case _                => error("multiple main functions")
  }

  // internal lists of functions
  private val funcs: ListBuffer[Func] = ListBuffer()

  // function id counter
  private var fidCount: Int = 0
  private def nextFId: Int = { val fid = fidCount; fidCount += 1; fid }

  // node id counter
  private var nidCount: Int = 0
  private def nextNId: Int = { val nid = nidCount; nidCount += 1; nid }

  // allocation site counter
  private var siteCount: Int = 0
  private def nextSite: Int = { val site = siteCount; siteCount += 1; site }
}
