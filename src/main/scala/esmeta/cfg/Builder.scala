package esmeta.cfg

import esmeta.util.BaseUtils.*
import esmeta.cfg.Utils.*
import scala.collection.mutable.ListBuffer

/** CFG builder */
class Builder {

  /** get CFG */
  lazy val cfg: CFG = CFG(main, funcs)

  /** function builder */
  case class FuncBuilder(
    main: Boolean,
    kind: Func.Kind,
    name: String,
    params: List[Param],
  ) {
    // function id
    private val fid: Int = nextFId

    // entry node
    private var entry: Option[Node] = None

    // previous edges
    private var prev: List[(Node, Boolean)] = Nil

    // temporal identifier id counter
    private def nextTId: Int = { val tid = tidCount; tidCount += 1; tid }
    private var tidCount: Int = 0

    /** get next temporal identifier */
    def newTId: Temp = Temp(nextTId)

    /** get next allocation site */
    def newSite: Int = nextSite

    /** get function */
    lazy val func =
      val func = Func(fid, main, kind, name, params, entry)
      funcs += func
      func

    /** add instructions */
    def addInst(insts: Inst*): Unit = prev match
      case List((block: Block, _)) => block.insts ++= insts
      case _ =>
        val block = Block(nextNId)
        connect(block)
        block.insts ++= insts
        prev = List((block, true))

    /** add call nodes */
    def addCall(lhs: Id, fexpr: Expr, args: List[Expr]): Unit =
      val call = Call(nextNId, lhs, fexpr, args)
      connect(call)
      prev = List((call, true))

    /** add branch nodes */
    def addBranch(
      kind: Branch.Kind,
      cond: Expr,
      thenF: => Unit,
      elseF: => Unit,
    ): Unit =
      val branch = Branch(nextNId, kind, cond)
      connect(branch)
      val thenPrev = { prev = List((branch, true)); thenF; prev }
      val elsePrev = { prev = List((branch, false)); elseF; prev }
      prev = thenPrev ++ elsePrev

    // connect previous edges to
    private def connect[T <: Node](node: T): T =
      prev foreach {
        case (block: Block, _)       => block.next = Some(node)
        case (call: Call, _)         => call.next = Some(node)
        case (branch: Branch, true)  => branch.thenNode = Some(node)
        case (branch: Branch, false) => branch.elseNode = Some(node)
      }
      prev = (node match {
        case _: Branch => Nil
        case _         => List((node, true))
      })
      node
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

  // internal lits of functions
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
