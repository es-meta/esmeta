package esmeta.cfg.util

import esmeta.util.BaseUtils.*
import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, *}
import scala.collection.mutable.{ListBuffer, Map => MMap}

/** CFG builder */
class Builder {

  /** get CFG */
  lazy val cfg: CFG = CFG(main, funcs)

  /** translate IR function to cfg function */
  def translate(irFunc: IRFunc): Func = {
    val IRFunc(head, body) = irFunc
    // entry node
    var entry: Option[Node] = None

    // previous edges
    var prev: List[(Node, Boolean)] = Nil

    // connect previous edges
    def connect(to: Node): Unit = {
      if (prev.isEmpty) entry = Some(to)
      prev foreach {
        case (block: Block, _)       => block.next = Some(to)
        case (call: Call, _)         => call.next = Some(to)
        case (branch: Branch, true)  => branch.thenNode = Some(to)
        case (branch: Branch, false) => branch.elseNode = Some(to)
      }
    }

    // aux
    def aux(inst: Inst): Unit = inst match {
      case normal: NormalInst =>
        val block = prev match
          case List((b: Block, _)) => b
          case _                   => val b = Block(nextNId); connect(b); b
        block.insts += normal
        prev = List((block, true))
      case ISeq(insts) => for { i <- insts } aux(i)
      case IIf(cond, thenInst, elseInst) =>
        val branch = Branch(nextNId, Branch.Kind.If, cond)
        connect(branch)
        val thenPrev = { prev = List((branch, true)); aux(thenInst); prev }
        val elsePrev = { prev = List((branch, false)); aux(elseInst); prev }
        prev = thenPrev ++ elsePrev
      case ILoop(kind, cond, body) =>
        val branch = Branch(nextNId, Branch.Kind.Loop(kind), cond)
        connect(branch)
        prev = List((branch, true)); aux(body); connect(branch)
        prev = List((branch, false))
      case ICall(lhs, fexpr, args) =>
        val call = Call(nextNId, lhs, fexpr, args)
        connect(call)
        prev = List((call, true))
    }
    aux(body)

    val func = Func(nextFId, head, entry)
    funcs += func
    func
  }

  /** function builder */
  case class FuncBuilder(head: IRFunc.Head) {
    // contexts
    private var contexts: List[ListBuffer[Inst]] = List()
    def newContext: Unit = contexts = ListBuffer() :: contexts
    def popContext: Inst = contexts match
      case current :: rest => contexts = rest; ISeq(current.toList)
      case _               => ??? // error
    def addInst(insts: Inst*): Unit = contexts match
      case current :: rest => current ++= insts
      case _               => ??? // error

    // temporal identifier id counter
    private def nextTId: Int = { val tid = tidCount; tidCount += 1; tid }
    private var tidCount: Int = 0

    /** get next temporal identifier */
    def newTId: Temp = Temp(nextTId)
    def newTIdWithExpr: (Temp, Expr) = { val x = newTId; (x, ERef(x)) }

    /** get next allocation site */
    def newSite: Int = nextSite

    /** get cfg function */
    def getFunc(body: Inst): Func =
      val func = translate(IRFunc(head, body))
      funcs += func
      func
  }

  // ---------------------------------------------------------------------------
  // Private Helpers
  // ---------------------------------------------------------------------------
  // get the main function
  private def main: Func = funcs.filter(_.head.main) match {
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
