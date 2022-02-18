package esmeta.cfg.util

import esmeta.util.BaseUtils.*
import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, *}
import scala.collection.mutable.{ListBuffer, Map => MMap}

/** CFG builder */
class Builder(program: Program) {

  /** get CFG */
  lazy val result: CFG = {
    for { f <- program.funcs } translate(f)
    CFG(main, funcs)
  }

  /** translate IR function to cfg function */
  private def translate(irFunc: IRFunc): Unit = {
    // body
    val body = irFunc.body

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

    val func = Func(nextFId, irFunc, entry)
    funcs += func
  }

  // ---------------------------------------------------------------------------
  // Private Helpers
  // ---------------------------------------------------------------------------
  // get the main function
  private def main: Func = funcs.filter(_.ir.main) match {
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
}
