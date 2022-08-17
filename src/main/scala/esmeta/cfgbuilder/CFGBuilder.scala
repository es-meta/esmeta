package esmeta.cfgbuilder

import esmeta.util.BaseUtils.*
import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.ir.util.AllocSiteSetter
import scala.collection.mutable.{ListBuffer, Map => MMap}

/** CFG builder */
object CFGBuilder:
  def apply(program: Program): CFG = new CFGBuilder(program).result

/** extensible helper of CFG builder */
class CFGBuilder(program: Program) {

  /** final result */
  lazy val result: CFG =
    asiteSetter.walk(program)
    for { f <- program.funcs } translate(f)
    val cfg = CFG(funcs.toList)
    cfg.program = program
    cfg

  /** translate IR function to cfg function */
  def translate(irFunc: IRFunc): Unit = {
    // body
    val body = irFunc.body

    // entry node
    var entry: Option[Node] = None

    // previous edges
    var prev: List[(Node, Boolean)] = Nil

    // connect previous edges
    def connect(to: Node, isLoopPred: Boolean = false): Unit = {
      if (prev.isEmpty) entry = Some(to)
      for { (node, tf) <- prev } {
        node.isLoopPred = isLoopPred
        (node, tf) match
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
      case inst @ IIf(cond, thenInst, elseInst) =>
        val branch = Branch(nextNId, Branch.Kind.If, cond)
        connect(branch.setInst(inst))
        val thenPrev = { prev = List((branch, true)); aux(thenInst); prev }
        val elsePrev = { prev = List((branch, false)); aux(elseInst); prev }
        prev = thenPrev ++ elsePrev
      case inst @ ILoop(kind, cond, body) =>
        val branch = Branch(nextNId, Branch.Kind.Loop(kind), cond)
        connect(branch.setInst(inst), isLoopPred = true)
        prev = List((branch, true)); aux(body); connect(branch)
        prev = List((branch, false))
      case callInst: CallInst =>
        val call = Call(nextNId, callInst)
        connect(call)
        prev = List((call, true))
    }
    aux(body)

    val func = Func(nextFId, irFunc, entry)
    funcs += func
  }

  /** allocation site setter */
  val asiteSetter: AllocSiteSetter = new AllocSiteSetter

  // ---------------------------------------------------------------------------
  // protected helpers
  // ---------------------------------------------------------------------------
  // internal lists of functions
  protected val funcs: ListBuffer[Func] = ListBuffer()

  // function id counter
  private var fidCount: Int = 0
  protected def nextFId: Int = { val fid = fidCount; fidCount += 1; fid }

  // node id counter
  private var nidCount: Int = 0
  protected def nextNId: Int = { val nid = nidCount; nidCount += 1; nid }
}
