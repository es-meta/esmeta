package esmeta.compile

import esmeta.spec.{Param => SParam, *}
import esmeta.cfg.*

/** Compiler from metalangauge to CFG */
class Compiler(val spec: Spec) {
  // main function id
  private var main: Int = -1
  // functions
  private var revFuncs: List[Func] = Nil

  /** get the compiled CFG */
  def cfg: CFG = ??? // CFG(main, revFuncs.reverse)

  /** get the most recently compiled function */
  def recentFunc: Func = revFuncs.head

  /** add an algorithm to a CFG as a function */
  def add(algo: Algorithm): Func = {
    val fid = nextFId
    val main = ???
    val head = algo.head
    val kind = getKind(head)
    val name = getName(head)
    val params = getParams(head)
    // TODO get CFG nodes
    val entry: Option[Node] = ???
    val func = Func(fid, main, kind, name, params, entry)
    revFuncs ::= func
    func
  }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // function id counter
  private var fidCount: Int = 0
  private def nextFId: Int = { val fid = fidCount; fidCount += 1; fid }

  // node id counter
  private var nidCount: Int = 0
  private def nextNId: Int = { val nid = nidCount; nidCount += 1; nid }

  // get function kind
  private def getKind(head: Head): Func.Kind = {
    import Func.Kind.*
    head match {
      case head: AbstractOperationHead       => AbsOp
      case head: NumericMethodHead           => NumMeth
      case head: SyntaxDirectedOperationHead => SynDirOp
      case head: ConcreteMethodHead          => ConcMeth
      case head: InternalMethodHead          => InternalMeth
      case head: BuiltinHead                 => Builtin
    }
  }

  // get function name
  private def getName(head: Head): String = {
    import Func.Kind.*
    head match {
      case head: AbstractOperationHead =>
        head.name
      case head: NumericMethodHead =>
        s"${head.ty}::${head.name}"
      case head: SyntaxDirectedOperationHead =>
        val Target = SyntaxDirectedOperationHead.Target
        val pre = head.target.fold("<DEFAULT>") {
          case Target(lhsName, idx, subIdx, _) => s"$lhsName[$idx,$subIdx]"
        }
        s"$pre.${head.methodName}"
      case head: ConcreteMethodHead =>
        s"${head.receiverParam.ty}.${head.methodName}"
      case head: InternalMethodHead =>
        s"${head.receiverParam.ty}.${head.methodName}"
      case head: BuiltinHead =>
        s"${head.ref}"
    }
  }

  // get function parameters
  private def getParams(head: Head): List[Param] = {
    import Func.Kind.*
    import Param.Kind.*
    head match {
      case head: AbstractOperationHead =>
        head.params.map(compileParam)
      case head: NumericMethodHead =>
        head.params.map(compileParam)
      case head: SyntaxDirectedOperationHead => ???
      case head: ConcreteMethodHead          => ???
      case head: InternalMethodHead          => ???
      case head: BuiltinHead                 => ???
    }
  }

  // compile an algorithm parameter to a function parameter
  private def compileParam(param: SParam): Param = ???
}
object Compiler {

  /** compile a specification to a CFG */
  def apply(spec: Spec): CFG = {
    val compiler = new Compiler(spec)
    for (algo <- spec.algorithms) compiler.add(algo)
    compiler.cfg
  }

  /** compile an algorithm to a function */
  def apply(spec: Spec, algo: Algorithm): Func = {
    val compiler = new Compiler(spec)
    compiler.add(algo)
    compiler.recentFunc
  }
}
