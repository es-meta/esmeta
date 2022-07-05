package esmeta.cfg

import esmeta.cfg.util.*
import esmeta.ir.{Type, Name, Func => IRFunc}
import esmeta.spec.Head
import esmeta.util.{Appender, UId}

/** CFG functions */
case class Func(
  id: Int,
  irFunc: IRFunc,
  entry: Option[Node],
) extends CFGElem
  with UId { func =>

  /** nodes */
  lazy val nodes: Set[Node] = entry.fold(Set())(_.reachable)

  /** a mapping from nid to nodes */
  lazy val nodeMap: Map[Int, Node] =
    (for (node <- nodes) yield node.id -> node).toMap

  /** algorithm heads */
  lazy val head: Option[Head] = irFunc.head

  /** check whether it is builtin */
  lazy val isBuiltin: Boolean =
    irFunc.kind == IRFunc.Kind.Builtin || irFunc.kind == IRFunc.Kind.BuiltinClo

  /** check wheter it is SDO */
  lazy val isSDO: Boolean = irFunc.kind == IRFunc.Kind.SynDirOp

  /** check wheter it needs normal completion wrapping */
  lazy val isReturnComp: Boolean = irFunc.kind match
    case IRFunc.Kind.SynDirOp if irFunc.name.endsWith(".Evaluation") => true
    case IRFunc.Kind.Builtin | IRFunc.Kind.BuiltinClo                => true
    case _ => irFunc.retTy.isCompletion

  /** function name */
  def name: String = irFunc.name

  /** conversion to a DOT format */
  def toDot(nid: Int = -1, _isExit: Boolean = false): String = new DotPrinter {
    val isExit: Boolean = _isExit
    def getId(func: Func): String = s"cluster${func.id}"
    def getId(node: Node): String = s"node${node.id}"
    def getName(func: Func): String = func.irFunc.name
    def getColor(node: Node): String = REACH
    def getColor(from: Node, to: Node): String = REACH
    def getBgColor(node: Node): String =
      if (node.id == nid) CURRENT else NORMAL
    def apply(app: Appender): Unit = addFunc(func, app)
  }.toString
}
