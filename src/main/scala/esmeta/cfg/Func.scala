package esmeta.cfg

import esmeta.cfg.util.{DotPrinter => CFGDotPrinter, *}
import esmeta.ir.{Param, Type, Name, Func => IRFunc, FuncKind, EYet}
import esmeta.spec.Head
import esmeta.ty.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import esmeta.util.{Appender, UId}

/** CFG functions */
case class Func(
  id: Int,
  irFunc: IRFunc,
  entry: Node,
) extends CFGElem
  with UId { func =>

  /** parameters */
  lazy val params: List[Param] = irFunc.params

  /** arity */
  lazy val arity: (Int, Int) = irFunc.arity

  /** all types */
  lazy val tys: List[Type] = retTy :: params.map(_.ty)

  /** parameter types */
  lazy val paramTys: List[Type] = params.map(_.ty)

  /** check whether parameter types are defined */
  lazy val isParamTysDefined: Boolean = paramTys.forall(_.isDefined)

  /** not yet supported instructions */
  lazy val yets: List[EYet] = irFunc.yets

  /** check completeness */
  lazy val complete: Boolean = irFunc.complete

  /** return types */
  lazy val retTy: Type = irFunc.retTy

  /** nodes */
  lazy val nodes: Set[Node] = entry.reachable

  /** a mapping from nid to nodes */
  lazy val nodeMap: Map[Int, Node] =
    (for (node <- nodes) yield node.id -> node).toMap

  /** algorithm heads */
  lazy val head: Option[Head] = irFunc.head

  /** algorithm head string */
  def headString: String = irFunc.headString

  /** check whether it is builtin */
  lazy val isBuiltin: Boolean = irFunc.kind == FuncKind.Builtin

  /** check whether it is SDO */
  lazy val isSDO: Boolean = irFunc.kind == FuncKind.SynDirOp

  /** check whether it is method operation */
  lazy val isMethod: Boolean =
    irFunc.kind == FuncKind.ConcMeth || irFunc.kind == FuncKind.InternalMeth

  /** check whether it is closure */
  lazy val isClo: Boolean =
    irFunc.kind == FuncKind.Clo || irFunc.kind == FuncKind.BuiltinClo

  lazy val isCont: Boolean =
    irFunc.kind == FuncKind.Cont

  /** check whether it needs normal completion wrapping */
  lazy val isReturnComp: Boolean = irFunc.kind match
    case FuncKind.SynDirOp if irFunc.name.endsWith(".Evaluation") => true
    case FuncKind.Builtin | FuncKind.BuiltinClo                   => true
    case _ => irFunc.retTy.isCompletion

  /** function name */
  def name: String = irFunc.name

  /** function name with id */
  def nameWithId: String = s"[$id] $name"

  /** normalized function name */
  def normalizedName: String = name.replace("/", "").replace("`", "")

  /** conversion to a DOT format */
  def dot: String = toDot()
  def toDot(targetId: Int = -1, isExit: Boolean = false): String =
    new Func.DotPrinter(this, targetId, isExit).toString

  /** dump in a DOT format */
  def dumpDot(
    dotPath: String,
    pdfPathOpt: Option[String] = None,
  ): Unit =
    // dump DOT format
    dumpFile(dot, dotPath)

    // dump PDF format
    for {
      pdfPath <- pdfPathOpt
      e <- getError(executeCmd(s"""dot -Tpdf "$dotPath" -o "$pdfPath""""))
    } error(s"""[DOT] [$name]: exception occurred while converting to pdf:
               |
               |$e""".stripMargin)

  /** dump in a DOT/PDF format */
  def dumpDot(
    dotPath: String,
    pdfPath: String,
  ): Unit = dumpDot(dotPath, Some(pdfPath))
}
object Func {
  class DotPrinter(
    func: Func,
    targetId: Int = -1,
    isExit: Boolean = false,
  ) extends CFGDotPrinter(func) {
    override lazy val exitBgColor: String = if (isExit) CURRENT else NORMAL
    override def getBgColor(node: Node): String =
      if (node.id == targetId) CURRENT else NORMAL
    override def toString: String =
      val app = new Appender
      (app >> "digraph ").wrap {
        app :> """graph [fontname = "Consolas"]"""
        app :> """node [fontname = "Consolas"]"""
        app :> """edge [fontname = "Consolas"]"""
        addTo(app)
      }.toString
  }
}
