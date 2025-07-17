package esmeta.cfg

import esmeta.cfg.util.{DotPrinter => CFGDotPrinter, *}
import esmeta.ir.{Param, Type, Name, Func => IRFunc, FuncKind, EYet}
import esmeta.spec.Head
import esmeta.ty.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import esmeta.util.{Appender, UId}
import scala.collection.mutable.{Map => MMap}

/** CFG functions */
case class Func(
  id: Int,
  irFunc: IRFunc,
  entry: Node,
) extends CFGElem
  with UId { func =>
  import FuncKind.*

  /** parameters */
  lazy val params: List[Param] = irFunc.params

  /** arity */
  lazy val arity: (Int, Int) = irFunc.arity

  /** kind */
  lazy val kind: FuncKind = irFunc.kind

  /** all types */
  lazy val tys: List[Type] = retTy :: params.map(_.ty)

  /** parameter types */
  lazy val paramTys: List[Type] = params.map(_.ty)

  /** check whether parameter types are defined */
  lazy val isParamTysDefined: Boolean = paramTys.forall(_.isDefined)

  /** check whether parameter types are defined */
  lazy val isParamTysPrecise: Boolean = paramTys.forall(!_.isImprec)

  /** not yet supported instructions */
  lazy val yets: List[EYet] = irFunc.yets

  /** not yet supported instructions (ignore in assertion instructions) */
  lazy val usefulYets: List[EYet] = irFunc.usefulYets

  /** check completeness */
  lazy val weakComplete: Boolean = usefulYets.isEmpty

  /** check completeness */
  lazy val complete: Boolean = irFunc.complete

  /** return types */
  lazy val retTy: Type = irFunc.retTy

  /** nodes */
  lazy val nodes: Set[Node] = entry.reachable

  /** a mapping from nid to nodes */
  lazy val nodeMap: Map[Int, Node] =
    (for (node <- nodes) yield node.id -> node).toMap

  /** a mapping from nodes to successors */
  lazy val succs: Map[Node, Set[Node]] =
    (for { node <- nodes } yield node -> node.succs).toMap

  /** a mapping from nodes to predecessors */
  lazy val preds: Map[Node, Set[Node]] = {
    val preds = MMap[Node, Set[Node]]()
    for {
      node <- nodes
      succ <- succs(node)
    } preds(succ) = preds.getOrElse(succ, Set()) + node
    preds.toMap.withDefaultValue(Set())
  }

  /** check whether it is an exit */
  def isExit(node: Node): Boolean = succs(node).isEmpty

  /** all exits */
  lazy val exits: Set[Node] = nodes.filter(isExit)

  /** algorithm heads */
  lazy val head: Option[Head] = irFunc.head

  /** algorithm head string */
  def headString: String = irFunc.headString

  /** check whether it is builtin */
  lazy val isBuiltin: Boolean = irFunc.kind == Builtin

  /** check whether it is SDO */
  lazy val isSDO: Boolean = irFunc.kind == SynDirOp

  /** get SDO information */
  lazy val sdoInfo: Option[SdoInfo] = name match
    case Func.sdoPattern(base, i, j, method) =>
      Some(SdoInfo.Base(this, base, i.toInt, j.toInt, method))
    case Func.defaultSdoPattern(method) =>
      Some(SdoInfo.Default(this, method))
    case _ => None

  /** check whether it is a closure */
  lazy val isClo: Boolean = irFunc.kind == Clo

  /** check whether it is a continuation */
  lazy val isCont: Boolean = irFunc.kind == Cont

  /** check whether it is method operation */
  lazy val isMethod: Boolean =
    irFunc.kind == ConcMeth || irFunc.kind == InternalMeth

  /** check whether it is an auxiliary function */
  lazy val isAux: Boolean = irFunc.kind == Aux

  private val baseNamePattern = """([^:]*)(:clo.*|:cont.*)""".r
  def baseName: String = name match
    case baseNamePattern(base, _) => base
    case _                        => name

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
    } raise(s"""[DOT] [$name]: exception occurred while converting to pdf:
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

  private lazy val defaultSdoPattern = """<DEFAULT>\.(\w+)""".r
  private lazy val sdoPattern = """(\w+)\[(\d+),(\d+)\]\.(\w+)""".r
}
