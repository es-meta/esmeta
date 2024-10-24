package esmeta.cfg

import esmeta.*
import esmeta.cfg.util.*
import esmeta.cfgBuilder.CFGBuilder
import esmeta.error.*
import esmeta.es.Initialize
import esmeta.ir.{Func as IRFunc, Program}
import esmeta.parser.{ESParser, AstFrom}
import esmeta.peval.{SpecializedFuncs}
import esmeta.spec.{Spec, Grammar}
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.ProgressBar
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Map => MMap, Set => MSet}

/** control-flow graphs (CFGs) */
case class CFG(
  funcs: List[Func] = Nil,
) extends CFGElem {

  /** backward edge to a program */
  var program: ir.Program = ir.Program()

  /** backward edge to a CFG builder, for incremental build */
  var cfgBuilder: Option[CFGBuilder] = None

  /** specialized function map */
  var sfMap: Option[SpecializedFuncs] = None

  var computeMain: (List[Func]) => Func = (fs: List[Func]) =>
    getUnique(fs, _.irFunc.main, "main function")

  var computeFuncMap: (List[Func]) => Map[Int, Func] = (fs: List[Func]) =>
    (for (func <- fs) yield func.id -> func).toMap

  var computeFnameMap: (List[Func]) => Map[String, Func] = (fs: List[Func]) =>
    (for (func <- fs) yield func.irFunc.name -> func).toMap

  var computeNodes: (List[Func]) => List[Node] = (fs: List[Func]) =>
    fs.flatMap(_.nodes)

  var computeNodesMap: (List[Func]) => Map[Int, Node] = (fs: List[Func]) =>
    (for {
      func <- fs
      node <- func.nodes
    } yield node.id -> node).toMap

  var computeFuncOf: (List[Func]) => Map[Node, Func] = (fs: List[Func]) =>
    (for {
      func <- fs
      node <- func.nodes
    } yield node -> func).toMap

  /** the main function */
  lazy val main: Func = computeMain(funcs)

  /** an ECMAScript parser */
  lazy val esParser: ESParser = program.esParser
  lazy val scriptParser: AstFrom = esParser("Script")

  /** initializer for initial states */
  lazy val init: Initialize = new Initialize(this)

  /** mapping from fid to functions */
  lazy val funcMap: Map[Int, Func] = computeFuncMap(funcs)

  /** mapping from function names to functions */
  lazy val fnameMap: Map[String, Func] = computeFnameMap(funcs)

  /** all nodes */
  lazy val nodes: List[Node] = computeNodes(funcs)

  /** mapping from nid to nodes */
  lazy val nodeMap: Map[Int, Node] = computeNodesMap(funcs)

  /** mapping from nodes to functions */
  lazy val funcOf: Map[Node, Func] = computeFuncOf(funcs)

  /** get a type model */
  lazy val tyModel: TyModel = spec.tyModel

  /** get the corresponding specification */
  lazy val spec: Spec = program.spec

  /** get the corresponding grammar */
  lazy val grammar: Grammar = spec.grammar

  /* SDO information helper */
  object sdoInfo {
    import SdoInfo.*

    // get propagation edges of SDO information
    val edges: MMap[String, MSet[(String, Int, Int)]] = MMap()
    for {
      prod <- grammar.prods
      name = prod.name if !(grammar.lexicalNames contains name)
      (rhs, idx) <- prod.rhsList.zipWithIndex
      subIdx <- (0 until rhs.countSubs)
      to = (name, idx, subIdx)
    } rhs.getNts(subIdx) match
      case List(Some(sub)) => edges.getOrElseUpdate(sub, MSet()) += to
      case _               =>

    /** SDOs for a pair of base and method name */
    val (
      defaults: Map[String, Default],
      noBase: Map[String, Set[Base]],
      simple: Map[(String, String), Set[Base]],
      indexed: Map[((String, Int), String), Set[Base]],
      subIndexed: Map[((String, Int, Int), String), Set[Base]],
    ) = {
      val defaults = MMap[String, Default]()
      val bases = MMap[((String, Int, Int), String), Base]()
      val noBase = MMap[String, MSet[Base]]()
      val simple = MMap[(String, String), MSet[Base]]()
      val indexed = MMap[((String, Int), String), MSet[Base]]()
      val subIndexed = MMap[((String, Int, Int), String), MSet[Base]]()
      def get[T, U](m: MMap[T, MSet[U]], k: T): MSet[U] =
        m.getOrElseUpdate(k, MSet())
      for {
        func <- funcs
        info <- func.sdoInfo
      } info match
        case default @ Default(_, m)       => defaults(m) = default
        case base @ Base(_, name, i, j, m) => bases(((name, i, j), m)) = base
      for {
        (_, base) <- bases
        method = base.method
        visited = MSet[String]()
        init = (base.name, base.i, base.j)
        worklist = QueueWorklist(List(init))
        // no propagation for default SDOs
        if !defaults.contains(method)
      } while (
        worklist.next.exists { (name, idx, subIdx) =>
          !visited.contains(name) && {
            visited += name
            get(noBase, method) += base
            get(simple, (name, method)) += base
            get(indexed, ((name, idx), method)) += base
            get(subIndexed, ((name, idx, subIdx), method)) += base
            for {
              to <- edges.getOrElse(name, Nil)
              if !bases.contains((to, method))
            } worklist += to
            true
          }
        }
      ) {}
      (
        defaults.toMap,
        noBase.view.mapValues(_.toSet).toMap,
        simple.view.mapValues(_.toSet).toMap,
        indexed.view.mapValues(_.toSet).toMap,
        subIndexed.view.mapValues(_.toSet).toMap,
      )
    }
  }

  /** get function by name */
  def getFunc(fname: String): Func =
    fnameMap.getOrElse(fname, throw UnknownFunc(fname))

  /** dump CFG */
  def dumpTo(baseDir: String): Unit =
    val dirname = s"$baseDir/func"
    dumpDir(
      name = "CFG functions",
      iterable = ProgressBar("Dump CFG functions", funcs, detail = false),
      dirname = dirname,
      getName = func => s"${func.normalizedName}.cfg",
    )

  /** dump in a DOT format */
  def dumpDot(
    baseDir: String,
    pdf: Boolean = true,
  ): Unit =
    mkdir(baseDir)
    val format = if (pdf) "DOT/PDF formats" else "a DOT format"
    val progress = ProgressBar(
      msg = s"Dump CFG functions in $format",
      iterable = funcs,
      getName = (x, _) => x.name,
      detail = false,
      concurrent = true,
    )
    for (func <- progress)
      val path = s"$baseDir/${func.normalizedName}"
      val dotPath = s"$path.dot"
      val pdfPath = if (pdf) Some(s"$path.pdf") else None
      func.dumpDot(dotPath, pdfPath)
}
