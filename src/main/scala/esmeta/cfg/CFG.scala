package esmeta.cfg

import esmeta.*
import esmeta.cfg.util.*
import esmeta.ir.Program
import esmeta.spec.{Spec, TypeModel, Grammar}
import esmeta.util.BaseUtils.*
import scala.collection.mutable.ListBuffer

/** control-flow graphs (CFGs) */
case class CFG(
  funcs: List[Func] = Nil,
) extends CFGElem {

  /** backward edge to a program */
  var program: ir.Program = ir.Program()

  /** the main function */
  lazy val main: Func = getUnique(funcs, _.ir.main, "main function")

  /** JavaScript parser */
  lazy val jsParser: js.util.Parser = program.jsParser

  /** mapping from fid to functions */
  lazy val funcMap: Map[Int, Func] =
    (for (func <- funcs) yield func.id -> func).toMap

  /** mapping from function names to functions */
  lazy val fnameMap: Map[String, Func] =
    (for (func <- funcs) yield func.ir.name -> func).toMap

  /** mapping from nid to nodes */
  lazy val nodeMap: Map[Int, Node] = (for {
    func <- funcs
    node <- func.nodes
  } yield node.id -> node).toMap

  /** mapping from nodes to functions */
  lazy val funcOf: Map[Node, Func] = (for {
    func <- funcs
    node <- func.nodes
  } yield node -> func).toMap

  /** get a type model */
  def typeModel: TypeModel = spec.typeModel

  /** get the corresponding specification */
  def spec: Spec = program.spec

  /** get the corresponding grammar */
  def grammar: Grammar = spec.grammar

  /** get syntax-directed operation(SDO) */
  // TODO refactor
  def getSDO = cached[(js.Ast, String), Option[(js.Ast, Func)]] {
    case (ast, operation) =>
      ast.chains.foldLeft[Option[(js.Ast, Func)]](None) {
        case (None, ast0) =>
          val subIdx = grammar.getSubIdx(ast0)
          val fname = s"${ast0.name}[${ast0.idx},${subIdx}].$operation"
          fnameMap.get(fname).map((ast0, _))
        case (res: Some[_], _) => res
      }
  }
}
