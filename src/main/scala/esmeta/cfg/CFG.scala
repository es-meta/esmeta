package esmeta.cfg

import esmeta.cfg.util.*
import esmeta.ir.Program
import esmeta.js.Ast
import esmeta.spec.{Spec, TypeModel, Grammar}
import esmeta.util.BaseUtils.*
import scala.collection.mutable.ListBuffer

/** control-flow graphs (CFGs) */
case class CFG(
  funcs: List[Func] = Nil,
) extends CFGElem {

  /** backward edge to a program */
  var program: Program = Program()

  /** the main function */
  lazy val main: Func = getUnique(funcs, _.ir.main, "main function")

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
  def getSDO = cached[(Ast, String), Option[Func]] {
    case (ast, operation) =>
      ast.chains.foldLeft[Option[Func]](None) {
        case (None, ast0) =>
          val subIdx = grammar.getSubIdx(ast0)
          val fname = s"${ast0.name}[${ast0.idx},${subIdx}].$operation"
          fnameMap.get(fname)
        case (res: Some[_], _) => res
      }
  }
}
