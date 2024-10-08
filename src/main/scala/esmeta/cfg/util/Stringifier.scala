package esmeta.cfg.util

import esmeta.LINE_SEP
import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.util.UId.given
import scala.collection.mutable.ListBuffer

/** stringifier for CFG */
class Stringifier(detail: Boolean, location: Boolean) {
  // stringifier for IR
  val irStringifier = IRElem.getStringifier((detail, location))
  import irStringifier.{given, *}

  // elements
  given elemRule: Rule[CFGElem] = (app, elem) =>
    elem match {
      case elem: CFG        => cfgRule(app, elem)
      case elem: Func       => funcRule(app, elem)
      case elem: Node       => nodeRule(app, elem)
      case elem: BranchKind => branchKindRule(app, elem)
    }

  // control-flow graphs (CFGs)
  given cfgRule: Rule[CFG] = (app, cfg) =>
    val CFG(funcs) = cfg
    given Rule[Iterable[Func]] = iterableRule(sep = LINE_SEP)
    app >> cfg.funcs.sorted

  // functions
  given funcRule: Rule[Func] = (app, func) =>
    val IRFunc(main, kind, name, params, retTy, _, _, _) = func.irFunc
    given Rule[Iterable[Param]] = iterableRule("(", ", ", ")")
    app >> func.id >> ": "
    app >> (if (main) "@main " else "") >> "def " >> kind
    app >> name >> params >> ": " >> retTy >> " "
    app.wrap {
      for (node <- func.nodes.toList.sorted) app :> node
    }

  // nodes
  given nodeRule: Rule[Node] = (app, node) =>
    app >> node.id >> ": "
    node match
      case Block(_, insts, next) =>
        insts match
          case ListBuffer(inst) => app >> inst
          case _                => app.wrap(for (inst <- insts) app :> inst)
        next.map(x => app >> " -> " >> x.id)
      case other: NodeWithInst => app >> other
    app

  // nodes with instruction backward edge
  // TODO handle location option
  given nodeWithInstRule: Rule[NodeWithInst] = (app, node) =>
    node match
      case Call(_, callInst, next) =>
        app >> callInst
        next.map(x => app >> " -> " >> x.id)
      case Branch(_, kind, cond, thenNode, elseNode) =>
        app >> kind >> " " >> cond
        thenNode.map(x => app >> " then " >> x.id)
        elseNode.map(x => app >> " else " >> x.id)
    app

  // branch kinds
  given branchKindRule: Rule[BranchKind] = (app, kind) =>
    import BranchKind.*
    app >> (kind match {
      case If    => "if"
      case While => "while"
    })
}
