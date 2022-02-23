package esmeta.cfg.util

import esmeta.LINE_SEP
import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import scala.collection.mutable.ListBuffer

/** stringifier for CFG */
class Stringifier(detail: Boolean, location: Boolean) {
  // stringifier for IR
  val irStringifier = IRElem.getStringifier((detail, location))
  import irStringifier.{given, *}

  // elements
  given elemRule: Rule[CFGElem] = (app, elem) =>
    elem match {
      case elem: CFG         => cfgRule(app, elem)
      case elem: Func        => funcRule(app, elem)
      case elem: Node        => nodeRule(app, elem)
      case elem: Branch.Kind => branchKindRule(app, elem)
    }

  // control-flow graphs (CFGs)
  given cfgRule: Rule[CFG] = (app, cfg) =>
    val CFG(funcs) = cfg
    given Rule[Iterable[Func]] = iterableRule(sep = LINE_SEP)
    given Ordering[Func] = Ordering.by(_.id)
    app >> cfg.funcs.sorted

  // functions
  given funcRule: Rule[Func] = (app, func) =>
    val IRFunc(main, kind, name, params, _, _) = func.ir
    given Rule[Iterable[IRFunc.Param]] = iterableRule("(", ", ", ")")
    app >> func.id >> ": "
    app >> (if (main) "@main " else "") >> "def " >> kind
    app >> name >> params >> " "
    app.wrap {
      given Ordering[Node] = Ordering.by(_.id)
      for (node <- func.nodes.toList.sorted) app :> node
    }

  // nodes
  given nodeRule: Rule[Node] = withLoc { (app, node) =>
    app >> node.id >> ": "
    node match
      case Block(_, insts, next) =>
        insts match
          case ListBuffer(inst) => app >> inst
          case _                => app.wrap(for (inst <- insts) app :> inst)
        next.map(x => app >> " -> " >> x.id)
      case Call(_, lhs, fexpr, args, next) =>
        given Rule[Iterable[Expr]] = iterableRule[Expr]("(", ", ", ")")
        app >> "call " >> lhs >> " = " >> fexpr >> args
        next.map(x => app >> " -> " >> x.id)
      case Branch(_, kind, cond, thenNode, elseNode) =>
        app >> kind >> " " >> cond
        thenNode.map(x => app >> " then " >> x.id)
        elseNode.map(x => app >> " else " >> x.id)
    app
  }

  // branch kinds
  given branchKindRule: Rule[Branch.Kind] = (app, kind) =>
    import Branch.Kind.*
    app >> (kind match {
      case If        => "if"
      case Loop(str) => s"loop[$str]"
    })

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // append locations
  private def withLoc[T <: Locational](rule: Rule[T]): Rule[T] = (app, elem) =>
    given Rule[Option[Loc]] = (app, locOpt) =>
      locOpt.fold(app)(app >> " @ " >> _.toString)
    rule(app, elem)
    if (location) app >> elem.loc else app
}
