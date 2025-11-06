package esmeta.test262.jalangi

import esmeta.interpreter.Interpreter
import esmeta.state.State
import java.io.PrintWriter
import esmeta.cfg.Node
import esmeta.cfg.Block
import esmeta.cfg.Call
import esmeta.cfg.Branch
import esmeta.state.Cursor
import esmeta.state.NodeCursor
import esmeta.state.ExitCursor
import esmeta.state.Value

import scala.collection.mutable.Set as MSet
import esmeta.ir.Name
import esmeta.ty.CompT
import esmeta.state.Str

class TracedInterpreter(
  st: State,
  log: Boolean = false,
  detail: Boolean = true, // turn off GC
  logPW: Option[PrintWriter] = None,
  timeLimit: Option[Int] = None,
) extends Interpreter(
    st = st,
    tyCheck = false,
    log = log,
    detail = detail,
    logPW = logPW,
    timeLimit = timeLimit,
  ) {

  val set = MSet[Value]()

  /** TODO implement tracing log by
    *
    * 1.tracing nodes and print when pattern matches (either or) 2. tag values
    * created by specific functions
    */
  override def eval(cursor: Cursor): Boolean = {
    {
      cursor match
        case ExitCursor(func)
            if """IdentifierReference\[0,0\]\.Evaluation""".r.matches(
              func.name,
            ) => {
          st.context.retVal
            .map(_._2)
            .foreach(referenceRecord =>
              set.add(referenceRecord)
              if (referenceRecord.isComp) {
                set.add(
                  st(referenceRecord.asAddr).get(Str("Value")).getOrElse(???),
                )
              },
            ) // v
        }
        case _ =>
    }
    super.eval(cursor)
  }

  override def eval(node: Node): Unit = {
    if (/* isEntry */ st.context.func.entry == node) {
      if (st.context.func.name == "GetValue") {
        st.context.locals.get(Name("V")) match {
          case Some(v) =>
            if (set.contains(v)) {
              println(
                st(v.asAddr).get(Str("ReferencedName")).getOrElse(???).asStr,
              )
            }
          case None =>
        }
      }
    }
    super.eval(node)
  }

  extension (v: Value) {
    def isComp: Boolean = {
      CompT.contains(v, st)
    }
  }
}
