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

import esmeta.ir.Name
import esmeta.ty.CompT
import esmeta.state.*

class TracedInterpreter(
  st: State,
  analysis: JalangiAnalysis,
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

  val JALANGI_READ_EXCLUDE_VARIABLES = Set(
    "undefined",
    "Infinity",
    "NaN",
    "null",
    "eval", // eval()
  )

  override def eval(node: Node): Unit = {
    node.id match
      case 671 =>
        /*
          @jalangi-hook: read
          89: def GetValue(V: ESValue | Record[ReferenceRecord]): Normal[ESValue] | Abrupt {
          ...
          671: {
          let base = V.Base
          assert (? base: Record[EnvironmentRecord])
          } -> 672
         */
        {
          st.context.locals.get(Name("V")) match {
            case None =>
            case Some(v) =>
              if (v.isAddr) {
                st(v.asAddr).get(Str("ReferencedName")) match
                  case Some(value)
                      if !JALANGI_READ_EXCLUDE_VARIABLES.contains(
                        value.asStr,
                      ) =>
                    analysis.read(value.asStr)
                  case _ =>
              }
          }
        }
      case 5699 =>
        /*
        @jalangi-hook: read
        1069: def ResolveThisBinding(): Normal[ESValue] | Throw {
            5699: call %0 = clo<"GetThisEnvironment">() -> 5700 */
        {
          analysis.read("this")
        }

      case 665 =>
        /*
        @jalangi-hook: getField
        1068: def GetValue(V: ESValue | Record[ReferenceRecord]): Normal[ESValue] | Abrupt {
        ...
        658: if (! (? V.ReferencedName: Record[Symbol] | String)) then 659 else 665
        ...
        665: call %10 = clo<"GetThisValue">(V) -> 666
         */
        {

          // for {
          //   obj <- st.context.locals.get(Name("V"))
          //   value <- st.context.locals.get(Name("value"))
          // }
          //  match {
          //   case None =>
          //   case Some(v) =>
          //     if (v.isAddr) {
          //       st(v.asAddr).get(Str("ReferencedName")) match
          //         case Some(value)
          //             if !JALANGI_READ_EXCLUDE_VARIABLES.contains(
          //               value.asStr,
          //             ) =>
          //           println(s"read: ${value.asStr}")
          //         case _ =>
          //     }
          // }
          // TODO how to print them in .toString form?
          st.context.locals.get(Name("V")) match {
            case None =>
            case Some(v) =>
              if (v.isAddr) {
                st(v.asAddr).get(Str("ReferencedName")) match
                  case Some(value) =>
                    val offset = value match
                      case Str(str) => Some(str)
                      // case Number(num) => Some(num.toString)
                      // case addr: Addr => Some(st(addr).toString)
                      // case v          => Some(v.toString)
                      case _ => None
                    analysis.getField(offset)
                  case _ =>
              }
          }
        }
      case 1901 =>
      /*
      @jalangi-hook: invokeFunPre
        165: def Call(F: ESValue, V: ESValue, argumentsList?: List[ESValue]): Normal[ESValue] | Throw {
          1901: if (! (exists argumentsList)) then 1902 else 1903
       */
      // analysis.invokeFunPre(s"???Call ${st.context.locals(Name("F"))}")
      case 1913 =>
      /*
        @jalangi-hook: invokeFunPre
        166: def Construct(F: Record[Constructor], argumentsList?: List[ESValue], newTarget?: Record[Constructor]): Normal[Record[Object]] | Throw {
          1913: if (! (exists newTarget)) then 1914 else 1915
       */
      // analysis.invokeFunPre(s"???Construct ${st.context.locals(Name("F"))}")
      case _ =>
    super.eval(node)
  }

  extension (v: Value) {
    def isComp: Boolean = {
      CompT.contains(v, st)
    }
    def isAddr: Boolean = {
      v match
        case _: esmeta.state.Addr => true
        case _                    => false
    }

    def isStr: Boolean = {
      v match
        case esmeta.state.Str(_) => true
        case _                   => false
    }
  }
}
