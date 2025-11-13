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
import esmeta.ty.*
import esmeta.state.*
import esmeta.es.builtin.INTRINSICS
import esmeta.error.NotSupported

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
      case 18403 | 18340 =>
        /*
        2077: def <BUILTIN>:INTRINSICS.Object.defineProperty(this: ESValue, ArgumentsList: List[ESValue], NewTarget: Record[Constructor] | Undefined): Unknown {
          18399: let __args__ = (record)[#823] -> 18400
          18400: if (< 0 (sizeof ArgumentsList)) then 18401 else 18402
          18401: {
            pop O < ArgumentsList
            expand __args__.O
          } -> 18403
          18402: let O = undefined -> 18403
          18403: if (< 0 (sizeof ArgumentsList)) then 18404 else 18405
         */
        /*
          2075: def <BUILTIN>:INTRINSICS.Object.defineProperties(this: ESValue, ArgumentsList: List[ESValue], NewTarget: Record[Constructor] | Undefined): Unknown {
            18336: let __args__ = (record)[#820] -> 18337
            18337: if (< 0 (sizeof ArgumentsList)) then 18338 else 18339
            18338: {
              pop O < ArgumentsList
              expand __args__.O
            } -> 18340
            18339: let O = undefined -> 18340
            18340: if (< 0 (sizeof ArgumentsList)) then 18341 else 18342
         */
        {
          st.context.locals.get(Name("O")) match {
            case Some(v) =>
              val isIntrinsics = v match
                case NamedAddr(name) => name.startsWith("INTRINSICS")
                case _               => false
              if (isIntrinsics) {
                throw NotSupported(
                  "Jalangi Traced Interpreter does not support Object.defineProperty on BuiltinFunctionObject",
                )
              }
            case _ =>
          }
        }
      case 17757 =>
        /*
        2059: def PerformEval(x: ESValue, strictCaller: Boolean, direct: Boolean): Normal[ESValue] | Throw {
          17757: assert (yet "If _direct_ is *false*, then _strictCaller_ is also *false*") -> 17758
         */
        throw NotSupported(
          "Turn off 'eval' for now, cause that makes too many issues.",
        )
      case 10151 =>
        /*
        @jalangi-hook: read
        1358: def <SYNTAX>:UnaryExpression[3,0].Evaluation(this: Ast[UnaryExpression[3]]): Unknown {
          10142: sdo-call %0 = this[0]->Evaluation() -> 10143
          10143: assert (? %0: Completion) -> 10144
          10144: if (? %0: Abrupt) then 10145 else 10146
          10145: return %0
          10146: %0 = %0.Value -> 10147
          10147: let val = %0 -> 10148
          10148: if (? val: Record[ReferenceRecord]) then 10149 else 10153
          10149: call %1 = clo<"IsUnresolvableReference">(val) -> 10150
          10150: if (= %1 true) then 10151 else 10153
          10151: call %2 = clo<"NormalCompletion">("undefined") -> 10152
         */
        {
          st.context.locals.get(Name("val")) match {
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
      case _ =>
    super.eval(node)
  }

  lazy val BuiltinFunctionObjectT =
    ValueTy(record = RecordTy("BuiltinFunctionObject"))

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
