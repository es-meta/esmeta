package esmeta.test262.jalangi

import esmeta.interpreter.Interpreter
import esmeta.state.State
import java.io.PrintWriter
import esmeta.cfg.*
import esmeta.state.*

import esmeta.ir.*
import esmeta.ty.*
import esmeta.es.builtin.INTRINSICS
import esmeta.error.NotSupported
import esmeta.util.BaseUtils.raise

class TracingInterpreter(
  st: State,
  analysis: Analysis,
  log: Boolean = false,
  detail: Boolean = true, // turn off GC
  logPW: Option[PrintWriter] = None,
  timeLimit: Option[Int] = None,
  // TODO print detailed log when noCompare is true
  // noCompare: Boolean = false,
) extends Interpreter(
    st = st,
    tyCheck = false,
    log = log,
    detail = detail,
    logPW = logPW,
    timeLimit = timeLimit,
  ) {

  override def eval(inst: NormalInst): Unit =  inst match 
    case IPrint(expr) =>
        val v = eval(expr)
        v match 
          case Str(str) => analysis.__print(str)
          case _ => raise(s"Jalangi Tracing Interpreter: IPrint with non-string value: $v")
    case _ => super.eval(inst)

  lazy val trace = { result; TraceLog(analysis.ts.toVector) }

  lazy val readmode =
    val jalangiExclue = Set(
                "undefined",
                "Infinity",
                "NaN",
                "null",
                "eval", // eval()
              )
    val nodeProfExclude =
              Set(
                "undefined",
                "null",
                "eval", // eval()
              )
    (str: String) => {
      lazy val isJalangiRead =
        !jalangiExclue.contains(str)
      lazy val isNodeProfRead =
        !nodeProfExclude.contains(str)
      (isJalangiRead, isNodeProfRead) match
        case (true, false)  => TraceMode.Jalangi.singleton
        case (false, true)  => TraceMode.NodeProf.singleton
        case (true, true)   => TraceMode.All
        case (false, false) => TraceMode.Empty
    }

  

  override def eval(node: Node): Unit = {
    node.id match
      case 676 =>
        /*
          @jalangi-hook: read
          89: def GetValue(V: ESValue | Record[ReferenceRecord]): Normal[ESValue] | Abrupt {
          ...
          676: {
            let base = V.Base
            assert (? base: Record[EnvironmentRecord])
          } -> 677
         */
        {
          st.context.locals.get(Name("V")) match {
            case None =>
            case Some(v) =>
              if (v.isAddr) {
                st(v.asAddr).get(Str("ReferencedName")) match
                  case Some(value) =>
                    given Set[TraceMode] = readmode(value.asStr) 
                    println(s"read: ${value.asStr}")
                    analysis.read(value.asStr)
                  case _ =>
              }
          }
        }
      case 5704 =>
        /*
        @jalangi-hook: read
        1069: def ResolveThisBinding(): Normal[ESValue] | Throw {
        5704: call %0 = clo<"GetThisEnvironment">() -> 5705 */
        {
          println(s"read: this")
          analysis.read("this")(using TraceMode.All)
        }

      case 670 =>
        /*
        @jalangi-hook: getField
        1068: def GetValue(V: ESValue | Record[ReferenceRecord]): Normal[ESValue] | Abrupt {
        ...
        663: if (! (? V.ReferencedName: Record[Symbol] | String)) then 664 else 670
        ...
        670: call %10 = clo<"GetThisValue">(V) -> 671
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
                    given Set[TraceMode] = readmode(offset.getOrElse(""))
                    analysis.getField(offset)
                  case _ =>
              }
          }
        }
      case 1906 =>
      /*
      @jalangi-hook: invokeFunPre
        165: def Call(F: ESValue, V: ESValue, argumentsList?: List[ESValue]): Normal[ESValue] | Throw {
          1906: if (! (exists argumentsList)) then 1907 else 1908
       */
      // analysis.invokeFunPre(s"???Call ${st.context.locals(Name("F"))}")
      case 1918 =>
      /*
        @jalangi-hook: invokeFunPre
        166: def Construct(F: Record[Constructor], argumentsList?: List[ESValue], newTarget?: Record[Constructor]): Normal[Record[Object]] | Throw {
        1918: if (! (exists newTarget)) then 1919 else 1920
       */
      // analysis.invokeFunPre(s"???Construct ${st.context.locals(Name("F"))}")
      case 18408 | 18345 =>
        /*
        2077: def <BUILTIN>:INTRINSICS.Object.defineProperty(this: ESValue, ArgumentsList: List[ESValue], NewTarget: Record[Constructor] | Undefined): Unknown {
          18404: let __args__ = (record)[#823] -> 18405
          18405: if (< 0 (sizeof ArgumentsList)) then 18406 else 18407
          18406: {
            pop O < ArgumentsList
            expand __args__.O
          } -> 18408
          18407: let O = undefined -> 18408
          18408: if (< 0 (sizeof ArgumentsList)) then 18409 else 18410
         */
        /*
          2075: def <BUILTIN>:INTRINSICS.Object.defineProperties(this: ESValue, ArgumentsList: List[ESValue], NewTarget: Record[Constructor] | Undefined): Unknown {
            18341: let __args__ = (record)[#820] -> 18342
            18342: if (< 0 (sizeof ArgumentsList)) then 18343 else 18344
            18343: {
              pop O < ArgumentsList
              expand __args__.O
            } -> 18345
            18344: let O = undefined -> 18345
            18345: if (< 0 (sizeof ArgumentsList)) then 18346 else 18347
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
      case 17762 =>
        /*
        2059: def PerformEval(x: ESValue, strictCaller: Boolean, direct: Boolean): Normal[ESValue] | Throw {
          17762: assert (yet "If _direct_ is *false*, then _strictCaller_ is also *false*") -> 17763
         */
        throw NotSupported(
          "Turn off 'eval' for now, cause that makes too many issues.",
        )
      case 10156 =>
        /*
        @jalangi-hook: read
        1358: def <SYNTAX>:UnaryExpression[3,0].Evaluation(this: Ast[UnaryExpression[3]]): Unknown {
          10147: sdo-call %0 = this[0]->Evaluation() -> 10148
          10148: assert (? %0: Completion) -> 10149
          10149: if (? %0: Abrupt) then 10150 else 10151
          10150: return %0
          10151: %0 = %0.Value -> 10152
          10152: let val = %0 -> 10153
          10153: if (? val: Record[ReferenceRecord]) then 10154 else 10158
          10154: call %1 = clo<"IsUnresolvableReference">(val) -> 10155
          10155: if (= %1 true) then 10156 else 10158
          10156: call %2 = clo<"NormalCompletion">("undefined") -> 10157
         */
        {
          st.context.locals.get(Name("val")) match {
            case None =>
            case Some(v) =>
              if (v.isAddr) {
                st(v.asAddr).get(Str("ReferencedName")) match
                  case Some(value) =>
                    given Set[TraceMode] = readmode(value.asStr)
                    println(s"read: ${value.asStr}")
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
