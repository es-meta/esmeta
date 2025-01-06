//package esmeta.constructor
//
//import esmeta.cfg.*
//import esmeta.error.{ESMetaError, NoBoolean, NoCallable, NoReturnValue}
//import esmeta.interpreter.Interpreter
//import esmeta.ir
//import esmeta.ir.*
//import esmeta.state.*
//
//import scala.collection.mutable.{Map as MMap, Set as MSet}
//
///* FunctionId to Ref Sets? Update Refs Until */
//type AbruptNodeInfos = MMap[Int, AbruptInfos]
//case class AbruptInfos(refs: MSet[Ref], pathCnt: Int)
//
//class Analyzer(
//  st: State,
//  targetNodeId: Int,
//  //  targetCallPath: String,
//) {
//  private val abruptBranchNodeInfos: AbruptNodeInfos = MMap()
//
//  def printRefs(refs: MSet[Ref]): Unit = {
//    refs.foreach {
//      case Global(name) =>
//        println(s"    Global: $name")
//      case Name(name) =>
//        println(s"    Name: $name")
//      case Temp(idx) =>
//        println(s"    Temp: $idx")
//      case Field(base, expr) =>
//        println(s"    Field with base: $base, expr: $expr")
//    }
//  }
//
//  def printAbruptNodeInfo(ani: AbruptNodeInfos): Unit = {
//    ani.foreach {
//      case (key, AbruptInfos(refs, pathCnt)) =>
//        println(s"\n-------- FunId : $key")
//        println(s"  Path count: $pathCnt")
//        println(s"  Refs:")
//        printRefs(refs)
//    }
//  }
//
//  def apply(): Unit = {
//    println("-------Logging")
//
//    val prov = new Interpreter(st.copied).result
//    val pAddr = prov.context.locals.get(ir.Name("result"))
//    val provinanceAddr = pAddr match
//      case Some(value) => value
//      case None        => throw ESMetaError("TMP : #3")
//
//    new PreAnalyzer(
//      st.copied,
//      targetNodeId,
//      abruptBranchNodeInfos,
//      provinanceAddr,
//    ).result
//
//    var flag = true
//    while (flag) {
//      flag = false
//      new RecAnalyzer(
//        st.copied,
//        targetNodeId,
//        abruptBranchNodeInfos,
//        flagUpdate = _ => { flag = true },
//      ).result
//    }
//
//    printAbruptNodeInfo(abruptBranchNodeInfos)
//  }
//
//}
//
//class PreAnalyzer(
//  st: State,
//  targetNodeId: Int,
//  //  iterationCnt: Int,
//  abruptBranchNodeInfos: AbruptNodeInfos,
//  provinanceAddr: Value,
//) extends Interpreter(st) {
//  private inline def cfg = st.cfg
//  private var flag = true
//  private var completionFlag = true
//  private var callCnt = 1
//
//  override def eval(cursor: Cursor): Boolean = cursor match
//    case NodeCursor(_, node, _) => eval(node); true
//    case ExitCursor(func) =>
//      st.callStack match
//        case Nil =>
//          st.context.retVal.map((_, v) => st.globals += GLOBAL_RESULT -> v)
//          false
//        case CallContext(ctxt, retId) :: rest =>
//          val (ret, value) = st.context.retVal.getOrElse(throw NoReturnValue)
//
//          if (value == provinanceAddr && completionFlag)
//            println("###" + ctxt)
//            val ai = abruptBranchNodeInfos.getOrElseUpdate(
//              ctxt.func.id,
//              AbruptInfos(MSet(), callCnt),
//            )
//            ai.refs += retId
//            callCnt += 1
//            completionFlag = false
//
//          st.context = ctxt
//          st.callStack = rest
//          setCallResult(retId, value)
//          true
//
//  override def eval(node: Node): Unit =
//    val funcId = cfg.funcOf(node).id
//    node match {
//      case Block(_, insts, _) =>
//        for (inst <- insts) {
//          super.eval(inst)
//          st.context.moveInst
//        }
//        st.context.moveNode
//      case branch: Branch =>
//        super.eval(branch.cond) match
//          case Bool(bool) =>
//            /* Abrupt 발생 시 관여한 ref 로깅 */
//            if (flag && branch.isAbruptNode && bool) {
//              branch.cond match
//                case ETypeCheck(xExpr, _) =>
//                  xExpr match
//                    case ERef(ref) =>
//                      val ai = abruptBranchNodeInfos.getOrElseUpdate(
//                        funcId,
//                        AbruptInfos(MSet(), callCnt),
//                      )
//                      ai.refs += ref
//                      callCnt += 1
//                    case _ => throw ESMetaError("TMP : #1")
//                case _ => throw ESMetaError("TMP : #2")
//
//              if (branch.id == targetNodeId) {
//                flag = false
//              }
//            }
//
//            moveBranch(branch, bool)
//          case v => throw NoBoolean(v)
//      case call: Call => super.eval(call)
//    }
//}
//
//class RecAnalyzer(
//  st: State,
//  targetNodeId: Int,
//  abruptBranchNodeInfos: AbruptNodeInfos,
//  flagUpdate: Unit => Unit,
//) extends Interpreter(st) {
//  private inline def cfg = st.cfg
//
//  override lazy val result: State =
//    val previousState = abruptBranchNodeInfos.view.mapValues(_.refs.size).toMap
//    while (step) {}
//    val currentState = abruptBranchNodeInfos.view.mapValues(_.refs.size).toMap
//
//    if (previousState != currentState) {
//      flagUpdate(())
//    }
//    st
//
//  override def eval(node: Node): Unit =
//    val funcId = cfg.funcOf(node).id
//    node match {
//      case Block(_, insts, _) =>
//        for (inst <- insts) {
//          eval(inst, funcId)
//          st.context.moveInst
//        }
//        st.context.moveNode
//      case branch: Branch =>
//        super.eval(branch.cond) match
//          case Bool(bool) => moveBranch(branch, bool)
//          case v          => throw NoBoolean(v)
//      case call: Call => eval(call, funcId)
//    }
//
//  def eval(inst: NormalInst, funcId: Int): Unit = {
//    inst match {
//      case IAssign(ref, expr) =>
//        abruptBranchNodeInfos.get(funcId) match {
//          case Some(AbruptInfos(refs, pathCnt)) =>
//            refs += ref
//          case None =>
//        }
//      case _ =>
//    }
//
//    super.eval(inst)
//  }
//
//  def eval(call: Call, funcId: Int): Unit =
//    call.callInst match
//      case ICall(lhs, fexpr, args) =>
//        super.eval(fexpr) match
//          case clo @ Clo(func, captured) =>
//            val vs = args.map(eval)
//
//            val newLocals = // 여기에 변수 호칭 매핑되어있음
//              getLocals(func.irFunc.params, vs, call, clo) ++ captured
//
//            abruptBranchNodeInfos.get(funcId) match {
//              case Some(AbruptInfos(refs, pathCnt)) if refs.contains(lhs) =>
//                args.foreach {
//                  case ERef(ref) => refs += ref
//                  case _         =>
//                }
//              case _ =>
//            }
//
//            st.callStack ::= CallContext(st.context, lhs)
//            st.context = createContext(call, func, newLocals, st.context)
//          case cont @ Cont(func, captured, callStack) =>
//            val vs = args.map(eval)
//            val newLocals =
//              getLocals(func.irFunc.params, vs, call, cont) ++ captured
//            st.callStack = callStack.map(_.copied)
//            val prevCtxt = st.callStack.headOption.map(_.context)
//            st.context = createContext(call, func, newLocals, prevCtxt)
//          case v => throw NoCallable(v)
//      case _ => super.eval(call)
//
//}
