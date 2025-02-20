package esmeta.fuzzer

import esmeta.{LINE_SEP}
import esmeta.ty.*
import esmeta.es.util.Coverage.*
import esmeta.state.Nearest
import scala.collection.mutable.{Set => MSet, Map => MMap}
import esmeta.cfg.{Func, Node}
import esmeta.util.SystemUtils.*
import io.circe.generic.auto._
import esmeta.ty.util.JsonProtocol
import esmeta.util.BaseUtils.*
import esmeta.es.util.Coverage
import esmeta.cfg.Call
import esmeta.spec.Spec

class TypeErrorDB() {
  private val stringifier = TyElem.getStringifier(false, false)
  import stringifier.given
  private val jsonProtocol = JsonProtocol
  import jsonProtocol.{*, given}

  var _db: MSet[DBRow] = MSet.empty

  def init: Unit =
    val tmpDb = readJson[List[DBRow]](
      "???/errorDB.json",
    )
    _db = MSet.from(tmpDb)

  def update(coverage: Coverage): Unit = addError(coverage.errorMap)

  def addError(errorMap: Map[String, Set[TypeError]]): Unit =
    for {
      (poc, typeErrorSet) <- errorMap
      typeError <- typeErrorSet
    }
      addError(poc, typeError)

  def addError(poc: String, error: TypeError): Unit =
    // ToDo : contain check
    _db += convertTypeError(error, poc)

  def dumpError: Unit =
    dumpJson(
      name = "error database",
      data = _db.toList.sortBy(_.id),
      filename = "errorDB.json",
      noSpace = false,
      silent = false,
    )

  def convertTypeError(error: TypeError, poc: String): DBRow = error match
    case ParamTypeMismatch(
          aap @ ArgAssignPoint(CallPoint(caller, callsite, callee), _),
          argTy,
        ) =>
      DBRow(
        _db.size + 1,
        "ParamTypeMismatch",
        callsite.id,
        Spec.currentVersion.toString,
        caller.name,
        Some(callee.name),
        Some(aap.param.lhs.name),
        Some(stringify(callsite.callInst.langOpt).trim()),
        "fuzzer",
        Some(poc),
        None,
        None,
      )
    case ReturnTypeMismatch(
          irp @ InternalReturnPoint(func, node, irReturn),
          retTy,
        ) =>
      DBRow(
        _db.size + 1,
        "ReturnTypeMismatch",
        node.id,
        Spec.currentVersion.toString,
        func.name,
        None,
        None,
        Some(stringify(irReturn.langOpt).trim()),
        "fuzzer",
        Some(poc),
        None,
        None,
      )
    case _ => esmeta.util.BaseUtils.error("[TypeErrorDB] wtf")

  // def init(analyzerErrorMap: Map[TypeErrorPoint, TypeError]) =
  //   this.analyzerErrorMap = analyzerErrorMap
  //   pendingError = MSet() ++ analyzerErrorMap.values.toSet
  //   analyzerErrorMap.keySet.foreach {
  //     case a @ ArgAssignPoint(CallPoint(_, callnode, _), _) =>
  //       nodeToPointMap += (callnode -> a)
  //       vulnerablePoints += callnode
  //     case i @ InternalReturnPoint(func, node, irReturn) =>
  //       nodeToPointMap += (node -> i)
  //       vulnerablePoints += node
  //     case _ =>
  //   }

  // /** set of all runtime errors */
  // var totalError: Set[Error] = Set()
  // // var rootError: MSet[RootError] = MSet()

  // /** unfound analyze reported errors */
  // var analyzerErrorMap: Map[TypeErrorPoint, TypeError] = Map()
  // var vulnerablePoints: MSet[Node] = MSet()
  // var nodeToPointMap: MMap[Node, TypeErrorPoint] = MMap()
  // var pendingError: MSet[TypeError] = MSet()

  // /** found analyze reported errors */
  // var verifiedError: MSet[Error] = MSet()

  // /** new runtime errors */
  // var discoveredError: MSet[Error] = MSet()

  // /** program visited node without error */
  // var reachedWithOutError: MMap[TypeErrorPoint, String] = MMap()

  // def addRuntimeErrors(
  //   runtimeErrors: Set[DetailedTypeError],
  //   code: String,
  // ): Unit = {
  //   for (runtimeError <- runtimeErrors) {
  //     isNewError(runtimeError, code) match {
  //       case Some(error) =>
  //         val contains = pendingError.foldLeft(false) { (acc, te) =>
  //           val found = (te, runtimeError) match
  //             case (
  //                   ParamTypeMismatch(p1, sa),
  //                   DetailedParamTypeMismatch(p2, da, _),
  //                 ) if (p1 == p2 && da <= sa) =>
  //               removeErrorFromAnalysis(runtimeError, te)
  //               verifiedError += error
  //               true
  //             case (
  //                   ReturnTypeMismatch(p1, sa),
  //                   DetailedReturnTypeMismatch(p2, da, _),
  //                 ) if (p1 == p2 && da <= sa) =>
  //               removeErrorFromAnalysis(runtimeError, te)
  //               verifiedError += error
  //               true
  //             case _ => false
  //           acc || found
  //         }

  //         if (!contains)
  //           discoveredError += error
  //       case None =>
  //     }
  //   }
  // }

  // private def removeErrorFromAnalysis(
  //   runtimeError: DetailedTypeError,
  //   analysisError: TypeError,
  // ): Unit =
  //   runtimeError match
  //     case DetailedParamTypeMismatch(
  //           ArgAssignPoint(CallPoint(_, callNode, _), _),
  //           _,
  //           _,
  //         ) =>
  //       vulnerablePoints -= callNode
  //     case DetailedReturnTypeMismatch(
  //           InternalReturnPoint(_, node, _),
  //           _,
  //           _,
  //         ) =>
  //       vulnerablePoints -= node
  //   pendingError -= analysisError

  // def check(
  //   nodeViewMap: Map[NodeView, Option[Nearest]],
  //   runtimeErrors: Set[DetailedTypeError],
  //   code: String,
  // ): Unit =
  //   for ((NodeView(node, _), _) <- nodeViewMap)
  //     if (vulnerablePoints.contains(node))
  //       val tep = nodeToPointMap.getOrElse(node, error("[TypeErrorManager] #1"))
  //       reachedWithOutError += (tep -> code)

  // private def isNewError(
  //   runtimeError: DetailedTypeError,
  //   code: String,
  // ): Option[Error] =
  //   if totalError.exists(_.error == runtimeError) then None
  //   else
  //     val newError = Error(nextEId, code, runtimeError)
  //     totalError += newError
  //     // rootError += getRoot(runtimeError)
  //     Some(newError)

  // private def removeDetail(detail: DetailedTypeError): TypeError = detail match
  //   case d: DetailedParamTypeMismatch  => ParamTypeMismatch(d.point, d.argTy)
  //   case d: DetailedReturnTypeMismatch => ReturnTypeMismatch(d.point, d.retTy)

  // // def getRoot(error: DetailedTypeError): RootError = error match
  // //   case p: DetailedParamTypeMismatch =>
  // //     RootError(p.point.callPoint.callee, p.errorDetail)
  // //   case r: DetailedReturnTypeMismatch => RootError(r.point.func, r.errorDetail)

  // private var eidCount: Int = 0
  // private def nextEId: Int = { val eid = eidCount; eidCount += 1; eid }

  // def dump(baseDir: String): Unit = {
  //   mkdir(baseDir)
  //   val orderedTotalError = totalError.toList
  //     .sortBy(_.id)
  //     .map(_.toString)
  //     .mkString(LINE_SEP + LINE_SEP)
  //   // val orderedRootError =
  //   //   rootError.toList.map(_.toString).mkString(LINE_SEP + LINE_SEP)
  //   val orderedPendingError =
  //     pendingError.toList.map(_.toString).mkString(LINE_SEP + LINE_SEP)
  //   val orderedVerifiedError = verifiedError.toList
  //     .sortBy(_.id)
  //     .map(_.toString)
  //     .mkString(LINE_SEP + LINE_SEP)
  //   val orderedDiscoveredError = discoveredError.toList
  //     .sortBy(_.id)
  //     .map(_.toString)
  //     .mkString(LINE_SEP + LINE_SEP)
  //   val orderedReachedWithOutError = reachedWithOutError
  //     .map { case (point, code) => s"Point : $point\nCode : $code" }
  //     .mkString(LINE_SEP + LINE_SEP)

  //   dumpFile(
  //     name = "total error",
  //     data = orderedTotalError,
  //     filename = s"$baseDir/total-error",
  //     silent = true,
  //   )
  //   // dumpFile(
  //   //   name = "root error",
  //   //   data = orderedRootError,
  //   //   filename = s"$baseDir/root-error",
  //   //   silent = true,
  //   // )
  //   dumpFile(
  //     name = "pending error",
  //     data = orderedPendingError,
  //     filename = s"$baseDir/pending-error",
  //     silent = true,
  //   )
  //   dumpFile(
  //     name = "verified error",
  //     data = orderedVerifiedError,
  //     filename = s"$baseDir/verified-error",
  //     silent = true,
  //   )
  //   dumpFile(
  //     name = "discovered error",
  //     data = orderedDiscoveredError,
  //     filename = s"$baseDir/discovered-error",
  //     silent = true,
  //   )
  //   dumpFile(
  //     name = "reached without error",
  //     data = orderedReachedWithOutError,
  //     filename = s"$baseDir/reached-without-error",
  //     silent = true,
  //   )
  // }
}
