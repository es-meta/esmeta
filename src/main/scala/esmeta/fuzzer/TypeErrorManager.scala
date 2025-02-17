package esmeta.fuzzer

import esmeta.{LINE_SEP}
import esmeta.ty.*

import scala.collection.mutable.{Set => MSet}
import esmeta.cfg.Func
import esmeta.util.SystemUtils.*
import io.circe.generic.auto._

case class Error (
    id : Int,
    code : String,
    error : DetailedTypeError
)

case class RootError (
    func: Func, // Callee / Return
    detail: List[ErrorDetail]
)

class TypeErrorManager() {
    def init(analyzeErrors: Set[TypeError]) = pendingError = MSet() ++ analyzeErrors

    /** set of all runtime errors */
    var totalError : Set[Error] = Set()
    var rootError: MSet[RootError] = MSet()

    /** unfound analyze reported errors */
    var pendingError : MSet[TypeError] = MSet()

    /** found analyze reported errors */
    var verifiedError : MSet[Error] = MSet()
    
    /** new runtime errors */
    var discoveredError : MSet[Error] = MSet()

    def addRuntimeErrors(runtimeErrors: MSet[DetailedTypeError], code: String): Unit = {
        for (runtimeError <- runtimeErrors) {
            isNewError(runtimeError, code) match {
                case Some(error) =>
                    val undetailedError = removeDetail(runtimeError)
                    if (pendingError.contains(undetailedError)) {
                        pendingError -= undetailedError
                        verifiedError += error
                    } else {
                        discoveredError += error
                    }
                case None => 
            }
        }
    }

    def isNewError(runtimeError : DetailedTypeError, code:String) : Option[Error] =
        if totalError.exists(_.error == runtimeError) then None
        else
            val newError = Error(nextEId, code, runtimeError) 
            totalError += newError
            rootError += getRoot(runtimeError)
            Some(newError)

    // def convertToDetail(noDetail: ParamTypeMismatch | ReturnTypeMismatch): DetailedTypeError = noDetail match
    //     case p: ParamTypeMismatch  => DetailedParamTypeMismatch(p.point, p.argTy, None)
    //     case r: ReturnTypeMismatch => DetailedReturnTypeMismatch(r.point, r.retTy, None)

    def removeDetail(detail: DetailedTypeError): TypeError = detail match
        case d: DetailedParamTypeMismatch  => ParamTypeMismatch(d.point, d.argTy)
        case d: DetailedReturnTypeMismatch => ReturnTypeMismatch(d.point, d.retTy)

    def getRoot(error: DetailedTypeError) : RootError = error match
        case p: DetailedParamTypeMismatch => RootError(p.point.callPoint.callee, p.errorDetail)
        case r: DetailedReturnTypeMismatch => RootError(r.point.func, r.errorDetail)
        
    private var eidCount: Int = 0
    private def nextEId: Int = { val eid = eidCount; eidCount += 1; eid }


    def dump(baseDir: String) : Unit = {
        mkdir(baseDir) 
        lazy val orderedTotalError = totalError.toList.sortBy(_.id).map(_.toString).mkString(LINE_SEP + LINE_SEP)
        lazy val orderedRootError = rootError.toList.map(_.toString).mkString(LINE_SEP + LINE_SEP)
        lazy val orderedPendingError = pendingError.toList.map(_.toString).mkString(LINE_SEP + LINE_SEP)
        lazy val orderedVerifiedError = verifiedError.toList.sortBy(_.id).map(_.toString).mkString(LINE_SEP + LINE_SEP)
        lazy val orderedDiscoveredError = discoveredError.toList.sortBy(_.id).map(_.toString).mkString(LINE_SEP + LINE_SEP)

        dumpFile(
            name = "total error",
            data = orderedTotalError,
            filename = s"$baseDir/total-error.json",
            silent = true,
        )
        dumpFile(
            name = "root error",
            data = orderedRootError,
            filename = s"$baseDir/root-error.json",
            silent = true,
        )
        dumpFile(
            name = "pending error",
            data = orderedPendingError,
            filename = s"$baseDir/pending-error.json",
            silent = true,
        )
        dumpFile(
            name = "verified error",
            data = orderedVerifiedError,
            filename = s"$baseDir/verified-error.json",
            silent = true,
        )
        dumpFile(
            name = "discovered error",
            data = orderedDiscoveredError,
            filename = s"$baseDir/discovered-error.json",
            silent = true,
        )
    }
}