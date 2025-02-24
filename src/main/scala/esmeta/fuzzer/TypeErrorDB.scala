package esmeta.fuzzer

import esmeta.{LINE_SEP, MANUALS_DIR}
import esmeta.ty.*
import esmeta.ty.util.*
import esmeta.es.util.Coverage.*
import esmeta.state.Nearest
import scala.collection.mutable.{Set => MSet, Map => MMap}
import esmeta.cfg.{Func, Node}
import esmeta.util.SystemUtils.*
import io.circe.generic.auto._
import esmeta.util.BaseUtils.*
import esmeta.es.util.Coverage
import esmeta.cfg.*
import esmeta.spec.Spec

class TypeErrorDB(cfg: CFG, source: String) {
  private val jsonProtocol = JsonProtocol2(cfg)
  import jsonProtocol.{*, given}

  // ToDo : change it to map and eliminate custom equality
  val _db: MSet[TypeErrorRecord] =
    MSet.from(readJson[Set[TypeErrorRecord]](s"$MANUALS_DIR/errorDB.json"))
  val _discoveredDB: MSet[TypeErrorRecord] = MSet.empty

  def pendingErrors: Int = _db.count(rec =>
    rec.source.size == 1 && rec.source.contains("adv-ty-refine"),
  )
  def verifiedErrors: Int = _db.count(rec =>
    rec.source.size > 1 && rec.source.contains("adv-ty-refine"),
  )
  def discoveredErrors: Int = _discoveredDB.size

  private var eidCount: Int = _db.size + 1
  protected def nextEId: Int = { val eid = eidCount; eidCount += 1; eid }

  def update(poc: String, errors: MSet[RuntimeTypeError]): Unit = for {
    error <- errors
  } update(poc, error)

  // ToDo : optimize by custom equalty or node map
  def update(poc: String, rtError: RuntimeTypeError): Unit =
    var inDB = false
    val RuntimeTypeError(point, value, st) = rtError
    for {
      rec <- _db
      if rec.error.point == point
    } {
      rec.error match
        case ptm @ ParamTypeMismatch(_, argTy) =>
          if (argTy.contains(value, st))
            inDB = true
            addNewSource(poc, rec)
        case rtm @ ReturnTypeMismatch(_, retTy) =>
          if (retTy.contains(value, st))
            inDB = true
            addNewSource(poc, rec)
        case _ => error("[TypeErrorDB] update failure by mismatch")
    }
    if !inDB then _discoveredDB += convertToTypeErrorRecord(poc, rtError)

  private def addNewSource(poc: String, ter: TypeErrorRecord): TypeErrorRecord =
    if (ter.poc.length > poc.length) ter.poc = poc
    ter.source = ter.source + source
    ter

  private def convertToTypeErrorRecord(
    poc: String,
    rtError: RuntimeTypeError,
  ): TypeErrorRecord =
    TypeErrorRecord(
      id = nextEId,
      kind = rtError.point match
        case aap: ArgAssignPoint      => "ParamTypeMismatch"
        case iip: InternalReturnPoint => "ReturnTypeMismatch"
        case _ => error("[TypeErrorDB] update failure by mismatch")
      ,
      source = Set(source),
      poc = poc,
      error = convertToTypeError(rtError),
    )

  // ToDo : refactor `typeOf`
  private def convertToTypeError(rtError: RuntimeTypeError): TypeError =
    val RuntimeTypeError(point, value, st) = rtError
    point match
      case aap: ArgAssignPoint      => ParamTypeMismatch(aap, st.typeOf(value))
      case iip: InternalReturnPoint => ReturnTypeMismatch(iip, st.typeOf(value))
      case _ => error("[TypeErrorDB] update failure by mismatch")

  // def pendingErrors: Int = _db.count()
  def dumpError: Unit =
    dumpJson(
      name = "updated error database",
      data = _db.toList.sortBy(_.id),
      filename = s"$MANUALS_DIR/errorDB/errorDB.json",
      noSpace = false,
      silent = false,
    )
    dumpJson(
      name = "discovered errors",
      data = _discoveredDB.toList.sortBy(_.id),
      filename = s"$MANUALS_DIR/errorDB/discoveredErrorDB.json",
      noSpace = false,
      silent = false,
    )
    println(
      s"[TypeErrorDB Result] db : #${_db.size} / discovered : #${_discoveredDB.size}",
    )
}
