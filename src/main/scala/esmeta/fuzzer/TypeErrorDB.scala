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
import esmeta.ty.ValueTopTy.record

case class TypeErrorRecord(
  id: Int,
  kind: String,
  var source: Set[String],
  var poc: String,
  notSupported: Boolean = false,
  notFixed: Boolean = false,
  error: TypeError,
)

class TypeErrorDB(cfg: CFG, source: String) {
  private val jsonProtocol = JsonProtocol2(cfg)
  import jsonProtocol.{*, given}

  private var _errorMap: MMap[TypeErrorPoint, TypeErrorRecord] = MMap.empty
  private val _newErrorMap: MMap[TypeErrorPoint, TypeErrorRecord] = MMap.empty

  def init: Unit =
    _errorMap.clear()
    _newErrorMap.clear()
    readJson[Array[TypeErrorRecord]](s"$MANUALS_DIR/errorDB.json").foreach(
      record => _errorMap += (record.error.point -> record),
    )

  def errors: Set[TypeErrorRecord] = Set.from(_errorMap.values)
  def discovered: Set[TypeErrorRecord] = Set.from(_newErrorMap.values)
  def pending = errors.filter(rec =>
    rec.source.size == 1 && rec.source.contains("adv-ty-refine"),
  )
  def verified = errors.filter(rec =>
    rec.source.size > 1 && rec.source.contains("adv-ty-refine"),
  )

  def update(poc: String, errors: MSet[RuntimeTypeError]): Unit = for {
    error <- errors
  } update(poc, error)

  def update(poc: String, rtError: RuntimeTypeError): Unit =
    val RuntimeTypeError(point, _, _) = rtError

    _errorMap.get(point) match
      case Some(record) => _errorMap += (point -> addNewSource(poc, record))
      case None =>
        _newErrorMap.get(point) match
          case Some(record) =>
            _newErrorMap += (point -> addNewSource(poc, record))
          case None =>
            _newErrorMap += (point -> convertToTypeErrorRecord(poc, rtError))

  private def addNewSource(
    poc: String,
    record: TypeErrorRecord,
  ): TypeErrorRecord =
    if (record.poc.length == 0 || record.poc.length > poc.length)
      record.poc = poc
    record.source += source
    record

  private def convertToTypeErrorRecord(
    poc: String,
    rtError: RuntimeTypeError,
  ): TypeErrorRecord =
    TypeErrorRecord(
      id = _errorMap.size + 1,
      kind = rtError.point match
        case aap: ArgAssignPoint      => "ParamTypeMismatch"
        case iip: InternalReturnPoint => "ReturnTypeMismatch"
        case _ => error("[TypeErrorDB] update failure by mismatch")
      ,
      source = Set(source),
      poc = poc,
      error = convertToTypeError(rtError),
    )

  private def convertToTypeError(rtError: RuntimeTypeError): TypeError =
    val RuntimeTypeError(point, value, st) = rtError
    point match
      case aap: ArgAssignPoint      => ParamTypeMismatch(aap, st.typeOf(value))
      case iip: InternalReturnPoint => ReturnTypeMismatch(iip, st.typeOf(value))
      case _ => error("[TypeErrorDB] update failure by mismatch")

  def dumpError(logDir: String): Unit =
    dumpJson(
      name = "updated error database",
      data = errors.toList.sortBy(_.id),
      filename = s"$logDir/errorDB.json",
      noSpace = false,
      silent = false,
    )
    dumpJson(
      name = "discovered errors",
      data = discovered.toList.sortBy(_.id),
      filename = s"$logDir/discoveredErrorDB.json",
      noSpace = false,
      silent = false,
    )
    println(
      s"[TypeErrorDB Result] db : #${_errorMap.size} / discovered : #${_newErrorMap.size}",
    )
}

object TypeErrorDB {
  def apply(cfg: CFG, source: String): TypeErrorDB =
    new TypeErrorDB(cfg, source)
}
