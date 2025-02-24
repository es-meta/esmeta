package esmeta.fuzzer

import esmeta.ty.*
import esmeta.lang.*
import esmeta.es.util.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Set => MSet, BitSet}
import io.circe.*, io.circe.generic.semiauto.*, io.circe.syntax.*

case class TypeErrorRecord(
  var id: Int = 0,
  kind: String,
  caller: String,
  callee: Option[String],
  idx: Option[Int],
  loc: String, // may differ by spec version, but very small diff
  source: BitSet = BitSet(),
  var poc: String = "",
  notSupported: Option[Boolean] = None, // manual
  unpatched: Option[Boolean] = None, // manual
)

object TypeErrorDB {

  private val _db: MSet[TypeErrorRecord] = MSet.empty

  def init(): Unit = {
    _db.clear()
    for { record <- ManualInfo.tpAlarms } add(record)
  }

  def add(record: TypeErrorRecord): Unit = {
    if (_db.exists(orig => matches(orig, record))) {
      val orig = _db.find(orig => matches(orig, record)).get
      orig.source |= record.source
      if (orig.poc.isEmpty || record.poc.length < orig.poc.length) {
        orig.poc = record.poc
      }
    } else {
      record.id = _db.size
      _db += record
    }
  }

  def update(cov: Coverage): Unit = {
    for ((code, errors) <- cov.errorMap) {
      for (error <- errors) {
        val record = convert(error)
        record.poc = code
        add(record)
      }
    }
  }

  def dump(baseDir: String): Unit = {
    import esmeta.ty.util.JsonProtocol.given
    dumpJson(
      name = "type error database",
      data = _db.toList.sorted,
      filename = s"$baseDir/type-error-database.json",
      noSpace = false,
      silent = false,
    )
  }

  def pending = _db.filter(_.source == BitSet(1)).toSet // only analyzer
  def verified = _db.filter(_.source == BitSet(0, 1)).toSet
  def discovered = _db.filter(_.source == BitSet(0)).toSet // only fuzzer

  import scala.math.Ordering.Implicits.seqOrdering
  given Ordering[TypeErrorRecord] = Ordering.by(_.id)

  // private helper
  private def matches(rec1: TypeErrorRecord, rec2: TypeErrorRecord): Boolean =
    rec1.kind == rec2.kind && rec1.caller == rec2.caller &&
    rec1.callee == rec2.callee && rec1.idx == rec2.idx && rec1.loc == rec2.loc

  private def convert(error: TypeError): TypeErrorRecord =
    val kind = error.getClass.getSimpleName
    val aux = (opt: Option[Syntax]) =>
      opt.flatMap(_.loc).map(_.toString).getOrElse("")
    error match
      case ParamTypeMismatch(point, _) =>
        TypeErrorRecord(
          kind = kind,
          caller = point.callPoint.caller.name,
          callee = Some(point.callPoint.callee.name),
          idx = Some(point.idx),
          loc = aux(point.callPoint.callsite.callInst.langOpt),
          source = BitSet(0),
        )
      case ReturnTypeMismatch(point, _) =>
        TypeErrorRecord(
          kind = kind,
          caller = point.func.name,
          callee = None,
          idx = None,
          loc = aux(point.irReturn.langOpt),
          source = BitSet(0),
        )
      case _ => BaseUtils.error("unexpected kind of TypeError")
}
