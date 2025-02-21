package esmeta.fuzzer

import esmeta.ty.*
import esmeta.{LINE_SEP, RESOURCE_DIR}
import esmeta.es.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Set => MSet, BitSet}
import io.circe.*, io.circe.generic.semiauto.*, io.circe.syntax.*
import esmeta.util.BasicJsonProtocol
import esmeta.RESOURCE_DIR

case class TypeErrorRecord(
  var id: Int = 0,
  kind: String,
  caller: String,
  callee: Option[String],
  idx: Option[Int],
  loc: Option[String], // may differ by spec version, but very small diff
  source: BitSet = BitSet(),
  var poc: String = "",
  notSupported: Option[Boolean] = None, // manual
  unpatched: Option[Boolean] = None, // manual
)

object JsonProtocol extends BasicJsonProtocol {
  given Decoder[BitSet] =
    Decoder.decodeIterable[Int, List].map(BitSet.fromSpecific)
  given Encoder[BitSet] = Encoder.encodeIterable[Int, List].contramap(_.toList)

  given Encoder[TypeErrorRecord] = deriveEncoder
  given Decoder[TypeErrorRecord] = deriveDecoder
}

object TypeErrorDB {
  private val jsonProtocol = JsonProtocol
  import jsonProtocol.{*, given}

  private val _db: MSet[TypeErrorRecord] = MSet.empty

  def init(): Unit =
    _db.clear()
    val readDb = readJson[List[TypeErrorRecord]](
      s"$RESOURCE_DIR/errorDB.json",
    )
    add(Set.from(readDb))

  def add(records: Set[TypeErrorRecord]): Unit =
    for (record <- records) { this.add(record) }

  def add(record: TypeErrorRecord): Unit =
    for (orig <- _db) {
      if (matches(orig, record)) {
        orig.source |= record.source
        if (
          orig.poc.nonEmpty ||
          record.poc.length < orig.poc.length
        ) orig.poc = record.poc
      } else {
        record.id = _db.size
        _db += record
      }
    }

  def update(cov: Coverage) =
    for ((code, errors) <- cov.errorMap) {
      for (error <- errors) {
        val record = convert(error)
        record.poc = code
        _db.add(record)
      }
    }

  def dump(baseDir: String) = dumpJson(
    name = "error database",
    data = _db,
    filename = s"$RESOURCE_DIR/newErrorDB.json",
    noSpace = false,
    silent = true,
  )
  def pending = _db.filter(_.source == BitSet(1)).toSet // only analyzer
  def verified = _db.filter(_.source == BitSet(0, 1)).toSet
  def discovered = _db.filter(_.source == BitSet(0)).toSet // only fuzzer

  // private helper
  private def matches(rec1: TypeErrorRecord, rec2: TypeErrorRecord): Boolean =
    rec1.kind == rec2.kind && rec1.caller == rec2.caller &&
    rec1.callee == rec2.callee && rec1.idx == rec2.idx && rec1.loc == rec2.loc

  private def convert(
    e: TypeError,
    fromAnalyzer: Boolean = false,
  ): TypeErrorRecord =
    e match
      case ParamTypeMismatch(point, _) =>
        TypeErrorRecord(
          kind = "ParamTypeMismatch",
          caller = point.callPoint.caller.name,
          callee = Some(point.callPoint.callee.name),
          idx = Some(point.idx),
          loc = point.callPoint.callsite.callInst.langOpt.map(_.toString),
          source = if (fromAnalyzer) BitSet(1) else BitSet(0),
        )
      case ReturnTypeMismatch(point, _) =>
        TypeErrorRecord(
          kind = "ReturnTypeMismatch",
          caller = point.func.name,
          callee = None,
          idx = None,
          loc = point.irReturn.langOpt.map(_.toString),
          source = if (fromAnalyzer) BitSet(1) else BitSet(0),
        )
      case _ => error("unexpected kind of TypeError")
}
