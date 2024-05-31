package esmeta.es.util.fuzzer

import esmeta.RESOURCE_DIR
import esmeta.util.BaseUtils.error
import esmeta.util.SystemUtils.readFile
import io.circe.Json
import io.circe.syntax._
import io.circe.parser.decode
import scala.collection.mutable.{Map => MMap, Set => MSet}

class MinifierDB(var map: MMap[String, MSet[String]]) {

  def labels: Iterable[String] = map.keys

  def asJson: Json = map.asJson

  def minimals: Iterable[String] = map.values.flatten

  def getLabel(program: String): Option[String] =
    map.find(_._2.exists(_ == program)).map(_._1)

}

object MinifierDB {
  def fromResource: MinifierDB =
    val file: String = readFile(s"$RESOURCE_DIR/minifyfuzz-db.json")
    decode[Map[String, List[String]]](file) match
      case Left(exception) => error(exception)
      case Right(v)        => MinifierDB(v)

  def apply(map: Map[String, List[String]]): MinifierDB =
    new MinifierDB(MMap.from(map.map((key, list) => key -> MSet.from(list))))
}
