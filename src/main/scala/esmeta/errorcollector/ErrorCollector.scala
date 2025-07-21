package esmeta.errorcollector

import esmeta.*
import esmeta.ty.{*, given}
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Map => MMap}

object ErrorCollector {

  val tyStringifier = TyElem.getStringifier(true, false)
  import tyStringifier.given

  def map: MMap[TypeErrorPoint, Map[String, Set[TypeError]]] = _map
  private val _map: MMap[TypeErrorPoint, Map[String, Set[TypeError]]] = MMap()

  def formatted: String =
    _map.toVector
      .sortBy(-_._2.values.map(_.size).sum)
      .map {
        case (errorPoint, sourceMap) =>
          val header = s"[TypeErrorPoint: ${errorPoint.node.simpleString}]"
          val sources = sourceMap.toVector
            .sortBy(-_._2.size)
            .map {
              case (source, errors) =>
                val sourceHeader = s"source: $source"
                val errorList = errors
                  .map { error =>
                    error.toString
                      .split("\n")
                      .zipWithIndex
                      .map {
                        case (line, 0) => s"- $line"
                        case (line, _) => s"  $line"
                      }
                      .mkString(LINE_SEP)
                  }
                  .mkString(LINE_SEP)
                s"$sourceHeader$LINE_SEP$errorList"
            }
            .mkString(LINE_SEP)
          s"$header$LINE_SEP$sources"
      }
      .mkString(LINE_SEP + LINE_SEP)

  def addError(source: String, error: TypeError) =
    val point = error.point
    map.updateWith(point) {
      case None => Some(Map(source -> Set(error)))
      case Some(inner) =>
        Some(inner + (source -> (inner.getOrElse(source, Set.empty) + error)))
    }

  def dump(logDir: Option[String]) = logDir match
    case Some(path) =>
      dumpFile(
        name = "detected type errors",
        data = formatted,
        filename = s"$path/spec-type-errors",
      )
    case None => println(formatted)
}
