package esmeta.analyzer.util

import esmeta.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import esmeta.error.ESMetaError
import esmeta.util.ManualInfo
import esmeta.analyzer.TypeError

object Fingerprint {

  def apply(err: Iterable[TypeError]): List[String] =
    err.map(_.toFingerprint).map(normStr).toList

  private def readFingerprintJson(path: String): Map[String, List[String]] =
    optional(readJson[Map[String, List[String]]](path))
      .getOrElse(throw ESMetaError(s"File not found: $path"))

  case class FingerprintDiff(
    added: List[String],
    removed: List[String],
    preserved: List[String],
  )
  def getDiff(
    leftPath: String,
    rightPath: String,
  ): FingerprintDiff = {
    val left: Set[String] = readFingerprintJson(leftPath).values.flatten.toSet
    val right: Set[String] = readFingerprintJson(rightPath).values.flatten.toSet
    val added = right.diff(left)
    val removed = left.diff(right)
    val preserved = left.intersect(right)
    FingerprintDiff(
      added.toList,
      removed.toList,
      preserved.toList,
    )
  }
}
