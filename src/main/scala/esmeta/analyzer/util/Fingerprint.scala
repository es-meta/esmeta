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

  def tagStatistics(
    manual: Map[String, List[String]],
    fp: List[String],
  ): Map[String, Int] = {
    val invertedManual = for {
      (tag, strings) <- manual
      string <- strings
    } yield string -> tag

    fp.map(string => invertedManual.getOrElse(string, "Unknown"))
      .groupBy(identity)
      .map { case (tag, instances) => tag -> instances.size }
  }

  def diffBetweenTwo(
    leftPath: String,
    rightPath: String,
    record: Boolean = true,
  ): (List[String], List[String]) = {
    val left =
      if (leftPath == "") Set()
      else
        optional(readFile(leftPath + "/fingerprints").split(LINE_SEP).toSet)
          .getOrElse(throw ESMetaError(s"File not found: $leftPath"))
    val right =
      optional(readFile(rightPath + "/fingerprints").split(LINE_SEP).toSet)
        .getOrElse(throw ESMetaError(s"File not found: $rightPath"))

    val ldiff = left.diff(right).toList.sorted
    val rdiff = right.diff(left).toList.sorted

    if (record) {
      val delPath = s"$rightPath/removed"
      val addPath = s"$rightPath/added"
      if (ldiff.nonEmpty)
        dumpFile(
          name = "deleted type errors",
          data = ldiff.mkString(LINE_SEP),
          filename = delPath,
        )
      if (rdiff.nonEmpty)
        dumpFile(
          name = "added type errors",
          data = rdiff.mkString(LINE_SEP),
          filename = addPath,
        )
    }

    // deleted, added
    (ldiff, rdiff)
  }
}
