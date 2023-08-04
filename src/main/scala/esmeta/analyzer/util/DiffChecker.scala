package esmeta.analyzer.util

import esmeta.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import esmeta.error.ESMetaError

object DiffChecker {
  def diffBetweenTwo(
    leftPath: String,
    rightPath: String,
    record: Boolean = true,
  ): (List[String], List[String]) = {
    val left = optional(readFile(leftPath).split(LINE_SEP).toSet)
      .getOrElse(throw ESMetaError(s"File not found: $leftPath"))
    val right = optional(readFile(rightPath).split(LINE_SEP).toSet)
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
          silent = true,
        )
      if (rdiff.nonEmpty)
        dumpFile(
          name = "added type errors",
          data = rdiff.mkString(LINE_SEP),
          filename = addPath,
          silent = true,
        )
    }

    (ldiff, rdiff)

    // deleted, added
  }
}
