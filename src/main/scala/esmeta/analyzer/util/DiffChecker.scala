package esmeta.analyzer.util

import esmeta.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import esmeta.error.ESMetaError

object DiffChecker {
  def diffBetweenTwo(
    leftPath: String,
    rightPath: String,
  ): (List[String], List[String]) = {
    val left = optional(readFile(leftPath).split(LINE_SEP).toSet)
      .getOrElse(throw ESMetaError(s"File not found: $leftPath"))
    val right = optional(readFile(rightPath).split(LINE_SEP).toSet)
      .getOrElse(throw ESMetaError(s"File not found: $rightPath"))

    // deleted, added
    (left.diff(right).toList.sorted, right.diff(left).toList.sorted)
  }
}
