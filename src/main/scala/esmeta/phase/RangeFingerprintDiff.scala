package esmeta.phase

import esmeta.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.analyzer.util.Fingerprint
import java.io.File

case object RangeFingerprintDiff extends Phase[Unit, Unit] {
  val name = "range-fingerprint-diff"

  val help =
    "extracts statistics from fingerprint differences across multiple fingerprint sequence"

  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    val path = cmdConfig.targets.head
    val fingerprints: List[String] = (
      for {
        file <- walkTree(path)
        pathname = file.toString
        if pathname.endsWith("fingerprints.json")
      } yield pathname
    ).toList.sorted
    def makePairs[T](list: List[T]): List[(T, T)] =
      list match {
        case Nil          => Nil
        case x :: Nil     => Nil
        case x :: y :: xs => (x, y) :: makePairs(y :: xs)
      }
    val pairs = makePairs(fingerprints)
    val diffs = pairs.map {
      case (left, right) =>
        (left, right, Fingerprint.getDiff(left, right))
    }
    println(
      diffs
        .map { (left, right, diff) =>
          s"(${left.split('/').takeRight(2).head} - ${right.split('/').takeRight(2).head}) > added: ${diff.added.length} | removed: ${diff.removed.length} | preserved: ${diff.preserved.length}"
        }
        .mkString(LINE_SEP),
    )

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
