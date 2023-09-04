package esmeta.phase

import esmeta.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.analyzer.util.Fingerprint
import java.io.File
import esmeta.analyzer.util.Fingerprint.FingerprintDiff
import esmeta.util.BaseUtils.ratioSimpleString

case object RangeFingerprintDiff extends Phase[Unit, Unit] {
  val name = "range-fingerprint-diff"

  val help =
    "extracts statistics from multiple fingerprint sequence"

  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    val dirPath = cmdConfig.targets.head
    val paths: List[String] = (
      for {
        file <- walkTree(dirPath)
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
    val pairs = makePairs(paths)
    val diffs = pairs.map {
      case (left, right) =>
        val leftVersion = left.split('/').takeRight(2).head
        val rightVersion = right.split('/').takeRight(2).head
        (leftVersion, rightVersion, Fingerprint.getDiff(left, right))
    }
    val initialFingerprint = Fingerprint.readFingerprintJson(paths.head)
    val lifespans = getLifespans(
      LifespanState(initialFingerprint.values.flatten.toList),
      diffs.map(_._3),
    )
    val summary = {
      (for {
        path <- paths
        fingerprint = Fingerprint.readFingerprintJson(path)
        (tag, values) <- fingerprint.toSeq
        value <- values
      } yield (tag, value)).toSet.groupMap(_._1)(_._2)
    }
    dumpFile(
      name = "difference in fingerprint between two revisions",
      data = Yaml(
        "count" -> diffs.map { (left, right, diff) =>
          Map(
            s"$left -> $right" -> Map(
              "added" -> diff.added.length,
              "removed" -> diff.removed.length,
              "preserved" -> diff.preserved.length,
            ),
          )
        },
        "detail" -> diffs.map { (left, right, diff) =>
          Map(
            s"$left -> $right" -> Map(
              "added" -> diff.added,
              "removed" -> diff.removed,
              "preserved" -> diff.preserved,
            ),
          )
        },
      ),
      filename = s"$LOG_DIR/range-fingerprint-diff/diffs.yml",
    )
    dumpFile(
      name =
        "difference in fingerprint between two revisions (csv without detail)",
      data = diffs
        .map { (left, right, diff) =>
          List(
            left,
            right,
            diff.added.length,
            diff.removed.length,
            diff.preserved.length,
          ).mkString(",")
        }
        .mkString(LINE_SEP),
      filename = s"$LOG_DIR/range-fingerprint-diff/diffs.csv",
    )
    dumpFile(
      name = "lifespan of fingerprints",
      data = lifespans
        .map { lifespan =>
          List(
            lifespan.fingerprint,
            paths(lifespan.appear._1),
            paths(lifespan.appear._2),
            (lifespan.appear._2 - lifespan.appear._1 + 1),
          ).mkString("\t")
        }
        .mkString(LINE_SEP),
      filename = s"$LOG_DIR/range-fingerprint-diff/lifespans.tsv",
    )
    dumpFile(
      name = "summary of all fingerprints",
      data = Yaml(
        Map("total" -> summary.values.flatten.size) ++
        (summary.map { (tag, values) =>
          (tag -> s"${values.size} (${ratioSimpleString(values.size, summary.values.flatten.size)})")
        }.toMap),
      ),
      filename = s"$LOG_DIR/range-fingerprint-diff/summary.yml",
    )

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}

case class LifespanState(
  alive: Map[String, Int],
  generation: Int,
)
object LifespanState {
  def apply(): LifespanState = LifespanState(alive = Map(), generation = 0)
  def apply(list: List[String]): LifespanState =
    LifespanState(alive = list.map(_ -> 0).toMap, generation = 0)
}
case class Lifespan(
  fingerprint: String,
  appear: (Int, Int),
)
def getLifespans(
  state: LifespanState,
  diffs: List[FingerprintDiff],
): List[Lifespan] = {
  val (finalState, lifespans) = diffs.foldLeft((state, List[Lifespan]())) {
    case ((curr, lst), diff) => {
      val spans = (for {
        removed <- diff.removed
        firstAppear <- curr.alive.get(removed)
      } yield Lifespan(removed, (firstAppear, curr.generation)))
      val next = LifespanState(
        alive = curr.alive -- diff.removed ++ diff.added.map(
          _ -> (curr.generation + 1),
        ),
        generation = curr.generation + 1,
      )
      (next, lst ++ spans)
    }
  }
  lifespans ++ finalState.alive.map { (fingerprint, firstAppear) =>
    Lifespan(
      fingerprint,
      (firstAppear, finalState.generation - 1),
    )
  }.toList
}
