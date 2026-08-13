package esmeta.rocq

import esmeta.{LINE_SEP, MANUALS_DIR}
import esmeta.ir.{Func, Program}
import esmeta.util.ProgressBar
import esmeta.util.SystemUtils.*
import java.util.concurrent.{ConcurrentLinkedQueue}
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*

/** Summary of file dumping and semantic IR-to-Rocq translation. */
case class RocqDumpSummary(
  succeeded: Int,
  failed: Int,
  translationSucceeded: Int,
  fallout: Int,
)

private case class RocqFallout(
  module: String,
  function: String,
  reasons: List[String],
)

/** Dumps one CRIS ITree implementation per IR function. */
class RocqGenerator(
  program: Program,
  stringifier: RocqStringifier,
) {
  def this(program: Program) = this(program, new RocqStringifier(program.funcs))

  private val supportFiles =
    List(
      "type.v",
      "manual_type.v",
      "op.v",
      "itree_state.v",
      "_CoqProject",
      "Makefile",
    )

  /** Hand-written proofs about the generated functions. */
  private val proofFiles =
    List(
      "Equiv_IsCompatiblePropertyDescriptor.v",
      "Proto_HeapState.v",
    )
  private val obsoleteRegistryFiles = List("Functypes.v", "f_run.v")

  def apply(func: Func): String = stringifier(func)
  def translate(func: Func): RocqTranslation = stringifier.translate(func)

  /** Dump one `.v` file per IR function and an aggregate `program.v` file. */
  def dumpTo(baseDir: String): RocqDumpSummary = {
    val dirname = s"$baseDir/func"
    val translationSucceeded = AtomicInteger()
    val fallout = AtomicInteger()
    val falloutDetails = ConcurrentLinkedQueue[RocqFallout]()
    val progress = ProgressBar(
      "Dump Rocq ITree function implementations",
      program.funcs,
      getName = (func, _) => func.name,
      detail = false,
    )
    dumpDir(
      name = "Rocq ITree function implementations",
      iterable = progress,
      dirname = dirname,
      getName = func => s"${RocqNaming.module(func)}.v",
      getData = func => {
        val translation = translate(func)
        translation.status match {
          case RocqTranslationStatus.Succeeded =>
            translationSucceeded.incrementAndGet()
          case RocqTranslationStatus.Fallout(reasons) =>
            fallout.incrementAndGet()
            falloutDetails.add(
              RocqFallout(
                RocqNaming.module(func),
                func.name,
                reasons,
              ),
            )
        }
        translation.source
      },
    )
    for (filename <- supportFiles ++ proofFiles)
      copyFile(
        s"$MANUALS_DIR/rocq/$filename",
        s"$dirname/$filename",
      )
    println(s"- Copied Rocq support and proof files into `$dirname`.")
    for (filename <- obsoleteRegistryFiles)
      deleteFile(s"$dirname/$filename")
    for {
      file <- listFiles(dirname)
      if file.getName.endsWith("_ITree.v")
    } deleteFile(file.toString)
    dumpFile(
      name = "Rocq function signatures",
      data = stringifier.signatures(program.funcs),
      filename = s"$dirname/Signatures.v",
    )
    dumpFile(
      name = "Rocq program module",
      data = stringifier.program(program.funcs),
      filename = s"$dirname/program.v",
    )
    val summary = RocqDumpSummary(
      succeeded = progress.summary.passCount,
      failed = progress.summary.failCount,
      translationSucceeded = translationSucceeded.get,
      fallout = fallout.get,
    )
    dumpFile(
      name = "Rocq translation report",
      data = report(
        summary,
        falloutDetails.asScala.toList,
        progress.summary.fail.all,
      ),
      filename = s"$baseDir/report.txt",
    )
    println("- Rocq dump summary:")
    println(f"  - succeeded: ${summary.succeeded}%,d")
    println(f"  - failed: ${summary.failed}%,d")
    println(f"  - translation succeeded: ${summary.translationSucceeded}%,d")
    println(f"  - fallout: ${summary.fallout}%,d")
    summary
  }

  private def report(
    summary: RocqDumpSummary,
    falloutDetails: List[RocqFallout],
    failedFunctions: List[String],
  ): String = {
    val translationTotal = summary.translationSucceeded + summary.fallout
    val translationSuccessRate =
      if (translationTotal == 0) 100.0
      else summary.translationSucceeded.toDouble / translationTotal * 100
    val summaryLines = List(
      "Rocq translation report",
      "=======================",
      f"total IR functions: ${program.funcs.size}%,d",
      f"file dump succeeded: ${summary.succeeded}%,d",
      f"file dump failed: ${summary.failed}%,d",
      f"translation succeeded: ${summary.translationSucceeded}%,d",
      f"fallout: ${summary.fallout}%,d",
      f"translation success rate: $translationSuccessRate%.2f%%",
    )
    val falloutLines =
      if (falloutDetails.isEmpty) List("Fallout details:", "(none)")
      else
        "Fallout details:" :: falloutDetails
          .sortBy(detail => (detail.module, detail.function))
          .flatMap { detail =>
            s"- ${detail.function} [func/${detail.module}.v]" ::
            detail.reasons.map(reason => s"  - $reason")
          }
    val failureLines =
      if (failedFunctions.isEmpty) List("File dump failures:", "(none)")
      else
        "File dump failures:" ::
        failedFunctions.sorted.map(name => s"- $name")

    (summaryLines ++ List("") ++ falloutLines ++ List("") ++ failureLines)
      .mkString(LINE_SEP) + LINE_SEP
  }
}

object RocqGenerator {
  def apply(program: Program): RocqGenerator = new RocqGenerator(program)
}
