package esmeta.phase

import esmeta.*
import esmeta.error.ESMetaError
import esmeta.ir.Program
import esmeta.rocq.RocqGenerator
import esmeta.util.{BoolOption, StrOption}
import esmeta.util.SystemUtils.*
import java.io.File
import java.nio.file.Path

/** `rocqgen` phase */
case object RocqGen extends Phase[Unit, Unit] {
  val name = "rocqgen"
  val help = "generates shallow Rocq ITree modules from ESMeta IR files."

  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = {
    val targets =
      if (cmdConfig.targets.isEmpty) List(IR_TEST_DIR)
      else cmdConfig.targets
    val inputs = collectInputs(targets)
    if (inputs.isEmpty) fail("no .ir files found")

    val planned = inputs.map {
      case (input, relative) =>
        input -> s"${config.out}/${rocqPath(relative)}"
    }
    val collisions = planned
      .groupBy(_._2)
      .collect { case (output, entries) if entries.size > 1 => output }
    if (collisions.nonEmpty)
      fail(
        s"multiple IR inputs map to: ${collisions.toList.sorted.mkString(", ")}",
      )

    mkdir(config.out)
    copyFile(
      s"$MANUALS_DIR/rocq/ITreeIR.v",
      s"${config.out}/ITreeIR.v",
    )
    val outputs = planned.map {
      case (input, output) =>
        RocqGenerator(
          Program.fromFile(input.toString),
          config.proofObligations,
        ).dumpTo(output)
        output
    }
    dumpProject(config.out, outputs)
    copyFile(
      s"$MANUALS_DIR/rocq/Makefile.itree",
      s"${config.out}/Makefile",
    )
    copyFile(
      s"$MANUALS_DIR/rocq/README.md",
      s"${config.out}/README.md",
    )
    println(s"- Generated ${outputs.size} shallow ITree module(s).")
  }

  def defaultConfig: Config = Config()

  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption(_.out = _),
      "set output directory (default: $ESMETA_HOME/logs/rocq).",
    ),
    (
      "proof-obligations",
      BoolOption(_.proofObligations = _),
      "generate path-sensitive proof obligations for IR assertions.",
    ),
  )

  case class Config(
    var out: String = s"$LOG_DIR/rocq",
    var proofObligations: Boolean = false,
  )

  private def collectInputs(
    targets: List[String],
  ): List[(File, String)] =
    targets
      .flatMap { targetName =>
        val target = File(targetName)
        if (!target.exists)
          fail(s"IR target does not exist: $targetName")
        if (target.isFile) {
          if (!irFilter(target.getName)) fail(s"not an .ir file: $targetName")
          List(target -> target.getName)
        } else {
          val root = target.toPath.toAbsolutePath.normalize
          walkTree(target)
            .filter(file => file.isFile && irFilter(file.getName))
            .map { file =>
              val path = file.toPath.toAbsolutePath.normalize
              file -> root.relativize(path).toString
            }
            .toList
        }
      }
      .sortBy(_._2)

  private def rocqPath(relative: String): String = {
    val path = Path.of(relative)
    val parent = Option(path.getParent).map(_.toString + "/").getOrElse("")
    val basename = removedExt(path.getFileName.toString)
    val encoded = basename.map {
      case char if char.isLetterOrDigit || char == '_' => char
      case _                                           => '_'
    }
    s"$parent$encoded.v"
  }

  private def dumpProject(out: String, outputs: List[String]): Unit = {
    val root = Path.of(out).toAbsolutePath.normalize
    val files = outputs.map(output =>
      root.relativize(Path.of(output).toAbsolutePath.normalize).toString,
    )
    val project =
      (List("-Q . ESMeta", "ITreeIR.v") ++ files).mkString(LINE_SEP) + LINE_SEP
    dumpFile(project, s"$out/_CoqProject")
  }

  private def fail(message: String): Nothing =
    throw ESMetaError(message, "RocqGenError")
}
