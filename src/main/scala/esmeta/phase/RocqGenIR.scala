package esmeta.phase

import esmeta.*
import esmeta.ir.Program
import esmeta.rocq.RocqGenerator
import esmeta.util.{BoolOption, StrOption}
import esmeta.util.SystemUtils.*
import java.io.File

/** `rocqgen-ir` phase
  *
  * Generates Rocq from a standalone `.ir` file rather than from ECMA-262. Such
  * a program is closed -- it carries its own `@main` and every function it
  * calls -- so the dump is a self-contained CRIS module, and the round trip
  * costs seconds instead of a full specification extraction.
  */
case object RocqGenIR extends Phase[Unit, Unit] {
  val name = "rocqgen-ir"
  val help = "generates Rocq files for standalone IR programs."
  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = for (filename <- cmdConfig.targets) {
    val program = Program.fromFile(filename)
    // One directory per input program: every `.ir` file declares its own
    // `main`, so a shared directory would have them overwrite each other.
    val baseDir = s"${config.out}/${stem(filename)}"
    // Unlike the specification dump, this one is regenerated constantly and a
    // function deleted from the `.ir` file must not linger as a stale module.
    rmdir(baseDir)
    println(s"- Generating Rocq for `$filename`.")
    RocqGenerator(program).dumpTo(baseDir, specProofs = false)
    copyProofs(stem(filename), s"$baseDir/func")
  }

  /** Copy the hand-written proofs kept for this program, if any.
    *
    * They live next to the support layer, under `manuals/rocq/ir/<stem>/`, so a
    * proof stays with the `.ir` file it is about instead of inside a log
    * directory the next run deletes.
    */
  private def copyProofs(stem: String, dirname: String): Unit =
    val proofDir = File(s"$MANUALS_DIR/rocq/ir/$stem")
    if (proofDir.isDirectory) {
      val proofs = listFiles(proofDir).filter(_.getName.endsWith(".v"))
      for (proof <- proofs)
        copyFile(proof.toString, s"$dirname/${proof.getName}")
      if (proofs.nonEmpty)
        println(
          s"- Copied ${proofs.size} hand-written proof(s) into `$dirname`.",
        )
    }

  /** The file name without its directory or `.ir` extension. */
  private def stem(filename: String): String =
    val base = File(filename).getName
    if (base.endsWith(".ir")) base.dropRight(".ir".length) else base

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    ("log", BoolOption(_.log = _), "turn on logging mode."),
    ("out", StrOption(_.out = _), "output directory (default: logs/rocq-ir)."),
  )
  case class Config(
    var log: Boolean = false,
    var out: String = ROCQ_IR_LOG_DIR,
  )
}
