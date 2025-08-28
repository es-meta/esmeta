package esmeta.phase

import esmeta.*
import esmeta.analyzer.eoggen.*
import esmeta.cfg.{CFG, Func}
import esmeta.es.*
import esmeta.parser.ESParser
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.nio.file.Path

/** `gen-eog` phase */
case object GenEOG extends Phase[CFG, Unit] {
  val name = "gen-eog"
  val help = "generates and execution-order graph (EOG)."

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    val targets = for {
      file <- walkTree(getFirstFilename(cmdConfig, name)).toList
      path = file.getAbsolutePath
      ext = getExt(file.getName)
      if ext == "js"
    } yield path
    if (targets.isEmpty)
      warn(s"No target files (.js) found")
    for {
      path <- targets
      dotFullPath = changeExt("js", "full.dot")(path)
      pdfFullPath = changeExt("js", "full.pdf")(path)
      dotPath = changeExt("js", "dot")(path)
      pdfPath = changeExt("js", "pdf")(path)
      filename = Path.of(path).getFileName.toString
      ast = cfg.scriptParser.fromFile(path)
      analyzer = EOGGenerator(
        cfg = cfg,
        ast = ast,
        log = config.log,
        useRepl = config.useRepl,
      )
    } do
      println(
        setColor(Console.YELLOW)(s"[$filename] Generating EOG for $path ..."),
      )
      suppress(s"${filename} analyze") { analyzer.analyze }
      suppress(s"${filename} dot.full") {
        dumpFile(analyzer.eog.dot, dotFullPath)
        assert(analyzer.eog.isValid, "EOG is invalid")
        executeCmd(s"""dot -Tpdf "$dotFullPath" -o "$pdfFullPath"""")
      }
      suppress(s"${filename} dot.simplified") {
        dumpFile(analyzer.eog.simplified.dot, dotPath)
        // assert(analyzer.eog.simplified.isValid, "(Simplified) EOG is invalid")
        executeCmd(s"""dot -Tpdf "$dotPath" -o "$pdfPath"""")
      }

  def suppress[T](tag: String)(body: => T): Unit = {
    try { body }
    catch {
      case e =>
        println(setColor(Console.RED)(s"[$tag] Error occurred: "))
        e.getMessage().linesIterator.foreach { line =>
          println(s"[$tag] $line")
        }
        e.getStackTrace().take(8).foreach { elem =>
          println(s"[$tag]   at $elem")
        }
    }
  }

  def nonBulk(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = {
    val filename = getFirstFilename(cmdConfig, name)
    val ast = cfg.scriptParser.fromFile(filename)
    val analyzer = EOGGenerator(
      cfg = cfg,
      ast = ast,
      log = config.log,
      useRepl = config.useRepl,
    )
    analyzer.analyze
    dumpFile(analyzer.eog.dot, s"${ANALYZE_LOG_DIR}/eog.dot")
    executeCmd(
      s"""dot -Tpdf "${ANALYZE_LOG_DIR}/eog.dot" -o "${ANALYZE_LOG_DIR}/eog.pdf"""",
    )
    dumpFile(analyzer.eog.simplified.dot, s"${ANALYZE_LOG_DIR}/eog2.dot")
    executeCmd(
      s"""dot -Tpdf "${ANALYZE_LOG_DIR}/eog2.dot" -o "${ANALYZE_LOG_DIR}/eog2.pdf"""",
    )
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "repl",
      BoolOption(_.useRepl = _),
      "use a REPL for type checking of ECMA-262.",
    ),
    (
      "log",
      BoolOption(_.log = _),
      "logging mode.",
    ),
  )
  case class Config(
    var useRepl: Boolean = false,
    var log: Boolean = false,
  )
}
