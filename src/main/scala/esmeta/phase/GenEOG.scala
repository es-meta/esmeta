package esmeta.phase

import esmeta.*
import esmeta.analyzer.eoggen.*
import esmeta.cfg.{CFG, Func}
import esmeta.es.*
import esmeta.parser.ESParser
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** `gen-eog` phase */
case object GenEOG extends Phase[CFG, Unit] {
  val name = "gen-eog"
  val help = "generates and execution-order graph (EOG)."

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    val files = walkTree(getFirstFilename(cmdConfig, name)).toSeq
    if (files.isEmpty) raise("No target files found.")
    if (files.length > 1) bulk(cfg, cmdConfig, config)
    else nonBulk(cfg, cmdConfig, config)

  def bulk(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = {
    for {
      file <- walkTree(getFirstFilename(cmdConfig, name)).toList
      path = file.getAbsolutePath
      ext = getExt(file.getName)
      if ext == "js"
      dotFullPath = changeExt("js", "full.dot")(path)
      pdfFullPath = changeExt("js", "full.pdf")(path)

      dotPath = changeExt("js", "dot")(path)
      pdfPath = changeExt("js", "pdf")(path)
    } do {
      println(s"Generating EOG for $path ...")
      val ast = cfg.scriptParser.fromFile(path)
      val analyzer = EOGGenerator(
        cfg = cfg,
        ast = ast,
        log = config.log,
        useRepl = config.useRepl,
      )
      analyzer.analyze
      dumpFile(analyzer.eog.dot, dotFullPath)
      executeCmd(s"""dot -Tpdf "$dotFullPath" -o "$pdfFullPath"""")
      dumpFile(analyzer.eog.simplified.dot, dotPath)
      executeCmd(s"""dot -Tpdf "$dotPath" -o "$pdfPath"""")
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
