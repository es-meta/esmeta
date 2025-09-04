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
      val colored = setColor(Console.CYAN)(s"[$filename]")
      var error: Boolean = false
      deleteFile(dotFullPath) // delete previous .dot file
      deleteFile(pdfFullPath) // delete previous .pdf file
      deleteFile(dotPath) // delete previous .dot file
      deleteFile(pdfPath) // delete previous .pdf file
      println("-" * 48)
      print(s"${colored} Generating EOG for ${setColor(Console.YELLOW)(path)}")
      suppress(s"${colored} [analyze]", { error = true }) { analyzer.analyze }
      suppress(s"${colored} [full]", { error = true }) {
        dumpFile(analyzer.eog.dot, dotFullPath)
        executeCmd(s"""dot -Tpdf "$dotFullPath" -o "$pdfFullPath"""")
        deleteFile(dotFullPath) // delete .dot file after generating .pdf
      }
      suppress(s"${colored} [simplified]", { error = true }) {
        dumpFile(analyzer.eog.simplified.dot, dotPath)
        executeCmd(s"""dot -Tpdf "$dotPath" -o "$pdfPath"""")
        deleteFile(dotPath) // delete .dot file after generating .pdf
      }
      println(
        if (error) setColor(Console.RED)(s"[$filename] Failed")
        else (" ~> " + setColor(Console.GREEN)(s"${pdfPath}")),
      )

  def suppress[T](tag: String, cleanup: => Unit = ())(body: => T): Unit = {
    try { body }
    catch {
      case e =>
        println(setColor(Console.RED)(s"$tag Error occurred: "))
        e.getMessage().linesIterator.foreach { line =>
          println(s"$tag $line")
        }
        e.getStackTrace().take(8).foreach { elem =>
          println(s"$tag ${" " * 8}at $elem")
        }
        cleanup
    }
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
