package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.es.*
import esmeta.interpreter.*
import esmeta.ir.Program
import esmeta.parser.{ESParser}
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.peval.{OverloadedFunc, PartialInterpreter, OverloadedIRFunc}

def findByName(ast : Ast, name : String): List[Ast] = ast match
  case l @ Lexical(n, str) if (n == name) => List(l)
  case s @ Syntactic(n, _, _, children) =>
    val fromChildren = children.map(
    _.map(findByName(_, name)).getOrElse(Nil)
   ).flatten
    val fromS = if (n == name) then List(s) else Nil
    fromS ::: fromChildren
  case _ => Nil

/** `astpeval` phase */
case object AstPeval extends Phase[CFG, Unit] {
  val name = "astpeval"
  val help = "partial-evaluated an ECMAScript file."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    val filename = getFirstFilename(cmdConfig, name)
    val pevalTargetName = "FunctionDeclarationInstantiation"
    val pevalTarget = cfg.fnameMap(pevalTargetName)
    val ast = ESParser(cfg.spec.grammar, debug = false)("Script").fromFile(filename)
    val fds = findByName(ast, "FunctionDeclaration")
    println(s"Found ${fds.size} FunctionDeclaration.");
    for (fd <- fds) {
      println(fd.name)
    }


    
    // if (config.multiple)
    //   var st = State(cfg, Context(cfg.main))
    //   for {
    //     path <- cmdConfig.targets
    //     file <- walkTree(path)
    //     filename = file.toString
    //     if jsFilter(filename)
    //   } st = run(cfg, config, filename)
    //   st
    // else run(cfg, config, getFirstFilename(cmdConfig, this.name))

  // def run(cfg: CFG, config: Config, filename: String): State =
  //   val (newCfg, overloads) =
  //     if (!config.peval) then
  //       (
  //         Initialize.fromFile(cfg, filename),
  //         Map.empty[String, Set[OverloadedFunc]],
  //       )
  //     else
  //       val st = Initialize.fromFile(cfg, filename)
  //       val map = PartialInterpreter(
  //         st = st,
  //         log = config.log,
  //         detail = config.detail,
  //       )
  //       val newCfg = (new CFGBuilder(
  //         Program(
  //           // this line is so bad
  //           st.cfg.program.funcs ::: (map
  //             .map(_._2.map { case OverloadedIRFunc(_, func) => func }.toList)
  //             .toList
  //             .flatten),
  //         ),
  //         false,
  //       )).result
  //       (
  //         Initialize.fromFile(cfg, filename),
  //         map.map((k, set) =>
  //           (
  //             k,
  //             set.map {
  //               case OverloadedIRFunc(cond, func) =>
  //                 OverloadedFunc(cond, func.name)
  //             },
  //           ),
  //         ),
  // //       )

  //   Interpreter(
  //     newCfg,
  //     log = config.log,
  //     detail = config.detail,
  //     overloads = overloads,
  //   )

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
    (
      "detail-log",
      BoolOption(c => { c.log = true; c.detail = true }),
      "turn on logging mode with detailed information.",
    ),
    (
      "peval",
      BoolOption(c => c.peval = true),
      "do partial evaluation just before execution.",
    ),
  )
  case class Config(
    // var timeLimit: Option[Int] = None,
    // var multiple: Boolean = false,
    var log: Boolean = false,
    var detail: Boolean = false,
    var peval: Boolean = false,
  )
}