package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.error.*
import esmeta.es.*
import esmeta.interpreter.*
import esmeta.ir.Program
import esmeta.parser.{ESParser}
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.peval.{OverloadedFunc, PartialEvaluator, OverloadedIRFunc}

import esmeta.peval.*
import scala.collection.mutable.{Map as MMap}
import esmeta.es.builtin.EXECUTION_STACK
import esmeta.ir.{Global, Name, Temp}

import scala.util.{Try}

/** `astirpeval` phase */
case object PEval extends Phase[CFG, Unit] {
  val name = "peval"
  val help = "partial-evaluated an ECMAScript file."

  val TARGET_NAME = "FunctionDeclarationInstantiation"

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    given CFG = cfg

    if (config.simplify < 0) then
      throw new PEvalOptError("config.simplify should be non-negative")

    val filename = getFirstFilename(cmdConfig, name)
    val pevalTarget = cfg.fnameMap(TARGET_NAME)
    val ast =
      ESParser(cfg.spec.grammar, debug = false)("Script").fromFile(filename)
    val fds = getAstsByName(ast, "FunctionDeclaration")
    println(s"Found ${fds.size} FunctionDeclaration.");

    val init = new Initialize(cfg)
    val globals = for {
      (x, v) <- init.initGlobal
      pv = v match
        case _: Addr => Unknown
        case _       => Known(v)
    } yield x -> pv

    for (fd <- fds) {

      val renamer = Renamer()
      val peval = PartialEvaluator(
        cfg = cfg,
        log = config.log,
        detail = config.detail,
        simplifyLevel = config.simplify,
        renamer = renamer,
      )

      val thisCallCount = renamer.newCallCount
      val func = pevalTarget.irFunc
      val st = PState(
        globals = globals,
        callStack = Nil,
        context = PContext(
          func = func,
          sensitivity = thisCallCount,
          locals = MMap(),
          ret = None,
          pathCondition = Nil,
        ),
        heap = PHeap(),
      )

      val addr_func_obj_record = renamer.newAddr

      st.allocRecord(
        addr_func_obj_record,
        "ECMAScriptFunctionObject",
        List(
          "FormalParameters" -> Known(
            AstValue(getAstsByName(fd, "FormalParameters").head),
          ),
          "ECMAScriptCode" -> Known(
            AstValue(getAstsByName(fd, "FunctionBody").head),
          ),
          "ThisMode" -> Known(ENUM_STRICT),
          "Strict" -> Known(Bool(true)), // ESMeta is always strict
        ),
      )

      st.define(
        renamer.get(Name("func"), st.context),
        Known(addr_func_obj_record),
      )
      st.define(
        renamer.get(Name("func"), st.context),
        Unknown,
      );

      println(s"Starting interpertaton from ${func.name}");
      Try {
        peval.run(func, st)
      }
        .map({
          case (inst, _) =>
            println(s"SUCCESSED PEVAL")
        })
        .recover({
          case (throwable) =>
            println(s"FAILED PEVAL: ${throwable}");
            throwable.printStackTrace();
        })
        .map((_) => println("Omit printing state..."))
    }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
    ),
    (
      "detail-log",
      BoolOption((c, b) => { c.log ||= b; c.detail = b }),
      "turn on logging mode with detailed information.",
    ),
    (
      "peval",
      BoolOption(_.peval = _),
      "do partial evaluation just before execution.",
    ),
    (
      "s",
      NumOption(_.simplify = _),
      "set level of simplify strategy. (0: do nothing, 1 (default): flatten, 2: flatten & usedef)",
    ),
  )
  case class Config(
    // var timeLimit: Option[Int] = None,
    // var multiple: Boolean = false,
    var log: Boolean = false,
    var detail: Boolean = false,
    var peval: Boolean = false,
    var simplify: Int = 1,
  )

  def getAstsByName(ast: Ast, name: String): List[Ast] = ast match
    case l @ Lexical(n, str) if (n == name) => List(l)
    case s @ Syntactic(n, _, _, children) =>
      val fromChildren = children
        .map(
          _.map(getAstsByName(_, name)).getOrElse(Nil),
        )
        .flatten
      val fromS = if (n == name) then List(s) else Nil
      fromS ::: fromChildren
    case _ => Nil

  def CloFer(name: String)(using cfg: CFG): Known[Clo] =
    Known(
      Clo(cfg.fnameMap(s"Record[FunctionEnvironmentRecord].$name"), Map.empty),
    )
  def CloDecl(name: String)(using cfg: CFG): Known[Clo] = Known(
    Clo(
      cfg.fnameMap(s"Record[DeclarativeEnvironmentRecord].$name"),
      Map.empty,
    ),
  )
}
