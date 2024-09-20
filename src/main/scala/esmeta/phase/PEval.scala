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
import esmeta.peval.{OverloadedFunc, PartialEvaluator, OverloadedIRFunc}

import esmeta.peval.*
import esmeta.peval.pstate.*
import scala.collection.mutable.{Map as MMap}
import esmeta.es.builtin.EXECUTION_STACK
import esmeta.ir.{Global, Name, Temp}

import scala.util.{Try}

/** `astirpeval` phase */
case object PEval extends Phase[CFG, Unit] {
  val name = "peval"
  val help = "partial-evaluated an ECMAScript file."

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    given CFG = cfg

    val filename = getFirstFilename(cmdConfig, name)
    val pevalTargetName = "FunctionDeclarationInstantiation"
    val pevalTarget = cfg.fnameMap(pevalTargetName)
    val ast =
      ESParser(cfg.spec.grammar, debug = false)("Script").fromFile(filename)
    val fds = getAstsbyName(ast, "FunctionDeclaration")
    println(s"Found ${fds.size} FunctionDeclaration.");

    for (fd <- fds) {

      val st1 = PState
        .fromState(Initialize.fromFile(cfg, filename))
        .setContext(pevalTarget.irFunc)
      val (addr_empty_map, st2) = st1.allocMap(Nil)
      val (addr_func_obj_record, st3) = st2.allocRecord(
        "ECMAScriptFunctionObject",
        List(
          "FormalParameters" -> Known(
            AstValue(
              getAstsbyName(fd, "FormalParameters").head,
            ),
          ),
          "ECMAScriptCode" -> Known(
            AstValue(getAstsbyName(fd, "FunctionBody").head),
          ),
          "ThisMode" -> Known(ENUM_STRICT),
          "Strict" -> Known(Bool(true)), // ESMeta is always strict
        ),
      )(using cfg)

      val (addr_lexical_env, st4) = st3.allocRecord(
        "FunctionEnvironmentRecord",
        List(
          "BindThisValue" -> CloFer("BindThisValue"),
          "CreateImmutableBinding" -> CloDecl(
            "CreateImmutableBinding",
          ),
          "CreateMutableBinding" -> CloDecl(
            "CreateMutableBinding",
          ),
          "DeleteBinding" -> CloDecl("DeleteBinding"),
          "FunctionObject" -> Known(addr_func_obj_record), //  #2043,
          "GetBindingValue" -> CloDecl("GetBindingValue"),
          "GetSuperBase" -> CloFer(
            "GetSuperBase",
          ), //  clo<Record[FunctionEnvironmentRecord].GetSuperBase>,
          "GetThisBinding" -> CloFer(
            "GetThisBinding",
          ), //  clo<Record[FunctionEnvironmentRecord].GetThisBinding>,
          "HasBinding" -> CloDecl(
            "HasBinding",
          ), //  clo<Record[DeclarativeEnvironmentRecord].HasBinding>,
          "HasSuperBinding" -> CloFer(
            "HasSuperBinding",
          ), //  clo<Record[FunctionEnvironmentRecord].HasSuperBinding>,
          "HasThisBinding" -> CloFer(
            "HasThisBinding",
          ), //  clo<Record[FunctionEnvironmentRecord].HasThisBinding>,
          "InitializeBinding" -> CloDecl(
            "InitializeBinding",
          ), //  clo<Record[DeclarativeEnvironmentRecord].InitializeBinding>,
          "NewTarget" -> Unknown, //  undefined,
          "OuterEnv" -> Unknown, // RuntimeValue,
          "SetMutableBinding" -> CloDecl(
            "SetMutableBinding",
          ), //  clo<Record[DeclarativeEnvironmentRecord].SetMutableBinding>,
          "ThisBindingStatus" -> Unknown, //  ~initialized~,
          "ThisValue" -> Unknown, //  undefined,
          "WithBaseObject" -> CloDecl(
            "WithBaseObject",
          ), //  clo<Record[DeclarativeEnvironmentRecord].WithBaseObject>,
          "__MAP__" -> Known(addr_empty_map), // some address,
        ),
      )(using cfg);

      // val (addr_exec_ctxt, st5) = st4.allocRecord(
      //   "ExecutionContext",
      //   List(
      //     "Function" -> Unknown,
      //     "Realm" -> Unknown,
      //     "ScriptOrModule" -> Unknown,
      //     "LexicalEnvironment" -> Known(addr_lexical_env),
      //     "VariableEnvironment" -> Unknown,
      //     "PrivateEnvironment" -> Unknown,
      //   ),
      // )(using cfg);

      // val (addr_exec_stck, st6) = st5.allocList(List(Known(addr_exec_ctxt)));
      // st6.define(Global(EXECUTION_STACK), Known(addr_exec_stck))
      st4.define(Name("func"), Known(addr_func_obj_record))
      st4.define(Name("argumentsList"), Unknown);
      val st7 = st4

      val st = st7
      println(s"Starting interpertaton from ${st.func.name}");
      Try {
        PartialEvaluator(
          initial = st,
          log = config.log,
          detail = config.detail,
        )
      }.map({
        case (inst, _) =>
          println(s"SUCCESSED EXECUTION");
          println(inst)
      }).recover({
        case (throwable) =>
          println(s"FAILED EXECUTION: ${throwable}");
          throwable.printStackTrace();
      }).map((_) => println("Omit printing state..."))
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
  )
  case class Config(
    // var timeLimit: Option[Int] = None,
    // var multiple: Boolean = false,
    var log: Boolean = false,
    var detail: Boolean = false,
    var peval: Boolean = false,
  )

  def getAstsbyName(ast: Ast, name: String): List[Ast] = ast match
    case l @ Lexical(n, str) if (n == name) => List(l)
    case s @ Syntactic(n, _, _, children) =>
      val fromChildren = children
        .map(
          _.map(getAstsbyName(_, name)).getOrElse(Nil),
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
