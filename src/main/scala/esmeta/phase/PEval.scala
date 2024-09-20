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

  val TARGET_NAME = "FunctionDeclarationInstantiation"

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    given CFG = cfg

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

    val peval = PartialEvaluator(
      cfg = cfg,
      log = config.log,
      detail = config.detail,
    )

    for (fd <- fds) {

      val st = PState(globals = globals)
      // .fromState(Initialize.fromFile(cfg, filename))
      // .setContext(pevalTarget.irFunc)
      // val (addr_empty_map, st) = st.allocMap(Nil)
      val func = pevalTarget.irFunc
      val addr_func_obj_record = st.allocRecord(
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

      // val (addr_lexical_env, st) = st.allocRecord(
      //   "FunctionEnvironmentRecord",
      //   List(
      //     "BindThisValue" -> CloFer("BindThisValue"),
      //     "CreateImmutableBinding" -> CloDecl(
      //       "CreateImmutableBinding",
      //     ),
      //     "CreateMutableBinding" -> CloDecl(
      //       "CreateMutableBinding",
      //     ),
      //     "DeleteBinding" -> CloDecl("DeleteBinding"),
      //     "FunctionObject" -> Known(addr_func_obj_record), //  #2043,
      //     "GetBindingValue" -> CloDecl("GetBindingValue"),
      //     "GetSuperBase" -> CloFer(
      //       "GetSuperBase",
      //     ), //  clo<Record[FunctionEnvironmentRecord].GetSuperBase>,
      //     "GetThisBinding" -> CloFer(
      //       "GetThisBinding",
      //     ), //  clo<Record[FunctionEnvironmentRecord].GetThisBinding>,
      //     "HasBinding" -> CloDecl(
      //       "HasBinding",
      //     ), //  clo<Record[DeclarativeEnvironmentRecord].HasBinding>,
      //     "HasSuperBinding" -> CloFer(
      //       "HasSuperBinding",
      //     ), //  clo<Record[FunctionEnvironmentRecord].HasSuperBinding>,
      //     "HasThisBinding" -> CloFer(
      //       "HasThisBinding",
      //     ), //  clo<Record[FunctionEnvironmentRecord].HasThisBinding>,
      //     "InitializeBinding" -> CloDecl(
      //       "InitializeBinding",
      //     ), //  clo<Record[DeclarativeEnvironmentRecord].InitializeBinding>,
      //     "NewTarget" -> Unknown, //  undefined,
      //     "OuterEnv" -> Unknown, // RuntimeValue,
      //     "SetMutableBinding" -> CloDecl(
      //       "SetMutableBinding",
      //     ), //  clo<Record[DeclarativeEnvironmentRecord].SetMutableBinding>,
      //     "ThisBindingStatus" -> Unknown, //  ~initialized~,
      //     "ThisValue" -> Unknown, //  undefined,
      //     "WithBaseObject" -> CloDecl(
      //       "WithBaseObject",
      //     ), //  clo<Record[DeclarativeEnvironmentRecord].WithBaseObject>,
      //     "__MAP__" -> Known(addr_empty_map), // some address,
      //   ),
      // )(using cfg);

      // val (addr_exec_ctxt, st) = st.allocRecord(
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

      // val (addr_exec_stck, st) = st.allocList(List(Known(addr_exec_ctxt)));
      // st.define(Global(EXECUTION_STACK), Known(addr_exec_stck))
      st.define(Name("func"), Known(addr_func_obj_record))
      st.define(Name("argumentsList"), Unknown);
      println(s"Starting interpertaton from ${func.name}");
      Try {
        peval.run(func.body, st)
      }
        .map({
          case (inst, _) =>
            println(s"SUCCESSED EXECUTION");
            println(inst)
        })
        .recover({
          case (throwable) =>
            println(s"FAILED EXECUTION: ${throwable}");
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
  )
  case class Config(
    // var timeLimit: Option[Int] = None,
    // var multiple: Boolean = false,
    var log: Boolean = false,
    var detail: Boolean = false,
    var peval: Boolean = false,
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
