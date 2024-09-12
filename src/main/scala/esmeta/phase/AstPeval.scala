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

import esmeta.peval.pstate.*
import scala.collection.mutable.{Map as MMap}
import esmeta.es.builtin.EXECUTION_STACK
import esmeta.ir.{Global, Name, Temp}

import scala.util.{Try}

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
    val ast =
      ESParser(cfg.spec.grammar, debug = false)("Script").fromFile(filename)
    val fds = getAstsbyName(ast, "FunctionDeclaration")
    println(s"Found ${fds.size} FunctionDeclaration.");

    for (fd <- fds) {

      val st = PState.fromState(Initialize.fromFile(cfg, filename))
      st.context = PContext(func = pevalTarget)

      val addr_empty_map = st.heap.allocMap(Nil)

      val addr_func_obj_record = st.heap.allocRecord(
        "ECMAScriptFunctionObject",
        List(
          "FormalParameters" -> AstValue(
            getAstsbyName(fd, "FormalParameters").head,
          ),
          "ECMAScriptCode" -> AstValue(getAstsbyName(fd, "FunctionBody").head),
          "ThisMode" -> ENUM_STRICT,
          "Strict" -> Bool(true), // ESMeta is always strict
        ),
      )(using cfg)

      def CloFer(name: String) : Clo = Clo(cfg.fnameMap(s"Record[FunctionEnvironmentRecord].$name"), Map.empty)
      def CloDecl(name: String) : Clo = Clo(cfg.fnameMap(s"Record[DeclarativeEnvironmentRecord].$name"), Map.empty)

      val addr_lexical_env = st.heap.allocRecord(
        "FunctionEnvironmentRecord",
        List(
          "BindThisValue" -> CloFer("BindThisValue"), 
          "CreateImmutableBinding" -> CloDecl("CreateImmutableBinding"),
          "CreateMutableBinding" -> CloDecl("CreateMutableBinding"),
          "DeleteBinding" -> CloDecl("DeleteBinding"),
          "FunctionObject" -> addr_func_obj_record, //  #2043,
          "GetBindingValue" -> CloDecl("GetBindingValue"),
          "GetSuperBase" -> CloFer("GetSuperBase"), //  clo<Record[FunctionEnvironmentRecord].GetSuperBase>,
          "GetThisBinding" -> CloFer("GetThisBinding"), //  clo<Record[FunctionEnvironmentRecord].GetThisBinding>,
          "HasBinding" -> CloDecl("HasBinding"), //  clo<Record[DeclarativeEnvironmentRecord].HasBinding>,
          "HasSuperBinding" -> CloFer("HasSuperBinding"), //  clo<Record[FunctionEnvironmentRecord].HasSuperBinding>,
          "HasThisBinding" -> CloFer("HasThisBinding"), //  clo<Record[FunctionEnvironmentRecord].HasThisBinding>,
          "InitializeBinding" -> CloDecl("InitializeBinding"), //  clo<Record[DeclarativeEnvironmentRecord].InitializeBinding>,
          "NewTarget" -> RuntimeValue, //  undefined,
          "OuterEnv" -> RuntimeValue, // RuntimeValue,
          "SetMutableBinding" -> CloDecl("SetMutableBinding"), //  clo<Record[DeclarativeEnvironmentRecord].SetMutableBinding>,
          "ThisBindingStatus" -> RuntimeValue, //  ~initialized~,
          "ThisValue" -> RuntimeValue, //  undefined,
          "WithBaseObject" -> CloDecl("WithBaseObject"), //  clo<Record[DeclarativeEnvironmentRecord].WithBaseObject>,
          "__MAP__" -> addr_empty_map, // some address,
        ),
      )(using cfg);

      val addr_exec_ctxt = st.heap.allocRecord(
        "ExecutionContext",
        List(
          "Function" -> RuntimeValue,
          "Realm" -> RuntimeValue,
          "ScriptOrModule" -> RuntimeValue,
          "LexicalEnvironment" -> addr_lexical_env,
          "VariableEnvironment" -> RuntimeValue,
          "PrivateEnvironment" -> RuntimeValue,
        ),
      )(using cfg);

      val addr_exec_stck = st.heap.allocList(List(addr_exec_ctxt));
      st.globals += (Global(EXECUTION_STACK) -> addr_exec_stck)

      st.context.locals += Name("func") -> addr_func_obj_record
      st.context.locals += Name("argumentsList") -> RuntimeValue // f(10) then ListObj(10).

      println(s"Starting interpertaton from ${st.context}");
      Try {
        PartialInterpreter(
          st = st,
          log = config.log,
          detail = config.detail,
        )
      }.map({
        case sst =>
          println(s"SUCCESSED EXECUTION");
          sst
      }).recover({
        case (throwable) =>
          println(s"FAILED EXECUTION: $throwable");
          st
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
}
