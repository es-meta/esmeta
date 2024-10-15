package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.error.*
import esmeta.es.*
import esmeta.es.builtin.EXECUTION_STACK
import esmeta.interpreter.*
import esmeta.ir.{Func, Program, Inst, Param, Global, Name, Temp}
import esmeta.parser.{ESParser}
import esmeta.peval.*
import esmeta.peval.util.*
import esmeta.peval.pstate.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*

import scala.collection.mutable.{Map as MMap}
import scala.util.{Try, Success, Failure}

/** `peval` phase */
case object PEval extends Phase[Program, Program] {
  val name = "peval"
  val help = "partial-evaluate a pre-defined target using an ECMAScript file."

  val TARGET_NAME = FUNC_DECL_INST

  def apply(
    program: Program,
    cmdConfig: CommandConfig,
    config: Config,
  ): Program = {

    if (config.simplify < 0 || config.simplify > 3) then
      throw new PEvalOptError("config.simplify should be in [0, 3]")

    val filename = getFirstFilename(cmdConfig, name)
    val target = program.funcs
      .find(_.name == TARGET_NAME)
      .getOrElse(throw PEvalOptError(s"peval target ${TARGET_NAME} not found"))
    val ast = ESParser(program.spec.grammar)("Script").fromFile(filename)
    val fds = getAllChildrenByName(ast, FUNC_DECL)

    /* NOTE: globals are not modified, so we can use the same globals for all overloads
    val init = new Initialize(cfg)
    val globals = for {
      (x, v) <- init.initGlobal
      pv = v match
        case _: Addr => Unknown
        case _       => Known(v)
    } yield x -> pv */

    val overloads = fds.zipWithIndex.flatMap((fd, idx) =>
      runPEvalWithKnownAst(target, fd, config, program) match

        case Failure(exception) =>
          print("Failed to run PEval: ")
          exception.printStackTrace();
          None

        case Success((newBody, newParams)) =>
          val newFunc = Func(
            target.main,
            target.kind,
            s"${target.name}PEvaled${idx}",
            newParams,
            target.retTy,
            newBody,
            NoOverloads,
            target.algo,
          )
          Some((newFunc, fd)),
    )

    if (config.log) then
      val pw = getPrintWriter(s"$PEVAL_LOG_DIR/summary")
      pw.println(s"Found ${fds.length} function declarations in ${filename}");
      pw.println(s"Generated ${overloads.length} overloads (should be 100%)");
      pw.flush
      dumpTo(PEVAL_LOG_DIR, overloads.map(_._1));

    overloadedProgram(program, overloads)
  }

  private def overloadedProgram(
    program: Program,
    overloads: List[(Func, Ast)],
  ): Program = {
    Program(
      List.from(overloads.map(_._1)) ::: program.funcs.map(f =>
        if (f.name == TARGET_NAME) then {
          val go =
            GetOverloads((args: Iterable[Value], st: State) =>
              for {
                addr <- args.headOption.flatMap {
                  case addr: Addr => Some(addr)
                  case _          => None
                }
                record <- st(addr) match
                  case r @ RecordObj(_, _) => Some(r)
                  case _                   => None
                asts <- record
                  .get(Str(FORMAL_PARAMS))
                  .zip(record.get(Str(ECMASCRIPT_CODE)))
                (formalParams, ecmaScriptCode) = asts
                f <- overloads
                  .find { // TODO : optimize finding matching overloads
                    case (func, decl) => {
                      lazy val formalParamsOfDecl =
                        getAllChildrenByName(decl, FORMAL_PARAMS).headOption
                          .map(AstValue.apply)
                      lazy val ecmaScriptCodeOfDecl =
                        getAllChildrenByName(decl, FUNC_BODY).headOption
                          .map(AstValue.apply)
                      formalParamsOfDecl == Some(formalParams) &&
                      ecmaScriptCodeOfDecl == Some(ecmaScriptCode)
                    }
                  }
                  .map(_._1)
                fname = f.name
              } yield fname,
            )
          withOverloadGetter(f, go)
        } else f,
      ),
      program.spec,
    )
  }

  private def runPEvalWithKnownAst(
    funcDeclInst: Func,
    funcDecls: Ast,
    config: Config,
    program: Program,
  ): Try[(Inst, List[Param])] = {

    val renamer = Renamer()
    val peval = PartialEvaluator(
      program = program,
      log = config.log,
      detail = config.detail,
      simplifyLevel = config.simplify,
      renamer = renamer,
    )

    val thisCallCount = renamer.newCallCount
    val func = funcDeclInst
    val st = PState(
      globals = Map.empty, // temp fix // globals,
      callStack = Nil,
      context = PContext(
        func = func,
        sensitivity = thisCallCount,
        locals = MMap(),
        ret = None,
      ),
      heap = PHeap(),
    )

    val addr_func_obj_record = renamer.newAddr

    st.allocRecord(
      addr_func_obj_record,
      "ECMAScriptFunctionObject",
      List(
        FORMAL_PARAMS -> Known(
          AstValue(getChildByName(funcDecls, FORMAL_PARAMS)),
        ),
        ECMASCRIPT_CODE -> Known(
          AstValue(getChildByName(funcDecls, FUNC_BODY)),
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
      renamer.get(Name("argumentsList"), st.context),
      Unknown,
    );

    // println(s"Starting interpertaton from ${func.name}");
    Try { peval.run(func, st) }.map(x => (x._1, x._2))
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
      "s",
      NumOption(_.simplify = _),
      """set level of simplify strategy.
  0: do nothing
  1 : flatten
  2 (default): flatten & syntactic optimization (reference transparency)
  3: flatten & syntactic & semantic optimization (use-def chain)""",
    ),
  )
  case class Config(
    // var timeLimit: Option[Int] = None,
    // var multiple: Boolean = false,
    var log: Boolean = false,
    var detail: Boolean = false,
    var simplify: Int = 1,
  )

  private def withOverloadGetter(
    func: Func,
    getOverloads: GetOverloads,
  ): Func = Func(
    func.main,
    func.kind,
    func.name,
    func.params,
    func.retTy,
    func.body,
    getOverloads,
    func.algo,
  )

  val (getChildByName, getAllChildrenByName)
    : ((Ast, String) => Ast, (Ast, String) => List[Ast]) =
    def aux(ast: Ast, name: String): List[Ast] = ast match
      case l @ Lexical(n, str) if (n == name) => List(l)
      case s @ Syntactic(n, _, _, children) =>
        val fromChildren = children
          .map(
            _.map(aux(_, name)).getOrElse(Nil),
          )
          .flatten
        val fromS = if (n == name) then List(s) else Nil
        fromS ::: fromChildren
      case _ => Nil
    (
      (ast: Ast, name: String) =>
        (aux(ast, name).headOption
          .getOrElse(throwPeval"cannot find child named ${name}")),
      aux(_, _),
    )

  private def CloFer(name: String)(using cfg: CFG): Known[Clo] =
    Known(
      Clo(cfg.fnameMap(s"Record[FunctionEnvironmentRecord].$name"), Map.empty),
    )
  private def CloDecl(name: String)(using cfg: CFG): Known[Clo] = Known(
    Clo(
      cfg.fnameMap(s"Record[DeclarativeEnvironmentRecord].$name"),
      Map.empty,
    ),
  )

  /** dump CFG */
  def dumpTo(baseDir: String, funcs: List[Func]): Unit =
    val dirname = s"$baseDir/overloads"
    dumpDir(
      name = "p-evaled functions",
      iterable = ProgressBar("Dump p-evaled functions", funcs, detail = false),
      dirname = dirname,
      getName = func => s"${func.name}.ir",
    )

  lazy val FORMAL_PARAMS = "FormalParameters"
  lazy val ECMASCRIPT_CODE = "ECMAScriptCode"
  lazy val FUNC_DECL = "FunctionDeclaration"
  lazy val FUNC_DECL_INST = "FunctionDeclarationInstantiation"
  lazy val FUNC_BODY = "FunctionBody"
}
