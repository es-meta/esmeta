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

// TODO sort imports
import esmeta.spec.Spec
import esmeta.peval.Unknown.get
import scala.collection.immutable.HashMap

/** `peval` phase */
case object PEval extends Phase[Program, Program] {
  val name = "peval"
  val help = "partial-evaluate a pre-defined target AO using an ES file."

  val TARGET_NAME = FUNC_DECL_INSTANT

  def apply(
    program: Program,
    cmdConfig: CommandConfig,
    config: Config,
  ): Program = {

    if (config.simplify < 0 || config.simplify > 3) then
      throw new PEvalOptError("config.simplify should be in [0, 3]")

    val target = program.funcs
      .find(_.name == TARGET_NAME)
      .getOrElse(
        throw PEvalOptError(s"peval target ${TARGET_NAME} not found in Program"),
      )

    val filename = getFirstFilename(cmdConfig, name)
    val fds = {
      val ast = program.spec.scriptParser.fromFile(filename)
      AstHelper.getAllChildrenByName(ast, FUNC_DECL)
    }

    /* NOTE: globals are not modified, so we can use the same globals for all overloads
    val init = new Initialize(cfg)
    val globals = for {
      (x, v) <- init.initGlobal
      pv = v match
        case _: Addr => Unknown
        case _       => Known(v)
    } yield x -> pv */

    val overloads = fds.zipWithIndex.flatMap((fd, idx) =>

      val (renamer, pst) = PartialEvaluator.prepareForFDI(target, fd);

      val peval = PartialEvaluator(
        program = program,
        log = config.log,
        detail = config.detail,
        simplifyLevel = config.simplify,
        renamer = renamer,
      )

      val pevalResult = Try(
        peval.run(
          target,
          pst,
          Some(s"${target.name}${idx}"),
        ),
      ).map(_._1)

      pevalResult match
        case Success(newFunc) => Some((newFunc, fd))
        case Failure(exception) =>
          print("Failed to run PEval: ")
          exception.printStackTrace();
          None,
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
    val funcs = program.funcs.map {
      case f if f.name != TARGET_NAME => f
      case f                          =>
        // TODO : optimize finding matching overloads
        val overloadsMap = {
          val astOfOverloads = overloads.map {
            case (func, decl) => {
              val formalParamsOfDecl =
                AstHelper
                  .getAllChildrenByName(decl, FORMAL_PARAMS)
                  .headOption
                  .map(AstValue.apply)
                  .getOrElse(throwPeval"formalParams not found")
              val ecmaScriptCodeOfDecl =
                AstHelper
                  .getAllChildrenByName(decl, FUNC_BODY)
                  .headOption
                  .map(AstValue.apply)
                  .getOrElse(throwPeval"ecmaScriptCode not found")
              (func, formalParamsOfDecl, ecmaScriptCodeOfDecl)
            }
          }
          HashMap.from(astOfOverloads.map {
            case (ol, fpOfDecl, escOfDecl) => (fpOfDecl, escOfDecl) -> ol.name
          })
        }

        val go =
          (args: Iterable[Value], st: State) =>
            for {
              addr <- args.headOption.flatMap {
                case addr: Addr => Some(addr)
                case _          => None
              }
              record <- st(addr) match
                case r: RecordObj => Some(r)
                case _            => None
              asts <- record
                .get(Str(FORMAL_PARAMS))
                .zip(record.get(Str(ECMASCRIPT_CODE)))
                .flatMap {
                  case (v1: AstValue, v2: AstValue) => Some((v1, v2))
                  case _                            => None
                }
              fname <- overloadsMap.get(asts)
            } yield fname
        Func(
          f.main,
          f.kind,
          f.name,
          f.params,
          f.retTy,
          f.body,
          GetOverloads(go),
          f.algo,
        )
    }
    Program(
      List.from(overloads.map(_._1)) ::: funcs,
      program.spec,
    )
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

  /** dump CFG */
  def dumpTo(baseDir: String, funcs: List[Func]): Unit =
    val dirname = s"$baseDir/overloads"
    dumpDir(
      name = "p-evaled functions",
      iterable = ProgressBar("Dump p-evaled functions", funcs, detail = false),
      dirname = dirname,
      getName = func => s"${func.name}.ir",
    )
}
