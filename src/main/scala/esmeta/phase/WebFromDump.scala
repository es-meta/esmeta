package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.web.WebServer

import esmeta.ir.*
import esmeta.ir.util.JsonProtocol.given
import esmeta.spec.*
import esmeta.spec.util.JsonProtocol.given
import esmeta.lang.*
import esmeta.lang.util.JsonProtocol.given
import esmeta.ty.*
import esmeta.ty.util.JsonProtocol.{given_Decoder_TyModel, given}

import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import io.circe.parser.decode
import esmeta.cfgBuilder.CFGBuilder

/** `web` phase */
case object WebFromDump extends Phase[Unit, Unit] {
  val name = "web-from-dump"
  val help = "starts a web server for an ECMAScript double debugger."
  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =

    val version = decode[Spec.Version](
      readFile(
        s"$DUMP_LOG_DIR/spec.version.json",
      ),
    ).getOrElse(???)
    val grammar = decode[Grammar](
      readFile(
        s"$DUMP_LOG_DIR/grammar.json",
      ),
    ).getOrElse(???)
    val algo = decode[List[Algorithm]](
      readFile(
        s"$DUMP_LOG_DIR/algorithms.json",
      ),
    ).getOrElse(???)
    val tables = decode[Map[String, Table]](
      readFile(
        s"$DUMP_LOG_DIR/spec.tables.json",
      ),
    ).getOrElse(???)
    val tyModel = decode[TyModel](
      readFile(
        s"$DUMP_LOG_DIR/tyModel.json",
      ),
    )(using given_Decoder_TyModel) match
      case Left(value)  => println(value); ???
      case Right(value) => value

    val spec = Spec(Some(version), grammar, algo, tables, tyModel)

    val funcs =
      decode[List[Func]](
        readFile(
          s"$DUMP_LOG_DIR/funcs.json",
        ),
      ).getOrElse(???)
    val program = Program(funcs = funcs, spec = spec)
    val newcfg = CFGBuilder(program)

    WebServer(newcfg, config.port).run

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "port",
      NumOption((c, i) => c.port = i),
      "web server port (default: 8080).",
    ),
  )
  case class Config(var port: Int = 8080)
}
