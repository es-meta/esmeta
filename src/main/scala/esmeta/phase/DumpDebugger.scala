package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.cfg.util.{JsonProtocol as CFGJsonProtocol}
import esmeta.ir.*
import esmeta.ir.util.JsonProtocol.given
import esmeta.ir.util.{UnitWalker as IRUnitWalker}
import esmeta.spec.{Algorithm, Grammar, Spec, Table}
import esmeta.spec.util.JsonProtocol.given
import esmeta.lang.Type
import esmeta.lang.util.JsonProtocol.given
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*;
import io.circe.parser.decode;
import scala.util.Try

/** `dump-debugger` phase */
case object DumpDebugger extends Phase[CFG, Unit] {
  val name = "dump-debugger"
  val help = "dump resources for standalone debugger"
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    dumpAndCheck("program")(cfg.program.asJson.spaces2)
    dumpAndCheck("funcs")(cfg.program.funcs)
    dumpAndCheck("grammar")(cfg.spec.grammar)
    dumpAndCheck("tyModel.decls")(cfg.spec.tyModel)
    dumpAndCheck("spec.tables")(cfg.spec.tables)
    dumpAndCheck("spec.version")(cfg.spec.version)
    dumpAndCheck("irFuncToCode")(
      cfg.asJson(using CFGJsonProtocol(cfg).irFuncToCode),
    )
    dumpAndCheck("irToSpecNameMap")(
      cfg.asJson(using CFGJsonProtocol(cfg).irToSpecNameMapEncoder),
    )
  end apply

  private def dumpAndCheck[T: Encoder: Decoder](
    tag: String,
  )(data: T): Try[T] = Try {
    val (elapsed, json) = time { data.asJson }
    dumpFile(json.noSpaces, s"$DUMP_DEBUGGER_LOG_DIR/$tag.json")
    val jsonString = json.toString

    time { decode[T](jsonString) } match {
      case read -> Right(t) =>
        println(
          s"\b\b\b[O] $tag, write: $elapsed ms, read(parse): $read ms",
        )
        t
      case _ -> Left(error) =>
        println(s"\b\b\b[X] $tag,\nerror: $error")
        throw error
    }
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
