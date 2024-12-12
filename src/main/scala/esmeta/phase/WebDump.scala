package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.ir.{Func, Program}
import esmeta.ir.util.JsonProtocolWithBackEdge.given
import esmeta.spec.{Algorithm, Grammar, Spec, Table}
import esmeta.spec.util.JsonProtocol.given
import esmeta.ty.TyModel
import esmeta.lang.Type
import esmeta.lang.util.JsonProtocol.given
import esmeta.util.*
import esmeta.util.SystemUtils.*

import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*;
import io.circe.parser.decode;

import scala.util.chaining.*
import scala.util.ChainingOps.*

/** `dump` phase */
case object WebDump extends Phase[CFG, Unit] {
  val name = "dump"
  val help = "dump grammar"
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = {
    dumpAndCheck("program")(cfg.program.asJson.spaces2)
    dumpAndCheck("funcs")(cfg.program.funcs)
    dumpAndCheck("spec")(cfg.spec)
    dumpAndCheck("grammar")(cfg.spec.grammar)
    dumpAndCheck("tyModel")(cfg.spec.tyModel)
    dumpAndCheck("algorithms")(cfg.spec.algorithms)
    dumpAndCheck("spec.tables")(cfg.spec.tables)
    dumpAndCheck("spec.version")(cfg.spec.version)

    ()
  }

  private def dumpAndCheck[T: Encoder: Decoder](tag: String)(data: T): Unit = {
    val json = data.asJson
    dumpFile(json.spaces2, s"$DUMP_LOG_DIR/$tag.json")
    decode[T](json.toString) match {
      case Right(_)    => println(s"$tag is correct")
      case Left(error) => println(s"$tag is incorrect, error: $error")
    }
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
