package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.ir.{Program}
import esmeta.ir.util.JsonProtocol.given
import esmeta.spec.{Grammar, Spec}
import esmeta.spec.util.JsonProtocol.given
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.web.DumpData

import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*;
import io.circe.parser.decode;

import scala.util.chaining.*
import scala.util.ChainingOps.*
import esmeta.util.ManualInfo.tyModel
import esmeta.spec.Table
import esmeta.ty.TyModel
import esmeta.spec.Algorithm

/** `dump` phase */
case object WebDump extends Phase[CFG, DumpData] {
  val name = "dump"
  val help = "dump grammar"
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): DumpData = {
    dumpFile(cfg.program.asJson.spaces2, s"$DUMP_LOG_DIR/program.dump")
    dumpFile(cfg.program.funcs.asJson.spaces2, s"$DUMP_LOG_DIR/funcs.json")
    dumpFile(cfg.spec.asJson.spaces2, s"$DUMP_LOG_DIR/spec.json")
    dumpFile(cfg.spec.grammar.asJson.spaces2, s"$DUMP_LOG_DIR/grammar.json")

    // //////// just test only ////////////////////

    checkRight("Spec.Version") {
      decode[Spec.Version](cfg.spec.version.asJson.toString)
    }

    checkRight("Spec.Tables") {
      decode[Map[String, Table]](cfg.spec.tables.asJson.toString)
    }

    checkRight("TyModel") {
      decode[TyModel](cfg.spec.tyModel.asJson.toString)
    }

    checkRight("Algorithms") {
      decode[List[Algorithm]](cfg.spec.algorithms.asJson.toString)
    }

    // ////////////////////////////////////////////

    val data =
      DumpData(
        program = cfg.program.asJson,
        funcs = cfg.program.funcs.asJson,
        spec = cfg.spec.asJson,
      )

    data.tap {

      case DumpData(program, funcs, spec) => {

        checkRight("Spec") {
          decode[Spec](spec.toString)
        }

        checkRight("Program") {
          decode[Program](program.toString)
        }

        checkRight("Funcs") {
          decode[Program](funcs.toString)
        }

        // checkRight("Grammar") {
        //   decode[Grammar](grammar.toString)
        // }

      }

    }
  }

  private def checkRight[T](tag: String)(result: Either[Error, T]): Unit = {
    result match {
      case Right(_)    => println(s"$tag is correct")
      case Left(error) => println(s"$tag is incorrect: $error")
    }
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
