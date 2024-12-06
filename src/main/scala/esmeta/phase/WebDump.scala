package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.ir.{Program}
import esmeta.ir.util.JsonProtocol.given
import esmeta.spec.{Grammar}
import esmeta.spec.util.JsonProtocol.given
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.web.DumpData

import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*;
import io.circe.parser.decode;

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
    dumpFile(cfg.spec.asJson.spaces2, s"$DUMP_LOG_DIR/spec.json")
    dumpFile(cfg.spec.grammar.asJson.spaces2, s"$DUMP_LOG_DIR/grammar.json")

    val data =
      DumpData(cfg.program.asJson, cfg.spec.asJson, cfg.spec.grammar.asJson)

    data match
      case DumpData(program, spec, grammar) => {
        val parsedProg =
          decode[Program](program.toString)(given_Decoder_Program)
        if parsedProg.isRight then println("Program is correct")
        else println("Program is incorrect")
        // if (decode[Spec](spec.toString)(given_Decoder_Spec).isRight) then
        //   println("Spec is correct")
        // else println("Spec is incorrect")
        val parsedGrammar = decode[Grammar](grammar.toString)(
          given_Decoder_Grammar,
        );
        if parsedGrammar.isRight then println("Grammar is correct")
        else println("Grammar is incorrect")
      }

    data
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
