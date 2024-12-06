package esmeta.phase

import esmeta.*
// import esmeta.ir.Program
// import esmeta.compiler.Compiler
import esmeta.spec.Spec
import esmeta.util.*
import esmeta.util.SystemUtils.*

import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*;

import scala.io.Source
// import cats.effect.IO
// import esmeta.spec.util.JsonProtocol.{given_Decoder_Spec};
import esmeta.spec.util.JsonProtocol.{given_Decoder_Grammar};
import esmeta.parser.ESParser
import esmeta.spec.Grammar

import io.circe.parser.decode

/** `dump...` phase */
case object DumpTest extends Phase[Unit, Unit] {
  val name = "dump-test"
  val help = "dump test"
  def apply(
    x: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = {
    // XXX no test for now
    x
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}