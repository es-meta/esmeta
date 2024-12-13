package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.ir.{Func, Program}
import esmeta.ir.util.JsonProtocolWithBackEdge.{
  instanceCounter,
  exprDecoder,
  given,
}
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

val x = given_Encoder_Intrinsic
val y = given_Decoder_Directive

import esmeta.ir.Inst

// fast
// import com.github.plokhotnyuk.jsoniter_scala.macros._
// import com.github.plokhotnyuk.jsoniter_scala.core._
// import esmeta.ir.util.FastJsonProtocol.given
import esmeta.util.BaseUtils.*
import esmeta.ir.Expr
import esmeta.ir.EBinary

/** `dump` phase */
case object WebDump extends Phase[CFG, Unit] {
  val name = "dump"
  val help = "dump grammar"
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = {

    println(instanceCounter)

    dumpAndCheck("program")(cfg.program.asJson.spaces2)

    println(instanceCounter)

    // // dumpAndCheck("funcIndex") {
    // //   cfg.program.funcs.map(_.name)
    // // }

    // //   println("json form is " + i.asJson)
    // //   i
    // // }

    // for (func <- cfg.program.funcs) {
    //   dumpAndCheck(s"func/${func.name}")(func)
    // }

    // // dumpAndCheck("parser test") {
    // //   Inst
    // //     .from("""
    // //         let x = 10
    // //         """)
    // //     .asJson
    // // }

    dumpAndCheck("funcs")(cfg.program.funcs)
    dumpAndCheck("spec")(cfg.spec)
    dumpAndCheck("grammar")(cfg.spec.grammar)
    dumpAndCheck("tyModel")(cfg.spec.tyModel)
    dumpAndCheck("algorithms")(cfg.spec.algorithms)
    dumpAndCheck("spec.tables")(cfg.spec.tables)
    // dumpAndCheck("spec.version")(cfg.spec.version)

    // fastDumpAndCheck("funcs")(cfg.program.funcs)

    ()
  }

  private def dumpAndCheck[T: Encoder: Decoder](tag: String)(data: T): Unit = {

    print("dump [...]")
    val (elapsed, json) = time { data.asJson }
    dumpFile(json.spaces2, s"$DUMP_LOG_DIR/$tag.json")
    print("\rdumped [file], ...")

    val jsonString = json.toString

    print("\rdumped [file], [memory], ...")
    time { decode[T](jsonString) } match {
      case read -> Right(_) =>
        println(
          s"\b\b\b$tag is correct, write: $elapsed ms, read(parse): $read ms",
        )
      case _ -> Left(error) =>
        println(s"\b\b\b$tag is incorrect, error: $error")
    }
  }

  // private def fastDumpAndCheck[T: JsonValueCodec](
  //   tag: String,
  // )(data: T): Unit = {
  //   val (elapsed, jsonString) = time {
  //     writeToString(data)
  //   }
  //   dumpFile(jsonString, s"$DUMP_LOG_DIR/$tag.fast.json")
  //   (time { readFromString[T](jsonString) }) match {
  //     case (read, Right(_)) =>
  //       println(
  //         s"$tag.fast is correct, write: $elapsed ms, read(parse): $read ms",
  //       )
  //     case (_, Left(error)) =>
  //       println(s"$tag.fast is incorrect, error: (silent)")
  //   }
  // }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
