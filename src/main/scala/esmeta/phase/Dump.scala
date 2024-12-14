package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.ir.{Func, Program}
import esmeta.ir.util.JsonProtocol.given
import esmeta.ir.util.{UnitWalker as IRUnitWalker}
import esmeta.spec.{Algorithm, Grammar, Spec, Table}
import esmeta.spec.util.JsonProtocol.given
import esmeta.ty.*
import esmeta.ty.util.JsonProtocol.{given}
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
import esmeta.lang.Syntax
import esmeta.ir.util.JsonProtocol
import esmeta.ir.AllocExpr
import esmeta.ir.Name

/** `dump` phase */
case object Dump extends Phase[CFG, Unit] {
  val name = "dump"
  val help = "dump grammar"
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = {
    dumpAndCheck("program")(cfg.program.asJson.spaces2)
    val newFuncs = dumpAndCheck("funcs")(cfg.program.funcs).get

    print("check locs ...")
    for {
      f <- cfg.program.funcs
      newF <- newFuncs.find(_.name == f.name)
    } do {
      class LocChecker extends IRUnitWalker {
        var list = List.empty[Option[Loc]]
        override def walk(inst: Inst): Unit = {
          list :+ inst.langOpt.flatMap(_.loc)
          super.walk(inst)
        }
        override def walk(expr: Expr): Unit = {
          list :+ expr.langOpt.flatMap(_.loc)
          super.walk(expr)
        }
      }
      val x = LocChecker()
      val y = LocChecker()
      x.walk(f)
      y.walk(newF)
      assert(x.list.length == y.list.length)
      x.list.zip(y.list).foreach {
        case a -> b =>
          assert(a == b)
      }

      class IllegalFinder extends IRUnitWalker {

        override def walk(var1: Name): Unit = {
          assert(var1.name != "false")
        }
      }
    }
    print("check success")

    dumpAndCheck("spec")(cfg.spec)
    dumpAndCheck("grammar")(cfg.spec.grammar)
    dumpAndCheck("tyModel")(cfg.spec.tyModel)
    dumpAndCheck("algorithms")(cfg.spec.algorithms)
    dumpAndCheck("spec.tables")(cfg.spec.tables)
    dumpAndCheck("spec.version")(cfg.spec.version)

    ()
  }

  private def dumpAndCheck[T: Encoder: Decoder](
    tag: String,
  )(data: T): Option[T] = {

    print("dump ...")
    val (elapsed, json) = time { data.asJson }
    dumpFile(json.spaces2, s"$DUMP_LOG_DIR/$tag.json")
    print("\rdump f...")

    val jsonString = json.toString

    print("\rdump fm...")
    time { decode[T](jsonString) } match {
      case read -> Right(t) =>
        println(
          s"\b\b\b[O] $tag, write: $elapsed ms, read(parse): $read ms",
        )
        Some(t)
      case _ -> Left(error) =>
        println(s"\b\b\b[X] $tag,\nerror: $error")
        None
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
