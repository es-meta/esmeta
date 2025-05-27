package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.dump.debugger.util.{
  JsonProtocol as DumpJsonProtocol,
  SerializationSanitizer,
}
import esmeta.ir.{Type as IRType, *}
import esmeta.ir.util.{UnitWalker as IRUnitWalker, Walker as IRWalker}
import esmeta.spec.{Algorithm, Grammar, Spec, Table}
import esmeta.lang.{Type as LangType}
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.web.util.JsonProtocol as WebJsonProtocol;
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*;
import io.circe.parser.decode;
import scala.util.Try
import scala.util.chaining.*

/** `dump-debugger` phase */
case object DumpDebugger extends Phase[CFG, Unit] {
  val name = "dump-debugger"
  val help =
    "dumps the resources required by the standalone debugger. (for internal use)"
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = { dumpStatic(cfg); dumpDynamic(cfg) }

  private def dumpDynamic(cfg: CFG): Unit = {
    dump("funcs.cfg")(cfg)(using WebJsonProtocol(cfg).cfgToFuncEncoder)
  }

  private def dumpStatic(cfg: CFG): Unit = {
    import DumpJsonProtocol.given

    dumpThenRead("funcs")(cfg.program.funcs) tap { _funcsOpt =>
      val funcs = _funcsOpt.getOrElse(Nil)
      println(s"[O] funcs size: ${funcs.size}")
      val serialized = cfg.program.funcs.map(SerializationSanitizer.walk)
      check("funcs.name") { funcs.map(_.name) == serialized.map(_.name) }
      check("funcs.kind") { funcs.map(_.kind) == serialized.map(_.kind) }
      check("funcs.main") { funcs.map(_.main) == serialized.map(_.main) }
      check("funcs.params") { funcs.map(_.params) == serialized.map(_.params) }
      check("funcs.retTy") { funcs.map(_.retTy) == serialized.map(_.retTy) }
      check("funcs.body") { funcs.map(_.body) == serialized.map(_.body) }
    }

    dumpThenRead("grammar")(cfg.spec.grammar) tap { grammar =>
      check("grammar")(grammar == Some(cfg.spec.grammar))
    }

    dumpThenRead("tyModel.decls")(cfg.spec.tyModel) tap { tm =>
      check("tyModel.decls")(tm == Some(cfg.spec.tyModel))
    }

    dumpThenRead("spec.tables")(cfg.spec.tables) tap { tables =>
      check("spec.tables")(tables == Some(cfg.spec.tables))
    }

    dumpThenRead("spec.version")(cfg.spec.version) tap { version =>
      check("spec.version")(version == Some(cfg.spec.version))
    }
  }

  private def dump[T: Encoder](tag: String)(data: T): (Long, Json) = {
    val tuple @ (_, json) = time { data.asJson }
    dumpFile(json.spaces2, s"$DUMP_DEBUGGER_LOG_DIR/$tag.json")
    tuple
  }

  private def check(tag: String)(condition: => Boolean): Unit = {
    if (!condition) {
      println(s"[Warning] Condition Failed: ${tag}")
    }
  }

  private def dumpThenRead[T: Encoder: Decoder](
    tag: String,
  )(data: T): Option[T] = Try {
    val (elapsed, json) = dump(tag)(data)
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
  }.toOption

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()

}
