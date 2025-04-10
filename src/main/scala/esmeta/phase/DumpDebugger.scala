package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.cfg.util.{JsonProtocol as CFGJsonProtocol}
import esmeta.ir.{Type as IRType, *}
import esmeta.ir.util.JsonProtocol.given
import esmeta.ir.util.{UnitWalker as IRUnitWalker, Walker as IRWalker}
import esmeta.spec.{Algorithm, Grammar, Spec, Table}
import esmeta.spec.util.JsonProtocol.given
import esmeta.lang.{Type as LangType}
import esmeta.lang.util.JsonProtocol.given
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
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
  ): Unit =
    val cfgProto = CFGJsonProtocol(cfg)

    dumpThenRead("program")(cfg.program) tap { program =>
      check("program")(
        program == Some(IRSerializationSanitizer.walk(cfg.program)),
      )
    }

    dumpThenRead("funcs")(cfg.program.funcs) tap { _funcsOpt =>
      val funcs = _funcsOpt.getOrElse(Nil)
      val serialized = cfg.program.funcs.map(IRSerializationSanitizer.walk)
      check("funcs") { funcs == serialized }
    }

    dumpThenRead("grammar")(cfg.spec.grammar) tap { grammar =>
      check("grammar")(grammar == Some(cfg.spec.grammar))
    }

    dumpThenRead("tyModel.decls")(cfg.spec.tyModel) tap { tyModel =>
      check("tyModel.decls")(tyModel == Some(cfg.tyModel))
    }

    dumpThenRead("spec.tables")(cfg.spec.tables) tap { tables =>
      check("spec.tables")(tables == Some(cfg.spec.tables))
    }

    dumpThenRead("spec.version")(cfg.spec.version) tap { version =>
      check("spec.version")(version == Some(cfg.spec.version))
    }

    dump("irFuncToCode")(cfg)(using cfgProto.irFuncToCode)
    dump("irToSpecNameMap")(cfg)(using cfgProto.irToSpecNameMapEncoder)

  end apply

  private def dump[T: Encoder](tag: String)(data: T): (Long, Json) = {
    val tuple @ (_, json) = time { data.asJson }
    dumpFile(json.noSpaces, s"$DUMP_DEBUGGER_LOG_DIR/$tag.json")
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

  extension (inst: Inst) {

    def flatten: Inst = {
      inst match
        case INop() => ISeq(Nil)
        case IIf(cond, thenInst, elseInst, isAbruptInst) =>
          IIf(
            cond,
            thenInst.flatten,
            elseInst.flatten,
            isAbruptInst,
          )
        case IWhile(cond, body) =>
          IWhile(
            cond,
            body.flatten,
          )
        case ISeq(insts) =>
          ISeq(
            insts.flatMap(_.flatten match
              case ISeq(insts) => insts
              case inst        => List(inst),
            ),
          )
        case _ => inst

    }
  }

  /** removes informations that are lost in serialization */
  object IRSerializationSanitizer extends IRWalker {

    override def walk(func: Func): Func =
      val Func(main, kind, name, params, retTy, body, _) = func
      Func(
        main,
        kind,
        name,
        params.map(walk),
        walk(retTy),
        walk(body),
        None,
      )

    override def walk(param: Param): Param = {
      val Param(lhs, ty, optional, _) = param
      Param(lhs, walk(ty), optional, None)
    }

    override def walk(inst: Inst): Inst = inst match
      case IIf(cond, thenInst, elseInst, isAbruptInst) =>
        IIf(
          walk(cond),
          walk(thenInst),
          walk(elseInst),
          false, // as default
        )
      case IWhile(cond, body) => IWhile(walk(cond), walk(body))
      case ISeq(insts)        => ISeq(insts.map(walk))
      case _                  => super.walk(inst)

    override def walk(ty: IRType): IRType = ty match
      case IRType(ty, _) => IRType(ty, None)

  }

}
