package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
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

/** `dump-debugger` phase */
case object DumpDebugger extends Phase[CFG, Unit] {
  val name = "dump-debugger"
  val help = "dump resources for standalone debugger"
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = {

    dumpAndCheck("program")(cfg.program.asJson.spaces2)
    dumpAndCheck("funcs")(cfg.program.funcs).get
    dumpAndCheck("grammar")(cfg.spec.grammar)
    dumpAndCheck("tyModel.decls")(cfg.spec.tyModel)
    dumpAndCheck("spec.tables")(cfg.spec.tables)
    dumpAndCheck("spec.version")(cfg.spec.version)
    dumpAndCheck("irFuncToCode")(
      Map.from(
        cfg.program.funcs.map { f =>
          f.name -> f.algo.map(_.code)
        },
      ),
    )
    dumpAndCheck("irToSpecNameMap") {
      import esmeta.web.routes.{SdoInfoJsonProtocol, given}

      object MethodNameParser extends BasicParsers {
        // override protected val whiteSpace: Regex = "".r
        lazy val funcName =
          ("Record[" ~> "\\w+".r <~ "]") ~ ("." ~> "\\w+".r) ^^ {
            case t ~ n => (t, n)
          }
      }

      extension (func: Func) {

        def methodNameRaw: Option[(String, String)] = MethodNameParser.parseAll(
          MethodNameParser.funcName,
          func.name,
        ) match {
          case MethodNameParser.Success(result, _) => Some(result)
          case _                                   => None
        }

        def methodName = func.kind match
          case FuncKind.InternalMeth =>
            func.methodNameRaw.map((t, n) => (t, s"[[${n}]]"))
          case FuncKind.ConcMeth => func.methodNameRaw
          case _                 => None
      }

      cfg.fnameMap.flatMap {
        case (name, f) if f.irFunc.algo.isEmpty => None
        case (name, f) =>
          val algo = f.irFunc.algo.get
          Some {
            Json.arr(
              name.asJson,
              Json
                .obj(
                  // name is unused
                  "name" -> algo.normalizedName.asJson,
                  "htmlId" -> algo.elem.parent().id().asJson,
                  "isBuiltIn" -> f.isBuiltin.asJson,
                  "isSdo" -> f.isSDO.asJson,
                  "sdoInfo" -> f.sdoInfo.asJson(using
                    SdoInfoJsonProtocol.encoder(using cfg),
                  ),
                  "isMethod" -> f.isMethod.asJson,
                  "sdoInfo" -> f.sdoInfo.asJson(using
                    SdoInfoJsonProtocol.encoder(using cfg),
                  ),
                  "methodInfo" -> f.irFunc.methodName.asJson,
                )
                .asJson,
            )
          }
      }.toList
      // .asJson
      // .spaces2
    }

  }

  private def dumpAndCheck[T: Encoder: Decoder](
    tag: String,
  )(data: T): Option[T] = {

    print("dump ...")
    val (elapsed, json) = time { data.asJson }
    dumpFile(json.spaces2, s"$DUMP_DEBUGGER_LOG_DIR/$tag.json")
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

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
