package esmeta.dump

import esmeta.*
import esmeta.cfg.*
import esmeta.error.ESMetaError
import esmeta.ir.{Type as IRType, Func as IrFunc, *}
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*
import scala.collection.mutable.{ListBuffer, Map as MMap, Set as MSet}

object DumpSecIdToFuncInfo {
  def apply(cfg: CFG): (MMap[String, Int], MMap[String, String]) =
    val secIdToFuncId: MMap[String, Int] = MMap.empty
    val secIdToFuncName: MMap[String, String] = MMap.empty

    for {
      func <- cfg.funcs
      irFunc = func.irFunc
      algo <- func.irFunc.algo
      sectionId = algo.elem.parent.id
    } {
      val secId =
        if (func.isSDO && func.sdoInfo.isDefined)
          s"$sectionId|${extractSDO(func.sdoInfo.get, cfg)}"
        else sectionId

      secIdToFuncId += (secId -> func.id)
      secIdToFuncName += (secId -> convertFuncName(func, cfg))
    }
    dumpJson(
      name = "secIdToFuncId",
      data = secIdToFuncId,
      filename = s"$DUMP_VISUALIZER_LOG_DIR/secIdToFuncId.json",
      silent = true,
    )
    dumpJson(
      name = "secIdToFuncName",
      data = secIdToFuncName,
      filename = s"$DUMP_VISUALIZER_LOG_DIR/secIdToFuncName.json",
      silent = true,
    )
    (secIdToFuncId, secIdToFuncName)

  def convertFuncName(func: Func, cfg: CFG): String =
    if (func.isMethod)
      val parsed = parseFuncName(func)
      if (func.kind == FuncKind.InternalMeth) s"[[$parsed]]" else parsed
    else if (func.isBuiltin) func.name.substring("INTRINSICS.".length)
    else if (func.isSDO) {
      func.sdoInfo.get match
        case SdoInfo.Default(func, method) => method
        case SdoInfo.Base(_, name, i, j, method) =>
          val buffer: ListBuffer[String] = ListBuffer.empty
          buffer += s"[$method] $name :"
          cfg.grammar.prods.find(_.name == name).foreach { prod =>
            val rhs = prod.rhsVec(i)
            rhs.getSymbols(j).flatMap(x => x).foreach { symbol =>
              (symbol.getT, symbol.getNt) match
                case (Some(t), None)  => buffer += t.term
                case (None, Some(nt)) => buffer += nt.name
                case _                =>
            }
          }
          buffer.mkString(" ")
    } else func.name

  def parseFuncName(func: Func): String =
    JsonParser.parseAll(JsonParser.methodName, func.name) match {
      case JsonParser.Success(result, _) => result._2
      case _ => throw ESMetaError(s"invalid method name ${func.name}")
    }

  // sectionId | astName | production
  def extractSDO(sdoInfo: SdoInfo, cfg: CFG): String = sdoInfo match
    case SdoInfo.Default(func, method) => "ToDo"
    case SdoInfo.Base(_, name, i, j, method) =>
      val buffer: ListBuffer[String] = ListBuffer.empty
      buffer += name
      cfg.grammar.prods.find(_.name == name).foreach { prod =>
        val rhs = prod.rhsVec(i)
        rhs.getSymbols(j).flatMap(x => x).foreach { symbol =>
          (symbol.getT, symbol.getNt) match
            case (Some(t), None)  => buffer += t.term
            case (None, Some(nt)) => buffer += nt.name
            case _                =>
        }
      }
      buffer.mkString("|")
}
