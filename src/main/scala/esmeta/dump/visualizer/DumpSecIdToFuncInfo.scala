package esmeta.dump.visualizer

import esmeta.*
import esmeta.cfg.*
import esmeta.error.ESMetaError
import esmeta.ir.{Type as IRType, Func as IrFunc, *}
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*
import scala.collection.mutable.{ListBuffer, Map as MMap, Set as MSet}

object DumpSecIdToFuncInfo {
  def apply(cfg: CFG): Unit =
    given CFG = cfg

    // secId to funcid, visual funcName, fallback id (closures)
    val secIdToFuncInfo: MMap[String, (Int, String, List[Int])] = MMap.empty

    dumpJson(
      name = "secIdToFunc",
      data = secIdToFuncInfo,
      filename = s"$DUMP_VISUALIZER_LOG_DIR/secIdToFunc.json",
      silent = true,
    )

  def convertFuncName(func: Func)(using cfg: CFG): String =
    if (func.isMethod)
      val parsed = parseMethodName(func)
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

  def parseMethodName(func: Func): String =
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
