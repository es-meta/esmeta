package esmeta.es

import esmeta.spec.*
import esmeta.spec.BuiltinPath.*
import esmeta.util.*
import esmeta.util.Appender.*

/** ECMAScript script program */
case class Script(code: Code, name: String) extends ESElem

/** ECMAScript code */
enum Code {
  case Normal(codeStr: String)
  case Builtin(
    path: BuiltinPath,
    thisArg: Option[String] = None,
    args: Option[List[String]] = None,
    isConstruct: Boolean = false,
    receiver: Boolean = false,
  )

  override def toString: String = this match {
    case Normal(codeStr) => codeStr
    case Builtin(path, thisArg, args, isConstruct, receiver) =>
      path match
        case YetPath(_) => ""
        case Getter(Prototype(proto, prop)) if receiver =>
          s"var x = {}; Object.setPrototypeOf(x, $proto); x$prop;"
        case Getter(base) => getString(base)
        case Setter(Prototype(proto, prop)) if receiver =>
          s"var x = {}; Object.setPrototypeOf(x, $proto); x$prop = ${args.head};"
        case Setter(base) => getString(base)
        case _ =>
          val pathStr = getString(path)
          if (!isConstruct) {
            val argsStr = (thisArg.get :: args.get).mkString("(", ", ", ")")
            s"$pathStr.call$argsStr;"
          } else if (args.isDefined) {
            val argsStr = args.get.mkString("(", ", ", ")")
            s"new $pathStr$argsStr;"
          } else s"new $pathStr;"
  }

  def size: Int = toString.length
}

/** get prototype paths and properties */
object Prototype:
  def unapply(path: BuiltinPath): Option[(String, String)] = path match
    case NormalAccess(NormalAccess(base, "prototype"), name) =>
      Some((s"${getString(base)}.prototype", s".$name"))
    case SymbolAccess(NormalAccess(base, "prototype"), symbol) =>
      Some((s"${getString(base)}.prototype", s"[Symbol.$symbol]"))
    case _ => None

/** get string of builtin path */
private def getString(path: BuiltinPath): String =
  (new Appender >> path).toString
private given builtinPathRule: Rule[BuiltinPath] = (app, path) =>
  path match
    case Base(name)               => app >> name
    case NormalAccess(base, name) => app >> base >> "." >> name
    case Getter(base)             => app >> base
    case Setter(base)             => app >> base
    case SymbolAccess(base, symbol) =>
      app >> base >> "[Symbol." >> symbol >> "]"
    case YetPath(name) => app >> "yet:" >> name.replace(" ", "")
