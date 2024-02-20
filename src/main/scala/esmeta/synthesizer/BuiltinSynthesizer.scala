package esmeta.synthesizer

import esmeta.cfg.*
import esmeta.error.*
import esmeta.es.*
import esmeta.es.util.*
import esmeta.spec.*
import esmeta.spec.BuiltinPath.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** An ECMAScript AST synthesizer for built-in libraries */
object BuiltinSynthesizer extends BuiltinSynthesizer
trait BuiltinSynthesizer extends Synthesizer {

  /** synthesizer name */
  def name: String = "BuiltinSynthesizer"

  /** get script */
  def script: String = choose(initPool)

  /** get initial pool */
  lazy val initPool: Vector[String] = (for {
    case BuiltinHead(path, _, _) <- cfg.spec.algorithms.map(_.head)
    code <- path match
      case YetPath(_) => Nil
      case Getter(base) =>
        getString(base) :: (base match
          case Prototype(proto, prop) =>
            List(s"var x = {}; Object.setPrototypeOf(x, $proto); x$prop;")
          case _ => Nil
        )
      case Setter(base) =>
        getString(base) :: (base match
          case Prototype(proto, prop) =>
            List(s"var x = {}; Object.setPrototypeOf(x, $proto); x$prop = 0;")
          case _ => Nil
        )
      case path =>
        val MAX_ARGS = 5
        val pathStr = getString(path)
        // calls
        val calls = for {
          argsLen <- Range(1, MAX_ARGS + 1).toList
          argsStr = List.fill(argsLen)("0").mkString("(", ", ", ")")
        } yield s"$pathStr.call$argsStr;"
        // construct without arguments
        val construct = s"new $pathStr;"
        // constructs with arguments
        val constructs = for {
          argsLen <- Range(0, MAX_ARGS).toList
          argsStr = List.fill(argsLen)("0").mkString("(", ", ", ")")
        } yield s"new $pathStr$argsStr;"
        calls ++ (construct :: constructs)
  } yield code).toVector

  // get prototype paths and properties
  object Prototype:
    def unapply(path: BuiltinPath): Option[(String, String)] = path match
      case NormalAccess(NormalAccess(base, "prototype"), name) =>
        Some((s"${getString(base)}.prototype", s".$name"))
      case SymbolAccess(NormalAccess(base, "prototype"), symbol) =>
        Some((s"${getString(base)}.prototype", s"[Symbol.$symbol]"))
      case _ => None

  // get string of builtin path
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

  /** for syntactic production */
  def apply(name: String, args: List[Boolean]): Syntactic =
    throw NotSupported(List("BuiltinSynthesizer.apply"))

  /** for lexical production */
  def apply(name: String): Lexical =
    throw NotSupported(List("BuiltinSynthesizer.apply"))
}
