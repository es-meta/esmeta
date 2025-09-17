package esmeta.fuzzer.synthesizer

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
class BuiltinSynthesizer(
  algorithms: List[Algorithm],
) extends Synthesizer {

  /** synthesizer name */
  def name: String = "BuiltinSynthesizer"

  /** get script */
  def script: Code = choose(initPool)

  /** get initial pool */
  lazy val initPool: Vector[Code] = (for {
    case BuiltinHead(path, _, _) <- algorithms.map(_.head)
    code <- path match
      case YetPath(_) => Nil
      case Getter(base) =>
        List(
          Code.Builtin(path),
          Code.Builtin(path, receiver = true),
        )
      case Setter(base) =>
        List(
          Code.Builtin(path, args = Some(List("0"))),
          Code.Builtin(path, args = Some(List("0")), receiver = true),
        )
      case path =>
        val MAX_ARGS = 5
        val calls = for {
          argsLen <- Range(0, MAX_ARGS).toList
          args = List.fill(argsLen)("0")
        } yield Code.Builtin(path, Some("0"), Some(args))
        // construct without arguments
        val construct = Code.Builtin(path, None, None, true)
        // constructs with arguments
        val constructs = for {
          argsLen <- Range(0, MAX_ARGS).toList
          args = List.fill(argsLen)("0")
        } yield Code.Builtin(path, None, Some(args), true)
        (calls ++ (construct :: constructs))
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
  def apply(
    name: String,
    args: List[Boolean],
    rhsIdx: Option[Int] = None,
  ): Syntactic =
    throw NotSupported(List("BuiltinSynthesizer.apply"))

  /** for lexical production */
  def apply(name: String): Lexical =
    throw NotSupported(List("BuiltinSynthesizer.apply"))
}
