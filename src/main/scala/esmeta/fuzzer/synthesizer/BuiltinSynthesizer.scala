package esmeta.fuzzer.synthesizer

import esmeta.cfg.*
import esmeta.error.*
import esmeta.es.*
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** An ECMAScript AST synthesizer for built-in libraries */
class BuiltinSynthesizer(algorithms: List[Algorithm]) extends Synthesizer {
  import BuiltinPath.*, BuiltinForm.*

  /** synthesizer name */
  def name: String = "BuiltinSynthesizer"

  /** get script */
  def script: String = choose(initPool).toString

  /** get initial pool */
  lazy val initPool: Vector[Code] = (for {
    case BuiltinHead(path, params, _) <- algorithms.map(_.head)
    raw <- path match
      case YetPath(_) => Nil
      case Getter(base) =>
        Code.Simple(getString(base)) :: (base match
          case Prototype(proto, prop) =>
            val args = List("x", proto).mkString(", ")
            List(
              Code.Simple(
                s"var x = {}; Object.setPrototypeOf($args); x$prop;",
              ),
            )
          case _ => Nil
        )
      case Setter(base) =>
        Code.Simple(getString(base)) :: (base match
          case Prototype(proto, prop) =>
            val args = List("x", proto).mkString(", ")
            List(
              Code.Simple(
                s"var x = {}; Object.setPrototypeOf($args); x$prop = 0;",
              ),
            )
          case _ => Nil
        )
      case path =>
        val MAX_ARGS = 5
        val pathStr = getString(path)
        val MIN_ARGS = params.count(_.kind == ParamKind.Normal)
        val thisCands = getBase(path) match
          case Some(base) => List("0", s"new $base")
          case None       => List("0")
        val calls = for {
          thisArg <- thisCands
          argsLen <- Range(MIN_ARGS, MAX_ARGS).toList
          args = List.fill(argsLen)("0")
        } yield Code.Builtin(Call(pathStr, thisArg, args))
        val constructs =
          Code.Builtin(Construct(pathStr, Nil)) :: (for {
            argsLen <- Range(MIN_ARGS, MAX_ARGS).toList
            args = List.fill(argsLen)("0")
          } yield Code.Builtin(Construct(pathStr, args)))
        val constructsWithNewTarget = for {
          argsLen <- Range(MIN_ARGS, MAX_ARGS).toList
          args = List.fill(argsLen)("0")
        } yield Code.Builtin(Construct(pathStr, args, Some("0")))
        calls ++ constructs ++ constructsWithNewTarget
    code <- raw match
      case Code.Builtin(Call(p, thisArg, args)) if p.contains(TA) =>
        taCtors.map(ta => Code.Builtin(Call(p.replace(TA, ta), thisArg, args)))
      case Code.Builtin(Construct(p, args, nt)) if p.contains(TA) =>
        taCtors.map(ta => Code.Builtin(Construct(p.replace(TA, ta), args, nt)))
      case Code.Simple(s) if s.contains(TA) =>
        taCtors.map(ta => Code.Simple(s.replace(TA, ta)))
      case other => List(other)
  } yield code).toVector

  // get prototype paths and properties
  object Prototype:
    def unapply(path: BuiltinPath): Option[(String, String)] = path match
      case NormalAccess(NormalAccess(base, "prototype"), name) =>
        Some((s"${getString(base)}.prototype", s".$name"))
      case SymbolAccess(NormalAccess(base, "prototype"), symbol) =>
        Some((s"${getString(base)}.prototype", s"[Symbol.$symbol]"))
      case _ => None

  // placeholder for %TypedArray%
  private val TA = "__TA__"
  private val taCtors: List[String] = List(
    "Int8Array",
    "Uint8Array",
    "Uint8ClampedArray",
    "Int16Array",
    "Uint16Array",
    "Int32Array",
    "Uint32Array",
    "BigInt64Array",
    "BigUint64Array",
    "Float16Array",
    "Float32Array",
    "Float64Array",
  )

  // get string of builtin path
  private def getString(path: BuiltinPath): String =
    (new Appender >> path).toString
  private given builtinPathRule: Rule[BuiltinPath] = (app, path) =>
    path match
      case Base("TypedArray")       => app >> s"(Object.getPrototypeOf($TA))"
      case Base(name)               => app >> name
      case NormalAccess(base, name) => app >> base >> "." >> name
      case Getter(base)             => app >> base
      case Setter(base)             => app >> base
      case SymbolAccess(base, symbol) =>
        app >> base >> "[Symbol." >> symbol >> "]"
      case YetPath(name) => app >> "yet:" >> name.replace(" ", "")

  // get base of builtin path
  private def getBase(path: BuiltinPath): Option[String] = path match
    case NormalAccess(NormalAccess(base, "prototype"), _) =>
      Some(receiver(base))
    case SymbolAccess(NormalAccess(base, "prototype"), _) =>
      Some(receiver(base))
    case Getter(base) => getBase(base)
    case Setter(base) => getBase(base)
    case _            => None

  // receiver of a %TypedArray%.prototype method must be a concrete instance
  private def receiver(base: BuiltinPath): String =
    if (base == Base("TypedArray")) TA else getString(base)

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
