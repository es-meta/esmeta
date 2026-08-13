package esmeta.es.util.polyfill

import esmeta.*
import esmeta.es.Polyfill
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import io.circe.yaml.scalayaml.parser as yamlParser

import java.io.File
import scala.collection.mutable.{Queue, Map as MMap, Set as MSet}

/** packager that turns generated polyfills into a usable CommonJS library
  *
  * A generated polyfill is a bare function body referring to abstract and
  * internal operations by unresolved symbols (`AO__x`, `IN__x`). This packager
  * resolves those symbols into `require` calls, wraps each polyfill in the
  * property-installation boilerplate its kind demands, and emits the
  * hand-written runtime it links against.
  *
  * Only the modules reachable from the selected built-ins are emitted, so the
  * library carries no dead code. Optimized runtime implementations take the
  * place of their baseline counterparts rather than living beside them, which
  * keeps the emitted tree the same shape either way.
  */
object PolyfillPackager {

  /** package polyfills into a CommonJS library rooted at `outDir`
    *
    * @param targets
    *   glob patterns selecting which built-ins to emit; all when empty
    * @param opt
    *   link optimized runtime implementations, matching the DSL-transformed
    *   generation the flag also turns on
    */
  def apply(
    polyfills: List[Polyfill],
    outDir: String,
    targets: List[String] = Nil,
    opt: Boolean = false,
  ): Unit = new PolyfillPackager(polyfills, outDir, targets, opt).generate

  /** abstract operations ESMeta cannot generate, backed by hand-written
    * implementations under `runtime/abstract/manual`
    */
  lazy val blacklist: Set[String] =
    val path = s"$POLYFILL_RESOURCE_DIR/blacklist.yaml"
    yamlParser
      .parse(readFile(path))
      .flatMap(_.as[List[String]])
      .fold(err => raise(s"failed to read $path: $err"), _.toSet)

  /** spec-mandated aliases and tags the generator cannot yet derive */
  val manualRules: Map[String, String] = Map(
    "Map" -> """Object.defineProperty(Map.prototype, Symbol.iterator, {
      |  value: Map.prototype.entries,
      |  writable: true,
      |  enumerable: false,
      |  configurable: true,
      |});
      |
      |Object.defineProperty(Map.prototype, Symbol.toStringTag, {
      |  value: "Map",
      |  writable: false,
      |  enumerable: false,
      |  configurable: true,
      |});""".stripMargin,
    "Set" -> """Object.defineProperty(Set.prototype, "keys", {
      |  value: Set.prototype.values,
      |  writable: true,
      |  enumerable: false,
      |  configurable: true,
      |});
      |
      |Object.defineProperty(Set.prototype, Symbol.iterator, {
      |  value: Set.prototype.values,
      |  writable: true,
      |  enumerable: false,
      |  configurable: true,
      |});
      |
      |Object.defineProperty(Set.prototype, Symbol.toStringTag, {
      |  value: "Set",
      |  writable: false,
      |  enumerable: false,
      |  configurable: true,
      |});""".stripMargin,
    "WeakMap" -> """Object.defineProperty(
      |  WeakMap.prototype,
      |  Symbol.toStringTag,
      |  {
      |    value: "WeakMap",
      |    writable: false,
      |    enumerable: false,
      |    configurable: true,
      |  },
      |);""".stripMargin,
    "WeakSet" -> """Object.defineProperty(
      |  WeakSet.prototype,
      |  Symbol.toStringTag,
      |  {
      |    value: "WeakSet",
      |    writable: false,
      |    enumerable: false,
      |    configurable: true,
      |  },
      |);""".stripMargin,
    "Promise" -> """Object.defineProperty(
      |  Promise.prototype,
      |  Symbol.toStringTag,
      |  {
      |    value: "Promise",
      |    writable: false,
      |    enumerable: false,
      |    configurable: true,
      |  },
      |);""".stripMargin,
  )

  /** kind of a polyfill, derived from its ESMeta name */
  enum Kind:
    case AO, NumberAO, BigIntAO, Obj, Method, Getter, Setter
    def isAO: Boolean = this match
      case AO | NumberAO | BigIntAO => true
      case _                        => false

  /** `INTRINSICS.<base>.<member>`, where `<member>` holds no dot */
  private val methodPattern = """INTRINSICS\..+\.[^.]+""".r

  /** unresolved reference to an abstract or internal operation */
  private val depPattern = """(?:AO|IN|SH|Number|BigInt)__[a-zA-Z0-9_]+""".r

  /** `require` of a relative module, as written in the hand-written runtime,
    * which uses either quote style
    */
  private val requirePattern =
    """require\(\s*["'](\.[^"']*)["']\s*\)""".r

  /** classify a polyfill by its name */
  def kindOf(name: String): Kind =
    if (name.startsWith("BigInt::")) Kind.BigIntAO
    else if (name.startsWith("Number::")) Kind.NumberAO
    else if (!name.startsWith("INTRINSICS.")) Kind.AO
    else if (name.contains("get:")) Kind.Getter
    else if (name.contains("set:")) Kind.Setter
    else if (methodPattern.matches(name)) Kind.Method
    else Kind.Obj

  /** the module a polyfill is emitted as, relative to the library root */
  def moduleOf(kind: Kind, name: String): String = kind match
    case Kind.NumberAO | Kind.BigIntAO =>
      val Array(prefix, rest) = name.split("::", 2)
      s"abstract/$prefix${rest.capitalize}"
    case Kind.AO => s"abstract/$name"
    case _       => name.stripPrefix("INTRINSICS.")

  /** the object a built-in installs onto, and the property it installs
    *
    * `bracketed` marks a computed key -- `Map[%Symbol.iterator%]` names the
    * expression `Symbol.iterator`, not the string `"Symbol.iterator"`.
    */
  case class Target(base: String, member: String, bracketed: Boolean)

  /** derive the installation target from a built-in polyfill name */
  def targetOf(kind: Kind, name: String): Target =
    val stripped = name
      .stripPrefix("INTRINSICS.")
      .replace("get:", "")
      .replace("set:", "")
    val open = stripped.indexOf("[%")
    val close = stripped.indexOf("%]")
    if (open >= 0 && close > open)
      Target(stripped.take(open), stripped.substring(open + 2, close), true)
    else if (kind == Kind.Obj) Target(stripped, "", false)
    else
      val dot = stripped.lastIndexOf('.')
      Target(stripped.take(dot), stripped.drop(dot + 1), false)

  /** compile a comma-separated glob list into a name predicate */
  def globFilter(patterns: List[String]): String => Boolean =
    if (patterns.isEmpty) _ => true
    else
      val regexes = patterns.map(pattern =>
        pattern.trim
          .split("\\*", -1)
          .map(java.util.regex.Pattern.quote)
          .mkString(".*")
          .r,
      )
      name => regexes.exists(_.matches(name))
}

/** extensible helper of the polyfill packager */
class PolyfillPackager(
  polyfills: List[Polyfill],
  outDir: String,
  targets: List[String],
  opt: Boolean,
) {

  import PolyfillPackager.*

  /** emit the whole library */
  def generate: Unit =
    if (selected.isEmpty)
      raise(
        s"no built-in matches the requested target: ${targets.mkString(",")}",
      )
    mkdir(outDir, remove = true)
    val emitted = installerFiles ++ generatedFiles ++ runtimeFiles ++ stubFiles
    emitted.foreach(dump)
    dump(indexFile)
    dump(packageFile)
    println(s"- Packaged ${selected.length} built-ins into `$outDir`.")
    println(
      s"  ${generatedFiles.length} generated abstract operations, " +
      s"${runtimeFiles.length} runtime modules" +
      (if (opt) " (optimized)" else ""),
    )
    if (unresolved.nonEmpty)
      println(s"- WARNING: ${unresolved.size} operations have no")
      println("  implementation; calls reaching them throw at runtime:")
      for ((module, referrer) <- unresolved.toList.sortBy(_._1))
        println(s"    ${nameOf(module)} (named by $referrer)")
    // A cycle is broken by deferring the edge that closes it, which this
    // packager can only do for the code it writes.
    val handWritten =
      cyclicEdges.filter((from, _) => !aoByModule.contains(from))
    if (handWritten.nonEmpty)
      println(s"- WARNING: ${handWritten.size} require cycles pass through a")
      println("  hand-written module, where the cycle cannot be broken here:")
      for ((from, to) <- handWritten.toList.sortBy(_._1))
        println(s"    $from -> $to")

  // ---------------------------------------------------------------------------
  // Classification
  // ---------------------------------------------------------------------------

  /** a polyfill paired with everything derived from its name */
  private case class Entry(poly: Polyfill, kind: Kind) {
    def name: String = poly.name
    lazy val module: String = moduleOf(kind, name)
    lazy val target: Target = targetOf(kind, name)

    /** the generated body, with internal operations already unwrapped */
    lazy val content: String = poly.toString.replace("AO__IN", "IN")
  }

  /** `INTRINSICS.yet:` polyfills stand for steps the generator could not
    * compile, so they never reach the library
    */
  private lazy val entries: List[Entry] = for {
    poly <- polyfills
    if !poly.name.startsWith("INTRINSICS.yet:")
  } yield Entry(poly, kindOf(poly.name))

  private lazy val (aos, builtins) = entries.partition(_.kind.isAO)

  /** generated abstract operations, keyed by the module they are emitted as */
  private lazy val aoByModule: Map[String, Entry] =
    aos.map(entry => entry.module -> entry).toMap

  /** built-ins ESMeta constructs from scratch, e.g. `Map`, `Promise` */
  private lazy val fromScratch: List[Entry] =
    builtins.filter(_.kind == Kind.Obj)

  /** a member belongs to a from-scratch built-in when its base object is rooted
    * at one -- `Map.prototype.get` belongs to `Map`, while `Array.prototype.at`
    * stands alone because `Array` is never built from scratch
    */
  private def rootOf(entry: Entry): String =
    entry.target.base.takeWhile(_ != '.')

  private lazy val (members, standalone) = builtins
    .filterNot(_.kind == Kind.Obj)
    .partition(entry => fromScratch.exists(_.target.base == rootOf(entry)))

  /** members of each from-scratch built-in, inlined into its installer */
  private def membersOf(base: Entry): List[Entry] =
    members.filter(rootOf(_) == base.target.base)

  /** the built-ins requested on the command line */
  private lazy val selected: List[Entry] =
    val matches = globFilter(targets)
    (standalone ++ fromScratch).filter(entry => matches(entry.module))

  // ---------------------------------------------------------------------------
  // Reachability
  // ---------------------------------------------------------------------------

  /** modules reachable from the selected built-ins, and the modules that were
    * named but have neither a generated nor a hand-written implementation
    *
    * Generated operations name their dependencies through unresolved symbols,
    * while the hand-written runtime names them through ordinary `require`
    * calls, so both forms are followed.
    */
  private lazy val (reachable, unresolved) =
    val seen = MSet[String]()
    val missing = MMap[String, String]()
    val namedBy = MMap[String, String]()
    val queue = Queue[String]()
    def push(by: String)(module: String): Unit =
      if (!seen(module))
        seen += module
        namedBy(module) = by
        queue.enqueue(module)
    for {
      entry <- selected
      content = entry.content + membersOf(entry).map(_.content).mkString
      dep <- generatedDeps(content)
    } push(entry.module)(dep)
    while (queue.nonEmpty) do
      val module = queue.dequeue
      aoByModule.get(module) match
        case Some(entry) => generatedDeps(entry.content).foreach(push(module))
        case None =>
          runtimeFile(module) match
            case Some(file) =>
              runtimeDeps(module, readFile(file.getPath)).foreach(push(module))
            case None => missing(module) = namedBy.getOrElse(module, "?")
    (seen.toSet, missing.toMap)

  /** modules named by unresolved symbols in generated code */
  private def generatedDeps(content: String): List[String] =
    for {
      dep <- depPattern.findAllIn(content).toList.distinct
      Array(category, fname) = dep.split("__", 2)
    } yield moduleFor(category, fname)

  /** modules named by `require` calls in the hand-written runtime */
  private def runtimeDeps(module: String, content: String): List[String] =
    val dir = parentOf(module)
    (for (m <- requirePattern.findAllMatchIn(content))
      yield normalize(s"$dir/${m.group(1)}")).toList.distinct

  /** where an unresolved symbol resolves to
    *
    * A numeric operation is named after the type it belongs to, so that
    * `Number::lessThan` and `BigInt::lessThan` stay distinct modules.
    */
  private def moduleFor(category: String, fname: String): String =
    if (category == "IN") s"internal/$fname"
    else
      val name =
        if (category == "Number" || category == "BigInt")
          s"$category${fname.capitalize}"
        else fname
      if (blacklist(name)) s"abstract/manual/$name" else s"abstract/$name"

  // ---------------------------------------------------------------------------
  // Runtime assets
  // ---------------------------------------------------------------------------

  /** the hand-written file backing a module
    *
    * In optimized mode the `.opt` implementation stands in for its baseline
    * counterpart, so callers keep addressing the module by its plain name.
    */
  private def runtimeFile(module: String): Option[File] =
    val optimized = File(s"$POLYFILL_RUNTIME_DIR/$module.opt.js")
    val baseline = File(s"$POLYFILL_RUNTIME_DIR/$module.js")
    if (opt && optimized.isFile) Some(optimized)
    else if (baseline.isFile) Some(baseline)
    else if (optimized.isFile) Some(optimized)
    else None

  private lazy val runtimeFiles: List[Out] = for {
    module <- reachable.toList.sorted
    if !aoByModule.contains(module)
    file <- runtimeFile(module)
  } yield Out(module, readFile(file.getPath), verbatim = true)

  /** a module ESMeta neither generates nor ships an implementation for
    *
    * A throwing stub keeps the rest of the library loadable: only a call that
    * actually reaches the operation fails, and it fails naming the operation
    * rather than as a missing-module error at load time.
    */
  private lazy val stubFiles: List[Out] = for {
    module <- unresolved.keys.toList.sorted
  } yield Out(
    module,
    join(
      "// Neither generated by ESMeta nor implemented by hand.",
      s"""module.exports = function () {
        |  throw new Error("minipoly: ${nameOf(module)} is not implemented");
        |};""".stripMargin,
    ),
  )

  // ---------------------------------------------------------------------------
  // Emission
  // ---------------------------------------------------------------------------

  private case class Out(
    module: String,
    content: String,
    verbatim: Boolean = false,
    ext: String = "js",
  )

  private def dump(out: Out): Unit =
    val body =
      if (out.verbatim) out.content
      else s""""use strict";$LINE_SEP$LINE_SEP${out.content}"""
    dumpFile(
      body.stripSuffix(LINE_SEP) + LINE_SEP,
      s"$outDir/${out.module}.${out.ext}",
    )

  /** reachable abstract operations, exported as plain functions */
  private lazy val generatedFiles: List[Out] = for {
    module <- reachable.toList.sorted
    entry <- aoByModule.get(module)
  } yield Out(
    module,
    join(
      imports(entry.content, module, parentOf(module)),
      s"module.exports = function _self${entry.content}",
    ),
  )

  /** one installer per selected built-in */
  private lazy val installerFiles: List[Out] =
    for (entry <- selected)
      yield entry.kind match
        case Kind.Obj => fromScratchFile(entry)
        case _        => standaloneFile(entry)

  /** a member installed onto an object the host already provides */
  private def standaloneFile(entry: Entry): Out =
    val Target(base, member, bracketed) = entry.target
    val guard = if (bracketed) s"$base[$member]" else s"$base.$member"
    Out(
      entry.module,
      join(
        imports(entry.content, entry.module, parentOf(entry.module)),
        s"""if (!$guard) {
        |${shift(install(entry), 1)}
        |}""".stripMargin,
      ),
    )

  /** a built-in ESMeta constructs from scratch, with its members inlined */
  private def fromScratchFile(entry: Entry): Out =
    val base = entry.target.base
    val own = membersOf(entry)
    val dir = parentOf(entry.module)
    Out(
      entry.module,
      join(
        imports(
          entry.content + own.map(_.content).mkString(LINE_SEP),
          entry.module,
          dir,
        ),
        s"function $base${entry.content}",
        s"""Object.defineProperty(globalThis, "$base", {
          |  value: $base,
          |  writable: true,
          |  enumerable: false,
          |  configurable: true,
          |});
          |
          |Object.defineProperty($base, "name", {
          |  value: "$base",
          |  writable: false,
          |  enumerable: false,
          |  configurable: true,
          |});
          |
          |Object.defineProperty($base, "prototype", {
          |  value: {},
          |  writable: false,
          |  enumerable: false,
          |  configurable: false,
          |});
          |
          |Object.defineProperty($base.prototype, "constructor", {
          |  value: $base,
          |  writable: true,
          |  enumerable: false,
          |  configurable: true,
          |});""".stripMargin,
        own.map(install).mkString(LINE_SEP + LINE_SEP),
        manualRules.getOrElse(base, ""),
      ),
    )

  /** the property-installation statement for one built-in member */
  private def install(entry: Entry): String =
    val Target(base, member, bracketed) = entry.target
    val key = if (bracketed) member else s""""$member""""
    val label = if (bracketed) s"[$member]" else member
    val body = entry.content
    entry.kind match
      case Kind.Method if bracketed =>
        s"""Object.defineProperty($base, $member, {
          |  value: function $body,
          |  writable: true,
          |  enumerable: false,
          |  configurable: true,
          |});""".stripMargin
      case Kind.Method =>
        s"""Object.defineProperty($base, "$member", {
          |  value: ({ $member$body }).$member,
          |  writable: true,
          |  enumerable: false,
          |  configurable: true,
          |});""".stripMargin
      case Kind.Getter | Kind.Setter =>
        val accessor = if (entry.kind == Kind.Getter) "get" else "set"
        val tmp = if (entry.kind == Kind.Getter) "tmpGetter" else "tmpSetter"
        s"""(function () {
          |  var $tmp = function $body;
          |  Object.defineProperty($tmp, "name", {
          |    value: "$accessor $label",
          |    writable: false,
          |    enumerable: false,
          |    configurable: true,
          |  });
          |  Object.defineProperty($base, $key, {
          |    $accessor: $tmp,
          |    enumerable: false,
          |    configurable: true,
          |  });
          |})();""".stripMargin
      case kind => raise(s"unexpected built-in kind: $kind")

  // ---------------------------------------------------------------------------
  // Entry points
  // ---------------------------------------------------------------------------

  private lazy val indexFile: Out = Out(
    "index",
    join(
      "// Installs the generated polyfills. Each module is side-effecting:" +
      LINE_SEP +
      "// requiring it defines the built-in when the host lacks it.",
      selected
        .map(_.module)
        .sorted
        .map(module => s"""require("./$module");""")
        .mkString(LINE_SEP),
    ),
  )

  private lazy val packageFile: Out = Out(
    "package",
    s"""{
      |  "name": "minipoly",
      |  "version": "0.0.0",
      |  "description": "polyfills generated from ECMA-262 by ESMeta",
      |  "main": "index.js",
      |  "sideEffects": true,
      |  "license": "BSD-3-Clause"
      |}""".stripMargin,
    verbatim = true,
    ext = "json",
  )

  // ---------------------------------------------------------------------------
  // Dependency resolution
  // ---------------------------------------------------------------------------

  /** dependency edges that close a cycle in the module graph
    *
    * A specification operation may be defined in terms of itself -- `ToNumber`
    * on an object converts to a primitive and starts over -- and two of them
    * may lean on each other. `require` cannot express that: whichever module
    * loads first sees a partner whose `module.exports` is still empty. The
    * edges reported here are resolved at first call instead, by which time
    * every module involved has finished loading.
    */
  private lazy val cyclicEdges: Set[(String, String)] =
    val onStack, done = MSet[String]()
    val edges = MSet[(String, String)]()
    def visit(module: String): Unit =
      onStack += module
      for (dep <- dependenciesOf(module))
        if (onStack(dep)) edges += (module -> dep)
        else if (!done(dep)) visit(dep)
      onStack -= module
      done += module
    reachable.toList.sorted.foreach(m => if (!done(m)) visit(m))
    edges.toSet

  /** dependencies declared by a module, whichever form it takes */
  private def dependenciesOf(module: String): List[String] =
    aoByModule.get(module) match
      case Some(entry) => generatedDeps(entry.content)
      case None =>
        runtimeFile(module).toList
          .flatMap(f => runtimeDeps(module, readFile(f.getPath)))

  /** resolve every unresolved symbol in `content` into a `require` */
  private def imports(content: String, self: String, dir: String): String =
    val lines = for {
      dep <- depPattern.findAllIn(content).toList.distinct.sorted
      Array(category, fname) = dep.split("__", 2)
      target = moduleFor(category, fname)
      path = relative(dir, target)
    } yield
      if (cyclicEdges(self -> target))
        s"var $dep = function () " +
        s"""{ return require("$path").apply(this, arguments); };"""
      else s"""var $dep = require("$path");"""
    lines.mkString(LINE_SEP)

  // ---------------------------------------------------------------------------
  // Path helpers
  // ---------------------------------------------------------------------------

  private def parentOf(module: String): String =
    module.lastIndexOf('/') match
      case -1 => ""
      case i  => module.take(i)

  private def nameOf(module: String): String =
    module.drop(module.lastIndexOf('/') + 1)

  /** collapse `.` and `..` segments into a library-root-relative module */
  private def normalize(path: String): String =
    path
      .split("/")
      .foldLeft(List[String]()) {
        case (acc, "" | ".") => acc
        case (acc, "..")     => acc.dropRight(1)
        case (acc, segment)  => acc :+ segment
      }
      .mkString("/")

  /** the `require` argument addressing `to` from a module inside `dir` */
  private def relative(dir: String, to: String): String =
    val from = if (dir.isEmpty) Nil else dir.split("/").toList
    val dest = to.split("/").toList
    val common = from.zip(dest).takeWhile(_ == _).length
    val ups = List.fill(from.length - common)("..")
    val downs = dest.drop(common)
    if (ups.isEmpty) ("." :: downs).mkString("/")
    else (ups ++ downs).mkString("/")

  // ---------------------------------------------------------------------------
  // Text helpers
  // ---------------------------------------------------------------------------

  /** join non-empty sections with a blank line between them */
  private def join(sections: String*): String =
    sections.map(_.trim).filter(_.nonEmpty).mkString(LINE_SEP + LINE_SEP)

  /** indent every line, so an embedded block keeps its shape */
  private def shift(code: String, indent: Int): String =
    val pad = "  " * indent
    code.linesIterator
      .map(line => if (line.isEmpty) line else pad + line)
      .mkString(LINE_SEP)
}
