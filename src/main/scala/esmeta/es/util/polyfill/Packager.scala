package esmeta.es.util.polyfill

import esmeta.*
import esmeta.spec.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import io.circe.yaml.scalayaml.parser as yamlParser

import java.io.File
import scala.collection.mutable.{Queue, Map as MMap, Set as MSet}

/** packager that turns generated polyfills into a usable CommonJS library */
object Packager {

  /** package polyfills into a CommonJS library rooted at `outDir`
    *
    * @param targets
    *   glob patterns selecting which built-ins to emit; all when empty
    * @param opt
    *   link optimized runtime implementations, matching the DSL-transformed
    *   generation the flag also turns on
    * @param skipped
    *   algorithms the generator dropped as unsupported, with the reason for
    *   each, which their stubs report
    */
  def apply(
    polyfills: List[Polyfill],
    outDir: String,
    targets: List[String] = Nil,
    opt: Boolean = false,
    skipped: Map[Head, String] = Map(),
  ): Unit = new Packager(polyfills, outDir, targets, opt, skipped).generate

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

  /** kind of a polyfill, derived from its algorithm head */
  enum Kind:
    case AO, Obj, Method, Getter, Setter
    def isAO: Boolean = this == AO

  /** `require` of a relative module, as written in the hand-written runtime,
    * which uses either quote style
    */
  private val requirePattern =
    """require\(\s*["'](\.[^"']*)["']\s*\)""".r

  /** classify a polyfill by its algorithm head */
  def kindOf(head: Head): Kind =
    import BuiltinPath.*
    head match
      case BuiltinHead(path, _, _) =>
        path match
          case Base(_)                                 => Kind.Obj
          case Getter(_)                               => Kind.Getter
          case Setter(_)                               => Kind.Setter
          case NormalAccess(_, _) | SymbolAccess(_, _) => Kind.Method
      case _ => Kind.AO

  /** the module a polyfill is emitted as, relative to the library root */
  def moduleOf(head: Head): String = head match
    case NumericMethodHead(baseTy, name, _, _) =>
      s"abstract/${baseTy.normalizedName}${name.capitalize}"
    case BuiltinHead(path, _, _) => path.toString
    case _                       => s"abstract/${head.fname}"

  /** the object a built-in installs onto, and the property it installs */
  case class Target(base: String, member: String, bracketed: Boolean)

  /** derive the installation target from a built-in path */
  def targetOf(path: BuiltinPath): Target =
    import BuiltinPath.*
    path match
      case Base(name)               => Target(name, "", false)
      case NormalAccess(base, name) => Target(base.toString, name, false)
      case SymbolAccess(base, symbol) =>
        Target(base.toString, s"Symbol.$symbol", true)
      case Getter(base) => targetOf(base)
      case Setter(base) => targetOf(base)

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

class Packager(
  polyfills: List[Polyfill],
  outDir: String,
  targets: List[String],
  opt: Boolean,
  skipped: Map[Head, String],
) {

  import Packager.*

  /** emit the whole library */
  def generate: Unit =
    if (selected.isEmpty)
      raise(
        s"no built-in matches the requested target: ${targets.mkString(",")}",
      )
    mkdir(outDir, remove = outDir == POLYFILL_OUT_DIR)
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
        val reason = skippedByModule.get(module).fold("")(r => s", skipped: $r")
        println(s"    ${nameOf(module)} (named by $referrer$reason)")
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

  /** a polyfill paired with everything derived from its head */
  private case class Entry(poly: Polyfill) {
    val kind: Kind = kindOf(poly.head)
    lazy val module: String = moduleOf(poly.head)
    lazy val target: Target = poly.head match
      case BuiltinHead(path, _, _) => targetOf(path)
      case head                    => raise(s"not a built-in: ${head.fname}")

    /** the generated body */
    lazy val content: String = poly.toString

    /** operations the body calls, plus the global object it installs onto */
    def deps: List[Polyfill.Dep] =
      if (kind == Kind.Obj) poly.deps :+ Polyfill.Dep.Internal("Global")
      else poly.deps
  }

  private lazy val entries: List[Entry] = polyfills.map(Entry(_))

  private lazy val (aos, builtins) = entries.partition(_.kind.isAO)

  /** generated abstract operations, keyed by the module they are emitted as */
  private lazy val aoByModule: Map[String, Entry] =
    aos.map(entry => entry.module -> entry).toMap

  /** built-ins ESMeta constructs from scratch, e.g. `Map`, `Promise` */
  private lazy val fromScratch: List[Entry] =
    builtins.filter(_.kind == Kind.Obj)

  /** the root object of a member's base, e.g. `Map` for `Map.prototype.get` */
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
      dep <- generatedDeps(entry.deps ++ membersOf(entry).flatMap(_.deps))
    } push(entry.module)(dep)
    while (queue.nonEmpty) do
      val module = queue.dequeue()
      aoByModule.get(module) match
        case Some(entry) => generatedDeps(entry.deps).foreach(push(module))
        case None =>
          runtimeFile(module) match
            case Some(file) =>
              runtimeDeps(module, readFile(file.getPath)).foreach(push(module))
            case None => missing(module) = namedBy.getOrElse(module, "?")
    (seen.toSet, missing.toMap)

  /** modules named by the dependencies of generated code */
  private def generatedDeps(deps: List[Polyfill.Dep]): List[String] =
    deps.map(moduleFor).distinct

  /** modules named by `require` calls in the hand-written runtime */
  private def runtimeDeps(module: String, content: String): List[String] =
    val dir = parentOf(module)
    (for (m <- requirePattern.findAllMatchIn(content))
      yield normalize(s"$dir/${m.group(1)}")).toList.distinct

  /** the module a dependency resolves to, mirroring [[moduleOf]] */
  private def moduleFor(dep: Polyfill.Dep): String =
    import Polyfill.Dep.*
    def abstractOp(name: String): String =
      if (blacklist(name)) s"abstract/manual/$name" else s"abstract/$name"
    dep match
      case Internal(name)    => s"internal/$name"
      case AO(name)          => abstractOp(name)
      case Numeric(ty, name) => abstractOp(s"$ty${name.capitalize}")
      case Enum(_)           => "internal/Enum"

  // ---------------------------------------------------------------------------
  // Runtime assets
  // ---------------------------------------------------------------------------

  /** the hand-written file backing a module */
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

  /** a module ESMeta neither generates nor ships an implementation for */
  private lazy val stubFiles: List[Out] = for {
    module <- unresolved.keys.toList.sorted
  } yield
    val why = skippedByModule.get(module) match
      case Some(reason) => s"is not supported ($reason)"
      case None         => "is not implemented"
    Out(
      module,
      join(
        "// Neither generated by ESMeta nor implemented by hand.",
        s"""module.exports = function () {
          |  throw new Error("minipoly: ${nameOf(module)} $why");
          |};""".stripMargin,
      ),
    )

  /** reasons for the skipped algorithms, keyed by the module they would be */
  private lazy val skippedByModule: Map[String, String] =
    skipped.map((head, reason) => moduleOf(head) -> reason)

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
      imports(entry.deps, module, parentOf(module)),
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
        imports(entry.deps, entry.module, parentOf(entry.module)),
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
        imports(entry.deps ++ own.flatMap(_.deps), entry.module, dir),
        s"function $base${entry.content}",
        s"""Object.defineProperty(IN__Global, "$base", {
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

  /** dependency edges that close a cycle in the module graph */
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
      case Some(entry) => generatedDeps(entry.deps)
      case None =>
        runtimeFile(module).toList
          .flatMap(f => runtimeDeps(module, readFile(f.getPath)))

  /** resolve every dependency into a `require` bound to its symbol */
  private def imports(
    deps: List[Polyfill.Dep],
    self: String,
    dir: String,
  ): String =
    val lines = for {
      dep <- deps.distinct.sortBy(_.symbol)
      target = moduleFor(dep)
      path = relative(dir, target)
    } yield dep match
      case Polyfill.Dep.Enum(name) =>
        s"""var ${dep.symbol} = require("$path")("$name");"""
      case _ if cyclicEdges(self -> target) =>
        s"var ${dep.symbol} = function () " +
        s"""{ return require("$path").apply(this, arguments); };"""
      case _ => s"""var ${dep.symbol} = require("$path");"""
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
