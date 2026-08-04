package esmeta.fv

import esmeta.BASE_DIR
import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.compiler.Compiler
import esmeta.error.InvalidExit
import esmeta.es.*
import esmeta.extractor.Extractor
import esmeta.interpreter.Interpreter
import esmeta.ir.{Func => IRFunc}
import esmeta.parser.ESValueParser
import esmeta.state.*
import esmeta.util.BaseUtils.optional
import esmeta.util.SystemUtils.*
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.Base64
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

/** Export the ECMAScript specification and the initial state it runs from into
  * small Rocq compilation units behind the stable `formal/validation/Spec.v`
  * facade, so the model can execute a real script rather than a hand-written IR
  * program.
  *
  * Emits, as one `prog`:
  *   - every spec function `FVExport` can translate (the rest are omitted and
  *     counted; a call to a missing function is UB in the model, which is
  *     honest — it is a function we do not have);
  *   - the initial globals and heap from `Initialize(cfg).from(source)`;
  *   - the source text and the cached AST, with `subIdx`, printed source and
  *     lexical-SDO answers precomputed per node (ADR-12, ADR-15).
  *
  * ADDRESSES (ADR-16). ESMeta's initial heap uses only `NamedAddr`s, and its
  * dynamic-allocation counter starts at 0. The model has `nat` addresses only,
  * so every initial address is renumbered to its position in the exported heap
  * list and the model's counter starts at the heap length. This is a bijection
  * on addresses, and nothing in the model observes an address's numeric value
  * (`IPrint` payloads carrying addresses are excluded by the
  * observable-behaviour spec, L-6), so equality — the only thing the semantics
  * does with addresses — is preserved.
  *
  * Usage: sbt "runMain esmeta.fv.FVInitState [source]"
  */
object FVInitState {

  import FVExport.{Unsupported, coqList, strLit, cstrLit, zLit, floatLit}

  private val TEST262_EXPORT_JOBS_OPTION = "--test262-export-jobs"
  private val DIRECT_OUTPUT_OPTION = "--direct-itree"
  private val DEFAULT_SOURCE = "var x = 1;"

  private[fv] def sourceForArgs(args: Array[String]): String =
    args.headOption match
      case Some(DIRECT_OUTPUT_OPTION) =>
        args.lift(1).filterNot(_.startsWith("--")).getOrElse(DEFAULT_SOURCE)
      case source =>
        source.filterNot(_.startsWith("--")).getOrElse(DEFAULT_SOURCE)

  private[fv] final case class DirectFunctionEmission(
    funId: String,
    /** Gallina identifier, which also names this function's own file. */
    gallinaId: String,
    source: String,
    ordinaryEntry: String,
    continuationEntry: String,
    isMain: Boolean,
    mainEntry: Option[String],
  )

  private[fv] final case class DirectSplitArtifacts(
    files: List[(String, String)],
    manifest: String,
  )

  /** Readable Gallina identifiers for the direct backend, one per function.
    *
    * Spec function names carry `::`, `.`, `[`, `]`, `,`, `%`, and backticks,
    * none of which a Gallina identifier admits. `::` becomes `__` (so
    * `BigInt::add` reads `BigInt__add`) and every other rejected character
    * becomes `_`. Mangling alone is not injective — `a.b` and `a,b` would
    * collide — so any name that is not unique after mangling keeps its
    * index, and the result is checked to be one-to-one before it is used.
    * Identifiers double as file names, so they must stay distinct.
    */
  private[fv] def directFunIds(funcs: List[IRFunc]): List[String] = {
    def mangle(name: String): String =
      name
        .replace("::", "__")
        .map(c => if (c.isLetterOrDigit || c == '_') c else '_')
    // Not [f.kind.toString]: that is the IR stringifier, which renders kinds
    // as `<NUM>:` and [AbsOp] as the empty string.
    def kindName(kind: esmeta.ir.FuncKind): String = kind match
      case esmeta.ir.FuncKind.AbsOp        => "AbsOp"
      case esmeta.ir.FuncKind.NumMeth      => "NumMeth"
      case esmeta.ir.FuncKind.SynDirOp     => "SynDirOp"
      case esmeta.ir.FuncKind.ConcMeth     => "ConcMeth"
      case esmeta.ir.FuncKind.InternalMeth => "InternalMeth"
      case esmeta.ir.FuncKind.Builtin      => "Builtin"
      case esmeta.ir.FuncKind.Clo          => "Clo"
      case esmeta.ir.FuncKind.Cont         => "Cont"
      case esmeta.ir.FuncKind.Aux          => "Aux"
    val kinds = funcs.map(f => s"ir_${kindName(f.kind)}_${mangle(f.name)}")
    val ambiguous = kinds.groupBy(identity).collect {
      case (key, copies) if copies.size > 1 => key
    }.toSet
    val ids = kinds.zipWithIndex.map { (id, index) =>
      if (ambiguous(id)) f"${id}_$index%04d" else id
    }
    val duplicates = ids.groupBy(identity).collect {
      case (key, copies) if copies.size > 1 => key
    }.toList.sorted
    if (duplicates.nonEmpty)
      throw new IllegalStateException(
        s"direct identifiers are not injective: ${duplicates.mkString(", ")}",
      )
    ids
  }

  /** Direct output covers the whole function domain minus a declared omission
    * set. `omittedFunIds` is that declaration: a function may be absent only
    * if it is named there, so nothing can disappear silently, and a name may
    * not be both emitted and omitted. Omission matches what the generic
    * exporter already does — the model simply has no such function, and
    * calling one that is missing is UB — and `DirectITreeExec` needs no
    * change, because the map it is handed is the executable map.
    */
  private[fv] def validateDirectDomains(
    expectedFunIds: List[String],
    ordinaryFunIds: List[String],
    continuationFunIds: List[String],
    hasMain: Boolean,
    mainEntryCount: Int,
    omittedFunIds: List[String] = Nil,
  ): Unit = {
    def duplicateKeys(label: String, keys: List[String]): Unit = {
      val duplicates = keys
        .groupBy(identity)
        .collect {
          case (key, copies) if copies.size > 1 => key
        }
        .toList
        .sorted
      if (duplicates.nonEmpty)
        throw new IllegalStateException(
          s"duplicate direct $label keys: ${duplicates.mkString(", ")}",
        )
    }
    val omitted = omittedFunIds.toSet
    def requireDomain(label: String, actual: List[String]): Unit = {
      duplicateKeys(label, actual)
      val expected = expectedFunIds.toSet
      val got = actual.toSet
      val claimed = got | omitted
      if (claimed != expected || (got & omitted).nonEmpty) {
        val missing = (expected -- claimed).toList.sorted
        val extra = (claimed -- expected).toList.sorted
        val both = (got & omitted).toList.sorted
        throw new IllegalStateException(
          s"direct $label domain mismatch: " +
          s"missing=${missing.mkString(",")}, extra=${extra.mkString(",")}, " +
          s"emitted-and-omitted=${both.mkString(",")}",
        )
      }
    }

    duplicateKeys("expected", expectedFunIds)
    duplicateKeys("omitted", omittedFunIds)
    requireDomain("ordinary", ordinaryFunIds)
    requireDomain("continuation", continuationFunIds)
    val expectedEntries = if (hasMain) 1 else 0
    if (mainEntryCount != expectedEntries)
      throw new IllegalStateException(
        s"direct entry mismatch: expected=$expectedEntries, actual=$mainEntryCount",
      )
  }

  /** Render direct definitions into deterministic contiguous shards. Direct
    * output owns a disjoint directory, manifest variable, and provenance key;
    * it never shares the generic SpecFuncs namespace or Test262 cache inputs.
    */
  private[fv] def renderDirectSplitArtifacts(
    functions: List[DirectFunctionEmission],
    chunkSize: Int = 32,
    expectedFunIds: List[String] = Nil,
    omittedFunIds: List[String] = Nil,
  ): DirectSplitArtifacts = {
    require(chunkSize > 0, "direct shard size must be positive")
    val expectedDomain =
      if (expectedFunIds.isEmpty) functions.map(_.funId) else expectedFunIds
    validateDirectDomains(
      expectedDomain,
      functions.map(_.funId),
      functions.map(_.funId),
      functions.exists(_.isMain),
      functions.count(_.mainEntry.nonEmpty),
      omittedFunIds,
    )

    val header =
      """(* AUTO-GENERATED by FVInitState --direct-itree; do not edit. *)
From Stdlib Require Import String ZArith List Floats PString.
Import ListNotations.
From CRIS Require Import CRIS.
From ESMetaFV Require Import
  Fragment Domain TestEncoding Events Semantics DirectSemantics.
From ESMetaFV.validation.spec_direct Require Import DirectNames.
Local Open Scope string_scope.

"""
    val namesFile =
      """(* AUTO-GENERATED by FVInitState --direct-itree; do not edit. *)
From Stdlib Require Import String List.
Import ListNotations.
Local Open Scope string_scope.

Definition direct_spec_fnames : list string :=
  """ + coqList(expectedDomain.map(strLit)) + ".\n"
    def suffix(index: Int): String = f"$index%04d"
    val chunks = functions.grouped(chunkSize).map(_.toList).toList
    // One function per chunk is the normal layout: the file is then named
    // after the function it holds, so a spec function can be opened directly.
    // Definition names inside stay index-based; they are plumbing the facade
    // chains, not something a reader looks up.
    def moduleName(chunk: List[DirectFunctionEmission], index: Int): String =
      if (chunkSize == 1) chunk.head.gallinaId
      else s"DirectFuncs_${suffix(index)}"
    val shardFiles = chunks.zipWithIndex.map { (chunk, index) =>
      val id = suffix(index)
      val out = new StringBuilder(header)
      chunk.foreach { function =>
        out ++= function.source
        if (!function.source.endsWith("\n")) out += '\n'
        out += '\n'
      }
      // The entry lists mention [mn] and the CRIS instance, so they need a
      // section that binds both.  Closing it turns each chunk into a function
      // of [mn], which is why the facade applies it (see below).
      out ++= s"Section direct_entries_$id.\n"
      out ++= "Context `{!crisG Γ Σ α β τ _S _I}.\n"
      out ++= "Variable mn : string.\n\n"
      out ++= s"Definition direct_ordinary_entries_chunk_$id :=\n  "
      out ++= coqList(chunk.map(_.ordinaryEntry))
      out ++= ".\n\n"
      out ++= s"Definition direct_continuation_entries_chunk_$id :=\n  "
      out ++= coqList(chunk.map(_.continuationEntry))
      out ++= ".\n"
      out ++= s"End direct_entries_$id.\n"
      s"${moduleName(chunk, index)}.v" -> out.toString
    }

    val facade = new StringBuilder(header)
    if (chunks.nonEmpty) {
      facade ++= "From ESMetaFV.validation.spec_direct Require Export\n  "
      facade ++= chunks.zipWithIndex
        .map(moduleName)
        .mkString("\n  ")
      facade ++= ".\n\n"
    }
    facade ++= "Section direct_facade.\n"
    facade ++= "Context `{!crisG Γ Σ α β τ _S _I}.\n"
    facade ++= "Variable mn : string.\n\n"
    if (chunks.nonEmpty) {
      for (index <- chunks.indices.reverse) {
        val id = suffix(index)
        val ordinaryTail =
          if (index + 1 < chunks.size)
            s"direct_ordinary_entries_tail_${suffix(index + 1)}"
          else "nil"
        val continuationTail =
          if (index + 1 < chunks.size)
            s"direct_continuation_entries_tail_${suffix(index + 1)}"
          else "nil"
        // [mn] is applied because the chunk comes from another file, where its
        // own section already closed over it; the tails are local to this
        // section, so they stay unapplied.  The append is scoped explicitly:
        // the header opens [string_scope], where `++` is string concatenation.
        facade ++=
          s"Definition direct_ordinary_entries_tail_$id :=\n" +
          s"  (direct_ordinary_entries_chunk_$id mn ++ $ordinaryTail)%list.\n"
        facade ++=
          s"Definition direct_continuation_entries_tail_$id :=\n" +
          s"  (direct_continuation_entries_chunk_$id mn ++ " +
          s"$continuationTail)%list.\n"
      }
    }
    val ordinaryEntries =
      if (chunks.isEmpty) "nil" else "direct_ordinary_entries_tail_0000"
    val continuationEntries =
      if (chunks.isEmpty) "nil" else "direct_continuation_entries_tail_0000"
    facade ++= s"\nDefinition direct_ir_funid_fnsems : fnsemmap :=\n" +
    s"  list_to_map $ordinaryEntries.\n"
    facade ++= s"Definition direct_ir_cont_fnsems : fnsemmap :=\n" +
    s"  list_to_map $continuationEntries.\n"
    functions.flatMap(_.mainEntry) match
      case mainEntry :: Nil =>
        facade ++= s"Definition direct_ir_entry := $mainEntry.\n"
        facade ++= "Definition direct_ir_fnsems : fnsemmap :=\n" +
        s"  list_to_map (direct_ir_entry :: $ordinaryEntries).\n"
      case Nil =>
        facade ++= "Definition direct_ir_fnsems : fnsemmap :=\n" +
        "  direct_ir_funid_fnsems.\n"
      case _ => throw new IllegalStateException("multiple direct main entries")
    facade ++= "End direct_facade.\n"

    val files = shardFiles ++ List(
      "DirectFuncs.v" -> facade.toString,
      "DirectNames.v" -> namesFile,
    )
    val manifestEntries = files.map(_._1).sorted.map { name =>
      s"validation/spec_direct/$name"
    }
    val manifest = new StringBuilder
    manifest ++= "# AUTO-GENERATED by FVInitState; do not edit.\n"
    manifest ++= "DIRECT_GENERATOR_PROVENANCE := direct-itree-v1\n"
    manifest ++= "DIRECT_GENERATED_SOURCES := \\\n"
    manifest ++= manifestEntries.zipWithIndex.map { (path, index) =>
      val continuation = if (index + 1 < manifestEntries.size) " \\" else ""
      s"  $path$continuation\n"
    }.mkString
    DirectSplitArtifacts(files, manifest.toString)
  }

  private[fv] enum Test262FailureClass:
    case ESMetaFailed
    case NotRepresentable(reason: String)

  private[fv] def classifyTest262Failure(
    error: Throwable,
  ): Test262FailureClass = error match
    case Unsupported(reason) =>
      Test262FailureClass.NotRepresentable(reason)
    case _ => Test262FailureClass.ESMetaFailed

  /** Stable FVPayload v5 wire tags for ESMeta mathematical operators. Keep the
    * exhaustive mapping named rather than depending on enum ordinals; the Scala
    * regression locks all 21 values against the decoder table.
    */
  private[fv] def mathOpTag(op: esmeta.ir.MOp): Int = op match
    case esmeta.ir.MOp.Expm1 => 0
    case esmeta.ir.MOp.Log10 => 1
    case esmeta.ir.MOp.Log2  => 2
    case esmeta.ir.MOp.Cos   => 3
    case esmeta.ir.MOp.Cbrt  => 4
    case esmeta.ir.MOp.Exp   => 5
    case esmeta.ir.MOp.Cosh  => 6
    case esmeta.ir.MOp.Sinh  => 7
    case esmeta.ir.MOp.Tanh  => 8
    case esmeta.ir.MOp.Acos  => 9
    case esmeta.ir.MOp.Acosh => 10
    case esmeta.ir.MOp.Asinh => 11
    case esmeta.ir.MOp.Atanh => 12
    case esmeta.ir.MOp.Asin  => 13
    case esmeta.ir.MOp.Atan2 => 14
    case esmeta.ir.MOp.Atan  => 15
    case esmeta.ir.MOp.Log1p => 16
    case esmeta.ir.MOp.Log   => 17
    case esmeta.ir.MOp.Sin   => 18
    case esmeta.ir.MOp.Sqrt  => 19
    case esmeta.ir.MOp.Tan   => 20

  private[fv] val mathToNumberHostTag = 7

  /** The four terminal Number-method composites whose result is supplied by the
    * current ESMeta implementation. This is deliberately distinct from raw IR
    * [BOp] semantics: ESMeta first converts both binary64 inputs to its decimal
    * Math domain, performs the Math operation, and converts back.
    */
  private[fv] enum NumberMathOp:
    case Add, Mul, Div, Pow

  private[fv] def numberMathOpTag(op: NumberMathOp): Int = op match
    case NumberMathOp.Add => 0
    case NumberMathOp.Mul => 1
    case NumberMathOp.Div => 2
    case NumberMathOp.Pow => 3

  private[fv] def rocqNumberMathOp(op: NumberMathOp): String = op match
    case NumberMathOp.Add => "NMAdd"
    case NumberMathOp.Mul => "NMMul"
    case NumberMathOp.Div => "NMDiv"
    case NumberMathOp.Pow => "NMPow"

  private[fv] val numberMathHostTag = 8
  private[fv] val numberSinHostTag = 9
  private[fv] val numberMathCompareHostTag = 10
  private[fv] val numberToMathHostTag = 11

  private[fv] enum NumberMathCompareOp:
    case Lt, Equal

  private[fv] enum NumberMathCompareDirection:
    case NumberLeft, NumberRight

  private[fv] def numberMathCompareOpTag(op: NumberMathCompareOp): Int =
    op match
      case NumberMathCompareOp.Lt    => 0
      case NumberMathCompareOp.Equal => 1

  private[fv] def numberMathCompareDirectionTag(
    direction: NumberMathCompareDirection,
  ): Int = direction match
    case NumberMathCompareDirection.NumberLeft  => 0
    case NumberMathCompareDirection.NumberRight => 1

  private[fv] def rocqNumberMathCompareOp(op: NumberMathCompareOp): String =
    op match
      case NumberMathCompareOp.Lt    => "NMCLt"
      case NumberMathCompareOp.Equal => "NMCEqual"

  private[fv] def rocqNumberMathCompareDirection(
    direction: NumberMathCompareDirection,
  ): String = direction match
    case NumberMathCompareDirection.NumberLeft  => "NMCNumberLeft"
    case NumberMathCompareDirection.NumberRight => "NMCNumberRight"

  private val MAX_EXACT_NUMBER_INTEGER = scala.math.BigInt(1) << 53

  /** The pure Rocq conversion is exact through 2^53. Larger integral Math
    * values use the captured Scala result so binary64 rounding stays typed and
    * explicit at the host boundary.
    */
  private[fv] def needsMathToNumberCapture(value: BigDecimal): Boolean =
    value.isWhole && value.toBigInt.abs > MAX_EXACT_NUMBER_INTEGER

  /** Preserve the existing value-payload tags while allowing HQMathOp to return
    * either finite Math or Infinity.
    */
  private[fv] def writeExtMathValuePayload(
    value: ExtMath,
    out: FVPayload.Encoder,
  ): Unit = value match
    case Math(decimal) =>
      if (!decimal.isWhole)
        throw FVExport.Unsupported(s"non-integer Math value: $decimal")
      out.tag(0)
      out.integer(decimal.toBigInt)
    case Infinity(positive) =>
      out.tag(12)
      out.bool(positive)

  private[fv] def parseTest262ExportJobs(args: Iterable[String]): Int = {
    val options = args.filter(_.startsWith(TEST262_EXPORT_JOBS_OPTION)).toList
    options match
      case Nil => 1
      case option :: Nil =>
        val prefix = s"$TEST262_EXPORT_JOBS_OPTION="
        option.stripPrefix(prefix).toIntOption match
          case Some(jobs) if option.startsWith(prefix) && jobs > 0 => jobs
          case _ =>
            throw IllegalArgumentException(
              s"$TEST262_EXPORT_JOBS_OPTION must be a positive integer " +
              "written as --test262-export-jobs=N",
            )
      case _ =>
        throw IllegalArgumentException(
          s"$TEST262_EXPORT_JOBS_OPTION may be supplied only once",
        )
  }

  /** Execute independent Test262 preparation jobs concurrently while preserving
    * input order. Artifact writes remain outside this helper so generated
    * filenames and manifests stay deterministic.
    */
  private[fv] def mapTest262WithJobs[A, B](
    inputs: List[A],
    jobs: Int,
  )(f: A => B): List[B] = {
    require(jobs > 0, "Test262 export jobs must be positive")
    if (jobs == 1) inputs.map(f)
    else {
      val (service, context) = fixedThread(jobs)
      try concurrent(inputs.map(input => () => f(input)))(using context).toList
      finally service.shutdown()
    }
  }

  private val splitSpecSourcePattern =
    raw"validation/spec/(SpecFuncs(?:_\d{4})?|SpecGlobals|SpecHeap(?:_\d{4})?)\.v".r

  // Per-function shards are named after their Gallina identifier, so the
  // pattern admits `ir_<Kind>_<mangled name>` alongside the two facades and
  // the historical `DirectFuncs_NNNN` grouping.
  private val directSpecSourcePattern =
    raw"validation/spec_direct/(?:DirectFuncs(?:_\d{4})?|DirectNames|ir_[A-Za-z0-9_']+)\.v".r

  private[fv] def validateDirectSplitSpecBase(formalDir: File): List[File] = {
    def invalid(detail: String): Nothing =
      throw new IllegalStateException(s"invalid direct Spec artifacts; $detail")

    val formalRoot = formalDir.toPath.toAbsolutePath.normalize
    if (!Files.isDirectory(formalRoot)) invalid(s"missing $formalRoot")
    val realFormalRoot = formalRoot.toRealPath()
    val manifest = formalRoot.resolve("validation/DirectSources.mk")
    if (!Files.isRegularFile(manifest)) invalid(s"missing $manifest")
    val lines = Files
      .readString(manifest, StandardCharsets.UTF_8)
      .linesIterator
      .map(_.takeWhile(_ != '#').trim)
      .filter(_.nonEmpty)
      .toList
    if (!lines.contains("DIRECT_GENERATOR_PROVENANCE := direct-itree-v1"))
      invalid("missing direct generator provenance")
    val assignmentIndex = lines.indexWhere(
      _.startsWith("DIRECT_GENERATED_SOURCES :="),
    )
    if (assignmentIndex < 0) invalid("missing DIRECT_GENERATED_SOURCES")
    val sourceLines = lines.drop(assignmentIndex)
    if (
      sourceLines.init
        .exists(!_.endsWith("\\")) || sourceLines.last.endsWith("\\")
    )
      invalid("malformed manifest continuation")
    val assignment = "DIRECT_GENERATED_SOURCES :="
    val entries = sourceLines.zipWithIndex.flatMap { (line, index) =>
      val value = if (index == 0) line.stripPrefix(assignment) else line
      value.stripSuffix("\\").trim.split("\\s+").filter(_.nonEmpty)
    }
    val duplicates = entries
      .groupBy(identity)
      .collect {
        case (entry, copies) if copies.size > 1 => entry
      }
      .toList
      .sorted
    if (duplicates.nonEmpty)
      invalid(s"duplicate source entries: ${duplicates.mkString(", ")}")
    val facadeEntry = "validation/spec_direct/DirectFuncs.v"
    if (!entries.contains(facadeEntry)) invalid(s"manifest omits $facadeEntry")
    val namesEntry = "validation/spec_direct/DirectNames.v"
    if (!entries.contains(namesEntry)) invalid(s"manifest omits $namesEntry")
    // Everything that is not one of the two facades is a function shard.
    // Per-function shards carry no numbering, so contiguity only constrains
    // the numbered layout; what rules out a gap in either layout is the
    // facade/manifest set equality below.
    val shards = entries.filterNot(entry =>
      entry == facadeEntry || entry == namesEntry,
    )
    if (shards.isEmpty) invalid("manifest contains no shard entries")
    val numberedShard = raw"validation/spec_direct/DirectFuncs_(\d{4})\.v".r
    val indices = shards.collect { case numberedShard(index) => index.toInt }
      .sorted
    if (indices.nonEmpty && indices != (0 to indices.last).toList)
      invalid("non-contiguous DirectFuncs shard entries")

    val resolved = entries.map { entry =>
      if (!directSpecSourcePattern.matches(entry))
        invalid(s"unsafe or unexpected source entry: $entry")
      val path = formalRoot.resolve(entry).normalize
      if (!path.startsWith(formalRoot))
        invalid(s"source entry escapes formal: $entry")
      if (!Files.isRegularFile(path)) invalid(s"missing $path")
      if (!path.toRealPath().startsWith(realFormalRoot))
        invalid(s"source entry escapes formal through symlink: $entry")
      path
    }
    val facade = formalRoot.resolve(facadeEntry)
    // Read the export block itself rather than scanning the whole facade:
    // per-function identifiers also appear in the entry definitions below it.
    val imported = raw"(?s)Require Export\s+(.*?)\."
      .r
      .findFirstMatchIn(Files.readString(facade, StandardCharsets.UTF_8))
      .toList
      .flatMap(_.group(1).split("\\s+"))
      .filter(_.nonEmpty)
      .map(module => s"validation/spec_direct/$module.v")
      .toSet
    val manifested = shards.toSet
    if (imported != manifested)
      invalid(
        "facade/manifest shard mismatch: " +
        s"missing imports=${(manifested -- imported).toList.sorted.mkString(",")}, " +
        s"unmanifested imports=${(imported -- manifested).toList.sorted.mkString(",")}",
      )
    val directory = formalRoot.resolve("validation/spec_direct")
    val actualOwned = Option(directory.toFile.listFiles()).toList.flatten
      .filter(_.isFile)
      .map(file => s"validation/spec_direct/${file.getName}")
      .filter(directSpecSourcePattern.matches)
      .toSet
    if (actualOwned != entries.toSet)
      invalid(
        "manifest/file-set mismatch: " +
        s"missing=${(entries.toSet -- actualOwned).toList.sorted.mkString(",")}, " +
        s"unmanifested=${(actualOwned -- entries.toSet).toList.sorted.mkString(",")}",
      )
    resolved.map(_.toFile)
  }

  /** Validate every source named by the generated split-Spec manifest before
    * doing the expensive specification extraction. Manifest paths are kept
    * inside [formal], limited to the generated module namespace, and checked
    * through symlinks as well as lexically.
    */
  private[fv] def validateReusableSplitSpecBase(
    formalDir: File,
  ): List[File] = {
    def invalid(detail: String): Nothing =
      throw new IllegalStateException(
        s"--reuse-test262-base requires the complete split Spec base; $detail",
      )

    val formalRoot = formalDir.toPath.toAbsolutePath.normalize
    if (!Files.isDirectory(formalRoot)) invalid(s"missing $formalRoot")
    val realFormalRoot = formalRoot.toRealPath()
    val validationDir = formalRoot.resolve("validation")
    val specFacade = validationDir.resolve("Spec.v")
    val manifest = validationDir.resolve("SpecSources.mk")
    List(specFacade, manifest).foreach { path =>
      if (!Files.isRegularFile(path)) invalid(s"missing $path")
    }

    val lines =
      Files
        .readString(manifest, StandardCharsets.UTF_8)
        .linesIterator
        .map(_.takeWhile(_ != '#').trim)
        .filter(_.nonEmpty)
        .toList
    val assignment = "SPEC_GENERATED_SOURCES :="
    if (lines.isEmpty || !lines.head.startsWith(assignment))
      invalid(s"invalid ${manifest.getFileName}: missing $assignment")
    if (
      lines.init.exists(!_.endsWith("\\")) ||
      lines.last.endsWith("\\")
    )
      invalid(s"invalid ${manifest.getFileName}: malformed continuation")

    val entries = lines.zipWithIndex.flatMap { (line, index) =>
      val value =
        if (index == 0) line.stripPrefix(assignment)
        else line
      value.stripSuffix("\\").trim.split("\\s+").filter(_.nonEmpty)
    }
    if (entries.isEmpty)
      invalid(s"invalid ${manifest.getFileName}: no source entries")
    val duplicates = entries
      .groupBy(identity)
      .collect {
        case (entry, copies) if copies.size > 1 => entry
      }
      .toList
      .sorted
    if (duplicates.nonEmpty)
      invalid(s"duplicate source entries: ${duplicates.mkString(", ")}")

    val requiredFacades =
      Set(
        "validation/spec/SpecFuncs.v",
        "validation/spec/SpecGlobals.v",
        "validation/spec/SpecHeap.v",
      )
    val missingFacades = requiredFacades -- entries.toSet
    if (missingFacades.nonEmpty)
      invalid(s"manifest omits ${missingFacades.toList.sorted.mkString(", ")}")

    def requireContiguousShards(prefix: String): Set[String] = {
      val shardPattern = raw"validation/spec/${prefix}_(\d{4})\.v".r
      val shards = entries.collect {
        case entry @ shardPattern(index) => entry -> index.toInt
      }
      val indices = shards.map(_._2).sorted
      if (indices.isEmpty)
        invalid(s"manifest contains no $prefix shard entries")
      if (indices != (0 to indices.last).toList)
        invalid(s"non-contiguous $prefix shard entries")
      shards.map(_._1).toSet
    }
    val funcShards = requireContiguousShards("SpecFuncs")
    val heapShards = requireContiguousShards("SpecHeap")

    val resolvedEntries = entries.map { entry =>
      if (!splitSpecSourcePattern.matches(entry))
        invalid(s"unsafe or unexpected source entry: $entry")
      val relative = java.nio.file.Path.of(entry)
      if (relative.isAbsolute)
        invalid(s"absolute source entry: $entry")
      val resolved = formalRoot.resolve(relative).normalize
      if (!resolved.startsWith(formalRoot))
        invalid(s"source entry escapes formal directory: $entry")
      if (!Files.isRegularFile(resolved)) invalid(s"missing $resolved")
      val realResolved = resolved.toRealPath()
      if (!realResolved.startsWith(realFormalRoot))
        invalid(
          s"source entry escapes formal directory through symlink: $entry",
        )
      resolved.toFile
    }

    def requireFacadeMatchesManifest(
      prefix: String,
      manifestShards: Set[String],
    ): Unit = {
      val facade = formalRoot.resolve(s"validation/spec/$prefix.v")
      val modulePattern = raw"\b${prefix}_\d{4}\b".r
      val importedShards = modulePattern
        .findAllIn(Files.readString(facade, StandardCharsets.UTF_8))
        .map(module => s"validation/spec/$module.v")
        .toSet
      if (importedShards != manifestShards) {
        val missing = (manifestShards -- importedShards).toList.sorted
        val extra = (importedShards -- manifestShards).toList.sorted
        invalid(
          s"$prefix facade/manifest shard mismatch: " +
          s"missing imports=${missing.mkString(",")}, " +
          s"unmanifested imports=${extra.mkString(",")}",
        )
      }
    }
    requireFacadeMatchesManifest("SpecFuncs", funcShards)
    requireFacadeMatchesManifest("SpecHeap", heapShards)

    resolvedEntries
  }

  private[fv] def parseExporterRequest(
    line: String,
  ): Either[String, Option[(Int, Int)]] =
    line.trim.split("\\s+").toList match
      case "QUIT" :: Nil => Right(None)
      case "EXPORT" :: offsetText :: countText :: Nil =>
        (offsetText.toIntOption, countText.toIntOption) match
          case (Some(offset), Some(count)) if offset >= 0 && count >= 0 =>
            Right(Some((offset, count)))
          case _ =>
            Left(
              s"invalid nonnegative EXPORT bounds: " +
              s"$offsetText $countText",
            )
      case _ =>
        Left("expected EXPORT <offset> <count> or QUIT")

  private def exporterError(message: String): String =
    Base64.getUrlEncoder.withoutPadding.encodeToString(
      message.getBytes(StandardCharsets.UTF_8),
    )

  private[fv] def skippedAssertBlocker(
    skippedYetAsserts: Int,
    failedToEvaluateAsserts: Int,
  ): Option[String] =
    Option.when(failedToEvaluateAsserts > 0)(
      s"ESMeta silently skipped $failedToEvaluateAsserts non-EYet " +
      "assertion(s) that failed to evaluate; " +
      s"$skippedYetAsserts bare EYet assertion(s) were intentional no-ops",
    )

  /** Test262 positive tests pass only through a normal ECMAScript exit.
    * Capturing a thrown Test262Error as the differential expectation would let
    * both engines agree on an assertion failure and falsely report PASS.
    */
  private[fv] def requireSuccessfulTest262Exit(result: Value): Unit =
    if result != Undef then throw InvalidExit(result)

  /** Keep every exactly representable lexical SDO answer and omit only an
    * answer outside the current Rocq value fragment. A later lookup of an
    * omitted method remains UB, so this lets unrelated code use the same AST
    * without rounding an exact decimal Math value to binary64.
    */
  private[fv] def representableLexicalSdos(
    entries: List[(String, Value)],
  ): List[(String, Value)] = entries.filter {
    case (_, Str(_))                => true
    case (_, Math(decimal))         => decimal.isWhole
    case (_, Number(_) | BigInt(_)) => true
    case (_, Undef)                 => true
    case _                          => false
  }

  /** Stable AST object identities within one exported program.
    *
    * Every generated program has its own identity namespace. Its cached Script
    * root is origin zero because the EParse fast path returns that exact
    * object; all other exported roots are numbered by Java reference identity,
    * never by case-class equality.
    */
  private[fv] final class AstOriginAllocator(cachedRoot: Option[Ast]) {
    private val ids =
      new java.util.IdentityHashMap[Ast, java.lang.Integer]()
    private var next = 1

    cachedRoot.foreach(root => ids.put(root, java.lang.Integer.valueOf(0)))

    def id(root: Ast): Int =
      val existing = ids.get(root)
      if existing != null then existing.intValue
      else
        val fresh = next
        next += 1
        ids.put(root, java.lang.Integer.valueOf(fresh))
        fresh
  }

  /** the six lexical SDOs (Interpreter.scala:525-536) */
  val LEX_SDOS =
    List("StringValue", "NumericValue", "MV", "SV", "TV", "TRV")

  /** Preserve timestamps for source-independent generated files so shard
    * changes do not invalidate the already compiled static Rocq model.
    */
  private def dumpFileIfChanged(data: String, filename: String): Boolean = {
    val file = File(filename)
    if (file.isFile && Files.readString(file.toPath) == data) false
    else {
      dumpFile(data, filename)
      true
    }
  }

  /** Replace only the generated Rocq source files owned by the split Spec
    * exporter. Removed chunks are deleted so a smaller future specification
    * cannot leave ambiguous source modules behind; compiled artifacts are left
    * to the Rocq build system.
    */
  private def dumpSplitSpecFiles(
    directory: String,
    files: List[(String, String)],
  ): (Int, Int) = {
    val dir = File(directory)
    Files.createDirectories(dir.toPath)
    val expected = files.map(_._1).toSet
    val owned = raw"Spec(?:Funcs|Heap)(?:_[0-9]{4})?\.v|SpecGlobals\.v".r
    val stale = Option(dir.listFiles()).toList.flatten.filter { file =>
      file.isFile && owned.matches(file.getName) && !expected(file.getName)
    }
    stale.foreach(file => Files.delete(file.toPath))
    val changed = files.count { (name, contents) =>
      dumpFileIfChanged(contents, s"$directory/$name")
    }
    (changed, stale.size)
  }

  private def dumpDirectSpecFiles(
    directory: String,
    files: List[(String, String)],
  ): (Int, Int) = {
    val dir = File(directory)
    Files.createDirectories(dir.toPath)
    val expected = files.map(_._1).toSet
    val owned = raw"DirectFuncs(?:_[0-9]{4})?\.v|DirectNames\.v".r
    val stale = Option(dir.listFiles()).toList.flatten.filter { file =>
      file.isFile && owned.matches(file.getName) && !expected(file.getName)
    }
    stale.foreach(file => Files.delete(file.toPath))
    val changed = files.count { (name, contents) =>
      dumpFileIfChanged(contents, s"$directory/$name")
    }
    (changed, stale.size)
  }

  /** Primitive results supplied by ESMeta's Scala host rather than by the IR
    * operational semantics. These are inputs to the Rocq execution, never the
    * final differential-test verdict.
    */
  private[fv] enum HostCapture:
    case Parse(
      text: String,
      ruleName: String,
      effectiveParams: List[Boolean],
      result: Value,
    )
    case ToStr(input: Value, radix: Int, result: Value)
    case StrToNumber(input: String, result: Value)
    case StrToBigInt(input: String, result: Value)
    case NumberPow(left: Double, right: Double, result: Value)
    case DoubleToLongChecked(input: Double, result: Value)
    case MathToNumber(input: scala.math.BigInt, result: Number)
    case NumberMathOp(
      op: FVInitState.NumberMathOp,
      left: Double,
      right: Double,
      result: Number,
    )
    case NumberSin(input: Double, result: Number)
    case NumberMathCompare(
      op: FVInitState.NumberMathCompareOp,
      direction: FVInitState.NumberMathCompareDirection,
      number: Double,
      integer: scala.math.BigInt,
      result: Bool,
    )
    case NumberToMath(input: Double, result: Math)
    case MathOp(
      op: esmeta.ir.MOp,
      args: List[BigDecimal],
      result: ExtMath,
    )

  private val dynamicParseLock = new AnyRef

  /** Capture every successful host primitive exactly where the ordinary ESMeta
    * interpreter would perform it. Subexpressions are evaluated through this
    * override, so nested host calls are captured too, once and in the same
    * left-to-right order as Interpreter.eval.
    */
  private[fv] class HostCapturingInterpreter(st0: State)
    extends FVExport.CapturingInterpreter(st0) {
    private[FVInitState] val hostEntries: ListBuffer[HostCapture] = ListBuffer()

    private[fv] def capturedHostEntryCount: Int = hostEntries.size

    private[fv] def capturedHostEntries: List[HostCapture] =
      hostEntries.toList

    // ESParser instances are shared through CFG. Keep only the parser call
    // itself serialized; the surrounding interpreter and AST processing stay
    // parallel and operate on task-owned state.
    private def parseDynamic(
      name: String,
      params: List[Boolean],
      source: String,
    ): Ast = FVInitState.dynamicParseLock.synchronized {
      esParser(name, params).from(source)
    }

    private[fv] def sameQuery(left: HostCapture, right: HostCapture): Boolean =
      (left, right) match
        case (
              HostCapture.Parse(lt, ln, lp, _),
              HostCapture.Parse(rt, rn, rp, _),
            ) =>
          lt == rt && ln == rn && lp == rp
        case (
              HostCapture.ToStr(li, lradix, _),
              HostCapture.ToStr(ri, rradix, _),
            ) =>
          li == ri && lradix == rradix
        case (
              HostCapture.StrToNumber(li, _),
              HostCapture.StrToNumber(ri, _),
            ) =>
          li == ri
        case (
              HostCapture.StrToBigInt(li, _),
              HostCapture.StrToBigInt(ri, _),
            ) =>
          li == ri
        case (
              HostCapture.NumberPow(ll, lr, _),
              HostCapture.NumberPow(rl, rr, _),
            ) =>
          esmeta.util.BaseUtils.doubleEquals(ll, rl) &&
          esmeta.util.BaseUtils.doubleEquals(lr, rr)
        case (
              HostCapture.DoubleToLongChecked(li, _),
              HostCapture.DoubleToLongChecked(ri, _),
            ) =>
          esmeta.util.BaseUtils.doubleEquals(li, ri)
        case (
              HostCapture.MathToNumber(li, _),
              HostCapture.MathToNumber(ri, _),
            ) =>
          li == ri
        case (
              HostCapture.NumberMathOp(lo, ll, lr, _),
              HostCapture.NumberMathOp(ro, rl, rr, _),
            ) =>
          lo == ro &&
          esmeta.util.BaseUtils.doubleEquals(ll, rl) &&
          esmeta.util.BaseUtils.doubleEquals(lr, rr)
        case (
              HostCapture.NumberSin(li, _),
              HostCapture.NumberSin(ri, _),
            ) =>
          esmeta.util.BaseUtils.doubleEquals(li, ri)
        case (
              HostCapture.NumberMathCompare(lo, ld, ln, li, _),
              HostCapture.NumberMathCompare(ro, rd, rn, ri, _),
            ) =>
          lo == ro && ld == rd &&
          esmeta.util.BaseUtils.doubleEquals(ln, rn) && li == ri
        case (
              HostCapture.NumberToMath(li, _),
              HostCapture.NumberToMath(ri, _),
            ) =>
          esmeta.util.BaseUtils.doubleEquals(li, ri)
        case (
              HostCapture.MathOp(lo, la, _),
              HostCapture.MathOp(ro, ra, _),
            ) =>
          lo == ro && la == ra
        case _ => false

    private def record(entry: HostCapture): Unit =
      hostEntries.find(sameQuery(_, entry)) match
        case Some(old) if old != entry =>
          throw IllegalStateException(
            s"host operation produced inconsistent results: $old / $entry",
          )
        case Some(_) => ()
        case None    => hostEntries += entry

    /** Evaluate exactly the generated Number-method terminal composite without
      * repeating either operand. Conversion and Math arithmetic mirror the
      * ordinary Interpreter cases; only two raw Number operands form a typed
      * host-cache query.
      */
    private def evalNumberMathComposite(
      op: NumberMathOp,
      bop: esmeta.ir.BOp,
      leftExpr: esmeta.ir.Expr,
      rightExpr: esmeta.ir.Expr,
    ): Value = {
      val left = eval(leftExpr)
      def toMath(value: Value, expr: esmeta.ir.Expr): Math = value match
        case CodeUnit(c)             => Math(c.toInt)
        case Math(n)                 => Math(n)
        case Number(n) if n.isFinite => Math(n)
        case BigInt(n)               => Math(n)
        case other =>
          throw esmeta.error.InvalidConversion(
            esmeta.ir.COp.ToMath,
            expr,
            other,
          )
      val leftMath = toMath(left, leftExpr)
      val right = eval(rightExpr)
      val rightMath = toMath(right, rightExpr)
      (left, right) match
        case (_: Number, _: Number) => ()
        case (Number(number), _) if leftMath.decimal.isWhole =>
          record(HostCapture.NumberToMath(number, leftMath))
        case (_, Number(number)) if rightMath.decimal.isWhole =>
          record(HostCapture.NumberToMath(number, rightMath))
        case _ => ()
      val mathResult =
        Interpreter.eval(bop, leftMath, rightMath)
      val result = mathResult match
        case Math(n) => Number(n.toDouble)
        case Infinity(pos) =>
          Number(if (pos) Double.PositiveInfinity else Double.NegativeInfinity)
        case other =>
          throw IllegalStateException(
            s"unexpected Number Math composite result: $op($left, $right) = $other",
          )
      (left, right) match
        case (Number(l), Number(r)) =>
          record(HostCapture.NumberMathOp(op, l, r, result))
        case _ => ()
      result
    }

    private def evalNumberSinComposite(inner: esmeta.ir.Expr): Value = {
      val input = eval(inner)
      input match
        case Number(n) if n.isFinite =>
          val mathResult = Interpreter.eval(esmeta.ir.MOp.Sin, List(Math(n)))
          val result = mathResult match
            case Math(value) => Number(value.toDouble)
            case Infinity(pos) =>
              Number(
                if (pos) Double.PositiveInfinity else Double.NegativeInfinity,
              )
          record(HostCapture.NumberSin(n, result))
          result
        case other =>
          val mathInput = other match
            case CodeUnit(c)             => Math(c.toInt)
            case Math(n)                 => Math(n)
            case Number(n) if n.isFinite => Math(n)
            case BigInt(n)               => Math(n)
            case invalid =>
              throw esmeta.error.InvalidConversion(
                esmeta.ir.COp.ToMath,
                inner,
                invalid,
              )
          val mathResult = Interpreter.eval(esmeta.ir.MOp.Sin, List(mathInput))
          mathResult match
            case Math(value) if !mathInput.decimal.isWhole || !value.isWhole =>
              throw FVExport.Unsupported(
                s"non-integer Math host result: Sin($mathInput) = $value",
              )
            case _ =>
              record(
                HostCapture.MathOp(
                  esmeta.ir.MOp.Sin,
                  List(mathInput.decimal),
                  mathResult,
                ),
              )
              mathResult match
                case Math(value) => Number(value.toDouble)
                case Infinity(pos) =>
                  Number(
                    if (pos) Double.PositiveInfinity
                    else Double.NegativeInfinity,
                  )
    }

    private def evalNumberMathComparisonComposite(
      bop: esmeta.ir.BOp,
      leftExpr: esmeta.ir.Expr,
      rightExpr: esmeta.ir.Expr,
    ): Value = {
      val op = bop match
        case esmeta.ir.BOp.Lt    => NumberMathCompareOp.Lt
        case esmeta.ir.BOp.Equal => NumberMathCompareOp.Equal
        case _ =>
          throw IllegalArgumentException(s"not a Math comparison: $bop")
      def toMath(value: Value, expr: esmeta.ir.Expr): Math = value match
        case CodeUnit(c)             => Math(c.toInt)
        case Math(n)                 => Math(n)
        case Number(n) if n.isFinite => Math(n)
        case BigInt(n)               => Math(n)
        case other =>
          throw esmeta.error.InvalidConversion(
            esmeta.ir.COp.ToMath,
            expr,
            other,
          )

      val left = eval(leftExpr)
      val leftMath = toMath(left, leftExpr)
      val right = eval(rightExpr)
      val rightMath = toMath(right, rightExpr)
      val result = Interpreter.eval(bop, leftMath, rightMath).asInstanceOf[Bool]
      (left, right) match
        case (_: Number, _: Number) => ()
        case (Number(number), _) if rightMath.decimal.isWhole =>
          record(
            HostCapture.NumberMathCompare(
              op,
              NumberMathCompareDirection.NumberLeft,
              number,
              rightMath.decimal.toBigInt,
              result,
            ),
          )
        case (_, Number(number)) if leftMath.decimal.isWhole =>
          record(
            HostCapture.NumberMathCompare(
              op,
              NumberMathCompareDirection.NumberRight,
              number,
              leftMath.decimal.toBigInt,
              result,
            ),
          )
        case _ => ()
      result
    }

    override def eval(expr: esmeta.ir.Expr): Value = expr match
      case esmeta.ir.EBinary(
            bop @ (esmeta.ir.BOp.Lt | esmeta.ir.BOp.Equal),
            esmeta.ir.EConvert(esmeta.ir.COp.ToMath, left),
            esmeta.ir.EConvert(esmeta.ir.COp.ToMath, right),
          ) =>
        evalNumberMathComparisonComposite(bop, left, right)

      case esmeta.ir.EConvert(esmeta.ir.COp.ToMath, inner) =>
        val input = eval(inner)
        input match
          case CodeUnit(c) => Math(c.toInt)
          case m: Math     => m
          case Number(number) if number.isFinite =>
            val result = Math(number)
            if result.decimal.isWhole then
              record(HostCapture.NumberToMath(number, result))
            result
          case BigInt(integer) => Math(integer)
          case other =>
            throw esmeta.error.InvalidConversion(
              esmeta.ir.COp.ToMath,
              inner,
              other,
            )

      case esmeta.ir.EConvert(
            esmeta.ir.COp.ToApproxNumber,
            esmeta.ir.EMathOp(
              esmeta.ir.MOp.Sin,
              List(esmeta.ir.EConvert(esmeta.ir.COp.ToMath, inner)),
            ),
          ) =>
        evalNumberSinComposite(inner)

      case esmeta.ir.EConvert(
            esmeta.ir.COp.ToNumber,
            esmeta.ir.EBinary(
              bop @ (esmeta.ir.BOp.Add | esmeta.ir.BOp.Mul | esmeta.ir.BOp.Div),
              esmeta.ir.EConvert(esmeta.ir.COp.ToMath, left),
              esmeta.ir.EConvert(esmeta.ir.COp.ToMath, right),
            ),
          ) =>
        val op = bop match
          case esmeta.ir.BOp.Add => NumberMathOp.Add
          case esmeta.ir.BOp.Mul => NumberMathOp.Mul
          case esmeta.ir.BOp.Div => NumberMathOp.Div
        evalNumberMathComposite(op, bop, left, right)

      case esmeta.ir.EConvert(
            esmeta.ir.COp.ToApproxNumber,
            esmeta.ir.EBinary(
              esmeta.ir.BOp.Pow,
              esmeta.ir.EConvert(esmeta.ir.COp.ToMath, left),
              esmeta.ir.EConvert(esmeta.ir.COp.ToMath, right),
            ),
          ) =>
        evalNumberMathComposite(
          NumberMathOp.Pow,
          esmeta.ir.BOp.Pow,
          left,
          right,
        )

      case esmeta.ir.ETrim(inner, isStarting) =>
        // ETrim only reads the parser's immutable whitespace code-point sets.
        // Do not hold the dynamic parser lock across operand evaluation.
        Str(trimString(eval(inner).asStr, isStarting, esParser))
      case esmeta.ir.EParse(code, rule) =>
        var hostQuery: Option[(String, String, List[Boolean])] = None
        val result =
          try
            // Keep both operand evaluations within the original catch
            // boundary and in their original left-to-right order.
            val input = eval(code)
            val grammarSymbol = eval(rule).asGrammarSymbol
            val GrammarSymbol(name, params) = grammarSymbol
            (input, grammarSymbol, this.st.sourceText, this.st.cachedAst) match
              case (
                    Str(x),
                    GrammarSymbol("Script", Nil),
                    Some(y),
                    Some(cached),
                  ) if x == y =>
                AstValue(cached)
              case (Str(x), _, _, _) =>
                hostQuery = Some((x, name, params))
                val parsed = parseDynamic(name, params, x)
                parsed.clearLoc
                AstValue(parsed)
              case (AstValue(sourceAst), _, _, _) =>
                val text =
                  sourceAst.toString(grammar = Some(this.st.cfg.grammar))
                val parserArgs =
                  if (params.isEmpty) sourceAst.getArgs else params
                hostQuery = Some((text, name, parserArgs))
                val parsed = parseDynamic(name, parserArgs, text)
                sourceAst.loc.map(parsed.rebaseLoc)
                AstValue(parsed)
              case (other, _, _, _) =>
                throw esmeta.error.InvalidParseSource(code, other)
          catch case _: Throwable => this.st.allocList(Nil)

        hostQuery.foreach { (text, ruleName, effectiveParams) =>
          val capturedResult = result match
            case ast: AstValue => ast
            // The Rocq host interpreter turns this sentinel into a fresh
            // empty List object, matching Interpreter's catch result.
            case _ => Undef
          record(
            HostCapture.Parse(
              text,
              ruleName,
              effectiveParams,
              capturedResult,
            ),
          )
        }
        result

      case esmeta.ir.EConvert(
            cop @ esmeta.ir.COp.ToStr(radixOpt),
            inner,
          ) =>
        val input = eval(inner)
        input match
          // Interpreter.scala:279 does not evaluate radix for an existing
          // string; preserving that detail prevents spurious effects/UB.
          case Str(s) => Str(s)
          case Number(d) =>
            val radix = radixOpt.fold(10)(e => eval(e).asInt)
            val result = Str(toStringHelper(d, radix))
            record(HostCapture.ToStr(input, radix, result))
            result
          case BigInt(n) =>
            val radix = radixOpt.fold(10)(e => eval(e).asInt)
            val result = Str(n.toString(radix))
            record(HostCapture.ToStr(input, radix, result))
            result
          case other =>
            throw esmeta.error.InvalidConversion(cop, inner, other)

      case esmeta.ir.EConvert(
            cop @ (esmeta.ir.COp.ToNumber | esmeta.ir.COp.ToApproxNumber),
            inner,
          ) =>
        val input = eval(inner)
        input match
          case Str(s) if cop == esmeta.ir.COp.ToNumber =>
            val result = ESValueParser.str2number(s)
            record(HostCapture.StrToNumber(s, result))
            result
          case Infinity(true) =>
            Number(Double.PositiveInfinity)
          case Infinity(false) =>
            Number(Double.NegativeInfinity)
          case Math(n) =>
            val double = n.toDouble
            val result = Number(double)
            if needsMathToNumberCapture(n) then
              record(HostCapture.MathToNumber(n.toBigInt, result))
            result
          case n: Number => n
          case other =>
            throw esmeta.error.InvalidConversion(
              cop,
              inner,
              other,
            )

      case esmeta.ir.EConvert(esmeta.ir.COp.ToBigInt, inner) =>
        val input = eval(inner)
        input match
          case Str(s) =>
            val result = ESValueParser.str2bigint(s)
            record(HostCapture.StrToBigInt(s, result))
            result
          case Math(n)   => BigInt(n.toBigInt)
          case Number(n) => BigInt(BigDecimal.exact(n).toBigInt)
          case n: BigInt => n
          case other =>
            throw esmeta.error.InvalidConversion(
              esmeta.ir.COp.ToBigInt,
              inner,
              other,
            )

      case esmeta.ir.EBinary(esmeta.ir.BOp.Pow, left, right) =>
        val leftValue = eval(left)
        val rightValue = eval(right)
        val result = Interpreter.eval(
          esmeta.ir.BOp.Pow,
          leftValue,
          rightValue,
        )
        (leftValue, rightValue, result) match
          case (Number(l), Number(r), value: Number) =>
            record(HostCapture.NumberPow(l, r, value))
          case _ => ()
        result

      case esmeta.ir.EMathOp(mop, exprs) =>
        val args = exprs.map(eval)
        val result = Interpreter.eval(mop, args)
        val mathArgs = args.collect { case Math(value) => value }
        if (mathArgs.size != args.size)
          throw IllegalStateException(
            s"unexpected successful MathOp arguments: $mop($args)",
          )
        if (mathArgs.exists(!_.isWhole))
          throw FVExport.Unsupported(
            s"non-integer Math host input: $mop($mathArgs)",
          )
        result match
          case Math(value) if !value.isWhole =>
            throw FVExport.Unsupported(
              s"non-integer Math host result: $mop($mathArgs) = $value",
            )
          case _ => record(HostCapture.MathOp(mop, mathArgs, result))
        result

      case esmeta.ir.EKeys(map, true) =>
        // Evaluate the map expression exactly once.  Calling super.eval after
        // inspecting it would repeat any effects in the expression.
        val addr = eval(map).asAddr
        this.st(addr) match
          case m: MapObj =>
            val indexed = for {
              case (Str(s), _) <- m.map.toVector
              classified = Obj.classifyIntegerKey(s)
              _ = record(
                HostCapture.StrToNumber(s, classified.number),
              )
              _ = record(
                HostCapture.ToStr(
                  classified.number,
                  10,
                  Str(classified.rendered),
                ),
              )
              _ = classified.checkedLong.foreach { checked =>
                val result =
                  checked.fold[Value](Undef)(i => Math(i))
                record(
                  HostCapture.DoubleToLongChecked(
                    classified.number.double,
                    result,
                  ),
                )
              }
              i <- classified.index
            } yield (s, i)
            this.st.allocList(
              indexed.sortBy(_._2).map { case (s, _) => Str(s) },
            )
          // Preserve Record behavior and List/other-object UB without
          // evaluating `map` a second time.
          case _ => this.st.keys(addr, intSorted = true)

      case _ => super.eval(expr)
  }

  def main(args: Array[String]): Unit = {
    val source = sourceForArgs(args)
    val reuseTest262Base = args.contains("--reuse-test262-base")
    val directOutput = args.contains(DIRECT_OUTPUT_OPTION)
    if (directOutput && reuseTest262Base)
      throw new IllegalArgumentException(
        s"$DIRECT_OUTPUT_OPTION cannot reuse generic Test262 artifacts; " +
        "direct generation must compile the complete current function domain",
      )
    val payloadOnly = args.contains("--payload-only")
    val test262ExportJobs = parseTest262ExportJobs(args)
    val reusableSplitSpecSources =
      Option.when(reuseTest262Base)(
        validateReusableSplitSpecBase(new File(s"$BASE_DIR/formal")),
      )
    println("[fv] extracting spec and building CFG")
    val cfg = CFGBuilder(Compiler(Extractor()))
    given CFG = cfg
    val st = Initialize(cfg).from(source)

    // ---- addresses: Addr -> list position (ADR-16) -------------------
    // Every address REFERENCED anywhere gets a slot, even one the heap
    // does not map (ESMeta's initial globals contain #CandidateExecution,
    // which is dangling): the slot exists so allocation cannot later reuse
    // that index, but it holds None so dereferencing is stuck, matching
    // ESMeta's UnknownAddr.
    def refsOf(v: Value): List[Addr] = v match
      case a: Addr     => List(a)
      case Clo(_, cap) => cap.values.toList.flatMap(refsOf)
      case _           => Nil
    val referenced = (
      st.globals.values.toList.flatMap(refsOf) ++
        st.heap.map.values.toList.flatMap {
          case RecordObj(_, m) => m.values.toList.flatMap(refsOf)
          case ListObj(vs)     => vs.toList.flatMap(refsOf)
          case MapObj(m) => m.toList.flatMap((k, v) => refsOf(k) ++ refsOf(v))
          case _         => Nil
        }
    ).toSet
    val mapped = st.heap.map.keySet.toSet
    val dangling = (referenced -- mapped).toList
    val ord: Addr => (Int, String, Long) = {
      case NamedAddr(n)   => (0, n, 0L)
      case DynamicAddr(l) => (1, "", l)
    }
    val addrs = mapped.toList.sortBy(ord) ++ dangling.sortBy(ord)
    val addrIdx: Map[Addr, Int] = addrs.zipWithIndex.toMap
    if (dangling.nonEmpty) {
      val examples = dangling.sortBy(ord).take(8).mkString(", ")
      val omitted = dangling.size - 8
      println(
        s"[fv] referenced but unmapped addresses (slot = None): " +
        s"${dangling.size}; examples: $examples" +
        (if (omitted > 0) s", ... ($omitted more)" else ""),
      )
    }
    if (st.heap.size != 0)
      println(s"[fv] WARNING: heap counter is ${st.heap.size}, not 0")

    // The base specification and every Test262 entry are separate programs,
    // hence separate exported-reference namespaces.
    val astOrigins = new ThreadLocal[AstOriginAllocator] {
      override def initialValue(): AstOriginAllocator =
        new AstOriginAllocator(st.cachedAst)
    }
    def astOriginId(root: Ast): Int = astOrigins.get().id(root)

    def value(v: Value): String = valueWith(v, cstrLit)

    /** Emit values occurring in per-test parse caches without expanding their
      * UTF-16 strings or AST source slices into explicit Rocq lists.
      */
    def valueWith(v: Value, emitCstr: String => String): String = v match
      case addr: Addr =>
        addrIdx.get(addr) match
          case Some(i) => s"(VAddr ${FVExport.natLit(i)})"
          case None    => throw Unsupported(s"unmapped address: $addr")
      case Clo(f, captured) =>
        val cs = captured.toList.sortBy(_._1.name).map { (n, cv) =>
          s"(${strLit(n.name)}, ${valueWith(cv, emitCstr)})"
        }
        s"(VClo ${strLit(f.name)} ${coqList(cs)})"
      case Math(d) =>
        if (!d.isWhole) throw Unsupported(s"non-integer Math value: $d")
        s"(VMath ${zLit(d.toBigInt)})"
      case Bool(b)     => s"(VBool $b)"
      case Str(s)      => s"(VStr ${emitCstr(s)})"
      case Undef       => "VUndef"
      case Null        => "VNull"
      case Enum(n)     => s"(VEnum ${strLit(n)})"
      case Number(d)   => s"(VNumber ${floatLit(d)})"
      case BigInt(n)   => s"(VBigInt ${zLit(n)})"
      case CodeUnit(c) => s"(VCodeUnit ${c.toInt})"
      case Infinity(p) => s"(VInfinity $p)"
      case GrammarSymbol(n, ps) =>
        s"(VGrammarSymbol ${strLit(n)} ${coqList(ps.map(_.toString))})"
      case AstValue(a) => astCursorWith(a, emitCstr)
      case _ => throw Unsupported(s"state value: ${v.getClass.getSimpleName}")

    def hostEntryWith(
      entry: HostCapture,
      emitCstr: String => String,
    ): String = entry match
      case HostCapture.Parse(text, ruleName, effectiveParams, result) =>
        s"(mkHostCacheEntry " +
        s"(HQParseText ${emitCstr(text)} ${strLit(ruleName)} " +
        s"${coqList(effectiveParams.map(_.toString))}) " +
        s"${valueWith(result, emitCstr)})"
      case HostCapture.ToStr(input, radix, result) =>
        s"(mkHostCacheEntry " +
        s"(HQToStr ${valueWith(input, emitCstr)} " +
        s"${zLit(scala.math.BigInt(radix))}) " +
        s"${valueWith(result, emitCstr)})"
      case HostCapture.StrToNumber(input, result) =>
        s"(mkHostCacheEntry " +
        s"(HQStrToNumber ${emitCstr(input)}) " +
        s"${valueWith(result, emitCstr)})"
      case HostCapture.StrToBigInt(input, result) =>
        s"(mkHostCacheEntry " +
        s"(HQStrToBigInt ${emitCstr(input)}) " +
        s"${valueWith(result, emitCstr)})"
      case HostCapture.NumberPow(left, right, result) =>
        s"(mkHostCacheEntry " +
        s"(HQNumberPow ${floatLit(left)} ${floatLit(right)}) " +
        s"${valueWith(result, emitCstr)})"
      case HostCapture.DoubleToLongChecked(input, result) =>
        s"(mkHostCacheEntry " +
        s"(HQDoubleToLongChecked ${floatLit(input)}) " +
        s"${valueWith(result, emitCstr)})"
      case HostCapture.MathToNumber(input, result) =>
        s"(mkHostCacheEntry (HQMathToNumber ${zLit(input)}) " +
        s"${valueWith(result, emitCstr)})"
      case HostCapture.NumberMathOp(op, left, right, result) =>
        s"(mkHostCacheEntry " +
        s"(HQNumberMathOp ${rocqNumberMathOp(op)} " +
        s"${floatLit(left)} ${floatLit(right)}) " +
        s"${valueWith(result, emitCstr)})"
      case HostCapture.NumberSin(input, result) =>
        s"(mkHostCacheEntry (HQNumberSin ${floatLit(input)}) " +
        s"${valueWith(result, emitCstr)})"
      case HostCapture.NumberMathCompare(
            op,
            direction,
            number,
            integer,
            result,
          ) =>
        s"(mkHostCacheEntry " +
        s"(HQNumberMathCompare ${rocqNumberMathCompareOp(op)} " +
        s"${rocqNumberMathCompareDirection(direction)} " +
        s"${floatLit(number)} ${zLit(integer)}) " +
        s"${valueWith(result, emitCstr)})"
      case HostCapture.NumberToMath(input, result) =>
        s"(mkHostCacheEntry (HQNumberToMath ${floatLit(input)}) " +
        s"${valueWith(result, emitCstr)})"
      case HostCapture.MathOp(op, args, result) =>
        def exactMath(d: BigDecimal): String =
          if (d.isWhole) zLit(d.toBigInt)
          else throw Unsupported(s"non-integer Math host value: $d")
        s"(mkHostCacheEntry " +
        s"(HQMathOp ${FVExport.rocqMOp(op)} " +
        s"${coqList(args.map(exactMath))}) " +
        s"${valueWith(result, emitCstr)})"

    def obj(o: Obj): String = o match
      case RecordObj(tname, m) =>
        val fs = m.toList.map { (f, v) => s"(${strLit(f)}, ${value(v)})" }
        s"(ORecord ${strLit(tname)} ${coqList(fs)})"
      case ListObj(vs) => s"(OList ${coqList(vs.toList.map(value))})"
      case MapObj(m) =>
        val es = m.toList.map { (k, v) => s"(${value(k)}, ${value(v)})" }
        s"(OMap ${coqList(es)})"
      case _ => throw Unsupported(s"obj: ${o.getClass.getSimpleName}")

    /** an AST node with everything grammar-derived precomputed */
    def ast(a: Ast): String = astWith(a, cstrLit)

    final case class AstCursorData(
      originId: Int,
      root: Ast,
      rootToLeaf: List[Int],
    )

    /** Export an ESMeta AstValue as a parsed-tree cursor. Parser-created ASTs
      * carry mutable parent links; following them by reference identity
      * reconstructs the unique root and reverse child-index path. A
      * broken/cyclic parent graph is rejected instead of approximated.
      */
    def astCursorData(a: Ast): AstCursorData =
      val seen = new java.util.IdentityHashMap[Ast, java.lang.Boolean]()
      @annotation.tailrec
      def ascend(cur: Ast, revPath: List[Int]): (Ast, List[Int]) =
        if seen.put(cur, java.lang.Boolean.TRUE) != null then
          throw Unsupported("cyclic AST parent chain")
        cur.parent match
          case None => (cur, revPath)
          case Some(parent) =>
            val idx = parent.children.indexWhere {
              case Some(child) => child eq cur
              case None        => false
            }
            if idx < 0 then
              throw Unsupported("AST parent does not contain child")
            ascend(parent, idx :: revPath)
      val (root, rootToLeaf) = ascend(a, Nil)
      val revPath = rootToLeaf.reverse
      AstCursorData(astOriginId(root), root, revPath)

    def astCursorWith(a: Ast, emitCstr: String => String): String =
      val cursor = astCursorData(a)
      s"(VAst (AstExported ${FVExport.natLit(cursor.originId)}) " +
      s"${astWith(cursor.root, emitCstr)} " +
      s"${coqList(cursor.rootToLeaf.map(FVExport.natLit))})"

    def lexicalSdos(lex: Lexical): List[(String, Value)] =
      representableLexicalSdos(
        LEX_SDOS.flatMap { method =>
          optional(Interpreter.eval(lex, method)).map(value => method -> value)
        },
      )

    def syntacticChildNames(name: String, rhsIdx: Int): List[String] =
      cfg.grammar.nameMap(name).rhsVec(rhsIdx).nts.map(_.name).toList

    /** The Test262 exporter supplies a compact [cstr] emitter so repeated AST
      * source slices remain native strings until extracted execution.
      */
    def astWith(a: Ast, emitCstr: String => String): String = a match
      case lex @ Lexical(name, str) =>
        val tbl = lexicalSdos(lex).map { (m, v) =>
          val lv = v match
            case Str(s)               => s"(LVStr ${emitCstr(s)})"
            case Math(d) if d.isWhole => s"(LVMath ${zLit(d.toBigInt)})"
            case Number(d)            => s"(LVNumber ${floatLit(d)})"
            case BigInt(n)            => s"(LVBigInt ${zLit(n)})"
            case Undef                => "LVUndef"
            case other =>
              throw Unsupported(s"lexical SDO $m -> $other")
          s"(${strLit(m)}, $lv)"
        }
        val parseSrc = a.toString(grammar = Some(cfg.grammar))
        val src = parseSrc.trim
        s"(ALex ${strLit(name)} ${strLit(str)} ${emitCstr(src)} " +
        s"${emitCstr(parseSrc)} ${coqList(tbl)})"
      case syn @ Syntactic(name, sargs, rhsIdx, children) =>
        val cs = children.toList.map {
          case Some(c) => s"(Some ${astWith(c, emitCstr)})"
          case None    => "None"
        }
        val childNames = syntacticChildNames(name, rhsIdx).map(strLit)
        val parseSrc = a.toString(grammar = Some(cfg.grammar))
        val src = parseSrc.trim
        s"(ASyn ${strLit(name)} ${coqList(sargs.map(_.toString))} " +
        s"${FVExport.natLit(rhsIdx)} ${FVExport.natLit(syn.subIdx)} " +
        s"${coqList(cs)} ${coqList(childNames)} ${emitCstr(src)} " +
        s"${emitCstr(parseSrc)})"

    /** Directly encode the extracted Fragment datatypes. Constructor tags are
      * part of FVPayload version 7 and are mirrored by payload_codec.ml.
      */
    def writeLexValue(
      method: String,
      value: Value,
      out: FVPayload.Encoder,
    ): Unit = value match
      case Str(s) =>
        out.tag(0)
        out.cstr(s)
      case Math(d) if d.isWhole =>
        out.tag(1)
        out.integer(d.toBigInt)
      case Number(d) =>
        out.tag(2)
        out.float64(d)
      case BigInt(n) =>
        out.tag(3)
        out.integer(n)
      case Undef =>
        out.tag(4)
      case other =>
        throw Unsupported(s"lexical SDO $method -> $other")

    def writeAstPayload(a: Ast, out: FVPayload.Encoder): Unit = a match
      case lex @ Lexical(name, str) =>
        out.tag(1)
        out.utf8(name)
        out.utf8(str)
        val parseSrc = a.toString(grammar = Some(cfg.grammar))
        out.cstr(parseSrc.trim)
        out.cstr(parseSrc)
        out.list(lexicalSdos(lex)) { (method, result) =>
          out.utf8(method)
          writeLexValue(method, result, out)
        }
      case syn @ Syntactic(name, sargs, rhsIdx, children) =>
        out.tag(0)
        out.utf8(name)
        out.list(sargs)(out.bool)
        out.nat(rhsIdx)
        out.nat(syn.subIdx)
        out.list(children.toList) {
          case None =>
            out.tag(0)
          case Some(child) =>
            out.tag(1)
            writeAstPayload(child, out)
        }
        out.list(syntacticChildNames(name, rhsIdx))(out.utf8)
        val parseSrc = a.toString(grammar = Some(cfg.grammar))
        out.cstr(parseSrc.trim)
        out.cstr(parseSrc)

    def writeValuePayload(v: Value, out: FVPayload.Encoder): Unit = v match
      case value: ExtMath =>
        writeExtMathValuePayload(value, out)
      case Bool(b) =>
        out.tag(1)
        out.bool(b)
      case Str(s) =>
        out.tag(2)
        out.cstr(s)
      case Undef =>
        out.tag(3)
      case Null =>
        out.tag(4)
      case Enum(name) =>
        out.tag(5)
        out.utf8(name)
      case addr: Addr =>
        out.tag(6)
        addrIdx.get(addr) match
          case Some(index) => out.nat(index)
          case None        => throw Unsupported(s"unmapped address: $addr")
      case Clo(func, captured) =>
        out.tag(7)
        out.utf8(func.name)
        out.list(captured.toList.sortBy(_._1.name)) { (name, value) =>
          out.utf8(name.name)
          writeValuePayload(value, out)
        }
      case AstValue(ast) =>
        val cursor = astCursorData(ast)
        out.tag(9)
        out.tag(0) // AstExported
        out.nat(cursor.originId)
        writeAstPayload(cursor.root, out)
        out.list(cursor.rootToLeaf)(out.nat)
      case Number(number) =>
        out.tag(10)
        out.float64(number)
      case BigInt(integer) =>
        out.tag(11)
        out.integer(integer)
      case CodeUnit(unit) =>
        out.tag(13)
        out.u16(unit.toInt)
      case GrammarSymbol(name, params) =>
        out.tag(14)
        out.utf8(name)
        out.list(params)(out.bool)
      case _ =>
        throw Unsupported(s"state value: ${v.getClass.getSimpleName}")

    def writeHostPayload(
      entry: HostCapture,
      out: FVPayload.Encoder,
    ): Unit = entry match
      case HostCapture.Parse(text, ruleName, effectiveParams, result) =>
        out.tag(0)
        out.cstr(text)
        out.utf8(ruleName)
        out.list(effectiveParams)(out.bool)
        writeValuePayload(result, out)
      case HostCapture.ToStr(input, radix, result) =>
        out.tag(1)
        writeValuePayload(input, out)
        out.integer(scala.math.BigInt(radix))
        writeValuePayload(result, out)
      case HostCapture.StrToNumber(input, result) =>
        out.tag(2)
        out.cstr(input)
        writeValuePayload(result, out)
      case HostCapture.NumberPow(left, right, result) =>
        out.tag(3)
        out.float64(left)
        out.float64(right)
        writeValuePayload(result, out)
      case HostCapture.DoubleToLongChecked(input, result) =>
        out.tag(4)
        out.float64(input)
        writeValuePayload(result, out)
      case HostCapture.StrToBigInt(input, result) =>
        out.tag(5)
        out.cstr(input)
        writeValuePayload(result, out)
      case HostCapture.MathOp(op, args, result) =>
        out.tag(6)
        out.tag(mathOpTag(op))
        out.list(args) { value =>
          if (value.isWhole) out.integer(value.toBigInt)
          else throw Unsupported(s"non-integer Math host input: $value")
        }
        writeValuePayload(result, out)
      case HostCapture.MathToNumber(input, result) =>
        out.tag(mathToNumberHostTag)
        out.integer(input)
        writeValuePayload(result, out)
      case HostCapture.NumberMathOp(op, left, right, result) =>
        out.tag(numberMathHostTag)
        out.tag(numberMathOpTag(op))
        out.float64(left)
        out.float64(right)
        writeValuePayload(result, out)
      case HostCapture.NumberSin(input, result) =>
        out.tag(numberSinHostTag)
        out.float64(input)
        writeValuePayload(result, out)
      case HostCapture.NumberMathCompare(
            op,
            direction,
            number,
            integer,
            result,
          ) =>
        out.tag(numberMathCompareHostTag)
        out.tag(numberMathCompareOpTag(op))
        out.tag(numberMathCompareDirectionTag(direction))
        out.float64(number)
        out.integer(integer)
        writeValuePayload(result, out)
      case HostCapture.NumberToMath(input, result) =>
        out.tag(numberToMathHostTag)
        out.float64(input)
        writeValuePayload(result, out)

    def encodeTestPayload(
      globalIndex: Int,
      relName: String,
      code: String,
      tast: Ast,
      hosts: List[HostCapture],
      result: Value,
      prints: List[Value],
    ): Array[Byte] =
      FVPayload.encode(globalIndex) { out =>
        out.utf8(relName)
        out.cstr(code)
        writeAstPayload(tast, out)
        out.list(hosts)(writeHostPayload(_, out))
        writeValuePayload(result, out)
        out.list(prints)(writeValuePayload(_, out))
      }

    // Anything the model cannot represent faithfully is made STUCK, never
    // approximated: an object with unrepresentable content becomes an
    // unmapped slot, and an unrepresentable global is omitted.  Touching
    // either is then undefined behaviour instead of a wrong answer.
    def tryEmit(f: => String): Option[String] =
      try Some(f)
      catch { case Unsupported(_) => None }

    val specOut = s"$BASE_DIR/formal/validation/Spec.v"
    val splitSpecDir = s"$BASE_DIR/formal/validation/spec"
    val splitSpecManifest =
      s"$BASE_DIR/formal/validation/SpecSources.mk"
    val directSpecDir = s"$BASE_DIR/formal/validation/spec_direct"
    val directSpecManifest = s"$BASE_DIR/formal/validation/DirectSources.mk"
    if (reuseTest262Base) {
      println(
        s"[fv] reusing $specOut and " +
        s"${reusableSplitSpecSources.fold(0)(_.size)} split sources",
      )
    } else {
      // ---- spec functions --------------------------------------------
      val funcDefs = ListBuffer[(String, String)]()
      val exportedFuncNames = scala.collection.mutable.Set[String]()
      val directEmissions = ListBuffer[DirectFunctionEmission]()
      val directOmitted = ListBuffer[String]()
      var skipped = 0
      val skipReasons = scala.collection.mutable.Map[String, Int]()
      val fnamesTerm = "direct_spec_fnames"
      val directIds =
        if (directOutput) directFunIds(cfg.program.funcs) else Nil
      for ((f, i) <- cfg.program.funcs.zipWithIndex) {
        try {
          if (directOutput) {
            val normalized = FVExport.normalizeForRocq(f)
            funcDefs += ((s"sf_$i", FVExport.rocqNormalizedFunc(normalized)))
            val direct =
              FVDirectExport.compileNormalized(directIds(i), normalized)
            directEmissions += DirectFunctionEmission(
              funId = direct.funId,
              gallinaId = directIds(i),
              source = direct.source,
              ordinaryEntry = direct.ordinaryEntry(fnamesTerm),
              continuationEntry = direct.continuationEntry(fnamesTerm),
              isMain = direct.main,
              mainEntry = direct
                .mainEntry(fnamesTerm)
                .map(value => s"(entry, $value)"),
            )
          } else funcDefs += ((s"sf_$i", FVExport.rocqFunc(f)))
          exportedFuncNames += f.name
        } catch {
          case Unsupported(msg) =>
            // Direct output used to require every function.  It now omits the
            // same functions the generic exporter omits, declaring them so
            // validateDirectDomains can still account for the whole domain.
            // A call to an omitted function is UB in the model, which is
            // honest: it is a function we do not have.
            if (directOutput) directOmitted += f.name
            skipped += 1
            val k = msg.takeWhile(_ != ':')
            skipReasons(k) = skipReasons.getOrElse(k, 0) + 1
        }
      }
      println(
        s"[fv] spec functions: ${funcDefs.size} exported, $skipped omitted",
      )
      if (directOutput && directOmitted.nonEmpty)
        println(
          s"[fv] direct omitted ${directOmitted.size}: " +
          directOmitted.mkString(", "),
        )
      val mainF = cfg.program.funcs.filter(_.main)
      println(
        s"[fv] main function(s): " + mainF
          .map(f =>
            s"${f.name} params=${f.params.size} " +
            (if (exportedFuncNames(f.name)) "EXPORTED"
             else "OMITTED"),
          )
          .mkString(", "),
      )
      for ((k, n) <- skipReasons.toList.sortBy(-_._2)) {
        println(f"[fv]   $n%5d  $k")
      }

      // Each translated function is closed with respect to generated [sf_i]
      // constants: calls remain symbolic [EClo] names and are resolved through
      // [spec_funcs].  Keep this invariant executable so future exporter
      // changes cannot silently introduce an inter-chunk Rocq dependency.
      val generatedFuncRef = raw"\bsf_[0-9]+\b".r
      val crossRefs = funcDefs.toList.flatMap { (name, body) =>
        generatedFuncRef.findAllIn(body).map(ref => s"$name->$ref")
      }
      if (crossRefs.nonEmpty)
        throw new IllegalStateException(
          "generated spec functions unexpectedly reference sf_i constants: " +
          crossRefs.take(20).mkString(", "),
        )
      println("[fv] verified: generated sf_i definitions are mutually closed")

      // ---- assemble split compilation units --------------------------
      val generatedHeader =
        s"""(* AUTO-GENERATED by `sbt "runMain esmeta.fv.FVInitState"`.
 *
 * This is one compilation unit of the ECMAScript IR-Core specification and
 * initial state.  Import validation/Spec.v for the stable public facade.
 *)
From Stdlib Require Import String ZArith List Floats PString.
Import ListNotations.
From ESMetaFV Require Import Fragment Domain TestEncoding.
Local Open Scope string_scope.
Local Open Scope Z_scope.

"""
      val facadeHeader =
        s"""(* AUTO-GENERATED by `sbt "runMain esmeta.fv.FVInitState"`.
 * Stable facade for the split ECMAScript IR-Core specification.
 *
 * source text : ${strLit(source)}
 * spec funcs  : ${funcDefs.size} exported, $skipped omitted
 * heap        : ${mapped.size} objects, ${dangling.size} unmapped slots
 * globals     : ${st.globals.size}
 *)
From Stdlib Require Import String ZArith List Floats PString.
Import ListNotations.
From ESMetaFV Require Import Fragment Domain TestEncoding.
From ESMetaFV.validation.spec Require Export SpecFuncs SpecGlobals SpecHeap.
Local Open Scope string_scope.
Local Open Scope Z_scope.

"""
      val chunkSize = 32
      val specFuncChunks =
        funcDefs.toList.grouped(chunkSize).map(_.toList).toList
      def chunkSuffix(idx: Int): String = f"$idx%04d"
      val funcFiles = specFuncChunks.zipWithIndex.map { (chunk, idx) =>
        val suffix = chunkSuffix(idx)
        val body = new StringBuilder(generatedHeader)
        chunk.foreach { (name, definition) =>
          body ++= s"Definition $name : func :=\n  $definition.\n"
        }
        body ++= s"\nDefinition spec_funcs_chunk_$suffix : list func :=\n  "
        body ++= coqList(chunk.map(_._1))
        body ++= ".\n"
        s"SpecFuncs_$suffix.v" -> body.toString
      }
      val funcsFacade = new StringBuilder(generatedHeader)
      if (specFuncChunks.nonEmpty) {
        funcsFacade ++= "From ESMetaFV.validation.spec Require Export\n  "
        funcsFacade ++= specFuncChunks.indices
          .map(idx => s"SpecFuncs_${chunkSuffix(idx)}")
          .mkString(" ")
        funcsFacade ++= ".\n\n"
        for (idx <- specFuncChunks.indices.reverse) {
          val suffix = chunkSuffix(idx)
          val tail =
            if (idx + 1 < specFuncChunks.size)
              s"spec_funcs_tail_${chunkSuffix(idx + 1)}"
            else "nil"
          funcsFacade ++= s"Definition spec_funcs_tail_$suffix : list func :=\n"
          funcsFacade ++= s"  spec_funcs_chunk_$suffix ++ $tail.\n"
        }
      }
      funcsFacade ++= "\nDefinition spec_funcs : list func :=\n  "
      funcsFacade ++=
        (if (specFuncChunks.isEmpty) "nil"
         else s"spec_funcs_tail_${chunkSuffix(0)}")
      funcsFacade ++= ".\n"

      // SOURCE_TEXT is the only global that varies with the script, so it is
      // left out here: Spec.v is source-independent and compiles once, and a
      // per-test file prepends its own SOURCE_TEXT.
      val gPairs = st.globals.toList
        .filter(_._1.name != "SOURCE_TEXT")
        .sortBy(_._1.name)
        .map { (g, v) =>
          g.name -> tryEmit(s"(${strLit(g.name)}, ${value(v)})")
        }
      val droppedGlobals = gPairs.collect { case (n, None) => n }
      if (droppedGlobals.nonEmpty)
        println(
          "[fv] globals omitted as unrepresentable (reads are stuck): " +
          droppedGlobals.mkString(", "),
        )
      val globalsFile = new StringBuilder(generatedHeader)
      globalsFile ++= s"Definition base_globals : list (String.string * val) :=\n  "
      globalsFile ++= coqList(gPairs.flatMap(_._2))
      globalsFile ++= ".\n"

      var droppedObjs = 0
      val heapTerms = addrs.map { a =>
        st.heap.map.get(a) match
          case None => "None"
          case Some(o) =>
            tryEmit(s"(Some ${obj(o)})").getOrElse {
              droppedObjs += 1
              "None"
            }
      }
      if (droppedObjs > 0)
        println(
          s"[fv] heap objects unrepresentable, slot left unmapped: " +
          s"$droppedObjs",
        )
      val heapChunks = heapTerms.grouped(chunkSize).map(_.toList).toList
      val heapFiles = heapChunks.zipWithIndex.map { (chunk, idx) =>
        val suffix = chunkSuffix(idx)
        val body = new StringBuilder(generatedHeader)
        body ++= s"Definition init_heap_chunk_$suffix : list (option obj) :=\n  "
        body ++= coqList(chunk)
        body ++= ".\n"
        s"SpecHeap_$suffix.v" -> body.toString
      }
      val heapFacade = new StringBuilder(generatedHeader)
      if (heapChunks.nonEmpty) {
        heapFacade ++= "From ESMetaFV.validation.spec Require Export\n  "
        heapFacade ++= heapChunks.indices
          .map(idx => s"SpecHeap_${chunkSuffix(idx)}")
          .mkString(" ")
        heapFacade ++= ".\n\n"
        for (idx <- heapChunks.indices.reverse) {
          val suffix = chunkSuffix(idx)
          val tail =
            if (idx + 1 < heapChunks.size)
              s"init_heap_tail_${chunkSuffix(idx + 1)}"
            else "nil"
          heapFacade ++= s"Definition init_heap_tail_$suffix : list (option obj) :=\n"
          heapFacade ++= s"  init_heap_chunk_$suffix ++ $tail.\n"
        }
      }
      heapFacade ++= "\nDefinition init_heap : list (option obj) :=\n  "
      heapFacade ++=
        (if (heapChunks.isEmpty) "nil"
         else s"init_heap_tail_${chunkSuffix(0)}")
      heapFacade ++= ".\n"

      // A script becomes a `prog` by supplying its source, parsed AST and
      // exact host-operation inputs; everything else above is shared.
      val specFacade = new StringBuilder(facadeHeader)
      specFacade ++= """Definition script_prog
  (src : cstr) (a : ast) (hosts : list host_cache_entry) : prog :=
  mkProgFull spec_funcs (Some src) (Some a) hosts
    (("SOURCE_TEXT", VStr src) :: base_globals) init_heap.
"""

      val splitFiles =
        funcFiles ++ heapFiles ++ List(
          "SpecFuncs.v" -> funcsFacade.toString,
          "SpecGlobals.v" -> globalsFile.toString,
          "SpecHeap.v" -> heapFacade.toString,
        )
      val (splitChanged, splitRemoved) =
        dumpSplitSpecFiles(splitSpecDir, splitFiles)
      val manifestEntries =
        splitFiles.map(_._1).sorted.map(name => s"validation/spec/$name")
      val manifest = new StringBuilder
      manifest ++= "# AUTO-GENERATED by FVInitState; do not edit.\n"
      manifest ++= "SPEC_GENERATED_SOURCES := \\\n"
      manifest ++= manifestEntries.zipWithIndex.map { (path, idx) =>
        val continuation = if (idx + 1 < manifestEntries.size) " \\" else ""
        s"  $path$continuation\n"
      }.mkString
      val manifestChanged =
        dumpFileIfChanged(manifest.toString, splitSpecManifest)
      val specChanged = dumpFileIfChanged(specFacade.toString, specOut)
      val specAction = if (specChanged) "wrote" else "unchanged"
      println(
        s"[fv] split Spec: ${funcFiles.size} function chunks, " +
        s"${heapFiles.size} heap chunks, $splitChanged files changed, " +
        s"$splitRemoved stale files removed; manifest " +
        s"${if (manifestChanged) "changed" else "unchanged"}",
      )
      println(
        s"[fv] $specAction $specOut (${specFacade.length / 1024} KiB)",
      )
      if (directOutput) {
        val artifacts = renderDirectSplitArtifacts(
          directEmissions.toList,
          chunkSize = 1,
          expectedFunIds = cfg.program.funcs.map(_.name),
          omittedFunIds = directOmitted.toList,
        )
        val (changed, removed) =
          dumpDirectSpecFiles(directSpecDir, artifacts.files)
        val manifestChanged =
          dumpFileIfChanged(artifacts.manifest, directSpecManifest)
        val validated =
          validateDirectSplitSpecBase(new File(s"$BASE_DIR/formal"))
        println(
          s"[fv] direct Spec: ${artifacts.files.size - 2} contiguous shards, " +
          s"$changed files changed, $removed stale files removed; manifest " +
          s"${if (manifestChanged) "changed" else "unchanged"}; " +
          s"validated ${validated.size} sources",
        )
      }
    }

    // ---- fixed JS source-equivalence witnesses ------------------------
    // These artifacts are intentionally produced by ESMeta's real Script
    // parser and ordinary interpreter.  Rocq subsequently proves facts about
    // the exported ASTs and host answers; the parser/export step remains an
    // explicit trusted frontend boundary rather than a hand-written IR test.
    if (args.headOption.contains("--js-equiv")) {
      val cases = List(
        (
          "constant_condition_left",
          s"$BASE_DIR/tests/fv/js-equiv/constant-condition-left.js",
          None,
        ),
        (
          "constant_condition_right",
          s"$BASE_DIR/tests/fv/js-equiv/constant-condition-right.js",
          None,
        ),
        (
          "optional_chain_left",
          s"$BASE_DIR/tests/fv/js-equiv/optional-chain-left.js",
          None,
        ),
        (
          "optional_chain_right",
          s"$BASE_DIR/tests/fv/js-equiv/optional-chain-right.js",
          None,
        ),
        (
          "asi_optional_chain_implicit",
          s"$BASE_DIR/tests/fv/js-equiv/asi-optional-chain-implicit.js",
          None,
        ),
        (
          "asi_optional_chain_explicit",
          s"$BASE_DIR/tests/fv/js-equiv/asi-optional-chain-explicit.js",
          Some("asi_optional_chain_implicit"),
        ),
      )
      val out = new StringBuilder
      out ++= """(* AUTO-GENERATED by `sbt "runMain esmeta.fv.FVInitState --js-equiv"`.
 *
 * Each input below was parsed by ESMeta's real Script parser and then run by
 * ESMeta before its effective source, cached AST, and exact host-operation
 * answers were emitted.  The frontend/export relation is a trusted artifact
 * boundary; the corresponding ITree execution proofs are in JSEquivProof.v.
 *)
From Stdlib Require Import String ZArith List Floats PString.
Import ListNotations.
From ESMetaFV Require Import Fragment Domain TestEncoding.
Local Open Scope string_scope.
Local Open Scope Z_scope.

"""
      val renderedWitnesses = scala.collection.mutable.Map[
        String,
        (String, String, String, String),
      ]()
      for ((name, path, aliasOf) <- cases) {
        val input = Files.readString(File(path).toPath, StandardCharsets.UTF_8)
        val (tast, effectiveSource) =
          dynamicParseLock.synchronized {
            cfg.scriptParser.fromWithCode(input)
          }
        val testState =
          Initialize(cfg).from(effectiveSource, tast, Some(path))
        val interp = new HostCapturingInterpreter(testState)
        val finalState = interp.result
        val result = finalState.globals.getOrElse(GLOBAL_RESULT, Undef)
        requireSuccessfulTest262Exit(result)
        if (interp.prints.nonEmpty)
          throw Unsupported(
            s"JS equivalence witness $name unexpectedly printed " +
            s"${interp.prints.size} value(s)",
          )
        skippedAssertBlocker(
          interp.skippedYetAsserts,
          interp.failedToEvaluateAsserts,
        ).foreach(reason => throw Unsupported(s"$name: $reason"))

        astOrigins.set(new AstOriginAllocator(Some(tast)))
        try {
          val hosts = interp.hostEntries.toList.map(hostEntryWith(_, cstrLit))
          val sourceTerm = cstrLit(effectiveSource)
          val astTerm = astWith(tast, cstrLit)
          val hostsTerm = coqList(hosts)
          out ++= s"Definition ${name}_input_source : cstr :=\n  " +
          s"${cstrLit(input)}.\n"
          aliasOf match {
            case None =>
              out ++= s"Definition ${name}_source : cstr :=\n  " +
              s"$sourceTerm.\n"
              out ++= s"Definition ${name}_ast : ast :=\n  $astTerm.\n"
              out ++=
                s"Definition ${name}_hosts : list host_cache_entry :=\n  " +
                s"$hostsTerm.\n\n"
            case Some(baseName) =>
              val (baseInput, baseSource, baseAst, baseHosts) =
                renderedWitnesses.getOrElse(
                  baseName,
                  throw new IllegalStateException(
                    s"JS equivalence alias $name refers to unavailable $baseName",
                  ),
                )
              if (input == baseInput)
                throw new IllegalStateException(
                  s"JS equivalence alias $name must have distinct input bytes",
                )
              if (
                sourceTerm != baseSource || astTerm != baseAst ||
                hostsTerm != baseHosts
              ) {
                val differences = List(
                  Option.when(sourceTerm != baseSource)("effective source"),
                  Option.when(astTerm != baseAst)("AST"),
                  Option.when(hostsTerm != baseHosts)("host answers"),
                ).flatten.mkString(", ")
                throw new IllegalStateException(
                  s"JS equivalence alias $name does not match $baseName after " +
                  s"parsing: $differences differ",
                )
              }
              out ++=
                s"Definition ${name}_source : cstr := ${baseName}_source.\n"
              out ++= s"Definition ${name}_ast : ast := ${baseName}_ast.\n"
              out ++=
                s"Definition ${name}_hosts : list host_cache_entry := " +
                s"${baseName}_hosts.\n\n"
          }
          renderedWitnesses(name) = (input, sourceTerm, astTerm, hostsTerm)
        } finally astOrigins.remove()
        println(
          s"[fv] JS equivalence witness $name: normal exit, " +
          s"${interp.hostEntries.size} host entr${if (interp.hostEntries.size == 1) "y"
          else "ies"}" + aliasOf.fold("")(base => s", aliases $base"),
        )
      }
      val path = s"$BASE_DIR/formal/validation/JSEquivArtifacts.v"
      val changed = dumpFileIfChanged(out.toString, path)
      println(s"[fv] ${if (changed) "wrote" else "unchanged"} $path")
      return
    }

    // ---- probe: every EParse this run performs -------------------------
    if (args.headOption.contains("--parse-probe")) {
      val path = args.lift(1).getOrElse("")
      val t262 =
        esmeta.test262.Test262(esmeta.test262.Test262.getVersion(None), cfg)
      val (past, pcode) =
        if (path.isEmpty) (st.cachedAst.get, source)
        else t262.loadTest(s"${esmeta.TEST262_TEST_DIR}/$path")
      val pst = Initialize(cfg).from(pcode, past)
      val hist = scala.collection.mutable.Map[String, Int]()
      val probe = new Interpreter(pst, timeLimit = Some(60)) {
        override def eval(expr: esmeta.ir.Expr): Value = expr match
          case esmeta.ir.EParse(code, rule) =>
            val c = super.eval(code)
            val r = super.eval(rule)
            val cached = (c, r, this.st.sourceText, this.st.cachedAst) match
              case (Str(x), GrammarSymbol("Script", Nil), Some(y), Some(_)) =>
                if (x == y) "cached-hit"
                else s"cached-MISS(len ${x.length} vs ${y.length})"
              case (_, r2, _, _) => s"not-Script(rule=$r2)"
            hist(cached) = hist.getOrElse(cached, 0) + 1
            super.eval(expr)
          case _ => super.eval(expr)
      }
      try probe.result
      catch { case e: Throwable => println(s"[fv] run ended: $e") }
      println(s"[fv] EParse evaluations during the run:")
      for ((k, c) <- hist.toList.sortBy(-_._2)) println(f"[fv]   $c%4d  $k")
      return
    }

    // ---- Test262 batch mode -------------------------------------------
    // Spec.v above is source-independent, so it compiles once and every
    // test is a small file that supplies only its source, its AST and its
    // expected observable (produced by running ESMeta).
    if (
      args.headOption.contains("--test262") ||
      args.headOption.contains("--test262-shard") ||
      args.headOption.contains("--test262-server")
    ) {
      val shardMode = args.headOption.contains("--test262-shard")
      val serverMode = args.headOption.contains("--test262-server")
      val batchMode = shardMode || serverMode
      val filt =
        args
          .lift(if (serverMode) 1 else if (shardMode) 3 else 2)
          .filterNot(_.startsWith("--"))
      val t262 =
        esmeta.test262.Test262(esmeta.test262.Test262.getVersion(None), cfg)
      def shortReason(reason: String): String =
        reason
          .replace('\t', ' ')
          .replace('\r', ' ')
          .replace('\n', ' ')
          .trim
          .take(240)
      val fullTargetPool = t262.allTargetTests.sortBy(_.relName)
      val fullTargetWithIndex = fullTargetPool.zipWithIndex
      val pool =
        if (batchMode)
          fullTargetWithIndex.filter((test, _) =>
            filt.fold(true)(test.relName.startsWith),
          )
        else
          fullTargetWithIndex.filter((test, _) =>
            filt.fold(test.relName.startsWith("language/"))(
              test.relName.startsWith,
            ),
          )
      if (pool.isEmpty && !batchMode) {
        println("[fv] no tests matched")
        return
      }
      if (batchMode && reuseTest262Base) {
        val inventory =
          new File(s"$BASE_DIR/formal/validation/test262-inventory.tsv")
        if (!inventory.isFile)
          throw new IllegalStateException(
            s"--reuse-test262-base requires existing ${inventory.getPath}",
          )
        println(s"[fv] reusing ${inventory.getPath}")
      } else if (batchMode) {
        val removedByName = t262.allRemoved.iterator.map {
          case (test, reasonPath) => test.relName -> reasonPath
        }.toMap
        val targetIndexByName =
          fullTargetWithIndex.iterator
            .map((test, idx) => test.relName -> idx)
            .toMap
        val reasonHistogram =
          t262.allRemoved.iterator
            .map((_, reasonPath) => shortReason(reasonPath.mkString("/")))
            .toList
            .groupMapReduce(identity)(_ => 1)(_ + _)
        val inventory = new StringBuilder
        inventory ++=
          s"# allTests=${t262.allTests.size}\ttarget=${fullTargetPool.size}\t" +
          s"filtered=${t262.allRemoved.size}\n"
        for (
          (reason, count) <- reasonHistogram.toList.sortBy {
            case (reason, count) => (-count, reason)
          }
        )
          inventory ++= s"# reason[$reason]=$count\n"
        inventory ++=
          "allTestIndex\tglobalTargetIndex\trelName\tdisposition\treason\n"
        for ((test, allIdx) <- t262.allTests.sortBy(_.relName).zipWithIndex)
          targetIndexByName.get(test.relName) match
            case Some(globalIdx) =>
              inventory ++=
                s"$allIdx\t$globalIdx\t${test.relName}\tTARGET\t\n"
            case None =>
              val reason = removedByName
                .get(test.relName)
                .map(_.mkString("/"))
                .map(shortReason)
                .getOrElse("missing filter reason")
              inventory ++=
                s"$allIdx\t-\t${test.relName}\tFILTERED\t$reason\n"
        dumpFile(
          inventory.toString,
          s"$BASE_DIR/formal/validation/test262-inventory.tsv",
        )
        println(
          s"[fv] wrote formal/validation/test262-inventory.tsv " +
          s"(${t262.allTests.size} total, ${fullTargetPool.size} target, " +
          s"${t262.allRemoved.size} filtered)",
        )
      }

      def emitBatch(
        want: Int,
        shardCount: Int,
        shardMode: Boolean,
      ): Unit = {
        if (shardMode && (want < 0 || shardCount < 0)) {
          throw new IllegalArgumentException(
            s"invalid test262 shard bounds: offset=$want count=$shardCount",
          )
        }
        val selected =
          if (shardMode) {
            val end = math
              .min(
                pool.size.toLong,
                want.toLong + shardCount.toLong,
              )
              .toInt
            pool.slice(want, end).map((test, globalIdx) => (globalIdx, test))
          } else {
            pool
              .grouped(math.max(1, pool.size / want))
              .map(_.head)
              .take(want)
              .map((test, globalIdx) => (globalIdx, test))
              .toList
          }
        if (shardMode)
          println(
            s"[fv] test262 shard: poolSize=${pool.size}, offset=$want, " +
            s"requested=$shardCount, selected=${selected.size}, " +
            s"bounds=[$want, ${want.toLong + shardCount.toLong})",
          )
        var esmetaFailed, notRepresentable = 0
        var nonYetSkippedAssertions = 0
        final case class EmittedEntry(
          globalIndex: Int,
          relName: String,
          rocqTerm: Option[String],
          payload: Array[Byte],
        )
        val entries = ListBuffer[EmittedEntry]()
        val shardRows = ListBuffer[(Int, String, String, String)]()
        // Preserve Scala/ECMAScript's UTF-16 code units exactly while keeping
        // the generated Rocq term compact.  Extract.v decodes four lowercase
        // hexadecimal digits back to one [cstr] element at the OCaml boundary.
        def utf16Hex(s: String): String =
          s.toCharArray.iterator.map(c => f"${c.toInt}%04x").mkString
        def compactCstr(s: String): String =
          s"(utf16_hex ${strLit(utf16Hex(s))}%pstring)"

        sealed trait PreparedEntry
        final case class PreparedFailure(
          globalIndex: Int,
          relName: String,
          reason: String,
        ) extends PreparedEntry
        final case class PreparedUnsupported(
          globalIndex: Int,
          relName: String,
          reason: String,
          failedToEvaluateAsserts: Int,
        ) extends PreparedEntry
        final case class PreparedEmission(entry: EmittedEntry)
          extends PreparedEntry

        def esmetaFailure(
          globalIndex: Int,
          relName: String,
          error: Throwable,
        ): PreparedFailure =
          PreparedFailure(
            globalIndex,
            relName,
            shortReason(
              s"${error.getClass.getSimpleName}: " +
              Option(error.getMessage).getOrElse(""),
            ),
          )

        def preparedFailure(
          globalIndex: Int,
          relName: String,
          error: Throwable,
          failedToEvaluateAsserts: Int = 0,
        ): PreparedEntry =
          classifyTest262Failure(error) match
            case Test262FailureClass.NotRepresentable(reason) =>
              PreparedUnsupported(
                globalIndex,
                relName,
                shortReason(reason),
                failedToEvaluateAsserts,
              )
            case Test262FailureClass.ESMetaFailed =>
              esmetaFailure(globalIndex, relName, error)

        // Prewarm all shared lazy/cache entries read without locking below.
        ESElem.getStringifier(true, false, Some(cfg.grammar))
        cfg.esParser.WhiteSpaceCPs
        cfg.esParser.LineTerminatorCPs

        val preparedEntries =
          mapTest262WithJobs(selected.toList, test262ExportJobs) {
            case (globalIdx, test) =>
              val relName = test.relName
              val path = s"${esmeta.TEST262_TEST_DIR}/$relName"
              val loaded =
                try {
                  Right(
                    dynamicParseLock.synchronized {
                      t262.loadTest(path, test.includes)
                    },
                  )
                } catch {
                  case NonFatal(e) => Left(esmetaFailure(globalIdx, relName, e))
                }
              loaded match
                case Left(failure) => failure
                case Right((tast, code)) =>
                  var activeInterpreter: Option[HostCapturingInterpreter] = None
                  val evaluated =
                    try {
                      val tst = Initialize(cfg).from(code, tast)
                      val ti = new HostCapturingInterpreter(tst)
                      activeInterpreter = Some(ti)
                      val fin = ti.result
                      val res = fin.globals.getOrElse(GLOBAL_RESULT, Undef)
                      requireSuccessfulTest262Exit(res)
                      Right((ti, res))
                    } catch {
                      case NonFatal(e) =>
                        Left(
                          preparedFailure(
                            globalIdx,
                            relName,
                            e,
                            activeInterpreter.fold(0)(
                              _.failedToEvaluateAsserts,
                            ),
                          ),
                        )
                    }
                  evaluated match
                    case Left(failure) => failure
                    case Right((ti, res)) =>
                      val hosts = ti.hostEntries.toList
                      val prints = ti.prints.toList
                      try {
                        skippedAssertBlocker(
                          ti.skippedYetAsserts,
                          ti.failedToEvaluateAsserts,
                        ).foreach(reason => throw Unsupported(reason))
                        // Each renderer gets a fresh worker-local identity namespace;
                        // the cached Script root remains origin zero in both.
                        val rocqTerm =
                          if payloadOnly then None
                          else {
                            astOrigins.set(new AstOriginAllocator(Some(tast)))
                            val hostTerms =
                              hosts.map(hostEntryWith(_, compactCstr))
                            Some(
                              s"(${strLit(relName)}, ${compactCstr(code)}, " +
                              s"${astWith(tast, compactCstr)}, " +
                              s"${coqList(hostTerms)}, " +
                              s"(${value(res)}, ${coqList(prints.map(value))}))",
                            )
                          }
                        astOrigins.set(new AstOriginAllocator(Some(tast)))
                        val payload = encodeTestPayload(
                          globalIdx,
                          relName,
                          code,
                          tast,
                          hosts,
                          res,
                          prints,
                        )
                        PreparedEmission(
                          EmittedEntry(globalIdx, relName, rocqTerm, payload),
                        )
                      } catch {
                        case Unsupported(reason) =>
                          PreparedUnsupported(
                            globalIdx,
                            relName,
                            shortReason(reason),
                            ti.failedToEvaluateAsserts,
                          )
                      } finally astOrigins.remove()
          }

        // Ordered aggregation is the only place that mutates counters,
        // manifests, and (below) output files.
        for (prepared <- preparedEntries) {
          prepared match
            case PreparedFailure(globalIdx, relName, reason) =>
              esmetaFailed += 1
              if (shardMode)
                shardRows += (
                  (
                    globalIdx,
                    relName,
                    "ESMETA_FAILED",
                    reason,
                  ),
                )
            case PreparedUnsupported(
                  globalIdx,
                  relName,
                  reason,
                  failedToEvaluateAsserts,
                ) =>
              notRepresentable += 1
              nonYetSkippedAssertions += failedToEvaluateAsserts
              if (shardMode)
                shardRows += (
                  (
                    globalIdx,
                    relName,
                    "NOT_REPRESENTABLE",
                    reason,
                  ),
                )
            case PreparedEmission(entry) =>
              entries += entry
              if (shardMode)
                shardRows += (
                  (
                    entry.globalIndex,
                    entry.relName,
                    "EMITTED",
                    "",
                  ),
                )
        }
        val entryType =
          "String.string * cstr * ast * list host_cache_entry * " +
          "(val * list val)"
        final case class EmittedArtifact(
          entry: EmittedEntry,
          localIndex: Int,
          id: String,
          payloadFile: String,
        )
        val payloadDir =
          new File(s"$BASE_DIR/formal/validation/payload")
        payloadDir.mkdirs()
        Option(payloadDir.listFiles())
          .getOrElse(Array.empty[File])
          .filter(file => file.isFile && file.getName.matches("T[0-9]+\\.fvt"))
          .foreach(_.delete())
        val artifacts = entries.zipWithIndex.map { (entry, idx) =>
          val id = f"$idx%03d"
          val relative = s"validation/payload/T$id.fvt"
          FVPayload.writeAtomic(
            new File(s"$BASE_DIR/formal/$relative").toPath,
            entry.payload,
          )
          EmittedArtifact(entry, idx, id, relative)
        }.toList

        // The default dual-output mode retains the old Rocq payload path solely
        // as a cross-check oracle while the new format is being audited.
        // Production campaigns pass --payload-only and skip this entire block.
        val entryModules =
          if payloadOnly then Nil
          else {
            val itreeDir =
              new File(s"$BASE_DIR/formal/validation/itree")
            itreeDir.mkdirs()
            Option(itreeDir.listFiles())
              .getOrElse(Array.empty[File])
              .filter(file =>
                file.isFile && file.getName.matches("T[0-9]+\\.v"),
              )
              .foreach(_.delete())
            artifacts.map { artifact =>
              val moduleName = s"T${artifact.id}"
              val definitionName = s"test_${artifact.id}"
              val entry = artifact.entry.rocqTerm.getOrElse(
                throw IllegalStateException(
                  s"missing Rocq cross-check term for $moduleName",
                ),
              )
              val moduleBody =
                s"""(* AUTO-GENERATED Test262 ITree payload ${artifact.id}. *)
From Stdlib Require Import String ZArith List Floats PString.
Import ListNotations.
From ESMetaFV Require Import Fragment Domain TestEncoding.
Local Open Scope string_scope.
Local Open Scope Z_scope.

Definition $definitionName : ($entryType) :=
  $entry.
"""
              dumpFile(moduleBody, s"${itreeDir.getPath}/$moduleName.v")
              (
                moduleName,
                definitionName,
                artifact.entry.globalIndex,
              )
            }
          }
        if (!payloadOnly) {
          val entryImports =
            if (entryModules.isEmpty) ""
            else
              "From ESMetaFV.validation.itree Require Import " +
              entryModules.map(_._1).mkString(" ") + ".\n"
          val entryNames =
            entryModules.map((moduleName, definitionName, _) =>
              s"$moduleName.$definitionName",
            )
          val invocation =
            if (shardMode) "--test262-shard OFFSET COUNT [prefix]"
            else "--test262 N [prefix]"
          val body =
            s"""(* AUTO-GENERATED by `sbt "runMain esmeta.fv.FVInitState $invocation"`.
 *
 * One entry per Test262 test: its name, its (semicolon-injected) source,
 * its parsed AST, and the observable ESMeta produced for it — the RESULT
 * global and the print trace.  Every source slice is represented by a
 * compact [utf16_hex] call and reconstructed as [cstr] during extracted
 * execution.  ESMeta is the oracle; nothing here is our expectation.
 *
 * emitted: ${entries.size}, ESMeta could not run: $esmetaFailed,
 * not representable by the model: $notRepresentable,
 * non-EYet assertions silently skipped: $nonYetSkippedAssertions
 *)
From Stdlib Require Import String List.
Import ListNotations.
From ESMetaFV Require Import Fragment.
$entryImports
Local Open Scope string_scope.

Definition tests : list ($entryType) :=
  ${coqList(entryNames)}.
"""
          dumpFile(body, s"$BASE_DIR/formal/validation/Tests.v")
        }
        if (shardMode) {
          val emittedLocalByGlobal =
            artifacts
              .map(artifact =>
                artifact.entry.globalIndex -> artifact.localIndex,
              )
              .toMap
          val payloadByGlobal =
            artifacts
              .map(artifact =>
                artifact.entry.globalIndex -> artifact.payloadFile,
              )
              .toMap
          val manifest = new StringBuilder
          manifest ++=
            s"# poolSize=${pool.size}\toffset=$want\trequested=$shardCount\t" +
            s"selected=${selected.size}\temitted=${entries.size}\n"
          manifest ++=
            "globalIndex\tlocalModuleIndex\tpayloadFile\trelName\t" +
            "disposition\treason\n"
          for (
            (globalIdx, relName, disposition, reason) <-
              shardRows.toList.sortBy(_._1)
          )
            manifest ++=
              s"$globalIdx\t${emittedLocalByGlobal.get(globalIdx).fold("-")(idx => f"$idx%03d")}\t${payloadByGlobal
                .getOrElse(globalIdx, "-")}\t" +
              s"$relName\t$disposition\t$reason\n"
          dumpFile(
            manifest.toString,
            s"$BASE_DIR/formal/validation/test262-shard.tsv",
          )
          println(
            s"[fv] wrote formal/validation/test262-shard.tsv " +
            s"(${shardRows.size} selected test(s))",
          )
        }
        println(
          s"[fv] test262: ${entries.size} test(s) -> " +
          s"formal/validation/payload/T*.fvt",
        )
        if (!payloadOnly)
          println(
            "[fv]   Rocq cross-check artifacts -> " +
            "formal/validation/itree/T*.v + validation/Tests.v",
          )
        println(
          s"[fv]   ESMeta could not run: $esmetaFailed, " +
          s"not representable: $notRepresentable, non-EYet assertions " +
          s"silently skipped: $nonYetSkippedAssertions " +
          "(failures are not counted as results)",
        )
      }

      if (serverMode) {
        println(s"FVEXPORT READY 1 ${pool.size}")
        System.out.flush()
        val lines = scala.io.Source.stdin.getLines()
        var running = true
        while (running && lines.hasNext) {
          val line = lines.next()
          parseExporterRequest(line) match
            case Right(None) =>
              println("FVEXPORT BYE")
              System.out.flush()
              running = false
            case Right(Some((offset, count))) =>
              if (offset > pool.size || count > pool.size - offset) {
                val reason =
                  s"export range [$offset, ${offset.toLong + count}) " +
                  s"escapes pool size ${pool.size}"
                println(
                  s"FVEXPORT ERROR $offset $count ${exporterError(reason)}",
                )
                System.out.flush()
              } else {
                val started = System.nanoTime()
                try {
                  emitBatch(offset, count, shardMode = true)
                  val elapsedMillis =
                    (System.nanoTime() - started) / 1000000L
                  println(
                    s"FVEXPORT DONE $offset $count $elapsedMillis",
                  )
                } catch {
                  case NonFatal(error) =>
                    val reason =
                      s"${error.getClass.getSimpleName}: " +
                      Option(error.getMessage).getOrElse("")
                    println(
                      s"FVEXPORT ERROR $offset $count " +
                      exporterError(reason),
                    )
                }
                System.out.flush()
              }
            case Left(reason) =>
              println(s"FVEXPORT ERROR - - ${exporterError(reason)}")
              System.out.flush()
        }
      } else {
        val want = args.lift(1).flatMap(_.toIntOption).getOrElse(10)
        val shardCount =
          if (shardMode) args.lift(2).flatMap(_.toIntOption).getOrElse(0)
          else 0
        emitBatch(want, shardCount, shardMode)
      }
      return
    }

    // ---- which spec functions does the run actually enter? -------------
    // Cheapest way to find out whether an omitted function is the reason
    // the model gets stuck: ask ESMeta, which can run the source.
    val visited = scala.collection.mutable.Set[String]()
    val probeSt = Initialize(cfg).from(source)
    val probe = new Interpreter(probeSt, timeLimit = Some(60)) {
      override def step: Boolean =
        visited += this.st.context.func.name
        super.step
    }
    try probe.result
    catch { case _: Throwable => () }
    val omittedNames = cfg.program.funcs
      .filter(f => scala.util.Try(FVExport.rocqFunc(f)).isFailure)
      .map(_.name)
      .toSet
    val hit = visited.toSet & omittedNames
    println(s"[fv] functions entered by this run: ${visited.size}")
    println(s"[fv] of those, omitted from the export: ${hit.size}")
    val byName = cfg.program.funcs.map(f => f.name -> f).toMap
    for (n <- hit.toList.sorted) {
      val rs = FVSpecScan.blockers(byName(n))
      println(s"[fv]   $n  <-  ${rs.toList.sorted.mkString(", ")}")
    }
    // the whole reachable set, so the work list is reachability-driven
    val allReasons = scala.collection.mutable.Map[String, Int]()
    for (n <- hit)
      for (r <- FVSpecScan.blockers(byName(n)))
        allReasons(r) = allReasons.getOrElse(r, 0) + 1
    println("[fv] blockers on the reachable set, by function count:")
    for ((r, c) <- allReasons.toList.sortBy(-_._2))
      println(f"[fv]   $c%3d  $r")

    // ---- which assertions does ESMeta silently skip during the run? ----
    // The model gets stuck on EYet, yet ESMeta completes: Interpreter.scala
    // 147-151 evaluates an asserted expression inside `optional(...)`, so a
    // throw skips the assertion.  Record exactly which ones, and whether
    // the swallowed expression could have had a side effect.
    val skipHist = scala.collection.mutable.Map[String, Int]()
    val skipSt = Initialize(cfg).from(source)
    val skipProbe = new Interpreter(skipSt, timeLimit = Some(60)) {
      override def eval(inst: esmeta.ir.NormalInst): Unit = inst match
        case esmeta.ir.IAssert(expr) =>
          try {
            val v = eval(expr)
            if (v != Bool(true))
              throw esmeta.error.AssertionFail(expr)
          } catch {
            case _: esmeta.error.AssertionFail =>
              throw esmeta.error.AssertionFail(expr)
            case e: Throwable =>
              val k = s"${e.getClass.getSimpleName}: " +
                s"${Option(e.getMessage).getOrElse("").take(60)}"
              skipHist(k) = skipHist.getOrElse(k, 0) + 1
          }
        case _ => super.eval(inst)
    }
    try skipProbe.result
    catch { case _: Throwable => () }
    println(
      s"[fv] assertions ESMeta silently skipped during the run: " +
      skipHist.values.sum,
    )
    for ((k, c) <- skipHist.toList.sortBy(-_._2).take(10))
      println(f"[fv]   $c%4d  $k")

    // ---- ESMeta's own run of the same source: the differential oracle --
    // A SEPARATE file so Spec.v's compile cost can be measured on its own.
    // Uses a fresh initial state; the one above must stay pristine.
    val runSt = Initialize(cfg).from(source)
    val interp = new HostCapturingInterpreter(runSt)
    val t0 = System.nanoTime()
    val outcome =
      try {
        val fin = interp.result
        val res = fin.globals.getOrElse(GLOBAL_RESULT, Undef)
        Right((res, interp.prints.toList))
      } catch { case e: Throwable => Left(e.toString.take(200)) }
    val ms = (System.nanoTime() - t0) / 1000000
    outcome match
      case Left(err) =>
        println(s"[fv] ESMeta could not run the source (${ms} ms): $err")
        println("[fv] no SpecRun.v emitted")
      case Right((res, prints)) =>
        println(
          s"[fv] ESMeta ran the source in ${ms} ms; " +
          s"RESULT=$res, ${prints.size} print(s)",
        )
        val terms = tryEmit(
          s"Ok (${value(res)}, ${coqList(prints.map(value))})",
        )
        terms match
          case None =>
            println("[fv] outcome not representable; no SpecRun.v emitted")
          case Some(expected) =>
            val r = new StringBuilder
            r ++= s"""(* AUTO-GENERATED by `sbt "runMain esmeta.fv.FVInitState"`.
 *
 * Differential check for stage G4/G5: ESMeta ran ${strLit(source)} from the
 * exported initial state and produced the observable below; compiling this
 * file checks that the Rocq reference interpreter agrees.
 *)
From Stdlib Require Import String ZArith List Floats PString.
Import ListNotations.
From ESMetaFV Require Import Fragment Domain Exec TestEncoding.
From ESMetaFV Require Import Spec.
Local Open Scope string_scope.
Local Open Scope Z_scope.

Definition this_src : cstr := ${cstrLit(source)}.
Definition this_ast : ast := ${st.cachedAst.fold(
              "(ALex \"\" \"\" nil nil nil)",
            )(a => ast(a))}.
Definition this_prog : prog :=
  script_prog this_src this_ast ${coqList(
              interp.hostEntries.toList.map(hostEntryWith(_, cstrLit)),
            )}.

Example spec_run_ok : run 10000000 this_prog = $expected.
Proof. vm_compute. reflexivity. Qed.
"""
            val ro = s"$BASE_DIR/formal/validation/SpecRun.v"
            dumpFile(r.toString, ro)
            println(s"[fv] wrote $ro")
  }
}
