package esmeta.fv

import esmeta.BASE_DIR
import esmeta.ty.*
import esmeta.util.{Fin, Inf}
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.parser.*
import java.io.File
import java.nio.file.{Files, Path}
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

/** Export ESMeta's record hierarchy and structural-refinement fields as Rocq
  * definitions. `RecordTy.contains` accepts not only nominal subtypes, but an
  * ancestor-tagged record that carries the fields added by a requested
  * descendant type; both pieces come from this one source.
  *
  * Source of truth: `logs/dump/debugger/tyModel.decls.json` (produce with `sbt
  * "run dump-debugger"`), reparsed with ESMeta's own `TyDecl` parser — no
  * ad-hoc regex — and queried through `TyModel.parentOf`.
  *
  * Usage: sbt "runMain esmeta.fv.FVTyModel" -> formal/TyModel.v
  */
object FVTyModel {

  private val chunkSize = 8
  private val coqProjectStart = "# BEGIN GENERATED FVTyModel SHARDS"
  private val coqProjectEnd = "# END GENERATED FVTyModel SHARDS"

  /** Preserve timestamps of unchanged generated modules so an exporter run does
    * not trigger a needless Rocq rebuild.
    */
  private def dumpFileIfChanged(data: String, filename: String): Boolean = {
    val file = File(filename)
    if (file.exists && readFile(filename) == data) false
    else {
      dumpFile(data, filename)
      true
    }
  }

  private def writeGenerated(data: String, filename: String): Unit = {
    val action = if (dumpFileIfChanged(data, filename)) "wrote" else "kept"
    println(s"[fv] $action $filename")
  }

  /** Remove only obsolete numbered modules owned by this generator. */
  private def removeStaleChunks(prefix: String, keepCount: Int): Unit = {
    val formalDir = File(s"$BASE_DIR/formal")
    val pattern = raw"${prefix}(\d+)\.v".r
    Option(formalDir.listFiles).getOrElse(Array.empty[File]).foreach { file =>
      file.getName match {
        case pattern(index) if index.toInt >= keepCount =>
          if (!file.delete)
            throw RuntimeException(
              s"failed to remove stale generated file: $file",
            )
          println(s"[fv] removed stale ${file.getPath}")
        case _ =>
      }
    }
  }

  /** Keep Rocq's editor/build manifest synchronized with the exact shard set
    * emitted by this generator. The surrounding project remains hand-owned.
    */
  private[fv] def renderCoqProject(
    project: String,
    parentCount: Int,
    bindingCount: Int,
  ): String = {
    require(parentCount >= 0, s"negative parent shard count: $parentCount")
    require(bindingCount >= 0, s"negative binding shard count: $bindingCount")

    val start = project.indexOf(coqProjectStart)
    val end = project.indexOf(coqProjectEnd)
    if (
      start < 0 || end < start ||
      project.indexOf(coqProjectStart, start + coqProjectStart.length) >= 0 ||
      project.indexOf(coqProjectEnd, end + coqProjectEnd.length) >= 0
    )
      throw IllegalArgumentException(
        s"formal/_CoqProject must contain exactly one $coqProjectStart / " +
        s"$coqProjectEnd block",
      )

    val shardLines =
      List("TyModelBase.v") ++
      parentShardNames(parentCount) ++
      bindingShardNames(bindingCount) ++
      List("TyModel.v")
    val block =
      s"$coqProjectStart\n${shardLines.mkString("\n")}\n$coqProjectEnd"
    project.substring(0, start) + block +
    project.substring(end + coqProjectEnd.length)
  }

  private def parentShardNames(count: Int): List[String] =
    List.tabulate(count)(idx => f"TyModelParent$idx%02d.v")

  private def bindingShardNames(count: Int): List[String] =
    List.tabulate(count)(idx => f"TyModelBindings$idx%02d.v")

  private def updateCoqProject(
    parentCount: Int,
    bindingCount: Int,
  ): Unit = {
    val path = s"$BASE_DIR/formal/_CoqProject"
    writeGenerated(
      renderCoqProject(readFile(path), parentCount, bindingCount),
      path,
    )
  }

  private val unsupportedLeafCounts =
    scala.collection.mutable.Map.empty[String, Int]

  private def unsupportedLeaf(kind: String): String = {
    unsupportedLeafCounts(kind) = unsupportedLeafCounts.getOrElse(kind, 0) + 1
    "RFCUnsupported"
  }

  private def strLit(str: String): String =
    "\"" + str.replace("\"", "\"\"") + "\""

  private def coqList(xs: Iterable[String]): String = {
    val list = xs.toList
    if (list.isEmpty) "nil"
    else list.mkString("(", " :: ", " :: nil)")
  }

  private def cstrLit(str: String): String =
    coqList(str.toCharArray.iterator.map(c => s"(${c.toInt}%Z)").toList)

  /** Exact `MathTy.contains` syntax for the integer-only Rocq `VMath` domain.
    *
    * `MathSetTy` may contain non-integral mathematical values that cannot occur
    * as `VMath`; omitting those values preserves exact membership for every
    * reachable Rocq math value.
    */
  private[fv] def mathConstraint(ty: MathTy): String =
    if (ty.isTop) "RFCMath"
    else
      ty.canon match
        case MathSignTy(Sign(neg, zero, pos)) =>
          s"(RFCMathSign $neg $zero $pos)"
        case MathIntTy(IntSignTy(Sign(neg, zero, pos))) =>
          s"(RFCMathIntSign $neg $zero $pos)"
        case MathIntTy(IntSetTy(set)) =>
          val values = set.toList.sorted.map(FVExport.zLit)
          s"(RFCMathIntSet ${coqList(values)})"
        case MathSetTy(set) =>
          val values = set.iterator
            .map(_.decimal)
            .filter(_.isWhole)
            .map(_.toBigInt)
            .toSet
            .toList
            .sorted
            .map(FVExport.zLit)
          s"(RFCMathSet ${coqList(values)})"

  /** A runtime-checkable encoding of `ValueTy.contains`.
    *
    * Heap containers retain their recursive shape instead of collapsing to a
    * bare address. The Rocq side turns this finite syntax into a lazy heap
    * query plan, so a type test reads only the fields/elements that ESMeta's
    * own `contains` implementation would inspect.
    */
  private[fv] def fieldConstraint(ty: ValueTy): String = {
    if (ty.isTop) "RFCAny"
    else if (ty.isBottom) "RFCNever"
    else {
      val parts = ListBuffer[String]()

      ty.clo match
        case CloTy.Bot =>
        case CloSetTy(names) =>
          parts +=
            s"(RFCCloNames ${coqList(names.toList.sorted.map(strLit))})"
        // CloTy.contains accepts every runtime closure for Top and Arrow.
        case _ => parts += "RFCClo"

      ty.cont match
        case Fin(set) if set.isEmpty =>
        case Inf                     => parts += "RFCCont"
        case _                       => parts += unsupportedLeaf("continuation")

      ty.record match
        case RecordTy.Top => parts += "RFCRecordTop"
        case RecordTy.Elem(map) if map.nonEmpty =>
          val targets = map.toList.sortBy(_._1).map { (name, fieldMap) =>
            val bindings = fieldMap.map.toList.sortBy(_._1).map {
              case (field, binding) =>
                s"(${strLit(field)}, (${binding.absent}, " +
                s"${fieldConstraint(binding.value)}))"
            }
            s"(${strLit(name)}, ${coqList(bindings)})"
          }
          parts += s"(RFCRecord ${coqList(targets)})"
        case _ =>

      ty.map match
        case MapTy.Top => parts += "RFCMapTop"
        case MapTy.Elem(key, value) =>
          parts += s"(RFCMap ${fieldConstraint(key)} ${fieldConstraint(value)})"
        case MapTy.Bot =>

      ty.list match
        case ListTy.Top => parts += "RFCListTop"
        case ListTy.Elem(elem) =>
          parts += s"(RFCList ${fieldConstraint(elem)})"
        case ListTy.Bot =>

      ty.ast match
        case AstTy.Simple(set) if set.isEmpty =>
        case AstTy.Top                        => parts += "RFCAst"
        case AstTy.Simple(set) =>
          parts += s"(RFCAstNames ${coqList(set.toList.sorted.map(strLit))})"
        case AstTy.Detail(name, idx) =>
          parts += s"(RFCAstDetail ${strLit(name)} ${FVExport.natLit(idx)})"

      ty.grammarSymbol match
        case Fin(set) if set.isEmpty =>
        case Inf                     => parts += "RFCGrammarSymbol"
        case _ => parts += unsupportedLeaf("grammar symbol")

      if (ty.codeUnit) parts += "RFCCodeUnit"

      ty.enumv match
        case Fin(set) if set.nonEmpty =>
          parts += s"(RFCEnum ${coqList(set.toList.sorted.map(strLit))})"
        case Inf => parts += "RFCEnumAny"
        case _   =>

      if (!ty.math.isBottom) parts += mathConstraint(ty.math)

      if (!ty.infinity.isBottom) {
        val allowFalse = ty.infinity.pos.contains(false)
        val allowTrue = ty.infinity.pos.contains(true)
        parts += s"(RFCInfinity $allowFalse $allowTrue)"
      }

      if (!ty.number.isBottom)
        parts +=
          (if (ty.number.isTop) "RFCNumber" else unsupportedLeaf("Number"))
      if (ty.bigInt) parts += "RFCBigInt"

      ty.str match
        case Fin(set) if set.isEmpty =>
        case Inf                     => parts += "RFCStr"
        case Fin(set) =>
          parts += s"(RFCStrSet ${coqList(set.toList.sorted.map(cstrLit))})"

      if (!ty.bool.isBottom) {
        val allowFalse = ty.bool.set.contains(false)
        val allowTrue = ty.bool.set.contains(true)
        parts += s"(RFCBool $allowFalse $allowTrue)"
      }

      if (ty.undef) parts += "RFCUndef"
      if (ty.nullv) parts += "RFCNull"

      parts.toList match
        case Nil      => "RFCNever"
        case x :: Nil => x
        case xs       => s"(RFCUnion ${coqList(xs)})"
    }
  }

  def readDecls(path: String): List[TyDecl] = {
    val source =
      try Files.readString(Path.of(path))
      catch
        case NonFatal(error) =>
          throw IllegalArgumentException(
            s"cannot read TyModel input at $path: ${error.getMessage}",
            error,
          )
    val json = parse(source).fold(
      error =>
        throw IllegalArgumentException(
          s"invalid TyModel JSON at $path: ${error.message}",
          error,
        ),
      identity,
    )
    val strs = json.hcursor
      .downField("TyModel")
      .downField("decls")
      .as[List[String]]
      .fold(
        error =>
          throw IllegalArgumentException(
            s"invalid TyModel.decls structure at $path: ${error.message}",
            error,
          ),
        identity,
      )
    if (strs.isEmpty)
      throw IllegalArgumentException(s"TyModel.decls is empty at $path")
    strs.zipWithIndex.map { (str, index) =>
      try TyDecl.from(str)
      catch
        case NonFatal(error) =>
          throw IllegalArgumentException(
            s"invalid TyModel.decls[$index] at $path: ${error.getMessage}",
            error,
          )
    }
  }

  def main(args: Array[String]): Unit = {
    unsupportedLeafCounts.clear()
    val path = s"$BASE_DIR/logs/dump/debugger/tyModel.decls.json"
    if (!new java.io.File(path).exists) {
      throw IllegalArgumentException(
        s"missing $path — run: sbt \"run dump-debugger\"",
      )
    }
    val decls = readDecls(path)
    val model = TyModel(decls)
    println(s"[fv] parsed ${decls.size} type declarations")

    val edges = decls.flatMap(d => model.parentOf(d.name).map(d.name -> _))
    println(s"[fv] ${edges.size} child->parent edges")

    val ownBindings = decls
      .map(_.name)
      .distinct
      .map { name =>
        val bindings = model.ownFieldsOf(name).toList.sortBy(_._1).map {
          case (field, binding) =>
            (field, binding.absent, fieldConstraint(binding.value))
        }
        name -> bindings
      }
      .filter(_._2.nonEmpty)
    println(s"[fv] ${ownBindings.size} types add refinement bindings")
    if (unsupportedLeafCounts.nonEmpty) {
      println("[fv] precise leaf constraints conservatively blocked:")
      for ((kind, count) <- unsupportedLeafCounts.toList.sortBy(_._1))
        println(f"[fv]   $count%4d  $kind")
    }

    val generatedHeader =
      """(** GENERATED by [esmeta.fv.FVTyModel] — DO NOT EDIT. *)
"""
    val base = new StringBuilder
    base ++= """(** * ESMetaFV.TyModelBase — shared record-refinement syntax
  *
  * Regenerate with:
  *   sbt "run dump-debugger" && sbt "runMain esmeta.fv.FVTyModel"
  *
  * Exported from ESMeta's own type model ([parentOf] and [ownFieldsOf]),
  * rather than guessing either the hierarchy or descendant discriminants.
  * Pinned to ESMeta 0.7.3 with ecma262 @ 84b38ad8.
  *)

From Stdlib Require Import String ZArith List.
Import ListNotations.
From ESMetaFV Require Import Fragment.
Local Open Scope string_scope.

(** Runtime-checkable field constraints used by the structural descendant
    branch of [RecordTy.contains].  Heap containers retain the recursive
    [ValueTy] shape that ESMeta checks.  A target record stores both its name
    and its inline [FieldMap] refinements; the latter matter for anonymous
    records and explicitly refined named records. *)
Inductive record_field_constraint : Type :=
| RFCAny
| RFCNever
| RFCUnion (cs : list record_field_constraint)
| RFCEnum (names : list string)
| RFCEnumAny
| RFCStr
| RFCStrSet (values : list cstr)
| RFCBool (allow_false allow_true : bool)
| RFCMath
| RFCMathSign (allow_neg allow_zero allow_pos : bool)
| RFCMathIntSign (allow_neg allow_zero allow_pos : bool)
| RFCMathSet (values : list Z)
| RFCMathIntSet (values : list Z)
| RFCInfinity (allow_neg allow_pos : bool)
| RFCNumber
| RFCBigInt
| RFCUndef
| RFCNull
| RFCCodeUnit
| RFCClo
| RFCCloNames (names : list string)
| RFCCont
| RFCAst
| RFCAstNames (names : list string)
| RFCAstDetail (name : string) (idx : nat)
| RFCGrammarSymbol
| RFCRecordTop
| RFCRecord
    (targets :
      list
        (string *
          list (string * (bool * record_field_constraint))))
| RFCMapTop
| RFCMap
    (key_constraint value_constraint : record_field_constraint)
| RFCListTop
| RFCList (element_constraint : record_field_constraint)
(** A precise leaf constraint that this exporter cannot yet encode.
    Its executable decision is [None]/UB, never a permissive kind test. *)
| RFCUnsupported
(** Kept as an explicit conservative boundary for hand-written regression
    terms and stale generated models.  [FVTyModel] no longer emits it. *)
| RFCAddr.

Record record_field_binding : Type := mkRecordFieldBinding {
  rfb_name : string;
  rfb_absent : bool;
  rfb_constraint : record_field_constraint;
}.
"""

    val baseOut = s"$BASE_DIR/formal/TyModelBase.v"
    writeGenerated(base.toString, baseOut)

    val sortedEdges = edges.sortBy(_._1)
    val parentChunks = sortedEdges.grouped(chunkSize).toList
    for ((chunk, idx) <- parentChunks.zipWithIndex) {
      val suffix = f"$idx%02d"
      val sb = new StringBuilder(generatedHeader)
      sb ++= """From Stdlib Require Import String.
From ESMetaFV Require Import TyModelBase.
Local Open Scope string_scope.

"""
      sb ++= s"Definition record_parent_chunk_$suffix (t : string) : option string :=\n"
      sb ++= "  match t with\n"
      for ((child, parent) <- chunk)
        sb ++= "  | \"%s\" => Some \"%s\"\n".format(child, parent)
      sb ++= "  | _ => None\n  end.\n"
      val out = s"$BASE_DIR/formal/TyModelParent$suffix.v"
      writeGenerated(sb.toString, out)
    }
    removeStaleChunks("TyModelParent", parentChunks.size)

    val sortedOwnBindings = ownBindings.sortBy(_._1)
    val bindingChunks = sortedOwnBindings.grouped(chunkSize).toList
    for ((chunk, idx) <- bindingChunks.zipWithIndex) {
      val suffix = f"$idx%02d"
      val sb = new StringBuilder(generatedHeader)
      sb ++= """From Stdlib Require Import String ZArith List.
Import ListNotations.
From ESMetaFV Require Import TyModelBase.
Local Open Scope string_scope.

"""
      sb ++=
        s"Definition record_own_bindings_chunk_$suffix (t : string) : option (list record_field_binding) :=\n"
      sb ++= "  match t with\n"
      for ((name, bindings) <- chunk) {
        val terms = bindings.map {
          case (field, absent, constraint) =>
            s"(mkRecordFieldBinding ${strLit(field)} $absent $constraint)"
        }
        sb ++= "  | \"%s\" => Some %s\n".format(name, coqList(terms))
      }
      sb ++= "  | _ => None\n  end.\n"
      val out = s"$BASE_DIR/formal/TyModelBindings$suffix.v"
      writeGenerated(sb.toString, out)
    }
    removeStaleChunks("TyModelBindings", bindingChunks.size)
    updateCoqProject(parentChunks.size, bindingChunks.size)

    val sb = new StringBuilder
    sb ++= """(** * ESMetaFV.TyModel — record refinement facade, GENERATED — DO NOT EDIT
  *
  * Regenerate with:
  *   sbt "run dump-debugger" && sbt "runMain esmeta.fv.FVTyModel"
  *
  * The generated lookup tables are split into bounded compilation units so
  * Rocq's native compiler does not have to assemble one enormous function.
  * This facade preserves the original exported names and behavior.
  *)

From Stdlib Require Import String List.
From ESMetaFV Require Import TyModelBase.
Include TyModelBase.
"""
    for (idx <- parentChunks.indices)
      sb ++= f"From ESMetaFV Require Import TyModelParent$idx%02d.\n"
    for (idx <- bindingChunks.indices)
      sb ++= f"From ESMetaFV Require Import TyModelBindings$idx%02d.\n"
    sb ++= "Local Open Scope string_scope.\n\n"
    sb ++= """(** Immediate parent of a record type name, if any. *)
Definition record_parent (t : string) : option string :=
"""
    for (idx <- parentChunks.indices) {
      val suffix = f"$idx%02d"
      sb ++= s"  match record_parent_chunk_$suffix t with\n"
      sb ++= "  | Some parent => Some parent\n"
      sb ++= "  | None =>\n"
    }
    sb ++= "  None\n"
    for (_ <- parentChunks.indices)
      sb ++= "  end\n"
    sb ++= ".\n\n"

    sb ++= """(** Bindings introduced directly by a record declaration.  When a child
    redeclares a field, the child binding overrides the ancestor binding,
    matching [TyModel.diffOf]'s map union. *)
Definition record_own_bindings (t : string) : list record_field_binding :=
"""
    for (idx <- bindingChunks.indices) {
      val suffix = f"$idx%02d"
      sb ++= s"  match record_own_bindings_chunk_$suffix t with\n"
      sb ++= "  | Some bindings => bindings\n"
      sb ++= "  | None =>\n"
    }
    sb ++= "  nil\n"
    for (_ <- bindingChunks.indices)
      sb ++= "  end\n"
    sb ++= ".\n\n"

    sb ++= """(** [record_subtype d a] holds when record type [d] is [a] or descends
    from it.  The fuel bound is the number of declarations, which exceeds
    the depth of any chain in the exported model. *)
Fixpoint record_subtype_fuel (n : nat) (d a : string) : bool :=
  if String.eqb d a then true else
  match n with
  | O => false
  | S n' =>
      match record_parent d with
      | Some p => record_subtype_fuel n' p a
      | None => false
      end
  end.

Definition record_subtype (d a : string) : bool :=
"""
    sb ++= "  record_subtype_fuel %d d a.\n".format(decls.size + 1)
    val out = s"$BASE_DIR/formal/TyModel.v"
    writeGenerated(sb.toString, out)

    // sanity: report a few chains the model must get right
    for (t <- List("ThrowCompletion", "NormalCompletion", "OrdinaryObject"))
      println(s"[fv]   $t ancestors: ${model.ancestorsOf(t).mkString(" <: ")}")
  }
}
