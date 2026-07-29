package esmeta.fv

import esmeta.BASE_DIR
import esmeta.ty.{TyDecl, TyModel}
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.parser.*
import scala.util.Try

/** Export ESMeta's record type-model subtyping relation as a Rocq
  * definition, so that the model's `ETypeCheck` is faithful to ESMeta's
  * `ty.contains` on record types instead of being guessed.
  *
  * Source of truth: `logs/dump/debugger/tyModel.decls.json` (produce with
  * `sbt "run dump-debugger"`), reparsed with ESMeta's own `TyDecl` parser
  * — no ad-hoc regex — and queried through `TyModel.parentOf`.
  *
  * Usage: sbt "runMain esmeta.fv.FVTyModel"  ->  formal/TyModel.v
  */
object FVTyModel {

  def readDecls(path: String): List[TyDecl] = {
    val json = parse(readFile(path)).getOrElse(Json.Null)
    val strs = json.hcursor
      .downField("TyModel")
      .downField("decls")
      .as[List[String]]
      .getOrElse(Nil)
    strs.flatMap(s => Try(TyDecl.from(s)).toOption)
  }

  def main(args: Array[String]): Unit = {
    val path = s"$BASE_DIR/logs/dump/debugger/tyModel.decls.json"
    if (!new java.io.File(path).exists) {
      println(s"[fv] missing $path — run: sbt \"run dump-debugger\"")
      return
    }
    val decls = readDecls(path)
    val model = TyModel(decls)
    println(s"[fv] parsed ${decls.size} type declarations")

    val edges = decls.flatMap(d => model.parentOf(d.name).map(d.name -> _))
    println(s"[fv] ${edges.size} child->parent edges")

    val sb = new StringBuilder
    sb ++= """(** * ESMetaFV.TyModel — record subtyping, GENERATED — DO NOT EDIT
  *
  * Regenerate with:
  *   sbt "run dump-debugger" && sbt "runMain esmeta.fv.FVTyModel"
  *
  * Exported from ESMeta's own type model (esmeta.ty.TyModel.parentOf), so
  * the model's [ETypeCheck] agrees with ESMeta's [ty.contains] on record
  * type names rather than guessing the hierarchy.  Pinned to ESMeta 0.7.3
  * with ecma262 @ 84b38ad8.
  *)

From Stdlib Require Import String.
Local Open Scope string_scope.

(** Immediate parent of a record type name, if any. *)
Definition record_parent (t : string) : option string :=
  match t with
"""
    for ((child, parent) <- edges.sortBy(_._1))
      sb ++= "  | \"%s\" => Some \"%s\"\n".format(child, parent)
    sb ++= """  | _ => None
  end.

(** [record_subtype d a] holds when record type [d] is [a] or descends
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
    dumpFile(sb.toString, out)
    println(s"[fv] wrote $out")

    // sanity: report a few chains the model must get right
    for (t <- List("ThrowCompletion", "NormalCompletion", "OrdinaryObject"))
      println(s"[fv]   $t ancestors: ${model.ancestorsOf(t).mkString(" <: ")}")
  }
}
