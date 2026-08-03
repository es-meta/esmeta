(** * ESMetaFV.TyModel — record refinement facade, GENERATED — DO NOT EDIT
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
From ESMetaFV Require Import TyModelParent00.
From ESMetaFV Require Import TyModelParent01.
From ESMetaFV Require Import TyModelParent02.
From ESMetaFV Require Import TyModelParent03.
From ESMetaFV Require Import TyModelParent04.
From ESMetaFV Require Import TyModelParent05.
From ESMetaFV Require Import TyModelParent06.
From ESMetaFV Require Import TyModelParent07.
From ESMetaFV Require Import TyModelParent08.
From ESMetaFV Require Import TyModelParent09.
From ESMetaFV Require Import TyModelBindings00.
From ESMetaFV Require Import TyModelBindings01.
From ESMetaFV Require Import TyModelBindings02.
From ESMetaFV Require Import TyModelBindings03.
From ESMetaFV Require Import TyModelBindings04.
From ESMetaFV Require Import TyModelBindings05.
From ESMetaFV Require Import TyModelBindings06.
From ESMetaFV Require Import TyModelBindings07.
From ESMetaFV Require Import TyModelBindings08.
From ESMetaFV Require Import TyModelBindings09.
From ESMetaFV Require Import TyModelBindings10.
From ESMetaFV Require Import TyModelBindings11.
From ESMetaFV Require Import TyModelBindings12.
From ESMetaFV Require Import TyModelBindings13.
Local Open Scope string_scope.

(** Immediate parent of a record type name, if any. *)
Definition record_parent (t : string) : option string :=
  match record_parent_chunk_00 t with
  | Some parent => Some parent
  | None =>
  match record_parent_chunk_01 t with
  | Some parent => Some parent
  | None =>
  match record_parent_chunk_02 t with
  | Some parent => Some parent
  | None =>
  match record_parent_chunk_03 t with
  | Some parent => Some parent
  | None =>
  match record_parent_chunk_04 t with
  | Some parent => Some parent
  | None =>
  match record_parent_chunk_05 t with
  | Some parent => Some parent
  | None =>
  match record_parent_chunk_06 t with
  | Some parent => Some parent
  | None =>
  match record_parent_chunk_07 t with
  | Some parent => Some parent
  | None =>
  match record_parent_chunk_08 t with
  | Some parent => Some parent
  | None =>
  match record_parent_chunk_09 t with
  | Some parent => Some parent
  | None =>
  None
  end
  end
  end
  end
  end
  end
  end
  end
  end
  end
.

(** Bindings introduced directly by a record declaration.  When a child
    redeclares a field, the child binding overrides the ancestor binding,
    matching [TyModel.diffOf]'s map union. *)
Definition record_own_bindings (t : string) : list record_field_binding :=
  match record_own_bindings_chunk_00 t with
  | Some bindings => bindings
  | None =>
  match record_own_bindings_chunk_01 t with
  | Some bindings => bindings
  | None =>
  match record_own_bindings_chunk_02 t with
  | Some bindings => bindings
  | None =>
  match record_own_bindings_chunk_03 t with
  | Some bindings => bindings
  | None =>
  match record_own_bindings_chunk_04 t with
  | Some bindings => bindings
  | None =>
  match record_own_bindings_chunk_05 t with
  | Some bindings => bindings
  | None =>
  match record_own_bindings_chunk_06 t with
  | Some bindings => bindings
  | None =>
  match record_own_bindings_chunk_07 t with
  | Some bindings => bindings
  | None =>
  match record_own_bindings_chunk_08 t with
  | Some bindings => bindings
  | None =>
  match record_own_bindings_chunk_09 t with
  | Some bindings => bindings
  | None =>
  match record_own_bindings_chunk_10 t with
  | Some bindings => bindings
  | None =>
  match record_own_bindings_chunk_11 t with
  | Some bindings => bindings
  | None =>
  match record_own_bindings_chunk_12 t with
  | Some bindings => bindings
  | None =>
  match record_own_bindings_chunk_13 t with
  | Some bindings => bindings
  | None =>
  nil
  end
  end
  end
  end
  end
  end
  end
  end
  end
  end
  end
  end
  end
  end
.

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
  record_subtype_fuel 113 d a.
