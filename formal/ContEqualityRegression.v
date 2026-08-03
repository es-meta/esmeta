(** Regression checks for ambiguity-aware continuation equality.

    Distinct nonempty frame-table identifiers are not a sound inequality
    witness for ESMeta's mutable [List[CallContext]] values. *)

From Stdlib Require Import String ZArith List Bool.
Import ListNotations.

From ESMetaFV Require Import Fragment Domain Semantics Exec.

Local Open Scope string_scope.
Local Open Scope Z_scope.

Definition cont1 : val := VCont "f" [("x", VMath 1)] (Some 1%nat).
Definition cont1_same : val := VCont "f" [("x", VMath 1)] (Some 1%nat).
Definition cont2 : val := VCont "f" [("x", VMath 1)] (Some 2%nat).
Definition cont_empty : val := VCont "f" [("x", VMath 1)] None.

(** Same represented stack and the empty-stack cases remain decidable. *)
Example cont_same_stack :
  eval_bop BEq cont1 cont1_same = Some (VBool true).
Proof. vm_compute. reflexivity. Qed.

Example cont_both_empty :
  val_eqb_partial cont_empty cont_empty = Some true.
Proof. vm_compute. reflexivity. Qed.

Example cont_empty_vs_nonempty :
  val_eqb_partial cont_empty cont1 = Some false.
Proof. vm_compute. reflexivity. Qed.

(** Unequal live identifiers are ambiguous only after all stable fields
    have compared equal. *)
Example cont_different_nonempty_stacks_are_ambiguous :
  eval_bop BEq cont1 cont2 = None.
Proof. vm_compute. reflexivity. Qed.

Example cont_function_inequality_short_circuits :
  val_eqb_partial
    (VCont "left" [] (Some 1%nat))
    (VCont "right" [] (Some 2%nat)) =
  Some false.
Proof. vm_compute. reflexivity. Qed.

Example cont_capture_inequality_short_circuits :
  val_eqb_partial
    (VCont "f" [("x", VMath 1)] (Some 1%nat))
    (VCont "f" [("x", VMath 2)] (Some 2%nat)) =
  Some false.
Proof. vm_compute. reflexivity. Qed.

Example closure_capture_threads_ambiguity :
  val_eqb_partial
    (VClo "g" [("k", cont1)])
    (VClo "g" [("k", cont2)]) =
  None.
Proof. vm_compute. reflexivity. Qed.

(** Captures have Scala finite-map semantics: iteration order is
    irrelevant and the last duplicate binding is the effective one. *)
Example closure_capture_order_is_irrelevant :
  val_eqb_partial
    (VClo "g" [("x", VMath 1); ("y", VBool true)])
    (VClo "g" [("y", VBool true); ("x", VMath 1)]) =
  Some true.
Proof. vm_compute. reflexivity. Qed.

Example closure_capture_last_duplicate_wins :
  val_eqb_partial
    (VClo "g" [("x", VMath 0); ("x", VMath 1)])
    (VClo "g" [("x", VMath 1)]) =
  Some true.
Proof. vm_compute. reflexivity. Qed.

Example closure_capture_effective_duplicate_differs :
  val_eqb_partial
    (VClo "g" [("x", VMath 1); ("x", VMath 0)])
    (VClo "g" [("x", VMath 1)]) =
  Some false.
Proof. vm_compute. reflexivity. Qed.

Example cont_capture_order_preserves_frame_ambiguity :
  val_eqb_partial
    (VCont "f" [("x", VMath 1); ("y", VBool true)] (Some 1%nat))
    (VCont "f" [("y", VBool true); ("x", VMath 1)] (Some 2%nat)) =
  None.
Proof. vm_compute. reflexivity. Qed.

(** Calling a closure folds captured bindings over parameter locals,
    exactly matching [getLocals(...) ++ captured]. *)
Definition parameter_env : env :=
  [(LName "x", VMath 0); (LName "y", VMath 2)].

Definition merged_capture : env :=
  merge_captured_env parameter_env
    [("x", VMath 1); ("z", VMath 3); ("x", VMath 4)].

Example captured_binding_overrides_parameter :
  env_lookup merged_capture (LName "x") = Some (VMath 4).
Proof. vm_compute. reflexivity. Qed.

Example uncaptured_parameter_is_preserved :
  env_lookup merged_capture (LName "y") = Some (VMath 2).
Proof. vm_compute. reflexivity. Qed.

Example new_captured_binding_is_added :
  env_lookup merged_capture (LName "z") = Some (VMath 3).
Proof. vm_compute. reflexivity. Qed.

Example capture_normalization_keeps_last_duplicate :
  captured_normalize
    [("x", VMath 0); ("y", VMath 2); ("x", VMath 1)] =
  [("y", VMath 2); ("x", VMath 1)].
Proof. vm_compute. reflexivity. Qed.

(** Membership preserves Scala List.contains short-circuit order. *)
Example contains_match_before_ambiguity :
  vals_contains_partial cont1 [cont1_same; cont2] = Some true.
Proof. vm_compute. reflexivity. Qed.

Example contains_ambiguity_before_match :
  vals_contains_partial cont1 [cont2; cont1_same] = None.
Proof. vm_compute. reflexivity. Qed.

(** Map scans no longer turn an ambiguous key comparison into absence or
    a fresh insertion/deletion result.  Insertion rejects continuation
    keys even in an empty map because their Scala hash depends on mutable
    captured CallContext state. *)
Definition ambiguous_cont_map : list (val * val) :=
  [(cont2, VMath 9)].

Example map_lookup_ambiguity :
  map_lookup_partial ambiguous_cont_map cont1 = None.
Proof. vm_compute. reflexivity. Qed.

Example map_insert_ambiguous_or_mutable_key_is_ub :
  map_insert_partial cont1 (VMath 10) ambiguous_cont_map = None.
Proof. vm_compute. reflexivity. Qed.

Example map_insert_mutable_key_empty_is_ub :
  map_insert_partial cont1 (VMath 10) [] = None.
Proof. vm_compute. reflexivity. Qed.

Example map_insert_nested_mutable_key_is_ub :
  map_insert_partial (VClo "g" [("k", cont1)]) (VMath 10) [] = None.
Proof. vm_compute. reflexivity. Qed.

Example map_delete_ambiguity :
  map_delete_partial cont1 ambiguous_cont_map = None.
Proof. vm_compute. reflexivity. Qed.

(** The AST-only [BEq] reference-identity branch remains separate from
    ordinary structural value equality. *)
Definition tiny_ast : ast := ALex "IdentifierName" "x" [120] [120] [].

Example ast_structural_equality_is_unchanged :
  val_eqb_partial
    (VAst (AstExported 1) tiny_ast [])
    (VAst (AstExported 2) tiny_ast []) =
  Some true.
Proof. vm_compute. reflexivity. Qed.

Example ast_beq_still_uses_reference_identity :
  eval_bop BEq
    (VAst (AstExported 1) tiny_ast [])
    (VAst (AstExported 2) tiny_ast []) =
  Some (VBool false).
Proof. vm_compute. reflexivity. Qed.
