From Stdlib Require Import String List ZArith Floats.
From ESMetaFV Require Import Fragment Domain ITreeExec ITreeCore Spec.

Import ListNotations.
Local Open Scope string_scope.

Definition untraced_tree_test_ast : ast :=
  ALex "IdentifierName" "x" nil nil nil.

Definition untraced_tree_test_input :
  String.string * cstr * ast * list host_cache_entry * (val * list val) :=
  ("untraced", nil, untraced_tree_test_ast, nil, (VUndef, nil)).

Example lexical_undefined_is_represented_exactly :
  ast_lex_sdo
    (ALex "NoSubstitutionTemplate" "\\1" nil nil
      [("TV", LVUndef)])
    "TV" = Some VUndef.
Proof. reflexivity. Qed.

Example production_exec_is_untraced : production_exec = exec_itree.
Proof. reflexivity. Qed.

(** The production Test262 tree must not consume fuel on diagnostic
    function-entry/exit markers.  A caller can still request the separately
    constructed traced tree through [tt_trace_func]. *)
Example make_test_tree_uses_untraced_semantics :
  tt_tree (make_test_tree untraced_tree_test_input) =
    exec_itree TEST262_MODULE
      (script_prog nil untraced_tree_test_ast nil).
Proof. reflexivity. Qed.

Example make_test_tree_retains_lazy_trace_projection target :
  tt_trace_func (make_test_tree untraced_tree_test_input) target =
    exec_itree_trace_func TEST262_MODULE
      (script_prog nil untraced_tree_test_ast nil) target.
Proof. reflexivity. Qed.

Example string_to_bigint_host_query_dispatches :
  host_cop_query CToBigInt (VStr nil) = Some (HQStrToBigInt nil).
Proof. reflexivity. Qed.

Example string_to_bigint_host_result_contract :
  host_result_well_typed (HQStrToBigInt nil) (VBigInt 0%Z) = true /\
  host_result_well_typed (HQStrToBigInt nil) VUndef = true /\
  host_result_well_typed (HQStrToBigInt nil) (VStr nil) = false.
Proof. repeat split; reflexivity. Qed.

Example mathop_exact_equality :
  host_query_eqb (HQMathOp MAtan2 [1%Z; 2%Z])
                 (HQMathOp MAtan2 [1%Z; 2%Z]) = true /\
  host_query_eqb (HQMathOp MAtan2 [1%Z; 2%Z])
                 (HQMathOp MAtan [1%Z; 2%Z]) = false /\
  host_query_eqb (HQMathOp MAtan2 [1%Z; 2%Z])
                 (HQMathOp MAtan2 [2%Z; 1%Z]) = false.
Proof. repeat split; reflexivity. Qed.

Example mathop_exact_arity_and_types :
  host_mathop_query MAtan [VMath 3%Z] = Some (HQMathOp MAtan [3%Z]) /\
  host_mathop_query MAtan nil = None /\
  host_mathop_query MAtan [VMath 3%Z; VMath 4%Z] = None /\
  host_mathop_query MAtan2 [VMath 3%Z; VMath 4%Z] =
    Some (HQMathOp MAtan2 [3%Z; 4%Z]) /\
  host_mathop_query MAtan2 [VMath 3%Z] = None /\
  host_mathop_query MAtan2 [VMath 3%Z; VBool true] = None /\
  host_mathop_query MAcosh [VMath 3%Z] = None /\
  host_mathop_query MAsinh [VMath 3%Z] = None /\
  host_mathop_query MAtanh [VMath 3%Z] = None.
Proof. repeat split; reflexivity. Qed.

Example mathop_host_result_contract :
  host_result_well_typed (HQMathOp MSqrt [4%Z]) (VMath 2%Z) = true /\
  host_result_well_typed (HQMathOp MSqrt [4%Z]) (VBool true) = false.
Proof. split; reflexivity. Qed.

Example mathop_typed_cache_lookup_is_exact :
  typed_host_cache_lookup
    (HQMathOp MSqrt [4%Z])
    [mkHostCacheEntry (HQMathOp MSqrt [4%Z]) (VMath 2%Z)] =
      Some (VMath 2%Z) /\
  typed_host_cache_lookup
    (HQMathOp MSqrt [9%Z])
    [mkHostCacheEntry (HQMathOp MSqrt [4%Z]) (VMath 2%Z)] = None /\
  typed_host_cache_lookup
    (HQMathOp MSqrt [4%Z])
    [mkHostCacheEntry (HQMathOp MSqrt [4%Z]) (VBool true)] = None.
Proof. repeat split; reflexivity. Qed.

Example math_to_number_exact_boundary_stays_pure :
  host_cop_query CToNumber (VMath 9007199254740992%Z) = None /\
  host_cop_query CToApproxNumber (VMath (-9007199254740992)%Z) = None /\
  eval_cop CToNumber (VMath 9007199254740992%Z) =
    Some (VNumber (9007199254740992.0000)%float).
Proof. repeat split; reflexivity. Qed.

Example math_to_number_inexact_values_query_for_both_conversions :
  host_cop_query CToNumber (VMath 9007199254740993%Z) =
    Some (HQMathToNumber 9007199254740993%Z) /\
  host_cop_query CToApproxNumber (VMath 9007199254740993%Z) =
    Some (HQMathToNumber 9007199254740993%Z).
Proof. split; reflexivity. Qed.

Example math_to_number_query_equality_keeps_the_signed_key :
  host_query_eqb (HQMathToNumber 9007199254740993%Z)
                 (HQMathToNumber 9007199254740993%Z) = true /\
  host_query_eqb (HQMathToNumber 9007199254740993%Z)
                 (HQMathToNumber (-9007199254740993)%Z) = false.
Proof. split; reflexivity. Qed.

Example math_to_number_host_result_contract_accepts_only_number :
  host_result_well_typed
    (HQMathToNumber 9007199254740993%Z)
    (VNumber (9007199254740992.0000)%float) = true /\
  host_result_well_typed
    (HQMathToNumber 9007199254740993%Z)
    (VMath 9007199254740992%Z) = false.
Proof. split; reflexivity. Qed.

Example math_to_number_typed_cache_lookup_is_exact :
  typed_host_cache_lookup
    (HQMathToNumber 9007199254740993%Z)
    [mkHostCacheEntry
       (HQMathToNumber 9007199254740993%Z)
       (VNumber (9007199254740992.0000)%float)] =
      Some (VNumber (9007199254740992.0000)%float) /\
  typed_host_cache_lookup
    (HQMathToNumber (-9007199254740993)%Z)
    [mkHostCacheEntry
       (HQMathToNumber 9007199254740993%Z)
       (VNumber (9007199254740992.0000)%float)] = None /\
  typed_host_cache_lookup
    (HQMathToNumber 9007199254740993%Z)
    [mkHostCacheEntry
       (HQMathToNumber 9007199254740993%Z)
       (VMath 9007199254740992%Z)] = None.
Proof. repeat split; reflexivity. Qed.

Definition observable_ast_child : ast :=
  ALex "Child" "x" nil nil nil.

Definition observable_ast_root : ast :=
  ASyn "Root" nil 0 0
    [Some observable_ast_child] ["Child"] nil nil.

Definition observable_other_ast_root : ast :=
  ASyn "Root" nil 0 0
    [Some (ALex "Child" "y" nil nil nil)] ["Child"] nil nil.

Example observable_same_alias_pattern_passes :
  observable_outcome_eqb
    (VAst (AstRuntime 1) observable_ast_root nil,
     [VAst (AstRuntime 1) observable_ast_root nil])
    (VAst (AstExported 10) observable_ast_root nil,
     [VAst (AstExported 10) observable_ast_root nil]) = true.
Proof. vm_compute. reflexivity. Qed.

Example observable_distinct_actual_refs_cannot_share_expected_ref :
  observable_outcome_eqb
    (VAst (AstRuntime 1) observable_ast_root nil,
     [VAst (AstRuntime 2) observable_ast_root nil])
    (VAst (AstExported 10) observable_ast_root nil,
     [VAst (AstExported 10) observable_ast_root nil]) = false.
Proof. vm_compute. reflexivity. Qed.

Example observable_one_actual_ref_cannot_map_to_two_expected_refs :
  observable_outcome_eqb
    (VAst (AstRuntime 1) observable_ast_root nil,
     [VAst (AstRuntime 1) observable_ast_root nil])
    (VAst (AstExported 10) observable_ast_root nil,
     [VAst (AstExported 11) observable_ast_root nil]) = false.
Proof. vm_compute. reflexivity. Qed.

Example observable_ast_identity_key_includes_cursor_path :
  observable_outcome_eqb
    (VAst (AstRuntime 1) observable_ast_root nil,
     [VAst (AstRuntime 1) observable_ast_root [0]])
    (VAst (AstExported 10) observable_ast_root nil,
     [VAst (AstExported 11) observable_ast_root [0]]) = true.
Proof. vm_compute. reflexivity. Qed.

Example observable_focused_ast_payload_stays_structural :
  observable_outcome_eqb
    (VAst (AstRuntime 1) observable_ast_root [0], nil)
    (VAst (AstExported 10) observable_other_ast_root [0], nil) = false.
Proof. vm_compute. reflexivity. Qed.

Example observable_nested_ast_alias_pattern_passes :
  observable_outcome_eqb
    (VClo "f"
       [("captured", VAst (AstRuntime 1) observable_ast_root nil)],
     [VAst (AstRuntime 1) observable_ast_root nil])
    (VClo "f"
       [("captured", VAst (AstExported 10) observable_ast_root nil)],
     [VAst (AstExported 10) observable_ast_root nil]) = true.
Proof. vm_compute. reflexivity. Qed.

Example observable_nested_ast_aliases_share_the_outcome_bijection :
  observable_outcome_eqb
    (VClo "f"
       [("captured", VAst (AstRuntime 1) observable_ast_root nil)],
     [VAst (AstRuntime 1) observable_ast_root nil])
    (VClo "f"
       [("captured", VAst (AstExported 10) observable_ast_root nil)],
    [VAst (AstExported 11) observable_ast_root nil]) = false.
Proof. vm_compute. reflexivity. Qed.

Example observable_closure_captures_ignore_map_iteration_order :
  observable_outcome_eqb
    (VClo "f" [("x", VBool true); ("y", VBool false)], nil)
    (VClo "f" [("y", VBool false); ("x", VBool true)], nil) = true.
Proof. vm_compute. reflexivity. Qed.

Example observable_continuation_captures_ignore_map_iteration_order :
  observable_outcome_eqb
    (VCont "f" [("x", VBool true); ("y", VBool false)] (Some 7), nil)
    (VCont "f" [("y", VBool false); ("x", VBool true)] (Some 7), nil) = true.
Proof. vm_compute. reflexivity. Qed.

Example observable_capture_duplicates_are_last_binding_wins :
  observable_outcome_eqb
    (VClo "f"
       [("x", VBool false); ("unused", VBool false); ("x", VBool true)],
     nil)
    (VClo "f"
       [("x", VBool false); ("x", VBool true); ("unused", VBool false)],
     nil) = true.
Proof. vm_compute. reflexivity. Qed.

Example observable_reordered_nested_ast_capture_threads_aliases :
  observable_outcome_eqb
    (VClo "f"
       [("plain", VBool true);
        ("node", VAst (AstRuntime 1) observable_ast_root nil)],
     [VAst (AstRuntime 1) observable_ast_root nil])
    (VClo "f"
       [("node", VAst (AstExported 10) observable_ast_root nil);
        ("plain", VBool true)],
     [VAst (AstExported 10) observable_ast_root nil]) = true.
Proof. vm_compute. reflexivity. Qed.

Example observable_capture_value_mismatch_fails :
  observable_outcome_eqb
    (VClo "f" [("x", VBool true); ("y", VBool false)], nil)
    (VClo "f" [("y", VBool true); ("x", VBool true)], nil) = false.
Proof. vm_compute. reflexivity. Qed.

Example observable_capture_domain_mismatch_fails :
  observable_outcome_eqb
    (VClo "f" [("x", VBool true)], nil)
    (VClo "f" [("x", VBool true); ("y", VBool false)], nil) = false.
Proof. vm_compute. reflexivity. Qed.

Example observable_continuation_frame_identity_remains_strict :
  observable_outcome_eqb
    (VCont "f" [("x", VBool true)] (Some 7), nil)
    (VCont "f" [("x", VBool true)] (Some 8), nil) = false.
Proof. vm_compute. reflexivity. Qed.
