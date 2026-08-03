(** Regression checks for ESMeta's EParse exception boundary.

    Operand evaluator exceptions are recoverable and retain effects emitted
    before the throw.  Missing/wrong host data remains model UB and must not
    be converted into a successful empty error list. *)

From Stdlib Require Import String List ZArith.
From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment Domain Semantics.
From ESMetaFV Require Exec.

Import ListNotations.
Local Open Scope string_scope.

Definition parse_empty_state : Exec.xstate :=
  Exec.mkXState nil nil nil None None nil 0.

Definition parse_state_with_heap (heap : list (option obj)) : Exec.xstate :=
  Exec.mkXState heap nil nil None None nil 0.

(** An immediate code failure skips the rule completely and allocates only
    the fresh parse-error list. *)
Example eparse_code_throw_skips_rule_exec :
  Exec.exec_expr parse_empty_state nil
    (EParse (EYet "code") (ESourceText (EList nil))) =
  Exec.Ok
    (parse_state_with_heap [Some (OList nil)], VAddr 0).
Proof. vm_compute. reflexivity. Qed.

(** Allocation performed before the code operand throws is retained, then
    the catch allocates a distinct error list. *)
Example eparse_code_partial_effect_is_retained_exec :
  Exec.exec_expr parse_empty_state nil
    (EParse
      (ESourceText (EList nil))
      (EGrammarSymbol "Script" nil)) =
  Exec.Ok
    (parse_state_with_heap
      [Some (OList nil); Some (OList nil)],
     VAddr 1).
Proof. vm_compute. reflexivity. Qed.

(** The rule cast happens after code evaluation and before source/cached
    state is consulted.  Code effects therefore remain on cast failure. *)
Example eparse_rule_cast_failure_retains_code_effect_exec :
  Exec.exec_expr parse_empty_state nil
    (EParse (EList nil) (EMath 0)) =
  Exec.Ok
    (parse_state_with_heap
      [Some (OList nil); Some (OList nil)],
     VAddr 1).
Proof. vm_compute. reflexivity. Qed.

Example eparse_invalid_source_is_caught_exec :
  Exec.exec_expr parse_empty_state nil
    (EParse (EMath 0) (EGrammarSymbol "Script" nil)) =
  Exec.Ok
    (parse_state_with_heap [Some (OList nil)], VAddr 0).
Proof. vm_compute. reflexivity. Qed.

Definition parse_failure_entry : host_cache_entry :=
  mkHostCacheEntry (HQParseText (cu "bad") "Script" nil) VUndef.

Definition parse_failure_state : Exec.xstate :=
  Exec.mkXState nil nil nil None None [parse_failure_entry] 0.

Example eparse_host_parse_failure_allocates_errors_exec :
  Exec.exec_expr parse_failure_state nil
    (EParse (EStr (cu "bad")) (EGrammarSymbol "Script" nil)) =
  Exec.Ok
    (Exec.mkXState
       [Some (OList nil)] nil nil None None [parse_failure_entry] 0,
     VAddr 0).
Proof. vm_compute. reflexivity. Qed.

(** A missing host entry is an exporter/model defect, not a JavaScript parse
    failure.  It must remain visible as UB. *)
Example eparse_host_cache_miss_is_not_caught_exec :
  Exec.exec_expr parse_empty_state nil
    (EParse (EStr (cu "bad")) (EGrammarSymbol "Script" nil)) =
  Exec.Stuck "EParse(host-cache-miss)".
Proof. vm_compute. reflexivity. Qed.

(** Scala converts a Math string index with [BigDecimal.toInt] before the
    bounds check.  2^32 therefore wraps to index zero instead of becoming an
    out-of-range parse-operand exception. *)
Example eparse_string_index_uses_scala_int32_exec :
  Exec.x_eval_read_target parse_empty_state nil
    (Exec.XField (VStr (65%Z :: nil)) (VMath 4294967296)) =
  Exec.Ok (parse_empty_state, EvalValue (VCodeUnit 65)).
Proof. vm_compute. reflexivity. Qed.

Example ordinary_string_index_uses_scala_int32_exec :
  Exec.read_target_x parse_empty_state nil
    (Exec.XField (VStr (65%Z :: nil)) (VMath 4294967296)) =
  Exec.Ok (VCodeUnit 65).
Proof. vm_compute. reflexivity. Qed.

(** An ambiguous represented continuation comparison is model UB.  It must
    not be caught as if the map key were merely absent. *)
Definition ambiguous_map_state : Exec.xstate :=
  parse_state_with_heap
    [Some
      (OMap
        [(VCont "same" nil (Some 0), VMath 1)])].

Example eparse_map_equality_ambiguity_is_not_caught_exec :
  Exec.x_eval_read_target ambiguous_map_state nil
    (Exec.XField (VAddr 0) (VCont "same" nil (Some 1))) =
  Exec.Stuck "EParse(map-key-equality)".
Proof. vm_compute. reflexivity. Qed.

Section ITREE_REGRESSIONS.
  Context `{!crisG Γ Σ α β τ _S _I}.
  Variable mn : string.

  Example eparse_operand_yet_is_catchable :
    denote_parse_operand mn (EYet "code") nil = Ret EvalThrow.
  Proof. reflexivity. Qed.

  Example eparse_unsupported_operand_remains_ub :
    denote_parse_operand mn
      (EBinary BAdd (EMath 1) (EMath 2)) nil = triggerUB.
  Proof. reflexivity. Qed.

  Example eparse_itree_code_throw_skips_rule :
    denote_expr mn
      (EParse (EYet "code") (ESourceText (EList nil))) nil =
    alloc_parse_errors mn.
  Proof.
    cbn [denote_expr denote_parse_operand eval_bind eval_throw].
    rewrite bind_ret_l. reflexivity.
  Qed.

End ITREE_REGRESSIONS.
