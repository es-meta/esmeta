(** * ESMetaFV.Validation — executable validation (PO-011, testing NOT proof)

    vm_compute-checked runs of the corpus mirrors under the executable
    reference interpreter [Exec.v], plus effect-sensitivity checks
    showing that the validation signal DOES detect duplicated calls and
    reordered effects, and a negative test for an intentionally wrong
    transformation.

    CLASSIFICATION.  Everything in this file is TESTING evidence
    (deliverable #8; PO-011).  It validates the executable interpreter —
    and, through the clause-by-clause mirroring documented in [Exec.v],
    the ITree denotation — against expected ESMeta behavior.  It proves
    nothing about the denotation itself (that connection is PO-013).

    The corpus programs' [assert] instructions encode ESMeta's expected
    results (gcd=14, fib 9=34, sum=55) and ESMeta's own EvalTinyTest
    passes them [repository fact]; a run reaching [Ok] here means every
    assert evaluated to true under our semantics too. *)

From Stdlib Require Import String ZArith List.
Import ListNotations.

From ESMetaFV Require Import TyModel Fragment Domain Exec Programs Transform.

Local Open Scope string_scope.
Local Open Scope Z_scope.

(** ** Corpus runs (expected: normal termination, no prints) *)

Example sum_ok : run 1000 sum_prog = Ok (VUndef, nil).
Proof. vm_compute. reflexivity. Qed.

Example duplicate_literals_ok :
  run 1000 duplicate_literals_prog = Ok (VUndef, nil).
Proof. vm_compute. reflexivity. Qed.

Example initial_parse_priority_ok :
  run 1000 initial_parse_priority_prog = Ok (VUndef, nil).
Proof. vm_compute. reflexivity. Qed.

Example captured_param_priority_ok :
  run 1000 captured_param_prog = Ok (VUndef, nil).
Proof. vm_compute. reflexivity. Qed.

Example ast_parent_cursor_ok :
  run 1000 ast_parent_exists_prog = Ok (VUndef, nil).
Proof. vm_compute. reflexivity. Qed.

Example ast_sdo_descendant_cursor_ok :
  run 1000 cursor_sdo_prog = Ok (VUndef, nil).
Proof. vm_compute. reflexivity. Qed.

Example ast_reference_equality_same_cursor_ok :
  run 1000 ast_eq_prog = Ok (VUndef, nil).
Proof. vm_compute. reflexivity. Qed.

Example ast_reference_equality_distinct_roots_ok :
  run 1000 ast_distinct_roots_prog = Ok (VUndef, nil).
Proof. vm_compute. reflexivity. Qed.

Example ast_reference_equality_parent_roundtrip_ok :
  run 1000 ast_cursor_roundtrip_prog = Ok (VUndef, nil).
Proof. vm_compute. reflexivity. Qed.

(** [BOp.Eq] is reference equality for ASTs, while ordinary Scala value
    equality used by list membership and map operations remains structural. *)
Example ast_reference_and_structural_equality_are_distinct :
  ast_ref_eqb
      (AstExported 1) nil
      (AstExported 2) nil = false /\
  val_eqb
      (VAst (AstExported 1) named_ast_root nil)
      (VAst (AstExported 2) named_ast_root nil) = true.
Proof. vm_compute. split; reflexivity. Qed.

Example ast_host_parse_allocates_fresh_origin :
  run 1000 host_parse_fresh_prog = Ok (VUndef, nil).
Proof. vm_compute. reflexivity. Qed.

Example ast_runtime_leaf_allocates_fresh_origin :
  run 1000 runtime_leaf_fresh_prog = Ok (VUndef, nil).
Proof. vm_compute. reflexivity. Qed.

Example child_bearing_runtime_syntactic_stays_unsupported :
  run 1000 runtime_syn_prog = Stuck "ESyntactic(parent-alias)".
Proof. vm_compute. reflexivity. Qed.

Example gcd_ok : run 1000 gcd_prog = Ok (VUndef, nil).
Proof. vm_compute. reflexivity. Qed.

Example fibo_ok : run 100000 fibo_prog = Ok (VUndef, nil).
Proof. vm_compute. reflexivity. Qed.

(** ** Observable-effect run: prints in program order *)

Example print2_ok : run 1000 print2_prog = Ok (VUndef, [VMath 1; VMath 2]).
Proof. vm_compute. reflexivity. Qed.

(** ** Effect-sensitivity of the validation signal

    An effectful callee: [f() { print 7; return 1 }].  The source main
    calls it once and prints the result: trace [7; 1]. *)

Definition eff_f : func :=
  mkFunc false "f" nil (ISeq
    (IPrint (EMath 7) :: IReturn (EMath 1) :: nil)).

Definition eff_src_main : func :=
  mkFunc true "main" nil (ISeq
    (ICall (LName "t") (EClo "f" nil) nil ::
     IPrint (lref "t") :: nil)).

Definition eff_src : prog := mkProg (eff_f :: eff_src_main :: nil).

Example eff_src_trace : run 1000 eff_src = Ok (VUndef, [VMath 7; VMath 1]).
Proof. vm_compute. reflexivity. Qed.

(** A CORRECT fresh-temporary introduction (the T-1 shape targeted by
    Milestone 4): the call lands in a fresh temp first.  The observable
    trace is unchanged. *)

Definition eff_tgt_main : func :=
  mkFunc true "main" nil (ISeq
    (ICall (LTemp 0) (EClo "f" nil) nil ::
     ILet "t" (tref 0) ::
     IPrint (lref "t") :: nil)).

Definition eff_tgt : prog := mkProg (eff_f :: eff_tgt_main :: nil).

Example eff_temp_intro_preserves : run 1000 eff_src = run 1000 eff_tgt.
Proof. vm_compute. reflexivity. Qed.

(** THE REAL TRANSFORMATION (Transform.v): applying [t1_prog] to the
    effectful source preserves the observable run exactly.  This runs the
    actual transformation function, not a hand-written target. *)

Example t1_prog_preserves_eff : run 1000 eff_src = run 1000 (t1_prog eff_src).
Proof. vm_compute. reflexivity. Qed.

Example t1_prog_preserves_gcd : run 1000 gcd_prog = run 1000 (t1_prog gcd_prog).
Proof. vm_compute. reflexivity. Qed.

Example t1_prog_preserves_fibo :
  run 100000 fibo_prog = run 100000 (t1_prog fibo_prog).
Proof. vm_compute. reflexivity. Qed.

(** ** T-3 (spec-shaped optional access, ADR-10) — mirrored IR only

    The receiver is an effectful context call, so "evaluated exactly
    once" is observable.  Positive: the real transformation [t1_prog]
    preserves the trace on both the record and the nullish receiver.
    Negative: re-evaluating the receiver calls it twice — detected. *)

Example t3v_src_trace : run 1000 t3v_src = Ok (VUndef, [VMath 7; VMath 42]).
Proof. vm_compute. reflexivity. Qed.

Example t3v_null_trace : run 1000 t3v_null = Ok (VUndef, [VMath 7; VUndef]).
Proof. vm_compute. reflexivity. Qed.

Example t3v_preserved : run 1000 t3v_src = run 1000 (t1_prog t3v_src).
Proof. vm_compute. reflexivity. Qed.

Example t3v_null_preserved : run 1000 t3v_null = run 1000 (t1_prog t3v_null).
Proof. vm_compute. reflexivity. Qed.

(** The receiver-once obligation has real teeth: re-evaluation prints 7
    twice. *)
Example t3v_reeval_trace :
  run 1000 t3v_reeval = Ok (VUndef, [VMath 7; VMath 7; VMath 42]).
Proof. vm_compute. reflexivity. Qed.

Example t3v_reeval_detected : run 1000 t3v_src <> run 1000 t3v_reeval.
Proof. vm_compute. discriminate. Qed.

(** NEGATIVE TEST 1 — an INCORRECT transformation that duplicates the
    call.  The callee's print fires twice; the harness detects it. *)

Definition eff_bad_dup_main : func :=
  mkFunc true "main" nil (ISeq
    (ICall (LTemp 0) (EClo "f" nil) nil ::
     ICall (LTemp 1) (EClo "f" nil) nil ::
     ILet "t" (tref 0) ::
     IPrint (lref "t") :: nil)).

Definition eff_bad_dup : prog := mkProg (eff_f :: eff_bad_dup_main :: nil).

Example eff_dup_detected : run 1000 eff_src <> run 1000 eff_bad_dup.
Proof. vm_compute. discriminate. Qed.

(** NEGATIVE TEST 2 — an INCORRECT transformation that reorders the
    print past the call.  Same event multiset, different order; the
    harness detects the order change. *)

Definition eff_bad_reorder_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "t" (EMath 1) ::
     IPrint (lref "t") ::
     ICall (LTemp 0) (EClo "f" nil) nil :: nil)).

Definition eff_bad_reorder : prog :=
  mkProg (eff_f :: eff_bad_reorder_main :: nil).

Example eff_reorder_detected : run 1000 eff_src <> run 1000 eff_bad_reorder.
Proof. vm_compute. discriminate. Qed.

(** ** T-2 (optional-field desugaring) executable validation

    Positive: [t2_prog] preserves the run on both the record branch and
    the nullish branch.  Negative: an unguarded desugaring (field access
    without the nullish test) gets Stuck where the source prints
    undefined — the guard obligation is real. *)

Definition t2v_mkrec : func :=
  mkFunc false "mkrec" nil (ISeq
    (ILet "o" (ERecord "R" (("prop", EMath 7) :: nil)) ::
     IReturn (lref "o") :: nil)).

Definition t2v_main_rec : func :=
  mkFunc true "main" nil (ISeq
    (ICall (LName "r") (EClo "mkrec" nil) nil ::
     ILet "x" (EOptField (lref "r") "prop") ::
     IPrint (lref "x") :: nil)).

Definition t2v_rec : prog := mkProg (t2v_mkrec :: t2v_main_rec :: nil).

Example t2v_rec_src : run 1000 t2v_rec = Ok (VUndef, [VMath 7]).
Proof. vm_compute. reflexivity. Qed.

Example t2v_rec_preserved : run 1000 t2v_rec = run 1000 (t2_prog t2v_rec).
Proof. vm_compute. reflexivity. Qed.

Definition t2v_main_null : func :=
  mkFunc true "main" nil (ISeq
    (ILet "x" (EOptField ENull "prop") ::
     IPrint (lref "x") :: nil)).

Definition t2v_null : prog := mkProg (t2v_main_null :: nil).

Example t2v_null_src : run 1000 t2v_null = Ok (VUndef, [VUndef]).
Proof. vm_compute. reflexivity. Qed.

Example t2v_null_preserved : run 1000 t2v_null = run 1000 (t2_prog t2v_null).
Proof. vm_compute. reflexivity. Qed.

(** NEGATIVE — desugaring WITHOUT the nullish guard: *)

Definition t2_bad_desugar (k : nat) (lhs : local) (recv : expr)
    (fld : string) : inst :=
  ISeq (IAssign (RVar (VLocal (LTemp k))) recv ::
        IAssign (RVar (VLocal lhs))
          (ERef (RField (RVar (VLocal (LTemp k))) (EStr (cu fld)))) :: nil).

Definition t2v_null_bad_main : func :=
  mkFunc true "main" nil (ISeq
    (t2_bad_desugar 0 (LName "x") ENull "prop" ::
     IPrint (lref "x") :: nil)).

Definition t2v_null_bad : prog := mkProg (t2v_null_bad_main :: nil).

Example t2v_bad_detected : run 1000 t2v_null <> run 1000 t2v_null_bad.
Proof. vm_compute. discriminate. Qed.

(** NEGATIVE TEST 3 — skipped evaluation: dropping the call entirely
    loses the callee's print. *)

Definition eff_bad_skip_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "t" (EMath 1) :: IPrint (lref "t") :: nil)).

Definition eff_bad_skip : prog := mkProg (eff_f :: eff_bad_skip_main :: nil).

Example eff_skip_detected : run 1000 eff_src <> run 1000 eff_bad_skip.
Proof. vm_compute. discriminate. Qed.

(** ** Three-valued record-refinement regression checks

    These are deliberately tiny pure computations.  They lock the cases
    where a projected nested heap constraint is unknown without allowing
    that unknown to override a definitive result. *)

Example refinement_true_or_unknown :
  decision_or (Some true) None = Some true.
Proof. reflexivity. Qed.

Definition refinement_unknown_binding : record_field_binding :=
  mkRecordFieldBinding "nested" false RFCAddr.

Definition refinement_false_binding : record_field_binding :=
  mkRecordFieldBinding "required" false RFCMath.

Example refinement_false_and_unknown :
  record_bindings_decide
    (("nested", VAddr 0) :: nil)
    (refinement_unknown_binding :: refinement_false_binding :: nil)
  = Some false.
Proof. vm_compute. reflexivity. Qed.

Example exact_closure_refinement_accepts_named_function :
  record_constraint_decide
    (RFCCloNames ("Record[ECMAScriptFunctionObject].Call" :: nil))
    (VClo "Record[ECMAScriptFunctionObject].Call" nil)
  = Some true.
Proof. vm_compute. reflexivity. Qed.

Example exact_closure_refinement_rejects_other_function :
  record_constraint_decide
    (RFCCloNames ("Record[ECMAScriptFunctionObject].Call" :: nil))
    (VClo "Record[OrdinaryObject].Get" nil)
  = Some false.
Proof. vm_compute. reflexivity. Qed.

Example exact_ast_refinement_uses_focused_node :
  record_constraint_decide
    (RFCAstNames ("IdentifierName" :: nil))
    (VAst (AstExported 1) named_ast_root (0%nat :: nil))
  = Some true.
Proof. vm_compute. reflexivity. Qed.

Example exact_string_refinement_uses_utf16_units :
  record_constraint_decide
    (RFCStrSet ((90%Z :: 87%Z :: nil) :: nil))
    (VStr (90%Z :: 87%Z :: nil))
  = Some true.
Proof. vm_compute. reflexivity. Qed.

Example exact_math_sign_refinement_accepts_nonnegative :
  record_constraint_decide
    (RFCMathIntSign false true true) (VMath 0) = Some true /\
  record_constraint_decide
    (RFCMathIntSign false true true) (VMath 7) = Some true /\
  record_constraint_decide
    (RFCMathIntSign false true true) (VMath (-1)) = Some false.
Proof. vm_compute. repeat split; reflexivity. Qed.

Example exact_math_sign_refinement_rejects_non_math :
  record_constraint_decide
    (RFCMathSign true true true) (VBool false) = Some false.
Proof. vm_compute. reflexivity. Qed.

Example exact_math_set_refinement_is_finite_membership :
  record_constraint_decide
    (RFCMathIntSet ((-2)%Z :: 0%Z :: 3%Z :: nil)) (VMath 3) = Some true /\
  record_constraint_decide
    (RFCMathSet ((-2)%Z :: 0%Z :: 3%Z :: nil)) (VMath 2) = Some false.
Proof. vm_compute. split; reflexivity. Qed.

(** T009's exact generated field is no longer [RFCUnsupported]. *)
Example array_iterator_next_index_accepts_zero :
  record_bindings_decide
    (("ArrayLikeIterationKind", VEnum "value") ::
     ("ArrayLikeNextIndex", VMath 0) ::
     ("IteratedArrayLike", VUndef) :: nil)
    (record_own_bindings "ArrayIteratorInstance")
  = Some true.
Proof. vm_compute. reflexivity. Qed.

Example array_iterator_next_index_rejects_negative :
  record_bindings_decide
    (("ArrayLikeIterationKind", VEnum "value") ::
     ("ArrayLikeNextIndex", VMath (-1)) ::
     ("IteratedArrayLike", VUndef) :: nil)
    (record_own_bindings "ArrayIteratorInstance")
  = Some false.
Proof. vm_compute. reflexivity. Qed.

Example unencoded_precise_leaf_is_unknown :
  record_constraint_decide RFCUnsupported (VMath 0) = None.
Proof. reflexivity. Qed.

Definition refinement_unknown_object : obj :=
  ORecord "ExecutionContext" (("Generator", VAddr 1) :: nil).

Example list_refinement_propagates_unknown :
  ty_check_obj_decide
    (TListOf (TRecord "GeneratorExecutionContext"))
    (OList (VAddr 0 :: nil))
    ((0%nat, refinement_unknown_object) :: nil)
  = None.
Proof. vm_compute. reflexivity. Qed.

(** The shared lazy heap-query checker discharges the concrete nested
    obligations that blocked [MakeConstructor] in every audited Test262
    sample.  The runtime tag is only [OrdinaryObject]; satisfying
    [ECMAScriptFunctionObject] therefore takes the structural descendant
    branch and recursively checks its Environment/Realm/list fields. *)

Definition function_object_fields : list (string * val) :=
  ("Call", VClo "Record[ECMAScriptFunctionObject].Call" nil) ::
  ("Environment", VAddr 1) ::
  ("PrivateEnvironment", VNull) ::
  ("FormalParameters", VAst (AstExported 1) named_ast_child nil) ::
  ("ECMAScriptCode", VAst (AstExported 1) named_ast_child nil) ::
  ("Realm", VAddr 2) ::
  ("ScriptOrModule", VNull) ::
  ("ThisMode", VEnum "global") ::
  ("Strict", VBool false) ::
  ("HomeObject", VUndef) ::
  ("SourceText", VStr (cu "")) ::
  ("Fields", VAddr 3) ::
  ("PrivateMethods", VAddr 4) ::
  ("ClassFieldInitializerName", VEnum "empty") ::
  ("IsClassConstructor", VBool false) ::
  nil.

Definition function_object_heap : list (option obj) :=
  Some (ORecord "OrdinaryObject" function_object_fields) ::
  Some (ORecord "GlobalEnvironmentRecord" nil) ::
  Some (ORecord "RealmRecord" nil) ::
  Some (OList nil) ::
  Some (OList nil) ::
  nil.

Definition function_object_state : xstate :=
  mkXState function_object_heap nil nil None None nil 0.

Example recursive_function_object_refinement_ok :
  run_heap_query_x function_object_state
    (ty_check_query type_check_fuel
      (TRecord "ECMAScriptFunctionObject") (VAddr 0))
  = Ok (Some true).
Proof. vm_compute. reflexivity. Qed.

Definition bad_function_object_state : xstate :=
  mkXState
    (Some (ORecord "OrdinaryObject" function_object_fields) ::
     Some (ORecord "GlobalEnvironmentRecord" nil) ::
     Some (ORecord "RealmRecord" nil) ::
     Some (OMap nil) ::
     Some (OList nil) ::
     nil)
    nil nil None None nil 0.

Example recursive_function_object_wrong_list_rejected :
  run_heap_query_x bad_function_object_state
    (ty_check_query type_check_fuel
      (TRecord "ECMAScriptFunctionObject") (VAddr 0))
  = Ok (Some false).
Proof. vm_compute. reflexivity. Qed.
