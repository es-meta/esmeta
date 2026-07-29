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

From ESMetaFV Require Import Fragment Domain Exec Programs Transform.

Local Open Scope string_scope.
Local Open Scope Z_scope.

(** ** Corpus runs (expected: normal termination, no prints) *)

Example sum_ok : run 1000 sum_prog = Ok (VUndef, nil).
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
          (ERef (RField (RVar (VLocal (LTemp k))) (EStr fld))) :: nil).

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
