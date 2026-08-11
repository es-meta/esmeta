(** * Probe: ECMA-262 — ToNumeric(v) = ToNumber(v) for non-BigInt primitives.

    Both algorithms are the REAL generated spec IR from validation/Spec.v.
    A tiny IR harness (main) calls the named algorithm on the global "ARG"
    and returns the completion's [[Value]] field, so runs are compared on
    the resulting primitive value (and prints), not on completion-record
    addresses, which legitimately differ between the two call paths.

    Scope: v ∈ {undefined, null, booleans, numbers}.  Strings join on both
    paths at the same StringToNumber(s) call but with heaps whose allocation
    counters differ, so the symbolic normal forms are not syntactically
    equal; string instances are checked concretely instead.  Objects are
    correctly OUT of scope: for a BigInt wrapper object ToNumeric returns
    the BigInt while ToNumber throws. *)

From Stdlib Require Import String ZArith List Floats.
From ESMetaFV Require Import Fragment Domain Exec TestEncoding.
From ESMetaFV Require Import Spec.

Import ListNotations.
Local Open Scope string_scope.

(** IR harness: t0 <- call <alg>(ARG); return t0.Value *)
Definition harness_main (alg : string) : func :=
  mkFunc true "ESMetaFV.harness" nil (ISeq
    (ICall (LTemp 0) (EClo alg nil)
       (ERef (RVar (VGlobal "ARG")) :: nil) ::
     IReturn (ERef (RField (RVar (VLocal (LTemp 0))) (EStr (cu "Value"))))
     :: nil)).

Definition harness_prog (alg : string) (v : val) : prog :=
  mkProgFull (harness_main alg :: spec_funcs) None None nil
    (("ARG", v) :: base_globals) init_heap.

Definition FUEL : nat := 1000000.

Print run.

Definition to_numeric (v : val) : out (val * list val) :=
  run FUEL (harness_prog "ToNumeric" v).
Definition to_number (v : val) : out (val * list val) :=
  run FUEL (harness_prog "ToNumber" v).

  
(* Sanity: what do the runs actually produce? *)
Eval vm_compute in (to_numeric (VStr (cu "42"))).
Eval vm_compute in (to_number (VStr (cu "42"))).

(** The theorem, per primitive class.  undefined/null/booleans are closed
    terms; numbers are UNIVERSAL over all floats — the run is symbolic in
    [f] and both sides normalize to the same open term. *)

Theorem tonumeric_eq_tonumber_undef :
  to_numeric VUndef = to_number VUndef.
Proof. vm_compute. reflexivity. Qed.

Theorem tonumeric_eq_tonumber_null :
  to_numeric VNull = to_number VNull.
Proof. vm_compute. reflexivity. Qed.

Theorem tonumeric_eq_tonumber_bool :
  forall b : bool, to_numeric (VBool b) = to_number (VBool b).
Proof. destruct b; vm_compute; reflexivity. Qed.

Theorem tonumeric_eq_tonumber_number :
  forall f : float, to_numeric (VNumber f) = to_number (VNumber f).
Proof. intro f. vm_compute. reflexivity. Qed.

(** String instances (symbolic strings diverge as open terms; see header). *)
Theorem tonumeric_eq_tonumber_str_42 :
  to_numeric (VStr (cu "42")) = to_number (VStr (cu "43")).
Proof. vm_compute. reflexivity. Qed.

Theorem tonumeric_eq_tonumber_str_0 :
  forall s : string, to_numeric (VStr (cu s)) = to_number (VStr (cu s)).
Proof. intro s. vm_compute. reflexivity. Qed.

Theorem tonumeric_eq_tonumber_str_abc :
  to_numeric (VStr (cu "abc")) = to_number (VStr (cu "abc")).
Proof. vm_compute. reflexivity. Qed.

Theorem tonumeric_eq_tonumber_str_empty :
  to_numeric (VStr (cu "")) = to_number (VStr (cu ""))
.
Proof. vm_compute. reflexivity. Qed.
