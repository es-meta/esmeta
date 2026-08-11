(** * Probe: ToNumeric(true) = ToNumber(true) = 1  —  ON THE ITREE SEMANTICS.

    ToNumericEquivProbe.v proved the equivalence on the Exec route.  This
    file evaluates the SAME generated specification algorithms as ITree
    denotations, inside Rocq, using the relational evaluator of
    RelEvalProbe.v: each axiom-blocked [Any.downcast] at call/store
    boundaries is discharged by [Any.upcast_downcast] instead of reduction.

    The store starts with just the argument global and an allocation
    counter; completion records allocated along the way live in the
    relational store. *)

From Stdlib Require Import String ZArith List Floats.
From CRIS Require Import CRIS.
From ESMetaFV Require Import
  Fragment Domain Events Semantics ITreeExec RelEvalProbe.
From ESMetaFV Require Import Spec.

Import ListNotations.
Local Open Scope string_scope.

Definition spec_lk (fn : irname) : option func :=
  List.find (fun f => String.eqb (f_name f) fn) spec_funcs.

Definition spec_fnames : list string := List.map f_name spec_funcs.

(** IR harness: t0 <- call <alg>(ARG); return t0.Value *)
Definition itree_harness (alg : string) : func :=
  mkFunc true "ESMetaFV.harness" nil (ISeq
    (ICall (LTemp 0) (EClo alg nil)
       (ERef (RVar (VGlobal "ARG")) :: nil) ::
     IReturn (ERef (RField (RVar (VLocal (LTemp 0))) (EStr (cu "Value"))))
     :: nil)).

Section TREE.
Context `{!crisG Γ Σ α β τ _S _I}.
Definition harness_tree0 (alg : string) : itree crisE val :=
  denote_fbody "probe" spec_fnames (itree_harness alg) (nil, nil).
End TREE.

Definition harness_tree (alg : string)
  : ITreeS.ITreeDefinition.itree (@crisE execΣ) val :=
  @harness_tree0 execΣ alg.

Definition σ0 : gmap key Any.t :=
  <[("probe", "g$ARG") := (VBool true)↑]>
    (<[("probe", "alloc$") := (0%nat)↑]> ∅).

Theorem tonumeric_true_itree :
  exists σ', REval "probe" spec_fnames spec_lk σ0
    (harness_tree "ToNumeric") (VNumber (1.0)%float) σ'.
Proof.
  eexists. unfold REval. repeat rstep.
Qed.

Theorem tonumber_true_itree :
  exists σ', REval "probe" spec_fnames spec_lk σ0
    (harness_tree "ToNumber") (VNumber (1.0)%float) σ'.
Proof.
  eexists. unfold REval. repeat rstep.
Qed.
