(** * Probe: forall b : bool, ToNumeric(b) = ToNumber(b) — ITree route.

    Case split on [b] gives two closed relational runs; each is the same
    cost as the ground probe (ToNumericITreeProbe.v). *)

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

Definition σ0 (arg : val) : gmap key Any.t :=
  <[("probe", "g$ARG") := arg↑]>
    (<[("probe", "alloc$") := (0%nat)↑]> ∅).

Definition bool_to_number (b : bool) : val :=
  VNumber (if b then 1.0 else 0.0)%float.

Theorem tonumeric_tonumber_bool :
  forall b : bool, exists σ1 σ2,
    REval "probe" spec_fnames spec_lk (σ0 (VBool b))
      (harness_tree "ToNumeric") (bool_to_number b) σ1 /\
    REval "probe" spec_fnames spec_lk (σ0 (VBool b))
      (harness_tree "ToNumber") (bool_to_number b) σ2.
Proof.
  destruct b; do 2 eexists; split; unfold REval; repeat rstep.
Qed.
