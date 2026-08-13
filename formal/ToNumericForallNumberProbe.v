(** * Probe: forall f : float, ToNumeric(f) = ToNumber(f) = f — ITree route.

    The interesting case: [f] stays SYMBOLIC through the whole relational
    run.  This works iff every branch on the execution path tests only
    the type tag of the argument, never its value, so [repeat rstep] can
    keep stepping with a variable in the store. *)

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

Theorem tonumber_number :
  forall f : float, exists σ',
    REval "probe" spec_fnames spec_lk (σ0 (VNumber f))
      (harness_tree "ToNumber") (VNumber f) σ'.
Proof.
  intros f; eexists; unfold REval; repeat rstep.
Qed.

Theorem tonumeric_number :
  forall f : float, exists σ',
    REval "probe" spec_fnames spec_lk (σ0 (VNumber f))
      (harness_tree "ToNumeric") (VNumber f) σ'.
Proof.
  intros f; eexists; unfold REval; repeat rstep.
Qed.
