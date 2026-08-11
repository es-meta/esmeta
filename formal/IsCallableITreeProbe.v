(** * Gauge: IsCallable(undefined) = false on the ITree semantics, via the
    relational evaluator.  IsCallable returns the bool directly (no
    completion record), so the harness returns the call result as-is.
    This measures per-step cost of [rstep] at specification scale. *)

From Stdlib Require Import String ZArith List Floats.
From CRIS Require Import CRIS.
From ESMetaFV Require Import
  Fragment Domain Events Semantics ITreeExec RelEvalProbe.
From ESMetaFV Require Import Spec.

Import ListNotations.
Local Open Scope string_scope.

Definition spec_lk' (fn : irname) : option func :=
  List.find (fun f => String.eqb (f_name f) fn) spec_funcs.

Definition spec_fnames' : list string := List.map f_name spec_funcs.

Definition ic_harness : func :=
  mkFunc true "ESMetaFV.harness" nil (ISeq
    (ICall (LTemp 0) (EClo "IsCallable" nil)
       (ERef (RVar (VGlobal "ARG")) :: nil) ::
     IReturn (ERef (RVar (VLocal (LTemp 0)))) :: nil)).

Section TREE.
Context `{!crisG Γ Σ α β τ _S _I}.
Definition ic_tree0 : itree crisE val :=
  denote_fbody "probe" spec_fnames' ic_harness (nil, nil).
End TREE.

Definition ic_tree : ITreeS.ITreeDefinition.itree (@crisE execΣ) val :=
  @ic_tree0 execΣ.

Definition σic : gmap key Any.t :=
  <[("probe", "g$ARG") := VUndef↑]>
    (<[("probe", "alloc$") := (0%nat)↑]> ∅).

Theorem is_callable_undef_itree :
  exists σ', REval "probe" spec_fnames' spec_lk' σic
    ic_tree (VBool false) σ'.
Proof.
  eexists. unfold REval. repeat rstep.
Qed.
