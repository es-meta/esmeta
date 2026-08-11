(** * Probe: RELATIONAL big-step evaluation of denotation ITrees in Rocq.

    vm_compute cannot run these trees: call/state boundaries contain
    [Any.downcast], which is axiom-blocked (AnyDemo.v).  A PROOF can step
    where a computation cannot: evaluation is defined as an inductive
    relation over OBSERVED trees ([itree']), so each step is [hnf] +
    [change] + a constructor — no unification against cofix — and each
    blocked downcast is discharged by the propositional lemma
    [Any.upcast_downcast] instead of reduction.

    Handles exactly the events generated denotations emit: silent agE,
    keyed-store pgE (explicit gmap, values stored as [x↑]), and
    program-internal callE (callee denoted directly).  Validated on the
    toy [gcd] at the end of this file. *)

From Stdlib Require Import String ZArith List.
From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment Domain Events Semantics Programs ITreeExec.

Import ListNotations.
Local Open Scope string_scope.

Section RELEVAL.
Context `{!crisG Γ Σ α β τ _S _I}.

Variable mn : string.
Variable fnames : list string.
Variable lk : irname -> option func.

Definition rstore : Type := gmap key Any.t.

Local Notation obs t := (ITreeS.ITreeDefinition.observe t).

Inductive REvalF
  : rstore -> ITreeS.ITreeDefinition.itree' crisE val -> val -> rstore
    -> Prop :=
| REvF_ret σ v :
    REvalF σ (ITreeS.ITreeDefinition.RetF v) v σ
| REvF_tau σ t' v σ' :
    REvalF σ (obs t') v σ' ->
    REvalF σ (ITreeS.ITreeDefinition.TauF t') v σ'
| REvF_assume σ P k v σ' :
    REvalF σ (obs (k tt)) v σ' ->
    REvalF σ (ITreeS.ITreeDefinition.VisF (inl1 (Assume P)) k) v σ'
| REvF_guarantee σ P k v σ' :
    REvalF σ (obs (k tt)) v σ' ->
    REvalF σ (ITreeS.ITreeDefinition.VisF (inl1 (Guarantee P)) k) v σ'
| REvF_sget σ key a k v σ' :
    σ !! key = Some a ->
    REvalF σ (obs (k a)) v σ' ->
    REvalF σ
      (ITreeS.ITreeDefinition.VisF (inr1 (inr1 (inl1 (SGet key)))) k) v σ'
| REvF_sput σ key a k v σ' :
    REvalF (<[key := a]> σ) (obs (k tt)) v σ' ->
    REvalF σ
      (ITreeS.ITreeDefinition.VisF (inr1 (inr1 (inl1 (SPut key a)))) k) v σ'
| REvF_call σ fn a k f arg r σ1 v σ' :
    lk fn = Some f ->
    Any.downcast a = Some arg ->
    REvalF σ (obs (denote_fbody mn fnames f arg)) r σ1 ->
    REvalF σ1 (obs (k (r↑))) v σ' ->
    REvalF σ (ITreeS.ITreeDefinition.VisF (inr1 (inl1 (Call fn a))) k) v σ'.

Definition REval (σ : rstore) (t : itree crisE val) (v : val) (σ' : rstore)
  : Prop := REvalF σ (obs t) v σ'.

End RELEVAL.

(* ---------- step tactic ---------- *)

Ltac rnorm :=
  lazymatch goal with
  | |- REvalF ?mn ?fns ?lk ?σ ?o ?v ?σ' =>
      let o' := eval hnf in o in
      lazymatch o' with
      | ITreeS.ITreeDefinition.VisF ?e ?k =>
          let e' := eval cbn in e in
          change (REvalF mn fns lk σ (ITreeS.ITreeDefinition.VisF e' k) v σ')
      | _ => change (REvalF mn fns lk σ o' v σ')
      end
  | |- REval ?mn ?fns ?lk ?σ ?t ?v ?σ' =>
      unfold REval
  end.

Ltac rstep :=
  rnorm;
  first
    [ eapply REvF_ret
    | eapply REvF_tau
    | eapply REvF_assume
    | eapply REvF_guarantee
    | eapply REvF_sget; [vm_compute; reflexivity | ]
    | eapply REvF_sput
    | eapply REvF_call;
        [ vm_compute; reflexivity
        | apply Any.upcast_downcast
        | | ]
    | progress (ired; repeat rewrite Any.upcast_downcast) ].

(* ---------- toy validation: gcd 6 4 = 2, through real denotation ---------- *)

Definition toy_lk (fn : irname) : option func :=
  List.find (fun f => String.eqb (f_name f) fn) (p_funcs gcd_prog).

Definition toy_fnames : list string := List.map f_name (p_funcs gcd_prog).

Definition gcd_small_main : func :=
  mkFunc true "main" nil (ISeq
    (ICall (LName "g") (EClo "gcd" nil) (EMath 6 :: EMath 4 :: nil) ::
     IReturn (ERef (RVar (VLocal (LName "g")))) :: nil)).

Section TREE.
Context `{!crisG Γ Σ α β τ _S _I}.
Definition gcd_tree0 : itree crisE val :=
  denote_fbody "toy" toy_fnames gcd_small_main (nil, nil).
End TREE.

Definition gcd_tree : ITreeS.ITreeDefinition.itree (@crisE execΣ) val :=
  @gcd_tree0 execΣ.

Theorem gcd_6_4_evaluates_to_2 :
  exists σ', REval "toy" toy_fnames toy_lk ∅ gcd_tree (VMath 2) σ'.
Proof.
  eexists. unfold REval. repeat rstep.
Qed.
