(** * Prototype: the ESMeta heap as CRIS module state

    Today [IRFunctionInput] carries the whole [State] and [Exec_Result] hands it
    back, so the state is part of the *return value*.  CRIS relates returns with

      ist_with_eq Ist = λ '(st_src, v_src) '(st_tgt, v_tgt),
                          (⌜v_src = v_tgt⌝ ∗ Ist st_src st_tgt)

    a pure Rocq equality, and [Ist] only ranges over module-local states
    ([gmap key (option Any.t)]), which the ESMeta [State] is not.  So two abstract
    operations that allocate different intermediates can never be related: their
    [state_next_loc] differ, hence their returns differ.  Only operations whose
    heap behaviour is identical step for step can be proven equivalent.

    Moving the state into one module-local cell takes it out of the return value.
    [Ist] then ranges over it and can relate the two heaps -- or, as here, leave
    them unrelated when nothing observable depends on the difference.

    The demo is exactly the case that fails today: [Source] allocates a temporary
    and [Target] does not, and both return [op_true].

    Note what does *not* change: every pure [State_Completion] operation in
    manual_type.v is reused verbatim.  Only the lift/bind/call layer moves. *)

Require Import type manual_type op.
From Stdlib Require Import String.
From CRIS Require Import CRIS.

(** The result no longer carries a [State]. *)
Inductive Proto_Result (A : Type) : Type :=
  | PRESULT (value : A)
  | PFAIL.

Arguments PRESULT {A} _.
Arguments PFAIL {A}.

(** CRIS provides [cStartFunSim]; this wrapper additionally exposes the typed
    body and discharges the ill-typed call case.  Copied from
    ~/code/verify/day1/exercises/Optimizations.v, which defines it locally. *)
Ltac cStartTypedFunSim x :=
  cStartFunSim;
  cStepsS; cStepsT;
  lazymatch goal with
  | arg : Any.t |- _ =>
      destruct (Any.downcast arg) as [x|];
        cStepsS; [cStepsT|]; ss
  end.

Module ProtoHdr.
  Definition mn := "ESMetaProto".

  (** The single module-local cell holding the whole ESMeta state. *)
  Definition st_key := mn ↯ "state".

  Definition run : fnsig_t (list IRValue) (Proto_Result IRValue) :=
    fnsig "ESMetaProto.run" (fntyp (list IRValue) (Proto_Result IRValue)).
End ProtoHdr.

Section PROTO_MONAD.
  Context `{Σ : GRA}.

  Definition Proto_Comp (A : Type) : Type := itree crisE (Proto_Result A).

  Definition proto_return {A : Type} (value : A) : Proto_Comp A :=
    Ret (PRESULT value).

  Definition proto_fail {A : Type} : Proto_Comp A := Ret PFAIL.

  (** Reuse an existing pure state computation: read the cell, run it, write
      back. This is why manual_type.v needs no change. *)
  Definition proto_lift {A : Type} (computation : State_Completion A)
      : Proto_Comp A :=
    state <- cgetU ProtoHdr.st_key;;
    match computation state with
    | RESULT next value =>
        cput ProtoHdr.st_key next;;; Ret (PRESULT value)
    | FAIL => Ret PFAIL
    end.

  Definition proto_bind {A B : Type}
      (computation : Proto_Comp A)
      (continuation : A -> Proto_Comp B)
      : Proto_Comp B :=
    result <- computation;;
    match result with
    | PRESULT value => continuation value
    | PFAIL => Ret PFAIL
    end.
End PROTO_MONAD.

(** ** Source: allocates a temporary, then answers *)

Module ProtoSource. Section ProtoSource.
  Context `{!crisG Γ Σ α β τ _S _I}.

  Definition scopes : list string := cons ProtoHdr.mn nil.

  Definition run : list IRValue -> Proto_Comp IRValue :=
    fun _ =>
      proto_bind (proto_lift (allocate_list nil))
        (fun _ => proto_return op_true).

  Definition fnsems : fnsemmap :=
    {[fid ProtoHdr.run #
        (msk_scp scopes msk_true, (fsp_none, cfunU ProtoHdr.run run))]}.

  Program Definition smod : SMod.t := {|
    SMod.scopes := scopes;
    SMod.fnsems := fnsems;
    SMod.initial_st := {[ProtoHdr.st_key # (initial_state nil)↑]};
  |}.
  Solve All Obligations with mod_tac.

  Definition t : Mod.t := SMod.to_mod ∅ smod.
End ProtoSource. End ProtoSource.

(** ** Target: answers directly *)

Module ProtoTarget. Section ProtoTarget.
  Context `{!crisG Γ Σ α β τ _S _I}.

  Definition scopes : list string := cons ProtoHdr.mn nil.

  Definition run : list IRValue -> Proto_Comp IRValue :=
    fun _ => proto_return op_true.

  Definition fnsems : fnsemmap :=
    {[fid ProtoHdr.run #
        (msk_scp scopes msk_true, (fsp_none, cfunU ProtoHdr.run run))]}.

  Program Definition smod : SMod.t := {|
    SMod.scopes := scopes;
    SMod.fnsems := fnsems;
    SMod.initial_st := {[ProtoHdr.st_key # (initial_state nil)↑]};
  |}.
  Solve All Obligations with mod_tac.

  Definition t : Mod.t := SMod.to_mod ∅ smod.
End ProtoTarget. End ProtoTarget.

(** ** The equivalence

    [Ist] constrains only the *shape* of the two cells, not their contents: the
    heaps are free to diverge.  That freedom is the whole point -- it is what the
    value-threaded design cannot state. *)

Module ProtoProof. Section ProtoProof.
  Context `{!crisG Γ Σ α β τ _S _I}.

  Local Definition Source := ProtoSource.t.
  Local Definition Target := ProtoTarget.t.

  Definition Ist : ist_type Σ :=
    fun st_src st_tgt =>
      (∃ s t : State,
        ⌜st_src = {[ProtoHdr.st_key # s↑]} /\
         st_tgt = {[ProtoHdr.st_key # t↑]}⌝)%I.

  Lemma simF_run : ISim.sim_fun open Source Target Ist (fid ProtoHdr.run).
  Proof using.
    cStartTypedFunSim arguments.
    unfold ProtoSource.run, ProtoTarget.run.
    unfold proto_bind, proto_lift, proto_return.
    iDestruct "IST" as (s t) "%". destruct H as [-> ->].
    cStepsS. cStepsT. cStep.
    iSplit; [done |].
    iExists (match allocate_list nil s with
             | RESULT next _ => next
             | FAIL => s
             end), t.
    done.
  Qed.

  Lemma sim : ISim.t open Source Target emp%I Ist.
  Proof using.
    cStartModSim.
    all: try solve [mod_tac].
    (** A non-empty [initial_st] adds this goal: [Ist] must hold of the two
        initial cells before any call. *)
    - iIntros "_". iExists (initial_state nil), (initial_state nil). done.
    - apply simF_run.
  Qed.

  Lemma ctxr : ⊢ ctx_refines Target Source.
  Proof using. eapply main_adequacy, sim. Qed.
End ProtoProof. End ProtoProof.

(** Vacuity check.  Being able to prove this equivalence means little unless a
    *false* one fails, so the setup was checked against a negative control:

      sed 's/ProtoSource/NegSource/g; ... ' Proto_HeapState.v > Neg_HeapState.v
      # then make the target answer [op_false] instead of [op_true]

    That version fails at the [iSplit; [done |]] above, on the return-value
    equality [op_true = op_false] -- the differing heaps stay irrelevant, while a
    differing result is rejected.  It is not checked in because it cannot
    compile by design. *)
