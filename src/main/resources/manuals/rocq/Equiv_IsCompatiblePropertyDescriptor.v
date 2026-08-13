(** * Equivalence of two ECMA-262 abstract operations

    ECMA-262 defines IsCompatiblePropertyDescriptor entirely by delegation:

      IsCompatiblePropertyDescriptor(Extensible, Desc, Current)
        1. Return ValidateAndApplyPropertyDescriptor(undefined, "", Extensible,
           Desc, Current).

    so the operation extracted by ESMeta must be contextually equivalent to a
    direct call on ValidateAndApplyPropertyDescriptor.  This file states that
    equivalence and is the smallest end-to-end instance of the workflow we want:
    generated IR -> CRIS module -> ISim -> ctx_refines.

    Two points make this provable without translating the callee.

    - [ValidateAndApplyPropertyDescriptor] stays an uninterpreted [Call] event on
      both sides, so its own translation may still be fallout.  Only the two
      operations under comparison have to translate.
    - Neither module owns local state -- ESMeta threads the whole [State] through
      [IRFunctionInput]/[Exec_Result] as a value -- so [Ist] is trivial and the
      two call events are literally identical.

    [ISim.sim_fun] relates Source and Target at the *same* function id, so both
    sides are packaged as modules exporting [EquivHdr.run].

    Build with `make proofs` in the directory `esmeta rocqgen` writes. *)

Require Import type manual_type op itree_state Signatures.
Require Import AbsOp_IsCompatiblePropertyDescriptor.
From Stdlib Require Import ZArith String Ascii.
From CRIS Require Import CRIS.

(** CRIS stores function bodies behind an [Any.t] interface.  CRIS itself only
    provides [cStartFunSim]; this wrapper additionally exposes the typed body and
    discharges the ill-typed call case.  Copied verbatim from
    ~/code/verify/day1/exercises/Optimizations.v, where the workshop defines it
    locally. *)
Ltac cStartTypedFunSim x :=
  cStartFunSim;
  cStepsS; cStepsT;
  lazymatch goal with
  | arg : Any.t |- _ =>
      destruct (Any.downcast arg) as [x|];
        cStepsS; [cStepsT|]; ss
  end.

(** The single exported function id both implementations register under. *)
Module EquivHdr.
  Definition mn := "ESMetaEquiv".

  Definition run : fnsig_t IRFunctionInput IRFunctionOutput :=
    fnsig "ESMetaEquiv.IsCompatiblePropertyDescriptor" ir_function_type.
End EquivHdr.

(** ** Source: the operation as extracted from ECMA-262 *)

Module EquivSource. Section EquivSource.
  Context `{!crisG Γ Σ α β τ _S _I}.

  Definition scopes : list string := cons EquivHdr.mn nil.

  Definition run : IRFunctionInput -> itree crisE IRFunctionOutput :=
    fun input =>
      let '(arguments, state) := input in
      match arguments with
      | cons Extensible (cons Desc (cons Current nil)) =>
          ir_AbsOp_IsCompatiblePropertyDescriptor Extensible Desc Current state
      | _ => Ret FAIL
      end.

  Definition fnsems : fnsemmap :=
    {[fid EquivHdr.run #
        (msk_scp scopes msk_true,
         (fsp_none, cfunU EquivHdr.run run))]}.

  Program Definition smod : SMod.t := {|
    SMod.scopes := scopes;
    SMod.fnsems := fnsems;
    SMod.initial_st := ∅;
  |}.
  Solve All Obligations with mod_tac.

  Definition t : Mod.t := SMod.to_mod ∅ smod.
End EquivSource. End EquivSource.

(** ** Target: the delegation written directly *)

Module EquivTarget. Section EquivTarget.
  Context `{!crisG Γ Σ α β τ _S _I}.

  Definition scopes : list string := cons EquivHdr.mn nil.

  Definition run : IRFunctionInput -> itree crisE IRFunctionOutput :=
    fun input =>
      let '(arguments, state) := input in
      match arguments with
      | cons Extensible (cons Desc (cons Current nil)) =>
          itree_state_call sig_AbsOp_ValidateAndApplyPropertyDescriptor
            (cons IR_undefined
              (cons (IR_ESValue (StrV EmptyString))
                (cons Extensible (cons Desc (cons Current nil)))))
            state
      | _ => Ret FAIL
      end.

  Definition fnsems : fnsemmap :=
    {[fid EquivHdr.run #
        (msk_scp scopes msk_true,
         (fsp_none, cfunU EquivHdr.run run))]}.

  Program Definition smod : SMod.t := {|
    SMod.scopes := scopes;
    SMod.fnsems := fnsems;
    SMod.initial_st := ∅;
  |}.
  Solve All Obligations with mod_tac.

  Definition t : Mod.t := SMod.to_mod ∅ smod.
End EquivTarget. End EquivTarget.

(** ** The two refinements

    Contextual equivalence is refinement in both directions.  Both proofs unfold
    to the same shape: an arity check that returns [FAIL] on both sides, then one
    matching [Call] event, then matching returns. *)

Module EquivProof. Section EquivProof.
  Context `{!crisG Γ Σ α β τ _S _I}.

  Local Definition Source := EquivSource.t.
  Local Definition Target := EquivTarget.t.

  (** Both modules have empty local state, so every state pair is related. *)
  Definition Ist : ist_type Σ := fun _ _ => True%I.

  (** Argument lists of the wrong length return [FAIL] on both sides. *)
  Local Ltac solve_arity_mismatch := cStep; iSplit; done.

  (** Reduce both bodies to the point where the shared [Call] event is exposed.
      On the source side that means unfolding the generated monadic scaffolding.
      [itree_state_return] is [itree_state_lift (state_return v)], so all three
      have to go for the argument binds to become [Ret (RESULT state v)]; [ired]
      then applies the monad laws and the [Exec_Result] matches reduce by iota,
      leaving the same single [ccallU] the target performs directly. *)
  Local Ltac expose_shared_call :=
    unfold EquivSource.run, EquivTarget.run;
    unfold ir_AbsOp_IsCompatiblePropertyDescriptor;
    unfold itree_block_body, itree_block_return;
    unfold itree_state_bind, itree_state_return, itree_state_lift,
      itree_state_call, state_return;
    ired.

  Lemma simF_run :
    ISim.sim_fun open Source Target Ist (fid EquivHdr.run).
  Proof using.
    cStartTypedFunSim input.
    destruct input as [arguments state].
    destruct arguments as [| Extensible arguments1]; [solve_arity_mismatch |].
    destruct arguments1 as [| Desc arguments2]; [solve_arity_mismatch |].
    destruct arguments2 as [| Current arguments3]; [solve_arity_mismatch |].
    destruct arguments3 as [| junk rest]; [| solve_arity_mismatch].
    expose_shared_call.
    (** Both sides now emit [ccallU sig_AbsOp_ValidateAndApplyPropertyDescriptor]
        on identical arguments.  [cCall] hands [Ist] to the unknown context and
        resumes with an arbitrary return value and arbitrary post-call states --
        this is where the callee stays uninterpreted, so its own translation
        never enters the proof.  [Any.downcast] is the runtime return-type check
        [ccallU] inserts.  Destructing the typed reply collapses the source's
        residual [Exec_Result] match, which the target never built. *)
    cStepsS. cStepsT.
    cCall "IST" as (ret st_src' st_tgt') "IST".
    destruct Any.downcast as [result|].
    - destruct result; cStepsS; cStepsT; cStep; iSplit; done.
    - cStepsS. ss.
  Qed.

  Lemma sim : ISim.t open Source Target emp%I Ist.
  Proof using.
    cStartModSim.
    all: try solve [mod_tac].
    - apply simF_run.
  Qed.

  Lemma ctxr : ⊢ ctx_refines Target Source.
  Proof using. eapply main_adequacy, sim. Qed.

  (** The other direction. *)

  Lemma simF_run_rev :
    ISim.sim_fun open Target Source Ist (fid EquivHdr.run).
  Proof using.
    cStartTypedFunSim input.
    destruct input as [arguments state].
    destruct arguments as [| Extensible arguments1]; [solve_arity_mismatch |].
    destruct arguments1 as [| Desc arguments2]; [solve_arity_mismatch |].
    destruct arguments2 as [| Current arguments3]; [solve_arity_mismatch |].
    destruct arguments3 as [| junk rest]; [| solve_arity_mismatch].
    expose_shared_call.
    cStepsS. cStepsT.
    cCall "IST" as (ret st_src' st_tgt') "IST".
    destruct Any.downcast as [result|].
    - destruct result; cStepsS; cStepsT; cStep; iSplit; done.
    - cStepsS. ss.
  Qed.

  Lemma sim_rev : ISim.t open Target Source emp%I Ist.
  Proof using.
    cStartModSim.
    all: try solve [mod_tac].
    - apply simF_run_rev.
  Qed.

  Lemma ctxr_rev : ⊢ ctx_refines Source Target.
  Proof using. eapply main_adequacy, sim_rev. Qed.
End EquivProof. End EquivProof.
