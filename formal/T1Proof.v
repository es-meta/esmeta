(** * ESMetaFV.T1Proof — Milestone 4: contextual equivalence of T-1

    THE THEOREM.  For the exemplar effectful program [t1ex_src] — whose
    main calls an UNKNOWN, context-supplied function "f" and then prints
    the result — the module denoting the T-1-transformed program (the
    literal output of [t1_prog], Transform.v) is contextually equivalent
    to the module denoting the source:

      ctx_refines (ir_mod mn (t1_prog t1ex_src)) (ir_mod mn t1ex_src)
      ctx_refines (ir_mod mn t1ex_src) (ir_mod mn (t1_prog t1ex_src))

    [ctx_refines A B := ∀ Ctx, refines (A ★ Ctx) (B ★ Ctx)] quantifies
    over every linking context (CRIS), so the callee "f" ranges over
    every possible behavior: it may print, mutate its own state, call
    back into main re-entrantly, diverge, or crash.  Effect ordering is
    preserved by construction of the behavior/refinement definitions
    (trace inclusion; architecture note §3).

    SCOPE (per PO-006's recorded fallback): the theorem is for this
    program family, with the target produced by the real transformation
    function.  The schematic ∀-programs version remains open in the
    ledger (PO-006), stated as future work — it is NOT claimed here.

    Proof idiom adapted from the CRIS workshop
    (2026-verification-workshop, day1/answers/Optimizations.v):
    per-function [ISim.sim_fun] lemmas, [cCall] across the unknown call,
    [main_adequacy] at the end.  The [cStartTypedFunSim] tactic below is
    adapted from that file (workshop repo is read-only; the tactic is
    reproduced with attribution). *)

From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment Domain Events Semantics Transform.

Local Open Scope string_scope.

(** ** The exemplar source program

<<
@main def main() = {
  call t = clo<"f">()   // "f" is NOT defined here: the context provides it
  print t
}
>> *)

Definition t1ex_main : func :=
  mkFunc true "main" nil (ISeq
    (ICall (LName "t") (EClo "f" nil) nil ::
     IPrint (ERef (RVar (VLocal (LName "t")))) :: nil)).

Definition t1ex_src : prog := mkProg (t1ex_main :: nil).

(** The freshness precondition holds for the designated temporary by the
    general theorem [fresh_temp_is_fresh]; for this body the index is 0. *)
Example t1ex_fresh : fresh_temp (f_body t1ex_main) = 0%nat.
Proof. reflexivity. Qed.

Section T1PROOF.
  Context `{!crisG Γ Σ α β τ _S _I}.

  Definition t1mn : string := "ESMetaFV-T1".
  Definition T1Src : Mod.t := ir_mod t1mn t1ex_src.
  Definition T1Tgt : Mod.t := ir_mod t1mn (t1_prog t1ex_src).

  (** State relation: the module-local stores are equal.  Neither side's
      main touches the store (no allocation, no globals), so equality is
      preserved by every internal step and re-established by [cCall]. *)
  Definition Ist : ist_type Σ := fun s t => ⌜s = t⌝%I.

  (* Adapted from the CRIS workshop, day1/answers/Optimizations.v. *)
  Ltac cStartTypedFunSim x :=
    cStartFunSim;
    cStepsS; cStepsT;
    lazymatch goal with
    | arg : Any.t |- _ =>
        destruct (Any.downcast arg) as [x|];
          cStepsS; [cStepsT|]; ss
    end.

  (** ** Direction 1: the transformed module refines the source

      [ISim.t open T1Src T1Tgt] (source = spec side) yields
      [ctx_refines T1Tgt T1Src] by adequacy. *)

  Lemma simF_main_st :
    ISim.sim_fun open T1Src T1Tgt Ist (funid "main").
  Proof using.
    cStartTypedFunSim u.
    destruct u as [captured args].
    destruct args as [|a args'].
    2: { (* arity mismatch: UB on the spec side discharges the goal *)
      cStepsS. ss. }
    cStepsS. cStepsT.
    iDestruct "IST" as "%". subst.
    iAssert (Ist st_tgt st_tgt) as "IST"; [done|].
    cCall "IST" as (ret st_src' st_tgt') "IST".
    destruct (Any.downcast ret) as [rv|].
    2: { (* ill-typed return from the context: UB on the spec side *)
      cStepsS. ss. }
    iDestruct "IST" as "%". subst.
    cStepsS. cStepsT.
    rewrite !env_lookup_update_same.
    cStepsS. cStepsT.
    rewrite !env_lookup_update_same.
    cStepsS. cStepsT.
    unfold log_val.
    cStepsS. cStepsT.
    cStep as reply.
    cStep. iSplit; done.
  Qed.

  Lemma simF_entry_st :
    ISim.sim_fun open T1Src T1Tgt Ist entry.
  Proof using.
    cStartTypedFunSim u.
    cStepsS. cStepsT.
    iDestruct "IST" as "%". subst.
    iAssert (Ist st_tgt st_tgt) as "IST"; [done|].
    cCall "IST" as (ret st_src' st_tgt') "IST".
    destruct (Any.downcast ret) as [rv|].
    2: { cStepsS. ss. }
    iDestruct "IST" as "%". subst.
    cStepsS. cStepsT.
    unfold log_val.
    cStepsS. cStepsT.
    cStep as reply.
    cStep. iSplit; done.
  Qed.

  Lemma sim_st : ISim.t open T1Src T1Tgt emp%I Ist.
  Proof using.
    cStartModSim.
    all: try solve [mod_tac].
    all: try solve [iIntros "_"; done].
    all: try solve [apply simF_main_st].
    all: try solve [apply simF_entry_st].
  Qed.

  Lemma ctxr_tgt_src : ⊢ ctx_refines T1Tgt T1Src.
  Proof using. eapply main_adequacy, sim_st. Qed.

  (** ** Direction 2: the source refines the transformed module *)

  Lemma simF_main_ts :
    ISim.sim_fun open T1Tgt T1Src Ist (funid "main").
  Proof using.
    cStartTypedFunSim u.
    destruct u as [captured args].
    destruct args as [|a args'].
    2: { (* arity mismatch: UB on the spec side discharges the goal *)
      cStepsS. ss. }
    cStepsS. cStepsT.
    iDestruct "IST" as "%". subst.
    iAssert (Ist st_tgt st_tgt) as "IST"; [done|].
    cCall "IST" as (ret st_src' st_tgt') "IST".
    destruct (Any.downcast ret) as [rv|].
    2: { (* ill-typed return from the context: UB on the spec side *)
      cStepsS. ss. }
    iDestruct "IST" as "%". subst.
    cStepsS. cStepsT.
    rewrite !env_lookup_update_same.
    cStepsS. cStepsT.
    rewrite !env_lookup_update_same.
    cStepsS. cStepsT.
    unfold log_val.
    cStepsS. cStepsT.
    cStep as reply.
    cStep. iSplit; done.
  Qed.

  Lemma simF_entry_ts :
    ISim.sim_fun open T1Tgt T1Src Ist entry.
  Proof using.
    cStartTypedFunSim u.
    cStepsS. cStepsT.
    iDestruct "IST" as "%". subst.
    iAssert (Ist st_tgt st_tgt) as "IST"; [done|].
    cCall "IST" as (ret st_src' st_tgt') "IST".
    destruct (Any.downcast ret) as [rv|].
    2: { cStepsS. ss. }
    iDestruct "IST" as "%". subst.
    cStepsS. cStepsT.
    unfold log_val.
    cStepsS. cStepsT.
    cStep as reply.
    cStep. iSplit; done.
  Qed.

  Lemma sim_ts : ISim.t open T1Tgt T1Src emp%I Ist.
  Proof using.
    cStartModSim.
    all: try solve [mod_tac].
    all: try solve [iIntros "_"; done].
    all: try solve [apply simF_main_ts].
    all: try solve [apply simF_entry_ts].
  Qed.

  Lemma ctxr_src_tgt : ⊢ ctx_refines T1Src T1Tgt.
  Proof using. eapply main_adequacy, sim_ts. Qed.

  (** ** The Milestone 4 theorem: mutual contextual refinement *)

  Theorem t1_contextual_equivalence :
    (⊢ ctx_refines (ir_mod t1mn (t1_prog t1ex_src)) (ir_mod t1mn t1ex_src))
    /\
    (⊢ ctx_refines (ir_mod t1mn t1ex_src) (ir_mod t1mn (t1_prog t1ex_src))).
  Proof using.
    split; [exact ctxr_tgt_src | exact ctxr_src_tgt].
  Qed.

End T1PROOF.

(** Axiom audit: the theorem must depend on no axioms beyond those of the
    CRIS/Iris/ITreeS framework itself.  Inspect with:

      Print Assumptions t1_contextual_equivalence.  *)
Print Assumptions t1_contextual_equivalence.
