(** * ESMetaFV.T2Proof — contextual equivalence of the T-2 desugaring

    THE THEOREM.  For the exemplar program [t2ex_src] — main receives a
    value from an UNKNOWN context-supplied function "f", applies the
    synthetic optional-field access [EOptField] (ADR-9), and prints the
    result — the module denoting the T-2-desugared program (the literal
    output of [t2_prog], Transform.v) is contextually equivalent to the
    module denoting the source:

      ctx_refines (ir_mod mn (t2_prog t2ex_src)) (ir_mod mn t2ex_src)
      ctx_refines (ir_mod mn t2ex_src) (ir_mod mn (t2_prog t2ex_src))

    The proof performs a complete case analysis on the (arbitrary)
    receiver value returned by the context: nullish cases yield
    [VUndef] WITHOUT touching the heap on either side (the guard
    obligation — an unguarded desugaring is UB exactly there, see the
    executable negative test [t2v_bad_detected] in Validation.v);
    ill-typed receivers are symmetric UB; address receivers perform the
    same [SGet] against equal stores, with the same downcast/field
    case analysis on both sides.

    SCOPE: exemplar program family, real transformation output, all
    linking contexts — same fallback scope as T-1 (PO-014/PO-006);
    the schematic ∀-programs statement remains open.  [EOptField] is
    synthetic (ADR-9): equivalence is relative to the model's semantics
    of that construct; ESMeta cannot execute the source form. *)

From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment Domain Events Semantics Transform.

Local Open Scope string_scope.

(** ** The exemplar source program

<<
@main def main() = {
  call r = clo<"f">()        // "f" is context-supplied
  let x = r?.prop            // synthetic EOptField
  print x
}
>> *)

Definition t2ex_main : func :=
  mkFunc true "main" nil (ISeq
    (ICall (LName "r") (EClo "f" nil) nil ::
     ILet "x" (EOptField (ERef (RVar (VLocal (LName "r")))) "prop") ::
     IPrint (ERef (RVar (VLocal (LName "x")))) :: nil)).

Definition t2ex_src : prog := mkProg (t2ex_main :: nil).

Example t2ex_admissible : t2_ok_inst (f_body t2ex_main) = true.
Proof. reflexivity. Qed.

Section T2PROOF.
  Context `{!crisG Γ Σ α β τ _S _I}.

  Definition t2mn : string := "ESMetaFV-T2".
  Definition T2Src : Mod.t := ir_mod t2mn t2ex_src.
  Definition T2Tgt : Mod.t := ir_mod t2mn (t2_prog t2ex_src).

  (** Module-local stores stay equal (neither main writes the store;
      the paired reads in the address case read equal stores). *)
  Definition Ist : ist_type Σ := fun s t => ⌜s = t⌝%I.

  (* Bounded normalization rounds: alternate itree stepping with the
     environment and nullish-test rewrites.  [do n] bounds guarantee
     termination. *)
  Ltac t2round Hn Hu :=
    try rewrite !env_lookup_update_same;
    try rewrite Hn;
    try rewrite Hu;
    try cStepsS;
    try cStepsT.
  Ltac t2step Hn Hu := do 6 (t2round Hn Hu).

  (* Adapted from the CRIS workshop, day1/answers/Optimizations.v. *)
  Ltac cStartTypedFunSim x :=
    cStartFunSim;
    cStepsS; cStepsT;
    lazymatch goal with
    | arg : Any.t |- _ =>
        destruct (Any.downcast arg) as [x|];
          cStepsS; [cStepsT|]; ss
    end.

  (** Shared tail: from "receiver value [rv] bound, both sides at the
      nullish test" to Qed, by cases on [rv]. *)
  Ltac t2branches rv Hn Hu :=
    destruct (val_eqb rv VNull) eqn:Hn;
    [ (* null *)
      t2step Hn Hn;
      unfold log_val; try cStepsS; try cStepsT;
      cStep as reply; cStep; iSplit; done
    | destruct (val_eqb rv VUndef) eqn:Hu;
      [ (* undef *)
        t2step Hn Hu;
        unfold log_val; try cStepsS; try cStepsT;
        cStep as reply; cStep; iSplit; done
      | (* non-nullish: field access on both sides *)
        t2step Hn Hu;
        destruct rv;
          try (simpl in Hn; discriminate Hn);
          try (simpl in Hu; discriminate Hu);
        (* six receiver shapes; all but VAddr are symmetric UB *)
        t2step Hn Hu;
        try contradiction;
        (* VAddr: paired SGet on equal stores *)
        unfold get_obj, cgetU;
        do 2 (t2round Hn Hu);
        iApply wsim_sget_src; iApply wsim_sget_tgt;
        do 2 (t2round Hn Hu);
        lazymatch goal with
        | |- context [Any.downcast ?X] =>
            let Hd := fresh "Hd" in
            destruct (Any.downcast X) as [?o|] eqn:Hd;
            [| do 2 (t2round Hn Hu); contradiction ]
        end;
        lazymatch goal with
        | o : obj |- _ =>
            destruct o as [?vs|?tn ?fs];
            [ do 2 (t2round Hn Hu); contradiction |]
        end;
        do 2 (t2round Hn Hu);
        lazymatch goal with
        | fs : list (string * val) |- _ =>
            let Hf := fresh "Hf" in
            destruct (fields_lookup fs "prop") as [?pv|] eqn:Hf;
            [| do 2 (t2round Hn Hu); contradiction ]
        end;
        do 3 (t2round Hn Hu);
        unfold log_val; try cStepsS; try cStepsT;
        cStep as reply; cStep; iSplit; done
      ]
    ].

  (** ** Direction 1: desugared module refines the source *)

  Lemma simF_main_st :
    ISim.sim_fun open T2Src T2Tgt Ist (funid "main").
  Proof using.
    cStartTypedFunSim u.
    destruct u as [captured args].
    destruct args as [|a args'].
    2: { cStepsS. ss. }
    cStepsS. cStepsT.
    iDestruct "IST" as "%". subst.
    iAssert (Ist st_tgt st_tgt) as "IST"; [done|].
    cCall "IST" as (ret st_src' st_tgt') "IST".
    destruct (Any.downcast ret) as [rv|].
    2: { cStepsS. ss. }
    iDestruct "IST" as "%". subst.
    cStepsS. cStepsT.
    rewrite !env_lookup_update_same.
    cStepsS. cStepsT.
    t2branches rv Hn Hu.
  Qed.

  Lemma simF_entry_st :
    ISim.sim_fun open T2Src T2Tgt Ist entry.
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
    try rewrite !env_lookup_update_same.
    try cStepsS. try cStepsT.
    t2branches rv Hn Hu.
  Qed.

  Lemma sim_st : ISim.t open T2Src T2Tgt emp%I Ist.
  Proof using.
    cStartModSim.
    all: try solve [mod_tac].
    all: try solve [iIntros "_"; done].
    all: try solve [apply simF_main_st].
    all: try solve [apply simF_entry_st].
  Qed.

  Lemma ctxr_tgt_src : ⊢ ctx_refines T2Tgt T2Src.
  Proof using. eapply main_adequacy, sim_st. Qed.

  (** ** Direction 2: source refines the desugared module *)

  Lemma simF_main_ts :
    ISim.sim_fun open T2Tgt T2Src Ist (funid "main").
  Proof using.
    cStartTypedFunSim u.
    destruct u as [captured args].
    destruct args as [|a args'].
    2: { cStepsS. ss. }
    cStepsS. cStepsT.
    iDestruct "IST" as "%". subst.
    iAssert (Ist st_tgt st_tgt) as "IST"; [done|].
    cCall "IST" as (ret st_src' st_tgt') "IST".
    destruct (Any.downcast ret) as [rv|].
    2: { cStepsS. ss. }
    iDestruct "IST" as "%". subst.
    cStepsS. cStepsT.
    rewrite !env_lookup_update_same.
    cStepsS. cStepsT.
    t2branches rv Hn Hu.
  Qed.

  Lemma simF_entry_ts :
    ISim.sim_fun open T2Tgt T2Src Ist entry.
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
    try rewrite !env_lookup_update_same.
    try cStepsS. try cStepsT.
    t2branches rv Hn Hu.
  Qed.

  Lemma sim_ts : ISim.t open T2Tgt T2Src emp%I Ist.
  Proof using.
    cStartModSim.
    all: try solve [mod_tac].
    all: try solve [iIntros "_"; done].
    all: try solve [apply simF_main_ts].
    all: try solve [apply simF_entry_ts].
  Qed.

  Lemma ctxr_src_tgt : ⊢ ctx_refines T2Src T2Tgt.
  Proof using. eapply main_adequacy, sim_ts. Qed.

  (** ** The T-2 theorem: mutual contextual refinement *)

  Theorem t2_contextual_equivalence :
    (⊢ ctx_refines (ir_mod t2mn (t2_prog t2ex_src)) (ir_mod t2mn t2ex_src))
    /\
    (⊢ ctx_refines (ir_mod t2mn t2ex_src) (ir_mod t2mn (t2_prog t2ex_src))).
  Proof using.
    split; [exact ctxr_tgt_src | exact ctxr_src_tgt].
  Qed.

End T2PROOF.

(** Axiom audit, printed at every build. *)
Print Assumptions t2_contextual_equivalence.
