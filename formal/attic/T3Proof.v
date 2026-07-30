(** * ESMetaFV.T3Proof — spec-shaped optional access: receiver-once (ADR-10)

    THE THEOREM.  For the program [t3ex_src] — an IR-Core model of the
    CONTROL SHAPE of `x = f()?.prop` (receiver evaluated once, nullish
    guard, property access only on the non-nullish branch), with "f"
    supplied by the linking context — the module denoting the
    transformed program (the literal output of the verified
    transformation [t1_prog]) is contextually equivalent to the source:

      ctx_refines (ir_mod mn (t1_prog t3ex_src)) (ir_mod mn t3ex_src)
      ctx_refines (ir_mod mn t3ex_src) (ir_mod mn (t1_prog t3ex_src))

    WHY THIS SUPERSEDES T-2 (see ADR-10).  Every construct here mirrors
    real ESMeta IR — there is no synthetic source form.  Consequently:

    - both sides are executable by ESMeta and exportable by the
      differential harness (unlike T-2's synthetic source);
    - the source's control shape is taken from the normative text
      (quoted in Programs.v) instead of being invented by us;
    - the receiver is an effectful CONTEXT CALL, so "evaluated exactly
      once" is a genuinely OBSERVABLE obligation (each call is an event
      at the linking boundary): the wrong transformation that
      re-evaluates the receiver calls it twice and is detected —
      `t3v_reeval_detected` in Validation.v, traces [7;42] vs [7;7;42].

    WHAT IS *NOT* ESTABLISHED HERE.  This file does not prove — and the
    project nowhere proves — that JavaScript's `?.` IS the guarded form.
    That correspondence rests on reading the normative text (a
    paper-derived fact, strengthened by inspecting ESMeta's compiled IR;
    see ADR-10), plus an unverified modelling step from that text to
    [t3ex_src].  The model deliberately abstracts, and in one case
    DIVERGES from, real JavaScript:

    - no Reference Records / GetValue, hence no accessor (getter) calls,
      which in real JS can run arbitrary user code during step 2 and 4;
    - no prototype-chain walk and no accessor properties in
      EvaluatePropertyAccessWithIdentifierKey — we read a record field;
    - no ToObject coercion.  DIVERGENCE: in real JS `(42)?.foo` is
      `undefined` (a Number is not nullish, so the access proceeds and
      coerces); in this model a non-address receiver is UB.  The
      theorem's receiver case analysis therefore closes those shapes as
      UB rather than modelling JS behaviour (limitation L-8);
    - no abrupt-completion propagation (the spec's `?` prefixes).

    Consequently T-3 is a theorem about the *transformation's* treatment
    of an effectful receiver and a guarded branch, at the fragment level
    — not a statement about ECMAScript `?.`.

    The proof performs the full receiver case analysis: the context may
    return null or undefined (guard taken; property access must NOT
    happen — no heap event on that branch), an address (both sides read
    the same store with the same [SGet]), or any other value shape
    (symmetric UB).

    SCOPE: exemplar program family, real transformation output, all
    linking contexts — the same fallback scope as T-1/T-2 (PO-015);
    the schematic ∀-programs statement remains open. *)

From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment Domain Events Semantics Transform Programs.

Local Open Scope string_scope.

(** The transformation's freshness precondition, discharged by the
    general theorem [fresh_temp_is_fresh]; here the index is 0. *)
Example t3ex_fresh : fresh_temp (f_body t3_optaccess_main) = 0%nat.
Proof. reflexivity. Qed.

Section T3PROOF.
  Context `{!crisG Γ Σ α β τ _S _I}.

  Definition t3mn : string := "ESMetaFV-T3".
  Definition T3Src : Mod.t := ir_mod t3mn t3ex_src.
  Definition T3Tgt : Mod.t := ir_mod t3mn (t1_prog t3ex_src).

  Definition Ist : ist_type Σ := fun s t => ⌜s = t⌝%I.

  Ltac t3round Hn Hu :=
    (* D-1: record field keys arrive as code units; the round trip
       (Fragment.ascii_of_cstr_cu) exposes the field name again. *)
    try (rewrite ascii_of_cstr_cu; [|reflexivity]);
    try rewrite !env_lookup_update_same;
    try rewrite Hn;
    try rewrite Hu;
    try cStepsS;
    try cStepsT.
  Ltac t3step Hn Hu := do 12 (t3round Hn Hu).

  (* Adapted from the CRIS workshop, day1/answers/Optimizations.v. *)
  Ltac cStartTypedFunSim x :=
    cStartFunSim;
    cStepsS; cStepsT;
    lazymatch goal with
    | arg : Any.t |- _ =>
        destruct (Any.downcast arg) as [x|];
          cStepsS; [cStepsT|]; ss
    end.

  (* shared tail once the field value is in hand *)
  Ltac t3roundTAIL Hn Hu :=
    do 3 (t3round Hn Hu);
    unfold log_val; do 4 (try cStepsS; try cStepsT);
    cStep as reply; cStep; iSplit; done.

  (** Shared tail: from "the context returned [rv], both sides at the
      nullish guard" to Qed, by cases on [rv]. *)
  Ltac t3ub Hn Hu := do 3 (t3round Hn Hu); first [contradiction | ss | done].

  (** Closes the two nullish branches and leaves the non-nullish goal open.
      The VAddr continuation is INLINED at each call site: inside an Ltac
      body the `Any.downcast` pattern is elaborated at definition time and
      then fails to match the goal, while the same steps work inline. *)
  Ltac t3nullish rv Hn Hu :=
    destruct (val_eqb rv VUndef) eqn:Hu;
    [ t3roundTAIL Hu Hu
    | destruct (val_eqb rv VNull) eqn:Hn;
      [ t3roundTAIL Hu Hn | ] ].

  (** ** Direction 1: transformed module refines the source *)

  Lemma simF_main_st :
    ISim.sim_fun open T3Src T3Tgt Ist (funid "main").
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
    try rewrite !env_lookup_update_same.
    try cStepsS. try cStepsT.
    t3nullish rv Hn Hu.
    destruct rv;
      try (simpl in Hn; discriminate Hn);
      try (simpl in Hu; discriminate Hu).
    all: t3step Hn Hu.
    all: try (first [contradiction | ss]).
    (* only the address receiver survives: paired SGet on equal stores *)
    unfold get_obj, cgetU. do 2 (t3round Hn Hu).
    iApply wsim_sget_src. iApply wsim_sget_tgt. do 2 (t3round Hn Hu).
    match goal with
    | |- context [Any.downcast ?X] => destruct (Any.downcast X) as [oo|] eqn:Hd
    end.
    2: { t3ub Hn Hu. }
    destruct oo as [vs0|tn0 fs0|es0].
    { t3ub Hn Hu. }
    { do 2 (t3round Hn Hu).
      destruct (fields_lookup fs0 "prop") as [pv0|] eqn:Hf.
      { t3roundTAIL Hn Hu. } { t3ub Hn Hu. } }
    { do 2 (t3round Hn Hu).
      destruct (map_lookup es0 (VStr (cu "prop"))) as [pv1|] eqn:Hm.
      { t3roundTAIL Hn Hu. } { t3ub Hn Hu. } }
  Qed.

  Lemma simF_entry_st :
    ISim.sim_fun open T3Src T3Tgt Ist entry.
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
    t3nullish rv Hn Hu.
    destruct rv;
      try (simpl in Hn; discriminate Hn);
      try (simpl in Hu; discriminate Hu).
    all: t3step Hn Hu.
    all: try (first [contradiction | ss]).
    (* only the address receiver survives: paired SGet on equal stores *)
    unfold get_obj, cgetU. do 2 (t3round Hn Hu).
    iApply wsim_sget_src. iApply wsim_sget_tgt. do 2 (t3round Hn Hu).
    match goal with
    | |- context [Any.downcast ?X] => destruct (Any.downcast X) as [oo|] eqn:Hd
    end.
    2: { t3ub Hn Hu. }
    destruct oo as [vs0|tn0 fs0|es0].
    { t3ub Hn Hu. }
    { do 2 (t3round Hn Hu).
      destruct (fields_lookup fs0 "prop") as [pv0|] eqn:Hf.
      { t3roundTAIL Hn Hu. } { t3ub Hn Hu. } }
    { do 2 (t3round Hn Hu).
      destruct (map_lookup es0 (VStr (cu "prop"))) as [pv1|] eqn:Hm.
      { t3roundTAIL Hn Hu. } { t3ub Hn Hu. } }
  Qed.

  Lemma sim_st : ISim.t open T3Src T3Tgt emp%I Ist.
  Proof using.
    cStartModSim.
    all: try solve [mod_tac].
    all: try solve [iIntros "_"; done].
    all: try solve [apply simF_main_st].
    all: try solve [apply simF_entry_st].
  Qed.

  Lemma ctxr_tgt_src : ⊢ ctx_refines T3Tgt T3Src.
  Proof using. eapply main_adequacy, sim_st. Qed.

  (** ** Direction 2: source refines the transformed module *)

  Lemma simF_main_ts :
    ISim.sim_fun open T3Tgt T3Src Ist (funid "main").
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
    try rewrite !env_lookup_update_same.
    try cStepsS. try cStepsT.
    t3nullish rv Hn Hu.
    destruct rv;
      try (simpl in Hn; discriminate Hn);
      try (simpl in Hu; discriminate Hu).
    all: t3step Hn Hu.
    all: try (first [contradiction | ss]).
    (* only the address receiver survives: paired SGet on equal stores *)
    unfold get_obj, cgetU. do 2 (t3round Hn Hu).
    iApply wsim_sget_src. iApply wsim_sget_tgt. do 2 (t3round Hn Hu).
    match goal with
    | |- context [Any.downcast ?X] => destruct (Any.downcast X) as [oo|] eqn:Hd
    end.
    2: { t3ub Hn Hu. }
    destruct oo as [vs0|tn0 fs0|es0].
    { t3ub Hn Hu. }
    { do 2 (t3round Hn Hu).
      destruct (fields_lookup fs0 "prop") as [pv0|] eqn:Hf.
      { t3roundTAIL Hn Hu. } { t3ub Hn Hu. } }
    { do 2 (t3round Hn Hu).
      destruct (map_lookup es0 (VStr (cu "prop"))) as [pv1|] eqn:Hm.
      { t3roundTAIL Hn Hu. } { t3ub Hn Hu. } }
  Qed.

  Lemma simF_entry_ts :
    ISim.sim_fun open T3Tgt T3Src Ist entry.
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
    t3nullish rv Hn Hu.
    destruct rv;
      try (simpl in Hn; discriminate Hn);
      try (simpl in Hu; discriminate Hu).
    all: t3step Hn Hu.
    all: try (first [contradiction | ss]).
    (* only the address receiver survives: paired SGet on equal stores *)
    unfold get_obj, cgetU. do 2 (t3round Hn Hu).
    iApply wsim_sget_src. iApply wsim_sget_tgt. do 2 (t3round Hn Hu).
    match goal with
    | |- context [Any.downcast ?X] => destruct (Any.downcast X) as [oo|] eqn:Hd
    end.
    2: { t3ub Hn Hu. }
    destruct oo as [vs0|tn0 fs0|es0].
    { t3ub Hn Hu. }
    { do 2 (t3round Hn Hu).
      destruct (fields_lookup fs0 "prop") as [pv0|] eqn:Hf.
      { t3roundTAIL Hn Hu. } { t3ub Hn Hu. } }
    { do 2 (t3round Hn Hu).
      destruct (map_lookup es0 (VStr (cu "prop"))) as [pv1|] eqn:Hm.
      { t3roundTAIL Hn Hu. } { t3ub Hn Hu. } }
  Qed.

  Lemma sim_ts : ISim.t open T3Tgt T3Src emp%I Ist.
  Proof using.
    cStartModSim.
    all: try solve [mod_tac].
    all: try solve [iIntros "_"; done].
    all: try solve [apply simF_main_ts].
    all: try solve [apply simF_entry_ts].
  Qed.

  Lemma ctxr_src_tgt : ⊢ ctx_refines T3Src T3Tgt.
  Proof using. eapply main_adequacy, sim_ts. Qed.

  (** ** The T-3 theorem: mutual contextual refinement, mirrored IR only *)

  Theorem t3_contextual_equivalence :
    (⊢ ctx_refines (ir_mod t3mn (t1_prog t3ex_src)) (ir_mod t3mn t3ex_src))
    /\
    (⊢ ctx_refines (ir_mod t3mn t3ex_src) (ir_mod t3mn (t1_prog t3ex_src))).
  Proof using.
    split; [exact ctxr_tgt_src | exact ctxr_src_tgt].
  Qed.

End T3PROOF.

(** Axiom audit, printed at every build. *)
Print Assumptions t3_contextual_equivalence.
