(** * ESMetaFV.Events — observable-event interface of the IR-Core semantics

    This file fixes HOW the fragment's effects surface as CRIS events.
    It contains no denotation (Milestone 2) and no proofs; it exists so
    that the observable-behavior specification in
    [formal/docs/formal-verification/itree-transpiler-plan.md] has a checked,
    compilable counterpart.

    Event mapping (per the observable-behavior specification, which must
    be reviewed before Milestone 2 per Research Rule 4):

    - [IPrint v]            ↦ [IO "esmeta.print" v] — observable.
    - calls to functions of the *program*   ↦ internal (inlined/local).
    - calls to functions of the *context*   ↦ [Call fn args] — observable
      at the linking boundary (CRIS [callE]).
    - state (globals/heap)  ↦ CRIS keyed store events ([pgE]) — internal.
    - local environments    ↦ pure parameter threading — no event at all.
    - stuck interpreter states (assert failure, type mismatch, unknown
      variable) ↦ undefined behavior, provisionally (ADR-7, open).

    The CRIS event algebra (repository facts, quoted from the installed
    framework at CRIS/common/Events.v):

      Variant coreE : Type → Type :=
      | Choose (X : Type) : coreE X
      | Take (X : Type) : coreE X
      | IO {O I : Type} (fn : string) (args : O) : coreE I.

    Only [IO] contributes observable trace events; [Choose]/[Take] resolve
    silently (angelic/demonic) at the behavior level. *)

From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment.

Section EVENTS.
  Context `{!crisG Γ Σ α β τ _S _I}.

  (** The name under which the fragment's one primitive observable effect
      appears in traces.  [IPrint e] denotes to [log_val v] where [v] is
      the value of [e]. *)
  Definition log_fn : string := "esmeta.print".

  Definition log_val (v : val) : itree crisE unit :=
    trigger (IO (I := unit) log_fn v).

End EVENTS.

(** Sanity check that the CRIS behavior layer is reachable from this
    project: a closed interaction tree and its behavior predicate
    typecheck.  ([coreE] is Σ-independent, so no section context is
    needed here.) *)

Definition sanity_ret : itree coreE Any.t := Ret (42%Z)↑.

Check (Beh.of_itree sanity_ret).
