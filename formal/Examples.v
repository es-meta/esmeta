(** * ESMetaFV.Examples — packaged CRIS modules of the corpus programs

    The program terms themselves live in [Programs.v] (stdlib-only);
    this file packages them as CRIS modules and states the first
    denotation-level effect fact. *)

From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment Domain Events Semantics Programs.

Local Open Scope string_scope.
Local Open Scope Z_scope.

Section EXAMPLES.
  Context `{!crisG Γ Σ α β τ _S _I}.

  Definition SumMod : Mod.t := ir_mod "ESMetaFV-Sum" sum_prog.
  Definition GcdMod : Mod.t := ir_mod "ESMetaFV-Gcd" gcd_prog.
  Definition FiboMod : Mod.t := ir_mod "ESMetaFV-Fibo" fibo_prog.
  Definition Print2Mod : Mod.t := ir_mod "ESMetaFV-Print2" print2_prog.

  (** The closed behaviors these modules denote (sanity typechecks). *)
  Check (Beh.of_itree (LMod.compile (Mod.to_lmod SumMod ε) tt↑)).
  Check (Beh.of_itree (LMod.compile (Mod.to_lmod GcdMod ε) tt↑)).
  Check (Beh.of_itree (LMod.compile (Mod.to_lmod FiboMod ε) tt↑)).
  Check (Beh.of_itree (LMod.compile (Mod.to_lmod Print2Mod ε) tt↑)).

  (** First observable-effect fact: the denotation of [print2_main]'s body
      is literally the two log events in program order followed by normal
      completion — the semantics neither drops, duplicates, nor reorders
      the observable effects (observable-behavior spec O-1). *)
  Lemma print2_body_trace (mn : string) (fnames : list string) (ρ : env) :
    denote_inst mn fnames (f_body print2_main) ρ =
    (log_val (VMath 1);;;
     log_val (VMath 2);;;
     Ret (ρ, CNormal VUndef)).
  Proof. cbn. by ired. Qed.

End EXAMPLES.
