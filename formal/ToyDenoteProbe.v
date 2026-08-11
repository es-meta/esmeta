(** * Probe: observe the RAW denotation of the toy [sum] body — no call
    machine, no [exec_trans], no [Any] packaging.  The tree still has
    unresolved [crisE] state events, so [quiet_result_fuel] will stop at the
    first [Vis]; what we measure is whether reaching that point is cheap.
    If it is, the deep embedding / denoter is not the bottleneck — the
    execution machine around it is. *)

From Stdlib Require Import String List.
From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment Domain Events Semantics Programs ITreeExec.

Local Open Scope string_scope.

Fixpoint count_to_first_event {E : Type -> Type} {R : Type}
  (fuel : nat) (t : ITreeS.ITreeDefinition.itree E R) : option nat :=
  match fuel with
  | O => None
  | S fuel' =>
      match ITreeS.ITreeDefinition.observe t with
      | ITreeS.ITreeDefinition.RetF _ => Some 0
      | ITreeS.ITreeDefinition.TauF next =>
          option_map S (count_to_first_event fuel' next)
      | ITreeS.ITreeDefinition.VisF _ _ => Some 0
      end
  end.

Section PROBE.
Context `{!crisG Γ Σ α β τ _S _I}.

Definition sum_denote_tree : itree crisE val :=
  denote_fbody "toy" (f_name sum_main :: nil) sum_main (nil, nil).

End PROBE.

Definition sum_denote_exec : ITreeS.ITreeDefinition.itree crisE val :=
  @sum_denote_tree execΣ.

Time Eval vm_compute in (count_to_first_event 1000 sum_denote_exec).
