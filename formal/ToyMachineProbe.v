(** * Probe: a minimal vm_compute-friendly machine for [crisE] trees.

    [exec_itree] (ITreeExec.v) routes every step through interp layers,
    [Any.t] packaging, and an explicit call-stack machine; under vm_compute
    even the toy [sum] program did not finish.  This probe consumes the SAME
    raw denotation with a fuel-recursive handler that threads the keyed
    store directly, to test whether a re-assembled machine can make closed
    ITree execution computable inside Rocq.

    Scope: handles [agE] (trivially) and [pgE] (explicit gmap store).
    [callE] and [coreE] are reported as unhandled — [sum] uses neither. *)

From Stdlib Require Import String List.
From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment Domain Events Semantics Programs ITreeExec.

Local Open Scope string_scope.

Inductive mini_outcome (R : Type) : Type :=
| MiniDone (r : R) (steps : nat)
| MiniOut
| MiniStuck (site : string).
Arguments MiniDone {R}.
Arguments MiniOut {R}.
Arguments MiniStuck {R}.

Definition mini_store : Type := gmap (string * string) Any.t.

Fixpoint mini_run {R : Type}
  (fuel steps : nat) (σ : mini_store)
  (t : ITreeS.ITreeDefinition.itree (@crisE execΣ) R)
  : mini_outcome R :=
  match fuel with
  | O => MiniOut
  | S fuel' =>
      match ITreeS.ITreeDefinition.observe t with
      | ITreeS.ITreeDefinition.RetF r => MiniDone r steps
      | ITreeS.ITreeDefinition.TauF next =>
          mini_run fuel' (S steps) σ next
      | ITreeS.ITreeDefinition.VisF e k =>
          match e with
          | inl1 ag =>
              (* Assume/AssumeRes/Guarantee all answer [unit]. *)
              match ag in agE X return (X -> _) -> mini_outcome R with
              | Assume _ => fun k => mini_run fuel' (S steps) σ (k tt)
              | AssumeRes _ => fun k => mini_run fuel' (S steps) σ (k tt)
              | Guarantee _ => fun k => mini_run fuel' (S steps) σ (k tt)
              end k
          | inr1 (inl1 _) => MiniStuck "callE"
          | inr1 (inr1 (inl1 pg)) =>
              match pg in pgE X return (X -> _) -> mini_outcome R with
              | SPut key v => fun k =>
                  mini_run fuel' (S steps) (<[key := v]> σ) (k tt)
              | SGet key => fun k =>
                  match σ !! key with
                  | Some v => mini_run fuel' (S steps) σ (k v)
                  | None => MiniStuck "SGet: missing key"
                  end
              end k
          | inr1 (inr1 (inr1 _)) => MiniStuck "coreE"
          end
      end
  end.

Section TREE.
Context `{!crisG Γ Σ α β τ _S _I}.

Definition sum_denote_tree : itree crisE val :=
  denote_fbody "toy" (f_name sum_main :: nil) sum_main (nil, nil).

End TREE.

Definition sum_denote_exec : ITreeS.ITreeDefinition.itree (@crisE execΣ) val :=
  @sum_denote_tree execΣ.

Time Eval vm_compute in
  (match mini_run 1000000 0 ∅ sum_denote_exec with
   | MiniDone v n => inl (v, n)
   | MiniOut => inr "out of fuel"
   | MiniStuck s => inr ("stuck: " ++ s)
   end).
