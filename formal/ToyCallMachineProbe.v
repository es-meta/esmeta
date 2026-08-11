(** * Probe: extend the vm_compute-friendly mini machine with calls.

    [ToyMachineProbe.v] ran the pure [sum] tree, never exercising [callE].
    This probe runs [gcd_prog] — recursive calls through [ccallU], i.e. one
    [Any.t] upcast/downcast per call boundary — on a defunctionalized
    machine: an explicit stack of [Any.t -> itree] continuations inside one
    fuel-indexed Fixpoint.  No interp layering, store threaded directly. *)

From Stdlib Require Import String List.
From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment Domain Events Semantics Programs ITreeExec.

Import ListNotations.
Local Open Scope string_scope.

Inductive mini_outcome (R : Type) : Type :=
| MiniDone (r : R) (steps : nat)
| MiniOut
| MiniStuck (site : string).
Arguments MiniDone {R}.
Arguments MiniOut {R}.
Arguments MiniStuck {R}.

Definition mini_store : Type := gmap (string * string) Any.t.

Section MACHINE.
Context `{!crisG Γ Σ α β τ _S _I}.

Definition toy_mn : string := "toy".

Definition toy_fnames : list string :=
  List.map f_name (p_funcs gcd_prog).

Definition toy_lookup (fn : string) : option func :=
  List.find (fun f => String.eqb (f_name f) fn) (p_funcs gcd_prog).

Fixpoint mini_call_run
  (fuel steps : nat) (σ : mini_store)
  (cur : itree crisE val)
  (stack : list (Any.t -> itree crisE val))
  : mini_outcome val :=
  match fuel with
  | O => MiniOut
  | S fuel' =>
      match ITreeS.ITreeDefinition.observe cur with
      | ITreeS.ITreeDefinition.RetF v =>
          match stack with
          | nil => MiniDone v steps
          | k :: rest =>
              mini_call_run fuel' (S steps) σ (k (v↑)) rest
          end
      | ITreeS.ITreeDefinition.TauF next =>
          mini_call_run fuel' (S steps) σ next stack
      | ITreeS.ITreeDefinition.VisF e k =>
          match e with
          | inl1 ag =>
              match ag in agE X
                    return (X -> itree crisE val) -> mini_outcome val with
              | Assume _ => fun k => mini_call_run fuel' (S steps) σ (k tt) stack
              | AssumeRes _ => fun k => mini_call_run fuel' (S steps) σ (k tt) stack
              | Guarantee _ => fun k => mini_call_run fuel' (S steps) σ (k tt) stack
              end k
          | inr1 (inl1 c) =>
              match c in callE X
                    return (X -> itree crisE val) -> mini_outcome val with
              | Call fn a => fun k =>
                  match toy_lookup fn with
                  | None => MiniStuck ("unknown function: " ++ fn)
                  | Some f =>
                      match @Any.downcast ir_arg a with
                      | None => MiniStuck "argument downcast"
                      | Some arg =>
                          mini_call_run fuel' (S steps) σ
                            (denote_fbody toy_mn toy_fnames f arg)
                            (k :: stack)
                      end
                  end
              | Spawn _ _ => fun _ => MiniStuck "Spawn"
              | Yield _ => fun _ => MiniStuck "Yield"
              | GetTid => fun _ => MiniStuck "GetTid"
              end k
          | inr1 (inr1 (inl1 pg)) =>
              match pg in pgE X
                    return (X -> itree crisE val) -> mini_outcome val with
              | SPut key v => fun k =>
                  mini_call_run fuel' (S steps) (<[key := v]> σ) (k tt) stack
              | SGet key => fun k =>
                  match σ !! key with
                  | Some v => mini_call_run fuel' (S steps) σ (k v) stack
                  | None => MiniStuck "SGet: missing key"
                  end
              end k
          | inr1 (inr1 (inr1 _)) => MiniStuck "coreE"
          end
      end
  end.

Definition gcd_main_tree : itree crisE val :=
  denote_fbody toy_mn toy_fnames gcd_main (nil, nil).

Definition gcd_outcome : mini_outcome val :=
  mini_call_run 1000000 0 ∅ gcd_main_tree nil.

End MACHINE.

Time Eval vm_compute in
  (match @gcd_outcome execΣ with
   | MiniDone v n => inl (v, n)
   | MiniOut => inr "out of fuel"
   | MiniStuck s => inr ("stuck: " ++ s)
   end).
