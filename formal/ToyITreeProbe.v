(** * Probe: run the hand-mirrored toy IR programs (Programs.v) as closed
    ITrees inside Rocq via vm_compute.  Unlike ITreeSpecRunProbe.v, these
    programs have one or two functions and no spec/heap, so this isolates
    the CRIS interpreter-stack cost from the spec-closing cost. *)

From Stdlib Require Import String List.
From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment Domain ITreeExec Programs.

Local Open Scope string_scope.

Fixpoint quiet_result_fuel {E : Type -> Type} {R : Type}
  (fuel : nat) (t : ITreeS.ITreeDefinition.itree E R) : option R :=
  match fuel with
  | O => None
  | S fuel' =>
      match ITreeS.ITreeDefinition.observe t with
      | ITreeS.ITreeDefinition.RetF r => Some r
      | ITreeS.ITreeDefinition.TauF next => quiet_result_fuel fuel' next
      | ITreeS.ITreeDefinition.VisF _ _ => None
      end
  end.

Definition sum_tree : ITreeS.ITreeDefinition.itree coreE val :=
  exec_itree "toy" sum_prog.

Time Eval vm_compute in (quiet_result_fuel 1000 sum_tree).
Time Eval vm_compute in (quiet_result_fuel 100000 sum_tree).
Time Eval vm_compute in (quiet_result_fuel 1000000 sum_tree).
