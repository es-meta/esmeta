(** * Probe: run the exported "var x = 1;" script as a closed ITree in Rocq

    [validation/SpecRun.v] already checks this program on the [Exec.v]
    (fuel-based, non-ITree) route.  This probe closes the SAME program with
    [exec_itree] and observes it with [quiet_result_fuel] under [vm_compute],
    to see whether a whole-script ITree can be computed inside Rocq. *)

From Stdlib Require Import String List.
From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment Domain ITreeExec Spec.
From ESMetaFV.validation Require Import SpecRun.

Local Open Scope string_scope.

(* Inlined from JSClosedEquiv.v to keep this probe's build surface small. *)
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

Definition probe_tree : ITreeS.ITreeDefinition.itree coreE val :=
  exec_itree "ESMetaFV-ITree-Probe" this_prog.

Time Eval vm_compute in (quiet_result_fuel 100 probe_tree).
Time Eval vm_compute in (quiet_result_fuel 1000 probe_tree).
Time Eval vm_compute in (quiet_result_fuel 10000 probe_tree).
Time Eval vm_compute in (quiet_result_fuel 100000 probe_tree).
