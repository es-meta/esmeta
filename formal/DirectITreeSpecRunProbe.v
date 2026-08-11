(** * Probe: run "var x = 1;" through the DIRECT (shallow) ITree backend
    inside Rocq via vm_compute, mirroring ITreeSpecRunProbe.v (generic
    backend, which did not finish fuel 100 within 10 minutes). *)

From Stdlib Require Import String List.
From CRIS Require Import CRIS.
From ESMetaFV Require Import
  Fragment Domain ITreeExec DirectITreeExec DirectITreeCore.
From ESMetaFV.validation Require Import SpecRun.

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

Definition direct_probe_tree : ITreeS.ITreeDefinition.itree coreE val :=
  direct_production_exec (direct_script_prog this_src this_ast nil).

Time Eval vm_compute in (quiet_result_fuel 100 direct_probe_tree).
Time Eval vm_compute in (quiet_result_fuel 10000 direct_probe_tree).
Time Eval vm_compute in (quiet_result_fuel 10000000 direct_probe_tree).
