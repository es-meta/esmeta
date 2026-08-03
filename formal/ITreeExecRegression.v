(** Regression checks for continuation-stack cloning.

    A captured continuation may retain retired frames below a nonlocal
    escape point.  Cloning preserves those frames as poison: they remain
    present with [ef_live = false], and are rejected only if execution
    later attempts to return through them. *)

From Stdlib Require Import Bool Floats.
From CRIS Require Import CRIS.
From ESMetaFV Require Import ITreeExec.
From ESMetaFV Require Exec Programs.

Example math_host_exec_returns_converted_number :
  Exec.run 100 Programs.math_host_prog =
    Exec.Ok (Fragment.VNumber (2.0000000000000000)%float, nil).
Proof. vm_compute. reflexivity. Qed.

Example math_to_number_host_exec_returns_rounded_number :
  Exec.run 100 Programs.math_to_number_host_prog =
    Exec.Ok
      (Fragment.VNumber (9007199254740992.0000)%float, nil).
Proof. vm_compute. reflexivity. Qed.

Definition regression_continue (x : Any.t) : itree lmodE Any.t := Ret x.

Definition retired_root : exec_frame :=
  mkExecFrame None regression_continue false.

Definition live_child : exec_frame :=
  mkExecFrame (Some 0) regression_continue true.

Definition two_frame_stack : gmap nat exec_frame :=
  <[1 := live_child]> (<[0 := retired_root]> ∅).

(** The parent is cloned first at ID 2 and the child at ID 3.  Both the
    topology and the original liveness bits must be preserved. *)
Example clone_preserves_poison_and_parent_topology :
  match clone_exec_stack 2 (Some 1) 2 two_frame_stack with
  | Some cloned =>
      match
        esc_stack cloned,
        esc_frames cloned !! 2,
        esc_frames cloned !! 3
      with
      | Some child_id, Some parent, Some child =>
          Nat.eqb child_id 3
          && Nat.eqb (esc_next_frame cloned) 4
          && Bool.eqb (ef_live parent) false
          && Bool.eqb (ef_live child) true
          &&
          match ef_parent parent, ef_parent child with
          | None, Some parent_id => Nat.eqb parent_id 2
          | _, _ => false
          end
      | _, _, _ => false
      end
  | None => false
  end = true.
Proof. vm_compute. reflexivity. Qed.

Example clone_rejects_missing_frame :
  match
    clone_exec_stack 1 (Some 0) 1 (∅ : gmap nat exec_frame)
  with
  | None => true
  | Some _ => false
  end = true.
Proof. vm_compute. reflexivity. Qed.

Definition self_cycle_frame : exec_frame :=
  mkExecFrame (Some 0) regression_continue true.

Definition self_cycle_stack : gmap nat exec_frame :=
  <[0 := self_cycle_frame]> ∅.

(** Malformed cyclic external data consumes the finite clone fuel and is
    rejected without ever applying a saved continuation closure. *)
Example clone_rejects_self_cycle_on_fuel_exhaustion :
  match clone_exec_stack 2 (Some 0) 1 self_cycle_stack with
  | None => true
  | Some _ => false
  end = true.
Proof. vm_compute. reflexivity. Qed.
