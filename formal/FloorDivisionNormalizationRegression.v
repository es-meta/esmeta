(** Regression proofs for the exporter normalization of
    [floor (n / d)] to [(n - n mod d) / d] on nonnegative Math
    numerators and positive Math denominators.  The semantic evaluators
    remain unchanged: raw Math division is still admitted only when exact. *)

From Stdlib Require Import String List ZArith Lia.
From ESMetaFV Require Import Fragment Domain Exec.

Import ListNotations.
Local Open Scope Z_scope.
Local Open Scope string_scope.

Definition raw_floor_division (n d : Z) : expr :=
  EUnary UFloor (EBinary BDiv (EMath n) (EMath d)).

Definition normalized_floor_division (n d : Z) : expr :=
  EBinary BDiv
    (EBinary BSub
      (EMath n)
      (EBinary BMod (EMath n) (EMath d)))
    (EMath d).

Definition floor_division_state : xstate :=
  mkXState nil nil nil None None nil 0.

Lemma normalized_floor_division_algebra (n d : Z) :
  d <> 0 -> (n - n mod d) / d = n / d.
Proof.
  intros nonzero.
  pose proof (Z.div_mod n d nonzero) as decomposition.
  assert (n - n mod d = d * (n / d)) by lia.
  rewrite H, Z.mul_comm.
  apply Z.div_mul; assumption.
Qed.

Example normalized_odd_three_halves :
  exec_expr floor_division_state nil
    (normalized_floor_division 3 2) =
  Ok (floor_division_state, VMath 1).
Proof. vm_compute. reflexivity. Qed.

Example normalized_even_four_halves :
  exec_expr floor_division_state nil
    (normalized_floor_division 4 2) =
  Ok (floor_division_state, VMath 2).
Proof. vm_compute. reflexivity. Qed.

Example raw_even_division_remains_executable :
  exec_expr floor_division_state nil (raw_floor_division 4 2) =
  Ok (floor_division_state, VMath 2).
Proof. vm_compute. reflexivity. Qed.

Example normalized_typed_array_partial_length :
  exec_expr floor_division_state nil
    (normalized_floor_division 23 8) =
  Ok (floor_division_state, VMath 2).
Proof. vm_compute. reflexivity. Qed.

Example normalized_zero_numerator :
  exec_expr floor_division_state nil
    (normalized_floor_division 0 8) =
  Ok (floor_division_state, VMath 0).
Proof. vm_compute. reflexivity. Qed.

Example raw_odd_floor_division_is_stuck_but_normalized_succeeds :
  exec_expr floor_division_state nil (raw_floor_division 3 2) =
    Stuck "EBinary" /\
  exec_expr floor_division_state nil (normalized_floor_division 3 2) =
    Ok (floor_division_state, VMath 1).
Proof. split; vm_compute; reflexivity. Qed.

(** Lock the evaluator policy separately from the exporter rewrite. *)
Example general_math_division_remains_exact_only :
  eval_bop BDiv (VMath 4) (VMath 2) = Some (VMath 2) /\
  eval_bop BDiv (VMath 3) (VMath 2) = None.
Proof. split; reflexivity. Qed.
