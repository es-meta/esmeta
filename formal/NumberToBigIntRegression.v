(** Focused regressions for the exact Number -> BigInt -> Math
    sentinel used by generated integer-conversion functions. *)

From Stdlib Require Import List ZArith Floats.
From ESMetaFV Require Import Fragment Domain Exec.

Import ListNotations.

Example float_to_Z_trunc_positive_fraction :
  float_to_Z_trunc (3.7500000000000000)%float = Some 3%Z.
Proof. vm_compute. reflexivity. Qed.

Example float_to_Z_trunc_negative_fraction :
  float_to_Z_trunc (-3.7500000000000000)%float = Some (-3)%Z.
Proof. vm_compute. reflexivity. Qed.

Example float_to_Z_trunc_fractions_below_one :
  float_to_Z_trunc (0.75000000000000000)%float = Some 0%Z /\
  float_to_Z_trunc (-0.75000000000000000)%float = Some 0%Z.
Proof. split; vm_compute; reflexivity. Qed.

Example float_to_Z_trunc_smallest_subnormals :
  float_to_Z_trunc (4.9406564584124654e-324)%float = Some 0%Z /\
  float_to_Z_trunc (-4.9406564584124654e-324)%float = Some 0%Z.
Proof. split; vm_compute; reflexivity. Qed.

Example float_to_Z_trunc_signed_zero :
  float_to_Z_trunc (0.0000000000000000)%float = Some 0%Z /\
  float_to_Z_trunc (-0.0000000000000000)%float = Some 0%Z.
Proof. split; reflexivity. Qed.

Example float_to_Z_trunc_rejects_nonfinite :
  float_to_Z_trunc PrimFloat.nan = None /\
  float_to_Z_trunc PrimFloat.infinity = None /\
  float_to_Z_trunc PrimFloat.neg_infinity = None /\
  eval_cop CToBigInt (VNumber PrimFloat.nan) = None /\
  eval_cop CToBigInt (VNumber PrimFloat.infinity) = None /\
  eval_cop CToBigInt (VNumber PrimFloat.neg_infinity) = None.
Proof. repeat split; reflexivity. Qed.

Example number_to_bigint_accepts_exact_integer_boundaries :
  eval_cop CToBigInt
    (VNumber (9007199254740992.0000)%float) =
      Some (VBigInt 9007199254740992%Z) /\
  eval_cop CToBigInt
    (VNumber (-9007199254740992.0000)%float) =
      Some (VBigInt (-9007199254740992)%Z).
Proof. split; vm_compute; reflexivity. Qed.

(** ESMeta uses [BigDecimal.exact(n).toBigInt], so this conversion is not
    bounded by the ECMAScript safe-integer interval. *)
Example number_to_bigint_accepts_above_safe_integer_interval :
  float_to_Z_trunc (9007199254740994.0000)%float =
    Some 9007199254740994%Z /\
  eval_cop CToBigInt
    (VNumber (9007199254740994.0000)%float) =
      Some (VBigInt 9007199254740994%Z) /\
  eval_cop CToBigInt
    (VNumber (-9007199254740994.0000)%float) =
      Some (VBigInt (-9007199254740994)%Z).
Proof. repeat split; vm_compute; reflexivity. Qed.

Example number_to_bigint_truncates_fractions_toward_zero :
  eval_cop CToBigInt (VNumber (42.875000000000000)%float) =
    Some (VBigInt 42%Z) /\
  eval_cop CToBigInt (VNumber (-42.875000000000000)%float) =
    Some (VBigInt (-42)%Z).
Proof. split; vm_compute; reflexivity. Qed.

(** The exact Binary64 value represented by this source spelling is slightly
    different from the decimal spelling; both ESMeta and [float_to_Z_trunc]
    convert that represented value, not the source decimal. *)
Example number_to_bigint_accepts_large_binary64_witness :
  eval_cop CToBigInt
    (VNumber (1.2345678901234567e20)%float) =
      Some (VBigInt 123456789012345667584%Z).
Proof. vm_compute. reflexivity. Qed.

Example general_number_to_math_remains_integral_only :
  eval_cop CToMath (VNumber (2.5000000000000000)%float) = None.
Proof. vm_compute. reflexivity. Qed.

Definition number_to_bigint_state : xstate :=
  mkXState nil nil nil None None nil 0.

Example number_bigint_math_composite_executes_end_to_end :
  exec_expr number_to_bigint_state nil
    (EConvert CToMath
      (EConvert CToBigInt (ENumber (-17.875000000000000)%float))) =
  Ok (number_to_bigint_state, VMath (-17)%Z).
Proof. vm_compute. reflexivity. Qed.
