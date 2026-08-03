(** Regression checks for the Scala/JVM signed-32 coercions used by
    ESMeta's Math and BigInt operators. *)

From Stdlib Require Import ZArith Floats.
From ESMetaFV Require Import Fragment Domain.

Local Open Scope Z_scope.

(** Signed 32-bit boundaries and low-bit wrapping. *)
Example scala_to_int32_min :
  scala_to_int32 (-2147483648) = -2147483648.
Proof. vm_compute. reflexivity. Qed.

Example scala_to_int32_max :
  scala_to_int32 2147483647 = 2147483647.
Proof. vm_compute. reflexivity. Qed.

Example scala_to_int32_wrap_positive :
  scala_to_int32 2147483648 = -2147483648.
Proof. vm_compute. reflexivity. Qed.

Example scala_to_int32_wrap_huge :
  scala_to_int32 4294967295 = -1.
Proof. vm_compute. reflexivity. Qed.

(** Math [~n.toInt] complements the coerced machine integer, not the
    original unbounded mathematical integer. *)
Example math_bnot_wrap :
  eval_uop UBNot (VMath 2147483648) = Some (VMath 2147483647).
Proof. vm_compute. reflexivity. Qed.

Example math_bnot_int_min :
  eval_uop UBNot (VMath (-2147483648)) = Some (VMath 2147483647).
Proof. vm_compute. reflexivity. Qed.

Example math_bnot_int_max :
  eval_uop UBNot (VMath 2147483647) = Some (VMath (-2147483648)).
Proof. vm_compute. reflexivity. Qed.

Example math_bnot_truncates_low_bits :
  eval_uop UBNot (VMath 4294967296) = Some (VMath (-1)).
Proof. vm_compute. reflexivity. Qed.

(** Number [~n.toInt] keeps the Number result kind.  These are precisely the
    signed-int32 values produced by the specification's [ToInt32] call. *)
Example number_bnot_zero :
  eval_uop UBNot (VNumber (0.0)%float) =
    Some (VNumber (-1.0)%float).
Proof. vm_compute. reflexivity. Qed.

Example number_bnot_minus_one :
  eval_uop UBNot (VNumber (-1.0)%float) =
    Some (VNumber (0.0)%float).
Proof. vm_compute. reflexivity. Qed.

Example number_bnot_int_max :
  eval_uop UBNot (VNumber (2147483647.0)%float) =
    Some (VNumber (-2147483648.0)%float).
Proof. vm_compute. reflexivity. Qed.

Example number_bnot_int_min :
  eval_uop UBNot (VNumber (-2147483648.0)%float) =
    Some (VNumber (2147483647.0)%float).
Proof. vm_compute. reflexivity. Qed.

Example number_bnot_fractional_stays_outside_fragment :
  eval_uop UBNot (VNumber (1.5)%float) = None.
Proof. vm_compute. reflexivity. Qed.

Example bigint_bnot_is_unbounded :
  eval_uop UBNot (VBigInt 0) = Some (VBigInt (-1)).
Proof. vm_compute. reflexivity. Qed.

(** Shift counts are coerced through [toInt].  [Int.MinValue] and results
    beyond the conservative executable bit bound are unsupported; nearby
    and huge wrapped counts stay exact. *)
Example math_lshift_max_is_ub :
  eval_bop BLShift (VMath 1) (VMath 2147483647) = None.
Proof. vm_compute. reflexivity. Qed.

Example math_lshift_int_min_is_ub :
  eval_bop BLShift (VMath 1) (VMath (-2147483648)) = None.
Proof. vm_compute. reflexivity. Qed.

Example math_lshift_wrap_to_int_min_is_ub :
  eval_bop BLShift (VMath 1) (VMath 2147483648) = None.
Proof. vm_compute. reflexivity. Qed.

Example math_lshift_huge_wraps_negative :
  eval_bop BLShift (VMath 8) (VMath 4294967295) = Some (VMath 4).
Proof. vm_compute. reflexivity. Qed.

Example math_lshift_oversized_result_is_ub :
  eval_bop BLShift
    (VMath 1) (VMath scala_bigint_exec_max_bits) = None.
Proof. vm_compute. reflexivity. Qed.

Example bigint_rshift_negative :
  eval_bop BRShift (VBigInt 8) (VBigInt (-1)) = Some (VBigInt 16).
Proof. vm_compute. reflexivity. Qed.

Example bigint_rshift_wrap_to_int_min_is_ub :
  eval_bop BRShift (VBigInt 1) (VBigInt 2147483648) = None.
Proof. vm_compute. reflexivity. Qed.

(** Math power uses [isValidInt] before [toInt]; the non-integral floating
    fallback is outside ADR-5. *)
Example math_pow_max_jvm_exponent :
  eval_bop BPow (VMath 1) (VMath 999999999) =
  Some (VMath 1).
Proof. vm_compute. reflexivity. Qed.

Example math_pow_above_jvm_limit_is_ub :
  eval_bop BPow (VMath 1) (VMath 1000000000) = None.
Proof. vm_compute. reflexivity. Qed.

Example math_pow_large_nontrivial_result_is_ub :
  eval_bop BPow (VMath 2) (VMath 999999999) = None.
Proof. vm_compute. reflexivity. Qed.

Example math_pow_large_trivial_result_stays_exact :
  eval_bop BPow (VMath 1) (VMath 999999999) = Some (VMath 1).
Proof. vm_compute. reflexivity. Qed.

Example math_pow_above_valid_int_is_ub :
  eval_bop BPow (VMath 1) (VMath 2147483648) = None.
Proof. vm_compute. reflexivity. Qed.

Example math_pow_negative_is_ub :
  eval_bop BPow (VMath 2) (VMath (-1)) = None.
Proof. vm_compute. reflexivity. Qed.

Example math_pow_int_min_is_ub :
  eval_bop BPow (VMath 2) (VMath (-2147483648)) = None.
Proof. vm_compute. reflexivity. Qed.

(** BigInt power coerces first; nonnegative wrapped exponents are exact,
    while negative wrapped exponents correspond to the JVM exception. *)
Example bigint_pow_wraps_to_zero :
  eval_bop BPow (VBigInt 7) (VBigInt 4294967296) = Some (VBigInt 1).
Proof. vm_compute. reflexivity. Qed.

Example bigint_pow_negative_is_ub :
  eval_bop BPow (VBigInt 7) (VBigInt (-1)) = None.
Proof. vm_compute. reflexivity. Qed.

Example bigint_pow_huge_wraps_negative :
  eval_bop BPow (VBigInt 7) (VBigInt 4294967295) = None.
Proof. vm_compute. reflexivity. Qed.

Example bigint_pow_jvm_overflow_is_ub :
  eval_bop BPow (VBigInt 7) (VBigInt 2147483647) = None.
Proof. vm_compute. reflexivity. Qed.

Example bigint_pow_trivial_max_exponent_stays_exact :
  eval_bop BPow (VBigInt 1) (VBigInt 2147483647) =
  Some (VBigInt 1).
Proof. vm_compute. reflexivity. Qed.
