(** Focused regressions for generated Number -> Math -> Number composites. *)

From Stdlib Require Import String List ZArith Floats.
From ESMetaFV Require Import Fragment Domain Semantics Exec.

Import ListNotations.
Local Open Scope string_scope.

Definition number_math_add_expr (left right : expr) : expr :=
  EConvert CToNumber
    (EBinary BAdd
      (EConvert CToMath left)
      (EConvert CToMath right)).

Definition number_math_pow_expr (left right : expr) : expr :=
  EConvert CToApproxNumber
    (EBinary BPow
      (EConvert CToMath left)
      (EConvert CToMath right)).

Definition number_math_compare_expr (op : bop) (left right : expr) : expr :=
  EBinary op (EConvert CToMath left) (EConvert CToMath right).

Definition number_sin_expr (inner : expr) : expr :=
  EConvert CToApproxNumber
    (EMathOp MSin [EConvert CToMath inner]).

Definition number_math_state (hosts : list host_cache_entry) : xstate :=
  mkXState nil nil nil None None hosts 0.

Example number_math_query_equality_distinguishes_op_and_signed_zero :
  host_query_eqb
    (HQNumberMathOp NMAdd
      (0.0000000000000000)%float (1.0000000000000000)%float)
    (HQNumberMathOp NMAdd
      (0.0000000000000000)%float (1.0000000000000000)%float) = true /\
  host_query_eqb
    (HQNumberMathOp NMAdd
      (0.0000000000000000)%float (1.0000000000000000)%float)
    (HQNumberMathOp NMMul
      (0.0000000000000000)%float (1.0000000000000000)%float) = false /\
  host_query_eqb
    (HQNumberMathOp NMAdd
      (0.0000000000000000)%float (1.0000000000000000)%float)
    (HQNumberMathOp NMAdd
      (-0.0000000000000000)%float (1.0000000000000000)%float) = false /\
  host_query_eqb
    (HQNumberMathOp NMPow PrimFloat.nan (2.0000000000000000)%float)
    (HQNumberMathOp NMPow PrimFloat.nan (2.0000000000000000)%float) = true.
Proof. repeat split; reflexivity. Qed.

Example number_math_result_contract_accepts_only_number :
  host_result_well_typed
    (HQNumberMathOp NMDiv
      (1.0000000000000000)%float (2.0000000000000000)%float)
    (VNumber (0.50000000000000000)%float) = true /\
  host_result_well_typed
    (HQNumberMathOp NMDiv
      (1.0000000000000000)%float (2.0000000000000000)%float)
    (VMath 0%Z) = false.
Proof. split; reflexivity. Qed.

Example number_math_exec_uses_exact_typed_cache_hit :
  exec_expr
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathOp NMAdd
          (1.0000000000000000)%float (2.0000000000000000)%float)
        (VNumber (3.0000000000000000)%float)])
    nil
    (number_math_add_expr
      (ENumber (1.0000000000000000)%float)
      (ENumber (2.0000000000000000)%float)) =
  Ok
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathOp NMAdd
          (1.0000000000000000)%float (2.0000000000000000)%float)
        (VNumber (3.0000000000000000)%float)],
     VNumber (3.0000000000000000)%float).
Proof. reflexivity. Qed.

Example number_math_exec_fractional_numbers_keep_raw_host_key :
  exec_expr
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathOp NMAdd
          (1.5000000000000000)%float (2.2500000000000000)%float)
        (VNumber (3.7500000000000000)%float)])
    nil
    (number_math_add_expr
      (ENumber (1.5000000000000000)%float)
      (ENumber (2.2500000000000000)%float)) =
  Ok
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathOp NMAdd
          (1.5000000000000000)%float (2.2500000000000000)%float)
        (VNumber (3.7500000000000000)%float)],
     VNumber (3.7500000000000000)%float).
Proof. reflexivity. Qed.

Example number_math_exec_missing_cache_fails_closed :
  exec_expr (number_math_state nil) nil
    (number_math_add_expr
      (ENumber (1.0000000000000000)%float)
      (ENumber (2.0000000000000000)%float)) =
  Stuck "EConvert(number-math-host)".
Proof. reflexivity. Qed.

Example number_math_exec_wrong_result_type_fails_closed :
  exec_expr
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathOp NMAdd
          (1.0000000000000000)%float (2.0000000000000000)%float)
        (VMath 3%Z)])
    nil
    (number_math_add_expr
      (ENumber (1.0000000000000000)%float)
      (ENumber (2.0000000000000000)%float)) =
  Stuck "EConvert(number-math-host)".
Proof. reflexivity. Qed.

Example number_math_exec_nonfinite_left_fails_before_right_error :
  exec_expr (number_math_state nil) nil
    (number_math_add_expr
      (ENumber PrimFloat.nan)
      (ERef (RVar (VLocal (LName "missing"))))) =
  Stuck "EConvert(number-math-left)".
Proof. reflexivity. Qed.

Example number_math_non_number_operands_keep_existing_pipeline :
  exec_expr (number_math_state nil) nil
    (number_math_add_expr (EBigInt 2%Z) (EMath 3%Z)) =
  Ok (number_math_state nil, VNumber (5.0000000000000000)%float).
Proof. reflexivity. Qed.

Example number_math_mixed_integral_number_uses_decimal_math_cache :
  exec_expr
    (number_math_state
      [mkHostCacheEntry
        (HQNumberToMath (2.0000000000000000)%float) (VMath 2%Z)])
    nil (number_math_add_expr
      (ENumber (2.0000000000000000)%float) (EMath 3%Z)) =
  Ok
    (number_math_state
      [mkHostCacheEntry
        (HQNumberToMath (2.0000000000000000)%float) (VMath 2%Z)],
     VNumber (5.0000000000000000)%float).
Proof. reflexivity. Qed.

Example number_to_math_uses_scala_bigdecimal_cache :
  exec_expr
    (number_math_state
      [mkHostCacheEntry
        (HQNumberToMath (1.2345678901234567e20)%float)
        (VMath 123456789012345670000%Z)])
    nil (EConvert CToMath (ENumber (1.2345678901234567e20)%float)) =
  Ok
    (number_math_state
      [mkHostCacheEntry
        (HQNumberToMath (1.2345678901234567e20)%float)
        (VMath 123456789012345670000%Z)],
     VMath 123456789012345670000%Z).
Proof. reflexivity. Qed.

Example number_to_math_query_preserves_signed_zero :
  host_query_eqb
    (HQNumberToMath (0.0000000000000000)%float)
    (HQNumberToMath (0.0000000000000000)%float) = true /\
  host_query_eqb
    (HQNumberToMath (0.0000000000000000)%float)
    (HQNumberToMath (-0.0000000000000000)%float) = false.
Proof. split; reflexivity. Qed.

Example number_to_math_missing_or_wrong_cache_fails_closed :
  exec_expr (number_math_state nil) nil
    (EConvert CToMath (ENumber (2.0000000000000000)%float)) =
  Stuck "EConvert(host)" /\
  exec_expr
    (number_math_state
      [mkHostCacheEntry
        (HQNumberToMath (2.0000000000000000)%float)
        (VNumber (2.0000000000000000)%float)])
    nil (EConvert CToMath (ENumber (2.0000000000000000)%float)) =
  Stuck "EConvert(host)".
Proof. split; reflexivity. Qed.

Example number_to_math_nonfinite_forged_cache_is_rejected :
  host_result_well_typed
    (HQNumberToMath PrimFloat.nan) (VMath 0%Z) = false /\
  host_result_well_typed
    (HQNumberToMath PrimFloat.infinity) (VMath 0%Z) = false /\
  host_result_well_typed
    (HQNumberToMath PrimFloat.neg_infinity) (VMath 0%Z) = false /\
  exec_expr
    (number_math_state
      [mkHostCacheEntry (HQNumberToMath PrimFloat.nan) (VMath 0%Z)])
    nil (EConvert CToMath (ENumber PrimFloat.nan)) =
  Stuck "EConvert" /\
  exec_expr
    (number_math_state
      [mkHostCacheEntry (HQNumberToMath PrimFloat.infinity) (VMath 0%Z)])
    nil (EConvert CToMath (ENumber PrimFloat.infinity)) =
  Stuck "EConvert".
Proof. repeat split; reflexivity. Qed.

(** The two allocations make evaluation count and order observable in the
    resulting heap.  Each raw operand is evaluated exactly once, left first. *)
Example number_math_raw_operands_evaluate_once_left_to_right :
  match
    exec_expr (number_math_state nil) nil
      (number_math_add_expr
        (ESizeOf (EList [EMath 11%Z]))
        (ESizeOf (EList [EMath 21%Z; EMath 22%Z])))
  with
  | Ok (st, VNumber result) =>
      andb (num_struct_eqb result (3.0000000000000000)%float)
        (match x_heap st with
         | [Some (OList [VMath 11%Z]);
            Some (OList [VMath 21%Z; VMath 22%Z])] => true
         | _ => false
         end)
  | _ => false
  end = true.
Proof. vm_compute. reflexivity. Qed.

Example number_math_compare_fractional_number_number_exact :
  exec_expr (number_math_state nil) nil
    (number_math_compare_expr BLt
      (ENumber (1.5000000000000000)%float)
      (ENumber (1.7500000000000000)%float)) =
  Ok (number_math_state nil, VBool true).
Proof. reflexivity. Qed.

Example number_math_compare_mixed_number_math_exact :
  exec_expr
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathCompare NMCLt NMCNumberLeft
          (-1.5000000000000000)%float (-1)%Z)
        (VBool true)]) nil
    (number_math_compare_expr BLt
      (ENumber (-1.5000000000000000)%float) (EMath (-1)%Z)) =
  Ok
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathCompare NMCLt NMCNumberLeft
          (-1.5000000000000000)%float (-1)%Z)
        (VBool true)], VBool true) /\
  exec_expr
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathCompare NMCLt NMCNumberRight
          (-1.5000000000000000)%float (-2)%Z)
        (VBool true)]) nil
    (number_math_compare_expr BLt
      (EMath (-2)%Z) (ENumber (-1.5000000000000000)%float)) =
  Ok
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathCompare NMCLt NMCNumberRight
          (-1.5000000000000000)%float (-2)%Z)
        (VBool true)], VBool true).
Proof. split; reflexivity. Qed.

Example number_math_compare_mixed_number_bigint_equality_exact :
  exec_expr
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathCompare NMCEqual NMCNumberLeft
          (2.0000000000000000)%float 2%Z)
        (VBool true)]) nil
    (number_math_compare_expr BEqual
      (ENumber (2.0000000000000000)%float) (EBigInt 2%Z)) =
  Ok
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathCompare NMCEqual NMCNumberLeft
          (2.0000000000000000)%float 2%Z)
        (VBool true)], VBool true) /\
  exec_expr
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathCompare NMCEqual NMCNumberLeft
          (2.5000000000000000)%float 2%Z)
        (VBool false)]) nil
    (number_math_compare_expr BEqual
      (ENumber (2.5000000000000000)%float) (EBigInt 2%Z)) =
  Ok
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathCompare NMCEqual NMCNumberLeft
          (2.5000000000000000)%float 2%Z)
        (VBool false)], VBool false).
Proof. split; reflexivity. Qed.

Example number_math_compare_bigdecimal_counterexample_uses_host :
  exec_expr
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathCompare NMCEqual NMCNumberLeft
          (1.2345678901234567e20)%float 123456789012345670000%Z)
        (VBool true)]) nil
    (number_math_compare_expr BEqual
      (ENumber (1.2345678901234567e20)%float)
      (EBigInt 123456789012345670000%Z)) =
  Ok
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathCompare NMCEqual NMCNumberLeft
          (1.2345678901234567e20)%float 123456789012345670000%Z)
        (VBool true)], VBool true).
Proof. reflexivity. Qed.

Example number_math_compare_negative_integral_lt_is_false :
  exec_expr
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathCompare NMCLt NMCNumberLeft
          (-3.0000000000000000)%float (-3)%Z)
        (VBool false)]) nil
    (number_math_compare_expr BLt
      (ENumber (-3.0000000000000000)%float) (EMath (-3)%Z)) =
  Ok
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathCompare NMCLt NMCNumberLeft
          (-3.0000000000000000)%float (-3)%Z)
        (VBool false)], VBool false).
Proof. reflexivity. Qed.

Example number_math_compare_query_identity_is_exact :
  host_query_eqb
    (HQNumberMathCompare NMCLt NMCNumberLeft
      (0.0000000000000000)%float 0%Z)
    (HQNumberMathCompare NMCLt NMCNumberLeft
      (0.0000000000000000)%float 0%Z) = true /\
  host_query_eqb
    (HQNumberMathCompare NMCLt NMCNumberLeft
      (0.0000000000000000)%float 0%Z)
    (HQNumberMathCompare NMCEqual NMCNumberLeft
      (0.0000000000000000)%float 0%Z) = false /\
  host_query_eqb
    (HQNumberMathCompare NMCLt NMCNumberLeft
      (0.0000000000000000)%float 0%Z)
    (HQNumberMathCompare NMCLt NMCNumberRight
      (0.0000000000000000)%float 0%Z) = false /\
  host_query_eqb
    (HQNumberMathCompare NMCLt NMCNumberLeft
      (0.0000000000000000)%float 0%Z)
    (HQNumberMathCompare NMCLt NMCNumberLeft
      (-0.0000000000000000)%float 0%Z) = false.
Proof. repeat split; reflexivity. Qed.

Example number_math_compare_cache_is_bool_only :
  host_result_well_typed
    (HQNumberMathCompare NMCLt NMCNumberLeft
      (1.5000000000000000)%float 2%Z) (VBool true) = true /\
  host_result_well_typed
    (HQNumberMathCompare NMCLt NMCNumberLeft
      (1.5000000000000000)%float 2%Z) (VMath 1%Z) = false.
Proof. split; reflexivity. Qed.

Example number_math_compare_missing_or_wrong_cache_fails_closed :
  exec_expr (number_math_state nil) nil
    (number_math_compare_expr BLt
      (ENumber (1.5000000000000000)%float) (EMath 2%Z)) =
  Stuck "EBinary(number-math-comparison-host)" /\
  exec_expr
    (number_math_state
      [mkHostCacheEntry
        (HQNumberMathCompare NMCLt NMCNumberLeft
          (1.5000000000000000)%float 2%Z)
        (VMath 1%Z)]) nil
    (number_math_compare_expr BLt
      (ENumber (1.5000000000000000)%float) (EMath 2%Z)) =
  Stuck "EBinary(number-math-comparison-host)".
Proof. split; reflexivity. Qed.

(** Raw Number [BLt] has an ESMeta-specific [-0 < +0] quirk.  The generated
    composite first converts both operands to Math, where both zeros compare
    equal, so the composite must remain false. *)
Example number_math_compare_signed_zero_uses_math_semantics :
  exec_expr (number_math_state nil) nil
    (number_math_compare_expr BLt
      (ENumber (-0.0000000000000000)%float)
      (ENumber (0.0000000000000000)%float)) =
  Ok (number_math_state nil, VBool false).
Proof. reflexivity. Qed.

Example number_math_compare_left_conversion_fails_before_right :
  exec_expr (number_math_state nil) nil
    (number_math_compare_expr BLt
      (ENumber PrimFloat.nan)
      (ERef (RVar (VLocal (LName "missing"))))) =
  Stuck "EConvert(number-compare-left)".
Proof. reflexivity. Qed.

Example number_sin_query_and_result_are_narrowly_typed :
  host_query_eqb
    (HQNumberSin (0.50000000000000000)%float)
    (HQNumberSin (0.50000000000000000)%float) = true /\
  host_query_eqb
    (HQNumberSin (0.0000000000000000)%float)
    (HQNumberSin (-0.0000000000000000)%float) = false /\
  host_result_well_typed
    (HQNumberSin (0.50000000000000000)%float)
    (VNumber (0.47942553860420301)%float) = true /\
  host_result_well_typed
    (HQNumberSin (0.50000000000000000)%float) (VMath 0%Z) = false.
Proof. repeat split; reflexivity. Qed.

Example number_sin_composite_uses_exact_typed_cache :
  exec_expr
    (number_math_state
      [mkHostCacheEntry
        (HQNumberSin (0.50000000000000000)%float)
        (VNumber (0.47942553860420301)%float)])
    nil (number_sin_expr (ENumber (0.50000000000000000)%float)) =
  Ok
    (number_math_state
      [mkHostCacheEntry
        (HQNumberSin (0.50000000000000000)%float)
        (VNumber (0.47942553860420301)%float)],
     VNumber (0.47942553860420301)%float).
Proof. reflexivity. Qed.

Example number_sin_composite_missing_cache_fails_closed :
  exec_expr (number_math_state nil) nil
    (number_sin_expr (ENumber (0.50000000000000000)%float)) =
  Stuck "EConvert(number-sin-host)".
Proof. reflexivity. Qed.

Example number_sin_non_number_keeps_existing_math_host_pipeline :
  exec_expr
    (number_math_state
      [mkHostCacheEntry (HQMathOp MSin [0%Z]) (VMath 0%Z)])
    nil (number_sin_expr (EMath 0%Z)) =
  Ok
    (number_math_state
      [mkHostCacheEntry (HQMathOp MSin [0%Z]) (VMath 0%Z)],
     VNumber (0.0000000000000000)%float).
Proof. reflexivity. Qed.

Example number_sin_raw_operand_evaluates_once :
  match
    exec_expr
      (number_math_state
        [mkHostCacheEntry
          (HQNumberSin (1.0000000000000000)%float)
          (VNumber (0.84147098480789650)%float)])
      nil
      (number_sin_expr
        (EConvert CToNumber (ESizeOf (EList [EMath 7%Z]))))
  with
  | Ok (st, VNumber result) =>
      andb (num_struct_eqb result (0.84147098480789650)%float)
        (match x_heap st with
         | [Some (OList [VMath 7%Z])] => true
         | _ => false
         end)
  | _ => false
  end = true.
Proof. vm_compute. reflexivity. Qed.
