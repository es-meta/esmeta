(** Focused regressions for ESMeta's extended mathematical values.

    Since esmeta#348, successful host mathematical operators may return a
    finite [Math] value or either infinity.  NaN remains outside [ExtMath] and
    is therefore not a valid successful host result. *)

From Stdlib Require Import String List ZArith Floats.
From ESMetaFV Require Import Fragment Domain Semantics Exec.

Import ListNotations.
Local Open Scope string_scope.

Definition ext_math_state (hosts : list host_cache_entry) : xstate :=
  mkXState nil nil nil None None hosts 0.

Definition exp_1000_query : host_query :=
  HQMathOp MExp [1000%Z].

Definition exp_1000_expr : expr :=
  EMathOp MExp [EMath 1000%Z].

Example math_host_result_accepts_finite_and_infinite_ext_math :
  host_result_well_typed exp_1000_query (VMath 1%Z) = true /\
  host_result_well_typed exp_1000_query (VInfinity true) = true /\
  host_result_well_typed exp_1000_query (VInfinity false) = true /\
  host_result_well_typed exp_1000_query
    (VNumber PrimFloat.infinity) = false.
Proof. repeat split; reflexivity. Qed.

Example math_host_infinity_executes_through_typed_cache :
  exec_expr
    (ext_math_state
      [mkHostCacheEntry exp_1000_query (VInfinity true)])
    nil exp_1000_expr =
  Ok
    (ext_math_state
      [mkHostCacheEntry exp_1000_query (VInfinity true)],
     VInfinity true).
Proof. reflexivity. Qed.

Example math_host_infinity_converts_to_approximate_number_end_to_end :
  exec_expr
    (ext_math_state
      [mkHostCacheEntry exp_1000_query (VInfinity true)])
    nil (EConvert CToApproxNumber exp_1000_expr) =
  Ok
    (ext_math_state
      [mkHostCacheEntry exp_1000_query (VInfinity true)],
     VNumber PrimFloat.infinity).
Proof. reflexivity. Qed.

Example math_host_wrong_result_type_still_fails_closed :
  exec_expr
    (ext_math_state
      [mkHostCacheEntry exp_1000_query
        (VNumber PrimFloat.infinity)])
    nil exp_1000_expr =
  Stuck "EMathOp(host)".
Proof. reflexivity. Qed.

Example approximate_number_accepts_both_infinities :
  eval_cop CToApproxNumber (VInfinity true) =
    Some (VNumber PrimFloat.infinity) /\
  eval_cop CToApproxNumber (VInfinity false) =
    Some (VNumber PrimFloat.neg_infinity).
Proof. split; reflexivity. Qed.

Example approximate_number_is_identity_on_binary64_numbers :
  eval_cop CToApproxNumber
    (VNumber (-0.0000000000000000)%float) =
  Some (VNumber (-0.0000000000000000)%float) /\
  eval_cop CToApproxNumber (VNumber PrimFloat.nan) =
  Some (VNumber PrimFloat.nan).
Proof. split; reflexivity. Qed.
