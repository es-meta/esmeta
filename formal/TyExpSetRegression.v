(** Regression checks for exact finite primitive refinements in [tyexp]. *)

From Stdlib Require Import String ZArith List Bool.
From ESMetaFV Require Import Fragment Domain Exec.

Import ListNotations.
Local Open Scope Z_scope.

Definition primitive_query_state : xstate :=
  mkXState nil nil nil None None nil 0.

Definition check_primitive (t : tyexp) (v : val) :=
  run_heap_query_x primitive_query_state
    (ty_check_query type_check_fuel t v).

Example math_int_set_accepts_negative_zero_positive_members :
  ty_check_prim (TMathIntSet [-2; 0; 3]) (VMath (-2)) = true /\
  ty_check_prim (TMathIntSet [-2; 0; 3]) (VMath 0) = true /\
  ty_check_prim (TMathIntSet [-2; 0; 3]) (VMath 3) = true.
Proof. repeat split; vm_compute; reflexivity. Qed.

Example math_int_set_rejects_nonmember_and_wrong_kind :
  ty_check_prim (TMathIntSet [-2; 0; 3]) (VMath 2) = false /\
  ty_check_prim (TMathIntSet [-2; 0; 3]) (VBool true) = false.
Proof. split; vm_compute; reflexivity. Qed.

Example math_int_set_union_and_query :
  ty_check_prim (TUnion [TBoolSet true false; TMathIntSet [-2; 0; 3]])
    (VMath 3) = true /\
  check_primitive (TMathIntSet [-2; 0; 3]) (VMath 4) = Ok (Some false).
Proof. split; vm_compute; reflexivity. Qed.

Example infinity_sign_sets_accept_only_the_selected_sign :
  ty_check_prim (TInfinity true false) (VInfinity false) = true /\
  ty_check_prim (TInfinity true false) (VInfinity true) = false /\
  ty_check_prim (TInfinity false true) (VInfinity true) = true /\
  ty_check_prim (TInfinity false true) (VInfinity false) = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

Example infinity_set_rejects_wrong_kind_and_works_in_union_query :
  ty_check_prim (TInfinity true false) (VMath 0) = false /\
  ty_check_prim (TUnion [TBoolSet false true; TInfinity false true])
    (VInfinity true) = true /\
  check_primitive (TInfinity true false) (VInfinity false) = Ok (Some true).
Proof. repeat split; vm_compute; reflexivity. Qed.

Example bool_singletons_accept_and_reject_same_kind :
  ty_check_prim (TBoolSet true false) (VBool false) = true /\
  ty_check_prim (TBoolSet true false) (VBool true) = false /\
  ty_check_prim (TBoolSet false true) (VBool true) = true /\
  ty_check_prim (TBoolSet false true) (VBool false) = false.
Proof. repeat split; vm_compute; reflexivity. Qed.

Example bool_set_rejects_wrong_kind_and_works_in_union_query :
  ty_check_prim (TBoolSet true false) (VMath 0) = false /\
  ty_check_prim (TUnion [TMathIntSet [1]; TBoolSet false true])
    (VBool true) = true /\
  check_primitive (TBoolSet true false) (VBool true) = Ok (Some false).
Proof. repeat split; vm_compute; reflexivity. Qed.

Definition unicode_sample : cstr := [54620; 55357; 56832].

Example string_set_accepts_ascii_and_non_ascii_members :
  ty_check_prim (TStrSet [[104; 97; 110; 100; 108; 101]; unicode_sample])
    (VStr [104; 97; 110; 100; 108; 101]) = true /\
  ty_check_prim (TStrSet [[104; 97; 110; 100; 108; 101]; unicode_sample])
    (VStr unicode_sample) = true.
Proof. split; vm_compute; reflexivity. Qed.

Example string_set_rejects_nonmember_and_wrong_kind :
  ty_check_prim (TStrSet [unicode_sample]) (VStr [120]) = false /\
  ty_check_prim (TStrSet [unicode_sample]) (VBool false) = false.
Proof. split; vm_compute; reflexivity. Qed.

Example string_set_union_and_query :
  ty_check_prim (TUnion [TBoolSet true false; TStrSet [unicode_sample]])
    (VStr unicode_sample) = true /\
  check_primitive (TStrSet [unicode_sample]) (VStr unicode_sample) =
    Ok (Some true).
Proof. split; vm_compute; reflexivity. Qed.
