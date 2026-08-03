(** Regression checks for exported Math integer refinements and ETrim's
    ECMAScript whitespace set. *)

From Stdlib Require Import String ZArith List.
From ESMetaFV Require Import Fragment Domain Exec.

Import ListNotations.
Local Open Scope Z_scope.
Local Open Scope string_scope.

Example math_int_accepts_all_signs :
  ty_check_prim (TMathInt true true true) (VMath (-7)) = true.
Proof. vm_compute. reflexivity. Qed.

Example nonnegative_math_int_accepts_zero :
  ty_check_prim (TMathInt false true true) (VMath 0) = true.
Proof. vm_compute. reflexivity. Qed.

Example nonnegative_math_int_rejects_negative :
  ty_check_prim (TMathInt false true true) (VMath (-1)) = false.
Proof. vm_compute. reflexivity. Qed.

Example finite_enum_accepts_member :
  ty_check_prim (TEnumNames ["unset"]) (VEnum "unset") = true.
Proof. vm_compute. reflexivity. Qed.

Example finite_enum_rejects_nonmember :
  ty_check_prim (TEnumNames ["unset"]) (VEnum "empty") = false.
Proof. vm_compute. reflexivity. Qed.

Example trim_start_keeps_trailing_whitespace :
  cstr_trim [9; 32; 120; 160] true = [120; 160].
Proof. vm_compute. reflexivity. Qed.

Example trim_end_keeps_leading_whitespace :
  cstr_trim [9; 120; 8233; 65279] false = [9; 120].
Proof. vm_compute. reflexivity. Qed.

Example trim_does_not_remove_obsolete_mongolian_vowel_separator :
  cstr_trim [6158; 120] true = [6158; 120].
Proof. vm_compute. reflexivity. Qed.

Definition required_fields_state (fields : list (string * val)) : xstate :=
  mkXState [Some (ORecord "" fields)] nil nil None None nil 0.

Definition check_key_value_fields (fields : list (string * val)) :=
  run_heap_query_x (required_fields_state fields)
    (ty_check_query type_check_fuel
      (TRecordFields "" ["Key"; "Value"]) (VAddr 0)).

Example required_record_fields_accept_present_values :
  check_key_value_fields [("Key", VMath 0); ("Value", VBool true)] =
    Ok (Some true).
Proof. vm_compute. reflexivity. Qed.

Example required_record_fields_accept_present_undef :
  check_key_value_fields [("Key", VUndef); ("Value", VUndef)] =
    Ok (Some true).
Proof. vm_compute. reflexivity. Qed.

Example required_record_fields_reject_missing_value :
  check_key_value_fields [("Key", VMath 0)] = Ok (Some false).
Proof. vm_compute. reflexivity. Qed.

Definition non_record_state : xstate :=
  mkXState [Some (OList nil)] nil nil None None nil 0.

Example required_record_fields_reject_non_record :
  run_heap_query_x non_record_state
    (ty_check_query type_check_fuel
      (TRecordFields "" ["Key"; "Value"]) (VAddr 0)) =
    Ok (Some false).
Proof. vm_compute. reflexivity. Qed.

Definition constructor_fields_state
  (type_name : string) (fields : list (string * val)) : xstate :=
  mkXState [Some (ORecord type_name fields)] nil nil None None nil 0.

Definition check_constructor_fields
  (type_name : string) (fields : list (string * val)) :=
  run_heap_query_x (constructor_fields_state type_name fields)
    (ty_check_query type_check_fuel
      (TRecordFields "Object" ["Call"; "Construct"]) (VAddr 0)).

Definition check_function_fields
  (type_name : string) (fields : list (string * val)) :=
  run_heap_query_x (constructor_fields_state type_name fields)
    (ty_check_query type_check_fuel
      (TRecordFields "Object" ["Call"]) (VAddr 0)).

Example constructor_fields_accept_proper_object_descendant_without_fields :
  check_function_fields "BuiltinFunctionObject" nil = Ok (Some true) /\
  check_constructor_fields "BuiltinFunctionObject" nil = Ok (Some true).
Proof. split; vm_compute; reflexivity. Qed.

Example constructor_fields_accept_exact_object_with_required_fields :
  check_constructor_fields "Object"
    [("Call", VUndef); ("Construct", VUndef)] = Ok (Some true).
Proof. vm_compute. reflexivity. Qed.

Example constructor_fields_reject_exact_object_missing_required_field :
  check_constructor_fields "Object" [("Call", VUndef)] = Ok (Some false).
Proof. vm_compute. reflexivity. Qed.

Example constructor_fields_reject_unrelated_record :
  check_constructor_fields "ModuleRecord"
    [("Call", VUndef); ("Construct", VUndef)] = Ok (Some false).
Proof. vm_compute. reflexivity. Qed.
