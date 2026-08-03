From Stdlib Require Import ZArith String.
Require Import type manual_type.

(* ------------------------------------------------------------------------- *)
(* Helpers for IR operator results                                           *)
(* ------------------------------------------------------------------------- *)

Definition op_boolean_value (value : Boolean) : IRValue :=
  IR_ESValue (BoolV value).

Definition op_bool_result (value : bool) : IRValue :=
  op_boolean_value (if value then trueB else falseB).

Definition op_true : IRValue := op_boolean_value trueB.
Definition op_false : IRValue := op_boolean_value falseB.

(* ------------------------------------------------------------------------- *)
(* Unary operators                                                           *)
(* ------------------------------------------------------------------------- *)

Definition op_neg (operand : IRValue) : State_Completion IRValue :=
  match operand with
  | IR_ESValue (BigintV value) =>
      state_return (IR_ESValue (BigintV (Z.opp value)))
  | _ => fun _ => FAIL
  end.

Definition op_not (operand : IRValue) : State_Completion IRValue :=
  match operand with
  | IR_ESValue (BoolV trueB) => state_return op_false
  | IR_ESValue (BoolV falseB) => state_return op_true
  | _ => fun _ => FAIL
  end.

(* ------------------------------------------------------------------------- *)
(* Binary numeric operators                                                  *)
(* ------------------------------------------------------------------------- *)

Definition op_add
    (left right : IRValue)
    : State_Completion IRValue :=
  match left, right with
  | IR_ESValue (BigintV left_value),
    IR_ESValue (BigintV right_value) =>
      state_return
        (IR_ESValue (BigintV (Z.add left_value right_value)))
  | _, _ => fun _ => FAIL
  end.

Definition op_sub
    (left right : IRValue)
    : State_Completion IRValue :=
  match left, right with
  | IR_ESValue (BigintV left_value),
    IR_ESValue (BigintV right_value) =>
      state_return
        (IR_ESValue (BigintV (Z.sub left_value right_value)))
  | _, _ => fun _ => FAIL
  end.

Definition op_mul
    (left right : IRValue)
    : State_Completion IRValue :=
  match left, right with
  | IR_ESValue (BigintV left_value),
    IR_ESValue (BigintV right_value) =>
      state_return
        (IR_ESValue (BigintV (Z.mul left_value right_value)))
  | _, _ => fun _ => FAIL
  end.

Definition op_lt
    (left right : IRValue)
    : State_Completion IRValue :=
  match left, right with
  | IR_ESValue (BigintV left_value),
    IR_ESValue (BigintV right_value) =>
      state_return (op_bool_result (Z.ltb left_value right_value))
  | _, _ => fun _ => FAIL
  end.

Definition op_equal
    (left right : IRValue)
    : State_Completion IRValue :=
  match left, right with
  | IR_ESValue (BigintV left_value),
    IR_ESValue (BigintV right_value) =>
      state_return (op_bool_result (Z.eqb left_value right_value))
  | _, _ => fun _ => FAIL
  end.

(* ------------------------------------------------------------------------- *)
(* Binary equality and Boolean operators                                     *)
(* ------------------------------------------------------------------------- *)

Definition op_basic_eq_operand (value : IRValue) : bool :=
  match value with
  | IR_ESValue (UndefV _)
  | IR_ESValue (NullV _)
  | IR_ESValue (BoolV _)
  | IR_ESValue (StrV _)
  | IR_ESValue (BigintV _)
  | IR_Address _
  | IR_Enum _ => true
  | _ => false
  end.

Definition op_eq
    (left right : IRValue)
    : State_Completion IRValue :=
  match left, right with
  | IR_ESValue (BigintV left_value),
    IR_ESValue (BigintV right_value) =>
      state_return (op_bool_result (Z.eqb left_value right_value))
  | IR_ESValue (StrV left_value),
    IR_ESValue (StrV right_value) =>
      state_return (op_bool_result (String.eqb left_value right_value))
  | IR_Address left_address, IR_Address right_address =>
      state_return (op_bool_result (loc_eqb left_address right_address))
  | IR_Enum left_name, IR_Enum right_name =>
      state_return (op_bool_result (String.eqb left_name right_name))
  | IR_ESValue (BoolV trueB), IR_ESValue (BoolV trueB)
  | IR_ESValue (BoolV falseB), IR_ESValue (BoolV falseB)
  | IR_ESValue (UndefV undefined), IR_ESValue (UndefV undefined)
  | IR_ESValue (NullV null), IR_ESValue (NullV null) =>
      state_return op_true
  | _, _ =>
      if andb (op_basic_eq_operand left) (op_basic_eq_operand right)
      then state_return op_false
      else fun _ => FAIL
  end.

Definition op_xor
    (left right : IRValue)
    : State_Completion IRValue :=
  match left, right with
  | IR_ESValue (BoolV trueB), IR_ESValue (BoolV falseB)
  | IR_ESValue (BoolV falseB), IR_ESValue (BoolV trueB) =>
      state_return op_true
  | IR_ESValue (BoolV trueB), IR_ESValue (BoolV trueB)
  | IR_ESValue (BoolV falseB), IR_ESValue (BoolV falseB) =>
      state_return op_false
  | _, _ => fun _ => FAIL
  end.
