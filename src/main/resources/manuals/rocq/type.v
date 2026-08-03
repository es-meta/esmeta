From Stdlib Require Import PeanoNat List ZArith String Ascii.
From Stdlib Require Export ListDef.
Import ListNotations.

Set Implicit Arguments.
Open Scope string_scope.

(* ------------------------------------------------------------------------- *)
(* ECMAScript language values                                               *)
(* ------------------------------------------------------------------------- *)

Inductive Undefined : Type :=
  | undefined.

Inductive Null : Type :=
  | null.

Inductive Boolean : Type :=
  | trueB
  | falseB.

Definition Str := string.
Definition str_eqb := String.eqb.

Inductive Description : Type :=
  | undef (value : Undefined)
  | str (value : Str).

Inductive Symbol : Type :=
  | sym (id : nat) (description : Description).

Definition Symbol_eqb (left right : Symbol) : bool :=
  match left, right with
  | sym left_id _, sym right_id _ => Nat.eqb left_id right_id
  end.

(* A finite IEEE-754 number is kept in sign/mantissa/exponent form for now. *)
Inductive Number : Type :=
  | num (sign mantissa exponent : Z)
  | pos_zero
  | neg_zero
  | pos_inf
  | neg_inf
  | NaN.

Notation BigInt := Z.

Inductive Property_key : Type :=
  | StrKey (key : Str)
  | SymKey (key : Symbol).

Definition Property_key_eqb (left right : Property_key) : bool :=
  match left, right with
  | StrKey left_key, StrKey right_key => str_eqb left_key right_key
  | SymKey left_key, SymKey right_key => Symbol_eqb left_key right_key
  | _, _ => false
  end.

(* ESMeta's state/Value.scala distinguishes stable named addresses used for
 * intrinsics and global structures from fresh dynamic addresses. *)
Inductive loc : Type :=
  | named_loc (name : Str)
  | dynamic_loc (index : nat).

Definition loc_eqb (left right : loc) : bool :=
  match left, right with
  | named_loc left_name, named_loc right_name =>
      String.eqb left_name right_name
  | dynamic_loc left_index, dynamic_loc right_index =>
      Nat.eqb left_index right_index
  | _, _ => false
  end.

(* ECMAScript objects are identities (heap locations), not inline states. *)
Inductive Object : Type :=
  | obj (ptr : loc).

Inductive ECMAScript_language_value : Type :=
  | UndefV (value : Undefined)
  | NullV (value : Null)
  | BoolV (value : Boolean)
  | StrV (value : Str)
  | SymV (value : Symbol)
  | NumV (value : Number)
  | BigintV (value : BigInt)
  | ObjV (value : Object).

Notation ESValue := ECMAScript_language_value.
Notation Value := ECMAScript_language_value.

Inductive ESValueType : Type :=
  | UndefinedT
  | NullT
  | BooleanT
  | StringT
  | SymbolT
  | NumberT
  | BigintT
  | ObjectT.

Notation Types := ESValueType.

(* ------------------------------------------------------------------------- *)
(* Values used by the ESMeta IR                                              *)
(* ------------------------------------------------------------------------- *)

(* ESMeta distinguishes named locals, compiler-generated temporaries, and
 * globals in ir/Ref.scala.  These are metalanguage variables; in particular,
 * they are not ECMAScript lexical-environment bindings. *)
Inductive IRLocal : Type :=
  | IR_Name (name : Str)
  | IR_Temp (index : nat).

Definition IRLocal_eqb (left right : IRLocal) : bool :=
  match left, right with
  | IR_Name left_name, IR_Name right_name =>
      String.eqb left_name right_name
  | IR_Temp left_index, IR_Temp right_index =>
      Nat.eqb left_index right_index
  | _, _ => false
  end.

Definition IRGlobal := Str.
Definition IRGlobal_eqb := String.eqb.

Inductive IRVariable : Type :=
  | IR_Local (local : IRLocal)
  | IR_Global (global : IRGlobal).

(* These are ESMeta mathematical values, distinct from ECMAScript Numbers. *)
Inductive MathematicalValue : Type :=
  | math_value (sign mantissa exponent : Z).

Inductive MathematicalInfinity : Type :=
  | math_pos_inf
  | math_neg_inf.

Inductive IRAst : Type :=
  | IR_syntactic
      (name : Str)
      (arguments : list bool)
      (rhs_index : nat)
      (children : list (option IRAst))
  | IR_lexical
      (name : Str)
      (source_text : Str).

Record GrammarSymbol : Type := {
  grammar_name : Str;
  grammar_parameters : list bool;
}.

(*
 * Record, map, and list objects are represented by locations.  Their heap
 * payloads live in manual_type.v, avoiding a dependency from the value core
 * back to the manual type model.
 *
 * Closures contain the name of a generated function and the values captured
 * from the current local environment.  Continuation call stacks are deferred
 * until execution contexts are mechanized; captured locals are already kept.
 *)
Inductive IRValue : Type :=
  | IR_ESValue (value : ESValue)
  | IR_Address (address : loc)
  | IR_Closure
      (function_name : Str)
      (captured : list (IRLocal * IRValue))
  | IR_Continuation
      (function_name : Str)
      (captured : list (IRLocal * IRValue))
  | IR_AstValue (ast : IRAst)
  | IR_GrammarSymbol (symbol : GrammarSymbol)
  | IR_Math (value : MathematicalValue)
  | IR_Infinity (value : MathematicalInfinity)
  | IR_Enum (name : Str)
  | IR_CodeUnit (value : ascii).

Definition IR_undefined : IRValue := IR_ESValue (UndefV undefined).
Definition IR_null : IRValue := IR_ESValue (NullV null).
