Require Import type manual_type.
From CRIS Require Import CRIS.

(** The uniform call interface used by every generated ESMeta function. *)
Definition IRFunctionInput : Type := (list IRValue * State)%type.
Definition IRFunctionOutput : Type := Exec_Result IRValue.
Definition ir_function_type : fntyp_t IRFunctionInput IRFunctionOutput :=
  fntyp IRFunctionInput IRFunctionOutput.

(** A state computation that may also emit CRIS events. *)
Definition ITree_State_Completion `{Σ : GRA} (A : Type) : Type :=
  State -> itree crisE (Exec_Result A).

Definition itree_state_lift `{Σ : GRA} {A : Type}
    (computation : State_Completion A)
    : ITree_State_Completion A :=
  fun state => Ret (computation state).

Definition itree_state_return `{Σ : GRA} {A : Type}
    (value : A)
    : ITree_State_Completion A :=
  itree_state_lift (state_return value).

Definition itree_state_fail `{Σ : GRA} {A : Type}
    : ITree_State_Completion A :=
  fun _ => Ret FAIL.

Definition itree_state_bind `{Σ : GRA} {A B : Type}
    (computation : ITree_State_Completion A)
    (continuation : A -> ITree_State_Completion B)
    : ITree_State_Completion B :=
  fun state =>
    result <- computation state;;
    match result with
    | RESULT next_state value => continuation value next_state
    | OUT_OF_FUEL => Ret OUT_OF_FUEL
    | FAIL => Ret FAIL
    end.

(** A function call is an event, so recursive calls are not Gallina recursion. *)
Definition itree_state_call `{Σ : GRA}
    (signature : fnsig_t IRFunctionInput IRFunctionOutput)
    (arguments : list IRValue)
    : ITree_State_Completion IRValue :=
  fun state => ccallU signature (arguments, state).
