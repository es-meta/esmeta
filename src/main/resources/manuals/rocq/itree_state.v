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
    | FAIL => Ret FAIL
    end.

(** A function call is an event, so recursive calls are not Gallina recursion. *)
Definition itree_state_call `{Σ : GRA}
    (signature : fnsig_t IRFunctionInput IRFunctionOutput)
    (arguments : list IRValue)
    : ITree_State_Completion IRValue :=
  fun state => ccallU signature (arguments, state).

(* ------------------------------------------------------------------------- *)
(* Instruction blocks                                                        *)
(* ------------------------------------------------------------------------- *)

(** An IR instruction sequence either falls through to whatever follows it or
    returns from the enclosing function.  ESMeta's IR is structured -- [IIf] and
    [IWhile] rather than jumps -- so that one distinction is all a block needs,
    and it costs one [option]: [None] fell through, [Some value] returned.

    Without this, a branch could only be translated by duplicating everything
    that follows it into both arms, which is exponential in the nesting depth. *)
Definition ITree_Block `{Σ : GRA} : Type :=
  ITree_State_Completion (option IRValue).

Definition itree_block_fallthrough `{Σ : GRA} : ITree_Block :=
  itree_state_return None.

(** Evaluate the returned expression, then stop the enclosing block. *)
Definition itree_block_return `{Σ : GRA}
    (computation : ITree_State_Completion IRValue)
    : ITree_Block :=
  itree_state_bind computation (fun value => itree_state_return (Some value)).

(** Run [next] only when [block] falls through; a return propagates outwards. *)
Definition itree_block_seq `{Σ : GRA}
    (block next : ITree_Block)
    : ITree_Block :=
  itree_state_bind block (fun outcome =>
    match outcome with
    | Some value => itree_state_return (Some value)
    | None => next
    end).

(** Branch on an already-evaluated condition, keeping the evaluation order of
    the condition explicit in the generated code.  A non-boolean condition is an
    IR type error rather than a specification behaviour. *)
Definition itree_block_if `{Σ : GRA}
    (condition : IRValue)
    (then_block else_block : ITree_Block)
    : ITree_Block :=
  match condition with
  | IR_ESValue (BoolV trueB) => then_block
  | IR_ESValue (BoolV falseB) => else_block
  | _ => itree_state_fail
  end.

(** Run a block as a whole function body.  ESMeta IR functions end in [IReturn],
    so falling off the end is an IR error, not a normal undefined return. *)
Definition itree_block_body `{Σ : GRA}
    (block : ITree_Block)
    : ITree_State_Completion IRValue :=
  itree_state_bind block (fun outcome =>
    match outcome with
    | Some value => itree_state_return value
    | None => itree_state_fail
    end).
