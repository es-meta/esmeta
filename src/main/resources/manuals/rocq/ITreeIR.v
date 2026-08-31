From Stdlib Require Import List String Bool ZArith NArith.
From ITree Require Import Core.ITreeDefinition.

Import ListNotations.
Open Scope string_scope.

(** Values needed to state the result of translating ESMeta IR.  Primitive
    operations are deliberately events: this file describes the shallow ITree
    interface, not an embedded copy of the IR syntax or an interpreter. *)
Inductive IRLocal : Type :=
  | IR_Name (name : string)
  | IR_Temp (index : nat).

Definition IRLocal_eqb (left right : IRLocal) : bool :=
  match left, right with
  | IR_Name left_name, IR_Name right_name => String.eqb left_name right_name
  | IR_Temp left_index, IR_Temp right_index => Nat.eqb left_index right_index
  | _, _ => false
  end.

Inductive IRValue : Type :=
  | IR_Undefined
  | IR_Null
  | IR_Bool (value : bool)
  | IR_String (value : string)
  | IR_Math (coefficient exponent : Z)
  | IR_Infinity (positive : bool)
  | IR_Number (bits : N)
  | IR_BigInt (value : Z)
  | IR_Enum (name : string)
  | IR_CodeUnit (value : N)
  | IR_Address (address : N)
  | IR_Closure
      (function_name : string)
      (captured : list (IRLocal * IRValue))
  | IR_Continuation
      (function_name : string)
      (captured : list (IRLocal * IRValue))
  | IR_Syntactic
      (name : string)
      (arguments : list bool)
      (rhs_index : nat)
      (children : list (option IRValue))
  | IR_Lexical (name : string) (value : IRValue)
  | IR_GrammarSymbol (name : string) (parameters : list bool).

Definition IRLocalEnv : Type := list (IRLocal * IRValue).

Fixpoint ir_lookup_local (local : IRLocal) (environment : IRLocalEnv)
    : option IRValue :=
  match environment with
  | [] => None
  | (current, value) :: tail =>
      if IRLocal_eqb local current
      then Some value
      else ir_lookup_local local tail
  end.

Fixpoint ir_set_local
    (local : IRLocal) (value : IRValue) (environment : IRLocalEnv)
    : IRLocalEnv :=
  match environment with
  | [] => [(local, value)]
  | (current, old_value) :: tail =>
      if IRLocal_eqb local current
      then (current, value) :: tail
      else (current, old_value) :: ir_set_local local value tail
  end.

(** The effects that a later executable semantics must handle.  [PrimE] and
    [CommandE] form the intentionally small boundary for ESMeta primitives;
    generated terms still expose their operands and evaluation order.  Calls
    are events as well, so recursive IR does not become Gallina recursion. *)
Variant esmetaE : Type -> Type :=
  | PrimE (operation : string) (arguments : list IRValue) : esmetaE IRValue
  | CommandE (operation : string) (arguments : list IRValue) : esmetaE unit
  | CallE (callee : IRValue) (arguments : list IRValue) : esmetaE IRValue
  | SdoCallE
      (base : IRValue) (operation : string) (arguments : list IRValue)
      : esmetaE IRValue
  | PrintE (value : IRValue) : esmetaE unit.

Inductive IRResult (A : Type) : Type :=
  | IRSuccess (value : A)
  | IRFail.

Arguments IRSuccess {A} _.
Arguments IRFail {A}.

(** Normal computations produce a value.  A source-level [return] instead
    exits every enclosing bind until [ir_make_function] consumes it.  Keeping
    this control signal in the common computation result lets generated
    statements use the ordinary monadic sequencing notation directly. *)
Inductive IRControl (A : Type) : Type :=
  | IRContinue (value : A)
  | IRReturned (value : IRValue).

Arguments IRContinue {A} _.
Arguments IRReturned {A} _.

(** A translated computation threads only its function-local environment.
    Heap/global state belongs to the handler of [esmetaE], which keeps this
    translation layer independent of any particular executable model. *)
Definition IRComp (A : Type) : Type :=
  IRLocalEnv -> itree esmetaE (IRResult (IRLocalEnv * IRControl A)).

Definition IRFunction : Type :=
  list IRValue -> itree esmetaE (IRResult IRValue).

Definition ir_pure {A : Type} (value : A) : IRComp A :=
  fun environment => Ret (IRSuccess (environment, IRContinue value)).

Definition ir_return_value (value : IRValue) : IRComp unit :=
  fun environment => Ret (IRSuccess (environment, IRReturned value)).

Definition ir_fail {A : Type} : IRComp A :=
  fun _ => Ret IRFail.

Definition ir_bind {A B : Type}
    (computation : IRComp A) (continuation : A -> IRComp B) : IRComp B :=
  fun environment =>
    ITree.bind (computation environment) (fun result =>
      match result with
      | IRSuccess (next_environment, IRContinue value) =>
          continuation value next_environment
      | IRSuccess (next_environment, IRReturned value) =>
          Ret (IRSuccess (next_environment, IRReturned value))
      | IRFail => Ret IRFail
      end).

Definition ir_then {A B : Type}
    (computation : IRComp A) (continuation : IRComp B) : IRComp B :=
  ir_bind computation (fun _ => continuation).

Definition ir_return (computation : IRComp IRValue) : IRComp unit :=
  ir_bind computation ir_return_value.

(** Evaluate computation-valued operands from left to right. *)
Fixpoint ir_eval_values (computations : list (IRComp IRValue))
    : IRComp (list IRValue) :=
  match computations with
  | [] => ir_pure []
  | computation :: tail =>
      ir_bind computation (fun value =>
        ir_bind (ir_eval_values tail) (fun values =>
          ir_pure (value :: values)))
  end.

Fixpoint ir_eval_optional_values
    (computations : list (option (IRComp IRValue)))
    : IRComp (list (option IRValue)) :=
  match computations with
  | [] => ir_pure []
  | None :: tail =>
      ir_bind (ir_eval_optional_values tail) (fun values =>
        ir_pure (None :: values))
  | Some computation :: tail =>
      ir_bind computation (fun value =>
        ir_bind (ir_eval_optional_values tail) (fun values =>
          ir_pure (Some value :: values)))
  end.

Definition ir_logical_and
    (left right : IRComp IRValue) : IRComp IRValue :=
  ir_bind left (fun left_value =>
    match left_value with
    | IR_Bool true => right
    | IR_Bool false => ir_pure (IR_Bool false)
    | _ => ir_fail
    end).

Definition ir_logical_or
    (left right : IRComp IRValue) : IRComp IRValue :=
  ir_bind left (fun left_value =>
    match left_value with
    | IR_Bool true => ir_pure (IR_Bool true)
    | IR_Bool false => right
    | _ => ir_fail
    end).

Definition ir_syntactic
    (name : string) (arguments : list bool) (rhs_index : nat)
    (children : list (option (IRComp IRValue))) : IRComp IRValue :=
  ir_bind (ir_eval_optional_values children) (fun values =>
    ir_pure (IR_Syntactic name arguments rhs_index values)).

Definition ir_lexical
    (name : string) (computation : IRComp IRValue) : IRComp IRValue :=
  ir_bind computation (fun value => ir_pure (IR_Lexical name value)).

Definition ir_event {A : Type} (event : esmetaE A) : IRComp A :=
  fun environment =>
    ITree.bind (ITree.trigger event) (fun value =>
      Ret (IRSuccess (environment, IRContinue value))).

(** ITree-style surface syntax for [IRComp].  These helpers deliberately use
    [ir_bind] rather than [ITree.bind], so the local environment and [IRFail]
    keep flowing through a generated computation. *)
Definition trigger {A : Type} (event : esmetaE A) : IRComp A :=
  ir_event event.

Declare Scope ir_scope.
Delimit Scope ir_scope with ir.

Notation "value <- computation ;; continuation" :=
  (ir_bind computation (fun value => continuation))
  (at level 61, computation at next level, right associativity) : ir_scope.

Notation "computation ;;; continuation" :=
  (ir_then computation continuation)
  (at level 61, right associativity) : ir_scope.

Definition ir_primitive_values (operation : string) (arguments : list IRValue)
    : IRComp IRValue :=
  trigger (PrimE operation arguments).

(** Lift primitive families over computation-valued operands without changing
    the value-only event interface. *)
Definition ir_primitive
    (operation : string) (arguments : list (IRComp IRValue))
    : IRComp IRValue :=
  ir_bind (ir_eval_values arguments) (fun values =>
    ir_primitive_values operation values).

Definition ir_unary
    (operation : string) (operand : IRComp IRValue) : IRComp IRValue :=
  ir_primitive
    (String.append "unary." operation)
    [operand].

Definition ir_binary
    (operation : string) (left right : IRComp IRValue) : IRComp IRValue :=
  ir_primitive
    (String.append "binary." operation)
    [left; right].

Definition ir_variadic
    (operation : string) (operands : list (IRComp IRValue)) : IRComp IRValue :=
  ir_primitive (String.append "variadic." operation) operands.

Definition ir_math
    (operation : string) (operands : list (IRComp IRValue)) : IRComp IRValue :=
  ir_primitive (String.append "math." operation) operands.

Definition ir_convert
    (operation : string) (operands : list (IRComp IRValue)) : IRComp IRValue :=
  ir_primitive (String.append "convert." operation) operands.

Definition ir_command_values (operation : string) (arguments : list IRValue)
    : IRComp unit :=
  trigger (CommandE operation arguments).

Definition ir_command
    (operation : string) (arguments : list (IRComp IRValue)) : IRComp unit :=
  ir_bind (ir_eval_values arguments) (fun values =>
    ir_command_values operation values).

(** [push] evaluates the element before the list, matching ESMeta IR, while
    retaining the primitive's list-first argument order. *)
Definition ir_push
    (element list : IRComp IRValue) (front : bool) : IRComp unit :=
  ir_bind element (fun element_value =>
    ir_bind list (fun list_value =>
      ir_command_values "list.push"
        [list_value; element_value; IR_Bool front])).

Definition ir_pop (list : IRComp IRValue) (front : bool) : IRComp IRValue :=
  ir_bind list (fun list_value =>
    ir_primitive_values "list.pop" [list_value; IR_Bool front]).

Definition ir_call_values (callee : IRValue) (arguments : list IRValue)
    : IRComp IRValue :=
  trigger (CallE callee arguments).

Definition ir_call
    (callee : IRComp IRValue) (arguments : list (IRComp IRValue))
    : IRComp IRValue :=
  ir_bind callee (fun callee_value =>
    ir_bind (ir_eval_values arguments) (fun argument_values =>
      ir_call_values callee_value argument_values)).

Definition ir_sdo_call_values
    (base : IRValue) (operation : string) (arguments : list IRValue)
    : IRComp IRValue :=
  trigger (SdoCallE base operation arguments).

Definition ir_sdo_call
    (base : IRComp IRValue) (operation : string)
    (arguments : list (IRComp IRValue)) : IRComp IRValue :=
  ir_bind base (fun base_value =>
    ir_bind (ir_eval_values arguments) (fun argument_values =>
      ir_sdo_call_values base_value operation argument_values)).

Definition ir_print_value (value : IRValue) : IRComp unit :=
  trigger (PrintE value).

Definition ir_print (computation : IRComp IRValue) : IRComp unit :=
  ir_bind computation ir_print_value.

Definition ir_read_local (local : IRLocal) : IRComp IRValue :=
  fun environment =>
    match ir_lookup_local local environment with
    | Some value => Ret (IRSuccess (environment, IRContinue value))
    | None => Ret IRFail
    end.

Definition ir_write_local_value
    (local : IRLocal) (value : IRValue) : IRComp unit :=
  fun environment =>
    Ret (IRSuccess
      (ir_set_local local value environment, IRContinue tt)).

Definition ir_write_local
    (local : IRLocal) (computation : IRComp IRValue) : IRComp unit :=
  ir_bind computation (ir_write_local_value local).

Definition ir_local_exists (local : IRLocal) : IRComp IRValue :=
  fun environment =>
    Ret (IRSuccess
      (environment,
       IRContinue
         (IR_Bool
           match ir_lookup_local local environment with
           | Some _ => true
           | None => false
           end))).

Definition ir_capture_closure (name : string) (locals : list IRLocal)
    : IRComp IRValue :=
  fun environment =>
    let fix capture remaining :=
      match remaining with
      | [] => Some []
      | local :: tail =>
          match ir_lookup_local local environment, capture tail with
          | Some value, Some captured => Some ((local, value) :: captured)
          | _, _ => None
          end
      end in
    match capture locals with
    | Some captured =>
        Ret (IRSuccess
          (environment, IRContinue (IR_Closure name captured)))
    | None => Ret IRFail
    end.

Definition ir_capture_continuation (name : string) : IRComp IRValue :=
  fun environment =>
    let fix named_locals remaining :=
      match remaining with
      | [] => []
      | (IR_Name local_name, value) :: tail =>
          (IR_Name local_name, value) :: named_locals tail
      | (IR_Temp _, _) :: tail => named_locals tail
      end in
    Ret (IRSuccess
      (environment,
       IRContinue (IR_Continuation name (named_locals environment)))).

Inductive IRTarget : Type :=
  | IR_LocalTarget (local : IRLocal)
  | IR_GlobalTarget (name : string)
  | IR_FieldTarget (base field : IRValue).

Definition ir_field_target
    (base field : IRComp IRValue) : IRComp IRTarget :=
  ir_bind base (fun base_value =>
    ir_bind field (fun field_value =>
      ir_pure (IR_FieldTarget base_value field_value))).

Definition ir_read_target_value (target : IRTarget) : IRComp IRValue :=
  match target with
  | IR_LocalTarget local => ir_read_local local
  | IR_GlobalTarget name =>
      ir_primitive_values "ref.read-global" [IR_String name]
  | IR_FieldTarget base field =>
      ir_primitive_values "ref.read-field" [base; field]
  end.

Definition ir_read_target
    (target : IRComp IRTarget) : IRComp IRValue :=
  ir_bind target ir_read_target_value.

Definition ir_write_target_values (target : IRTarget) (value : IRValue)
    : IRComp unit :=
  match target with
  | IR_LocalTarget local => ir_write_local_value local value
  | IR_GlobalTarget name =>
      ir_command_values "ref.write-global" [IR_String name; value]
  | IR_FieldTarget base field =>
      ir_command_values "ref.write-field" [base; field; value]
  end.

Definition ir_write_target
    (target : IRComp IRTarget) (value : IRComp IRValue) : IRComp unit :=
  ir_bind target (fun target_value =>
    ir_bind value (ir_write_target_values target_value)).

Definition ir_target_exists_value (target : IRTarget) : IRComp IRValue :=
  match target with
  | IR_LocalTarget local => ir_local_exists local
  | IR_GlobalTarget name =>
      ir_primitive_values "ref.exists-global" [IR_String name]
  | IR_FieldTarget base field =>
      ir_primitive_values "ref.exists-field" [base; field]
  end.

Definition ir_target_exists
    (target : IRComp IRTarget) : IRComp IRValue :=
  ir_bind target ir_target_exists_value.

Definition ir_skip : IRComp unit := ir_pure tt.

Definition ir_if_value
    (condition : IRValue)
    (then_computation else_computation : IRComp unit) : IRComp unit :=
  match condition with
  | IR_Bool true => then_computation
  | IR_Bool false => else_computation
  | _ => ir_fail
  end.

(** Evaluate a computation-valued condition exactly once before selecting a
    branch. *)
Definition ir_if
    (condition : IRComp IRValue)
    (then_computation else_computation : IRComp unit) : IRComp unit :=
  ir_bind condition (fun condition_value =>
    ir_if_value condition_value then_computation else_computation).

(** [obligation] is the source-level verification condition generated for this
    assertion.  Requiring its proof makes every generated assertion an
    explicit Rocq proof obligation; the value check is retained so the ITree
    still fails if an event handler does not implement the assumed primitive
    semantics. *)
Definition ir_assert_value
    (condition : IRValue) (obligation : Prop) (_ : obligation) : IRComp unit :=
  match condition with
  | IR_Bool true => ir_pure tt
  | _ => ir_fail
  end.

Definition ir_assert
    (condition : IRComp IRValue)
    (obligation : Prop) (proof : obligation) : IRComp unit :=
  ir_bind condition (fun condition_value =>
    ir_assert_value condition_value obligation proof).

(** The loop state is the local environment.  A source-level return from the
    condition or body exits the loop and is propagated by the next bind. *)
Definition ir_while
    (condition : IRComp IRValue) (body : IRComp unit) : IRComp unit :=
  fun environment =>
    ITree.iter (fun current_environment =>
      ITree.bind (condition current_environment) (fun condition_result =>
        match condition_result with
        | IRFail => Ret (inr IRFail)
        | IRSuccess (next_environment, IRReturned value) =>
            Ret (inr (IRSuccess (next_environment, IRReturned value)))
        | IRSuccess (next_environment, IRContinue (IR_Bool false)) =>
            Ret (inr (IRSuccess (next_environment, IRContinue tt)))
        | IRSuccess (next_environment, IRContinue (IR_Bool true)) =>
            ITree.bind (body next_environment) (fun body_result =>
              match body_result with
              | IRFail => Ret (inr IRFail)
              | IRSuccess (body_environment, IRContinue _) =>
                  Ret (inl body_environment)
              | IRSuccess (body_environment, IRReturned value) =>
                  Ret (inr (IRSuccess
                    (body_environment, IRReturned value)))
              end)
        | IRSuccess (_, IRContinue _) => Ret (inr IRFail)
        end)) environment.

Notation "'ir_if' condition 'then' then_computation 'else' else_computation 'end'" :=
  (ir_if condition then_computation else_computation)
  (at level 60, condition at level 200,
   then_computation at level 200, else_computation at level 200,
   only parsing) : ir_scope.

Notation "'ir_while' condition 'do' body 'end'" :=
  (ir_while condition body)
  (at level 60, condition at level 200, body at level 200,
   only parsing) : ir_scope.

Fixpoint ir_bind_parameters
    (parameters : list IRLocal) (arguments : list IRValue)
    : option IRLocalEnv :=
  match parameters, arguments with
  | [], [] => Some []
  | parameter :: parameter_tail, argument :: argument_tail =>
      match ir_bind_parameters parameter_tail argument_tail with
      | Some environment => Some ((parameter, argument) :: environment)
      | None => None
      end
  | _, _ => None
  end.

Definition ir_make_function
    (parameters : list IRLocal) (body : IRComp unit)
    : IRFunction :=
  fun arguments =>
    match ir_bind_parameters parameters arguments with
    | None => Ret IRFail
    | Some environment =>
        ITree.bind (body environment) (fun result =>
          match result with
          | IRSuccess (_, IRContinue _) => Ret (IRSuccess IR_Undefined)
          | IRSuccess (_, IRReturned value) => Ret (IRSuccess value)
          | IRFail => Ret IRFail
          end)
    end.
