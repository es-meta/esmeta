From Stdlib Require Import PeanoNat List Bool String.
From Stdlib Require Export ListDef.
Require Import type.
Import ListNotations.

Set Implicit Arguments.
Open Scope string_scope.

(* ------------------------------------------------------------------------- *)
(* JSCert-style, flattened representation of ECMAScript objects              *)
(* ------------------------------------------------------------------------- *)

Definition TypeName := Str.
Definition FieldName := Str.

(* An internal method is the name of the generated Rocq function that
 * implements it.  The index prevents, for example, a [[Get]] implementation
 * from accidentally being installed as [[Set]]. *)
Inductive InternalMethodKind : Type :=
  | IMK_GetPrototypeOf
  | IMK_SetPrototypeOf
  | IMK_IsExtensible
  | IMK_PreventExtensions
  | IMK_GetOwnProperty
  | IMK_DefineOwnProperty
  | IMK_HasProperty
  | IMK_Get
  | IMK_Set
  | IMK_Delete
  | IMK_OwnPropertyKeys
  | IMK_Call
  | IMK_Construct.

Record InternalMethod (kind : InternalMethodKind) : Type := {
  internal_method_function : Str;
}.

Arguments internal_method_function {kind} _.

Definition named_method (kind : InternalMethodKind) (name : Str)
    : InternalMethod kind :=
  {| internal_method_function := name |}.

Record ObjectMethods : Type := {
  object_get_prototype_of_ : InternalMethod IMK_GetPrototypeOf;
  object_set_prototype_of_ : InternalMethod IMK_SetPrototypeOf;
  object_is_extensible_ : InternalMethod IMK_IsExtensible;
  object_prevent_extensions_ : InternalMethod IMK_PreventExtensions;
  object_get_own_property_ : InternalMethod IMK_GetOwnProperty;
  object_define_own_property_ : InternalMethod IMK_DefineOwnProperty;
  object_has_property_ : InternalMethod IMK_HasProperty;
  object_get_ : InternalMethod IMK_Get;
  object_set_ : InternalMethod IMK_Set;
  object_delete_ : InternalMethod IMK_Delete;
  object_own_property_keys_ : InternalMethod IMK_OwnPropertyKeys;
  object_call_ : option (InternalMethod IMK_Call);
  object_construct_ : option (InternalMethod IMK_Construct);
}.

Definition methods_with_invocation
    (methods : ObjectMethods)
    (call : option (InternalMethod IMK_Call))
    (construct : option (InternalMethod IMK_Construct))
    : ObjectMethods :=
  {|
    object_get_prototype_of_ := methods.(object_get_prototype_of_);
    object_set_prototype_of_ := methods.(object_set_prototype_of_);
    object_is_extensible_ := methods.(object_is_extensible_);
    object_prevent_extensions_ := methods.(object_prevent_extensions_);
    object_get_own_property_ := methods.(object_get_own_property_);
    object_define_own_property_ := methods.(object_define_own_property_);
    object_has_property_ := methods.(object_has_property_);
    object_get_ := methods.(object_get_);
    object_set_ := methods.(object_set_);
    object_delete_ := methods.(object_delete_);
    object_own_property_keys_ := methods.(object_own_property_keys_);
    object_call_ := call;
    object_construct_ := construct;
  |}.

Definition methods_with_define_own_property
    (methods : ObjectMethods)
    (implementation : InternalMethod IMK_DefineOwnProperty)
    : ObjectMethods :=
  {|
    object_get_prototype_of_ := methods.(object_get_prototype_of_);
    object_set_prototype_of_ := methods.(object_set_prototype_of_);
    object_is_extensible_ := methods.(object_is_extensible_);
    object_prevent_extensions_ := methods.(object_prevent_extensions_);
    object_get_own_property_ := methods.(object_get_own_property_);
    object_define_own_property_ := implementation;
    object_has_property_ := methods.(object_has_property_);
    object_get_ := methods.(object_get_);
    object_set_ := methods.(object_set_);
    object_delete_ := methods.(object_delete_);
    object_own_property_keys_ := methods.(object_own_property_keys_);
    object_call_ := methods.(object_call_);
    object_construct_ := methods.(object_construct_);
  |}.

(* These are all specialized internal slots appearing below Object in the
 * current manuals/types hierarchy.  They are optional because every heap
 * object has this same Rocq type.  A present slot contains the ESMeta IR value
 * stored in that slot; lists and manual records are represented by addresses. *)
Record SpecialObjectSlots : Type := {
  slot_parameter_map_ : option IRValue;

  slot_environment_ : option IRValue;
  slot_private_environment_ : option IRValue;
  slot_formal_parameters_ : option IRValue;
  slot_ecmascript_code_ : option IRValue;
  slot_constructor_kind_ : option IRValue;
  slot_realm_ : option IRValue;
  slot_script_or_module_ : option IRValue;
  slot_this_mode_ : option IRValue;
  slot_strict_ : option IRValue;
  slot_home_object_ : option IRValue;
  slot_source_text_ : option IRValue;
  slot_fields_ : option IRValue;
  slot_private_methods_ : option IRValue;
  slot_class_field_initializer_name_ : option IRValue;
  slot_is_class_constructor_ : option IRValue;
  slot_initial_name_ : option IRValue;
  slot_code_ : option IRValue;

  slot_index_ : option IRValue;
  slot_values_ : option IRValue;
  slot_capability_ : option IRValue;
  slot_remaining_elements_ : option IRValue;
  slot_already_called_ : option IRValue;

  slot_bound_target_function_ : option IRValue;
  slot_bound_this_ : option IRValue;
  slot_bound_arguments_ : option IRValue;
  slot_string_data_ : option IRValue;

  slot_iterated_array_like_ : option IRValue;
  slot_array_like_next_index_ : option IRValue;
  slot_array_like_iteration_kind_ : option IRValue;

  slot_viewed_array_buffer_ : option IRValue;
  slot_array_length_ : option IRValue;
  slot_byte_offset_ : option IRValue;
  slot_content_type_ : option IRValue;
  slot_typed_array_name_ : option IRValue;
  slot_byte_length_ : option IRValue;

  slot_module_ : option IRValue;
  slot_exports_ : option IRValue;
  slot_proxy_handler_ : option IRValue;
  slot_proxy_target_ : option IRValue;

  slot_for_in_object_ : option IRValue;
  slot_object_was_visited_ : option IRValue;
  slot_visited_keys_ : option IRValue;
  slot_remaining_keys_ : option IRValue;

  slot_boolean_data_ : option IRValue;
  slot_symbol_data_ : option IRValue;
  slot_error_data_ : option IRValue;
  slot_number_data_ : option IRValue;
  slot_bigint_data_ : option IRValue;
  slot_date_value_ : option IRValue;
  slot_regexp_matcher_ : option IRValue;
  slot_map_data_ : option IRValue;
  slot_set_data_ : option IRValue;
  slot_weak_map_data_ : option IRValue;
  slot_weak_set_data_ : option IRValue;

  slot_array_buffer_data_ : option IRValue;
  slot_array_buffer_byte_length_ : option IRValue;
  slot_array_buffer_detach_key_ : option IRValue;
  slot_array_buffer_max_byte_length_ : option IRValue;
  slot_array_buffer_byte_length_data_ : option IRValue;
  slot_data_view_ : option IRValue;

  slot_weak_ref_target_ : option IRValue;
  slot_cells_ : option IRValue;
  slot_cleanup_callback_ : option IRValue;
  slot_sync_iterator_record_ : option IRValue;

  slot_promise_state_ : option IRValue;
  slot_promise_result_ : option IRValue;
  slot_promise_fulfill_reactions_ : option IRValue;
  slot_promise_reject_reactions_ : option IRValue;
  slot_promise_is_handled_ : option IRValue;

  slot_generator_state_ : option IRValue;
  slot_generator_context_ : option IRValue;
  slot_generator_brand_ : option IRValue;
  slot_async_generator_state_ : option IRValue;
  slot_async_generator_context_ : option IRValue;
  slot_async_generator_queue_ : option IRValue;
}.

Definition empty_special_object_slots : SpecialObjectSlots :=
  {|
    slot_parameter_map_ := None;
    slot_environment_ := None;
    slot_private_environment_ := None;
    slot_formal_parameters_ := None;
    slot_ecmascript_code_ := None;
    slot_constructor_kind_ := None;
    slot_realm_ := None;
    slot_script_or_module_ := None;
    slot_this_mode_ := None;
    slot_strict_ := None;
    slot_home_object_ := None;
    slot_source_text_ := None;
    slot_fields_ := None;
    slot_private_methods_ := None;
    slot_class_field_initializer_name_ := None;
    slot_is_class_constructor_ := None;
    slot_initial_name_ := None;
    slot_code_ := None;
    slot_index_ := None;
    slot_values_ := None;
    slot_capability_ := None;
    slot_remaining_elements_ := None;
    slot_already_called_ := None;
    slot_bound_target_function_ := None;
    slot_bound_this_ := None;
    slot_bound_arguments_ := None;
    slot_string_data_ := None;
    slot_iterated_array_like_ := None;
    slot_array_like_next_index_ := None;
    slot_array_like_iteration_kind_ := None;
    slot_viewed_array_buffer_ := None;
    slot_array_length_ := None;
    slot_byte_offset_ := None;
    slot_content_type_ := None;
    slot_typed_array_name_ := None;
    slot_byte_length_ := None;
    slot_module_ := None;
    slot_exports_ := None;
    slot_proxy_handler_ := None;
    slot_proxy_target_ := None;
    slot_for_in_object_ := None;
    slot_object_was_visited_ := None;
    slot_visited_keys_ := None;
    slot_remaining_keys_ := None;
    slot_boolean_data_ := None;
    slot_symbol_data_ := None;
    slot_error_data_ := None;
    slot_number_data_ := None;
    slot_bigint_data_ := None;
    slot_date_value_ := None;
    slot_regexp_matcher_ := None;
    slot_map_data_ := None;
    slot_set_data_ := None;
    slot_weak_map_data_ := None;
    slot_weak_set_data_ := None;
    slot_array_buffer_data_ := None;
    slot_array_buffer_byte_length_ := None;
    slot_array_buffer_detach_key_ := None;
    slot_array_buffer_max_byte_length_ := None;
    slot_array_buffer_byte_length_data_ := None;
    slot_data_view_ := None;
    slot_weak_ref_target_ := None;
    slot_cells_ := None;
    slot_cleanup_callback_ := None;
    slot_sync_iterator_record_ := None;
    slot_promise_state_ := None;
    slot_promise_result_ := None;
    slot_promise_fulfill_reactions_ := None;
    slot_promise_reject_reactions_ := None;
    slot_promise_is_handled_ := None;
    slot_generator_state_ := None;
    slot_generator_context_ := None;
    slot_generator_brand_ := None;
    slot_async_generator_state_ := None;
    slot_async_generator_context_ := None;
    slot_async_generator_queue_ := None;
  |}.

Definition ObjectProperties := list (Property_key * IRValue).

(* As in JSCert, Object, FunctionObject, Constructor, ordinary objects, and
 * exotic objects all have this one type.  [object_type_name_] is a class/type
 * tag, not a Rocq subtype witness. *)
Record Object_state : Type := {
  object_type_name_ : TypeName;
  object_methods_ : ObjectMethods;
  object_prototype_ : option ESValue;
  object_extensible_ : option bool;
  object_private_elements_ : list IRValue;
  object_properties_ : ObjectProperties;
  object_special_slots_ : SpecialObjectSlots;
}.

Definition object_create
    (type_name : TypeName)
    (methods : ObjectMethods)
    : Object_state :=
  {|
    object_type_name_ := type_name;
    object_methods_ := methods;
    object_prototype_ := None;
    object_extensible_ := None;
    object_private_elements_ := [];
    object_properties_ := [];
    object_special_slots_ := empty_special_object_slots;
  |}.

Definition ordinary_object_create
    (type_name : TypeName)
    (methods : ObjectMethods)
    (prototype : ESValue)
    (extensible : bool)
    : Object_state :=
  {|
    object_type_name_ := type_name;
    object_methods_ := methods;
    object_prototype_ := Some prototype;
    object_extensible_ := Some extensible;
    object_private_elements_ := [];
    object_properties_ := [];
    object_special_slots_ := empty_special_object_slots;
  |}.

Definition object_with_methods
    (object : Object_state)
    (methods : ObjectMethods)
    : Object_state :=
  {|
    object_type_name_ := object.(object_type_name_);
    object_methods_ := methods;
    object_prototype_ := object.(object_prototype_);
    object_extensible_ := object.(object_extensible_);
    object_private_elements_ := object.(object_private_elements_);
    object_properties_ := object.(object_properties_);
    object_special_slots_ := object.(object_special_slots_);
  |}.

Definition object_set_prototype
    (object : Object_state)
    (prototype : option ESValue)
    : Object_state :=
  {|
    object_type_name_ := object.(object_type_name_);
    object_methods_ := object.(object_methods_);
    object_prototype_ := prototype;
    object_extensible_ := object.(object_extensible_);
    object_private_elements_ := object.(object_private_elements_);
    object_properties_ := object.(object_properties_);
    object_special_slots_ := object.(object_special_slots_);
  |}.

Definition object_set_extensible
    (object : Object_state)
    (extensible : option bool)
    : Object_state :=
  {|
    object_type_name_ := object.(object_type_name_);
    object_methods_ := object.(object_methods_);
    object_prototype_ := object.(object_prototype_);
    object_extensible_ := extensible;
    object_private_elements_ := object.(object_private_elements_);
    object_properties_ := object.(object_properties_);
    object_special_slots_ := object.(object_special_slots_);
  |}.

Definition object_with_private_elements
    (object : Object_state)
    (elements : list IRValue)
    : Object_state :=
  {|
    object_type_name_ := object.(object_type_name_);
    object_methods_ := object.(object_methods_);
    object_prototype_ := object.(object_prototype_);
    object_extensible_ := object.(object_extensible_);
    object_private_elements_ := elements;
    object_properties_ := object.(object_properties_);
    object_special_slots_ := object.(object_special_slots_);
  |}.

Definition object_with_properties
    (object : Object_state)
    (properties : ObjectProperties)
    : Object_state :=
  {|
    object_type_name_ := object.(object_type_name_);
    object_methods_ := object.(object_methods_);
    object_prototype_ := object.(object_prototype_);
    object_extensible_ := object.(object_extensible_);
    object_private_elements_ := object.(object_private_elements_);
    object_properties_ := properties;
    object_special_slots_ := object.(object_special_slots_);
  |}.

Definition object_with_invocation
    (object : Object_state)
    (call : option (InternalMethod IMK_Call))
    (construct : option (InternalMethod IMK_Construct))
    : Object_state :=
  object_with_methods
    object
    (methods_with_invocation object.(object_methods_) call construct).

Definition object_with_special_slots
    (object : Object_state)
    (slots : SpecialObjectSlots)
    : Object_state :=
  {|
    object_type_name_ := object.(object_type_name_);
    object_methods_ := object.(object_methods_);
    object_prototype_ := object.(object_prototype_);
    object_extensible_ := object.(object_extensible_);
    object_private_elements_ := object.(object_private_elements_);
    object_properties_ := object.(object_properties_);
    object_special_slots_ := slots;
  |}.

Definition object_for_array
    (object : Object_state)
    (define_own_property : InternalMethod IMK_DefineOwnProperty)
    : Object_state :=
  object_with_methods
    object
    (methods_with_define_own_property
      object.(object_methods_)
      define_own_property).

Definition object_state_is_callable (object : Object_state) : Prop :=
  exists call, object.(object_methods_).(object_call_) = Some call.

Definition object_state_is_constructor (object : Object_state) : Prop :=
  object_state_is_callable object /\
  exists construct, object.(object_methods_).(object_construct_) = Some construct.

Definition object_state_is_ordinary (object : Object_state) : Prop :=
  (exists prototype, object.(object_prototype_) = Some prototype) /\
  (exists extensible, object.(object_extensible_) = Some extensible).

(* The flattened representation can describe combinations that do not denote
 * a specification object.  Constructors and well-formedness predicates carry
 * the constraints that were formerly suggested by an extends hierarchy. *)
Definition well_formed_object_state (object : Object_state) : Prop :=
  match object.(object_methods_).(object_construct_) with
  | Some _ => object_state_is_callable object
  | None => True
  end.

Lemma constructor_is_callable :
  forall object,
    object_state_is_constructor object ->
    object_state_is_callable object.
Proof.
  intros object [callable _].
  exact callable.
Qed.

(* ------------------------------------------------------------------------- *)
(* Dynamically shaped specification records (not ECMAScript objects)         *)
(* ------------------------------------------------------------------------- *)

Definition Fields := list (FieldName * IRValue).

Fixpoint lookup_field (name : FieldName) (fields : Fields)
    : option IRValue :=
  match fields with
  | [] => None
  | (current, value) :: tail =>
      if String.eqb name current then Some value else lookup_field name tail
  end.

Fixpoint set_field (name : FieldName) (value : IRValue) (fields : Fields)
    : Fields :=
  match fields with
  | [] => [(name, value)]
  | (current, old_value) :: tail =>
      if String.eqb name current
      then (current, value) :: tail
      else (current, old_value) :: set_field name value tail
  end.

Record SpecRecord : Type := {
  spec_record_name : TypeName;
  spec_record_fields : Fields;
}.

Definition spec_record_lookup (record : SpecRecord) (name : FieldName)
    : option IRValue :=
  lookup_field name record.(spec_record_fields).

Definition spec_record_set
    (record : SpecRecord)
    (name : FieldName)
    (value : IRValue)
    : SpecRecord :=
  {|
    spec_record_name := record.(spec_record_name);
    spec_record_fields := set_field name value record.(spec_record_fields);
  |}.

Definition well_formed_fields (fields : Fields) : Prop :=
  NoDup (map fst fields).

Definition well_formed_spec_record (record : SpecRecord) : Prop :=
  well_formed_fields record.(spec_record_fields).

(* ------------------------------------------------------------------------- *)
(* Heaps                                                                      *)
(* ------------------------------------------------------------------------- *)

Definition Object_heap := list (loc * Object_state).

Fixpoint lookup_object_state (ptr : loc) (heap : Object_heap)
    : option Object_state :=
  match heap with
  | [] => None
  | (current, object) :: tail =>
      if loc_eqb ptr current then Some object else lookup_object_state ptr tail
  end.

Fixpoint modify_object_heap
    (ptr : loc)
    (new_object : Object_state)
    (heap : Object_heap)
    : Object_heap :=
  match heap with
  | [] => []
  | (current, object) :: tail =>
      if loc_eqb ptr current
      then (current, new_object) :: tail
      else (current, object) :: modify_object_heap ptr new_object tail
  end.

Definition lookup_object (object : Object) (heap : Object_heap)
    : option Object_state :=
  let '(obj ptr) := object in lookup_object_state ptr heap.

Definition is_Object (heap : Object_heap) (object : Object) : Prop :=
  exists state, lookup_object object heap = Some state.

Definition is_FunctionObject (heap : Object_heap) (object : Object) : Prop :=
  exists state,
    lookup_object object heap = Some state /\
    object_state_is_callable state.

Definition is_Constructor (heap : Object_heap) (object : Object) : Prop :=
  exists state,
    lookup_object object heap = Some state /\
    object_state_is_constructor state.

Lemma Constructor_is_FunctionObject :
  forall heap object,
    is_Constructor heap object ->
    is_FunctionObject heap object.
Proof.
  intros heap object [state [lookup constructor]].
  exists state.
  split.
  - exact lookup.
  - apply constructor_is_callable.
    exact constructor.
Qed.

(* Non-object ESMeta heap values remain in a separate, tagged heap. *)
Inductive HeapCell : Type :=
  | HC_Record (record : SpecRecord)
  | HC_Map (entries : list (IRValue * IRValue))
  | HC_List (values : list IRValue).

Definition Heap := list (loc * HeapCell).

Fixpoint lookup_cell (ptr : loc) (heap : Heap) : option HeapCell :=
  match heap with
  | [] => None
  | (current, cell) :: tail =>
      if loc_eqb ptr current then Some cell else lookup_cell ptr tail
  end.

Fixpoint modify_heap (ptr : loc) (new_cell : HeapCell) (heap : Heap) : Heap :=
  match heap with
  | [] => []
  | (current, cell) :: tail =>
      if loc_eqb ptr current
      then (current, new_cell) :: tail
      else (current, cell) :: modify_heap ptr new_cell tail
  end.

(* ------------------------------------------------------------------------- *)
(* State computations                                                        *)
(* ------------------------------------------------------------------------- *)

(* The global environment implements State.globals and contains bindings such
 * as REALM, EXECUTION_STACK, and INTRINSICS.  Generated Rocq functions bind
 * parameters and algorithm-local values directly instead of storing them in
 * the machine state. *)
Definition GlobalEnv := list (IRGlobal * IRValue).

Fixpoint lookup_global_env (global : IRGlobal) (environment : GlobalEnv)
    : option IRValue :=
  match environment with
  | [] => None
  | (current, value) :: tail =>
      if IRGlobal_eqb global current
      then Some value
      else lookup_global_env global tail
  end.

Fixpoint set_global_env
    (global : IRGlobal)
    (value : IRValue)
    (environment : GlobalEnv)
    : GlobalEnv :=
  match environment with
  | [] => [(global, value)]
  | (current, old_value) :: tail =>
      if IRGlobal_eqb global current
      then (current, value) :: tail
      else (current, old_value) :: set_global_env global value tail
  end.

Definition well_formed_global_env (environment : GlobalEnv) : Prop :=
  NoDup (map fst environment).

Record State : Type := {
  state_globals : GlobalEnv;
  state_object_heap : Object_heap;
  state_heap : Heap;
  state_next_loc : nat;
}.

Definition initial_state (globals : GlobalEnv) : State :=
  {|
    state_globals := globals;
    state_object_heap := [];
    state_heap := [];
    state_next_loc := O;
  |}.

Inductive Exec_Result (A : Type) : Type :=
  | RESULT (state : State) (value : A)
  | OUT_OF_FUEL
  | FAIL.

Arguments RESULT {A} _ _.
Arguments OUT_OF_FUEL {A}.
Arguments FAIL {A}.

(* This is the result of a stateful Rocq computation, not an ECMAScript
 * Completion Record.  Completion Records are ordinary heap-allocated
 * SpecRecords and are returned through the [A] value of [RESULT]. *)
Definition State_Completion (A : Type) : Type :=
  State -> Exec_Result A.

Definition state_return {A : Type} (value : A) : State_Completion A :=
  fun state => RESULT state value.

Definition out_of_fuel {A : Type} : State_Completion A :=
  fun _ => OUT_OF_FUEL.

Definition get_globals : State_Completion GlobalEnv :=
  fun state => RESULT state state.(state_globals).

Definition set_globals (environment : GlobalEnv) : State_Completion unit :=
  fun state =>
    RESULT
      {|
        state_globals := environment;
        state_object_heap := state.(state_object_heap);
        state_heap := state.(state_heap);
        state_next_loc := state.(state_next_loc);
      |}
      tt.

Definition read_global (global : IRGlobal) : State_Completion IRValue :=
  fun state =>
    match lookup_global_env global state.(state_globals) with
    | Some value => RESULT state value
    | None => FAIL
    end.

Definition write_global
    (global : IRGlobal)
    (value : IRValue)
    : State_Completion unit :=
  fun state =>
    RESULT
      {|
        state_globals := set_global_env global value state.(state_globals);
        state_object_heap := state.(state_object_heap);
        state_heap := state.(state_heap);
        state_next_loc := state.(state_next_loc);
      |}
      tt.

Definition global_exists (global : IRGlobal) : State_Completion bool :=
  fun state =>
    let value := lookup_global_env global state.(state_globals) in
    RESULT state
      match value with
      | Some _ => true
      | None => false
      end.

Definition state_bind {A B : Type}
    (computation : State_Completion A)
    (continuation : A -> State_Completion B)
    : State_Completion B :=
  fun state =>
    match computation state with
    | RESULT next value => continuation value next
    | OUT_OF_FUEL => OUT_OF_FUEL
    | FAIL => FAIL
    end.

Notation "'let*' result ':=' computation 'in' continuation" :=
  (state_bind computation (fun result => continuation))
  (at level 50, left associativity).

Definition get_heap : State_Completion Heap :=
  fun state => RESULT state state.(state_heap).

Definition get_object_heap : State_Completion Object_heap :=
  fun state => RESULT state state.(state_object_heap).

Definition set_heap (heap : Heap) : State_Completion unit :=
  fun state =>
    RESULT
      {|
        state_globals := state.(state_globals);
        state_object_heap := state.(state_object_heap);
        state_heap := heap;
        state_next_loc := state.(state_next_loc);
      |}
      tt.

Definition set_object_heap (heap : Object_heap) : State_Completion unit :=
  fun state =>
    RESULT
      {|
        state_globals := state.(state_globals);
        state_object_heap := heap;
        state_heap := state.(state_heap);
        state_next_loc := state.(state_next_loc);
      |}
      tt.

Definition allocate_object (object : Object_state) : State_Completion Object :=
  fun state =>
    let index := state.(state_next_loc) in
    let ptr := dynamic_loc index in
    let heap := (ptr, object) :: state.(state_object_heap) in
    RESULT
      {|
        state_globals := state.(state_globals);
        state_object_heap := heap;
        state_heap := state.(state_heap);
        state_next_loc := S index;
      |}
      (obj ptr).

Definition allocate_cell (cell : HeapCell) : State_Completion loc :=
  fun state =>
    let index := state.(state_next_loc) in
    let ptr := dynamic_loc index in
    let heap := (ptr, cell) :: state.(state_heap) in
    RESULT
      {|
        state_globals := state.(state_globals);
        state_object_heap := state.(state_object_heap);
        state_heap := heap;
        state_next_loc := S index;
      |}
      ptr.

(* ESMeta represents records, including Completion Records and Property
 * Descriptors, as addresses into the IR heap.  Neither has a dedicated
 * IRValue constructor here. *)
Definition allocate_record
    (name : TypeName)
    (fields : Fields)
    : State_Completion IRValue :=
  state_bind
    (allocate_cell
      (HC_Record {|
        spec_record_name := name;
        spec_record_fields := fields;
      |}))
    (fun address => state_return (IR_Address address)).

Definition read_record_field
    (base : IRValue)
    (name : FieldName)
    : State_Completion IRValue :=
  fun state =>
    match base with
    | IR_Address address =>
        match lookup_cell address state.(state_heap) with
        | Some (HC_Record record) =>
            match spec_record_lookup record name with
            | Some value => RESULT state value
            | None => FAIL
            end
        | _ => FAIL
        end
    | _ => FAIL
    end.

(* Field absence is different from a field whose value is IR_undefined.
 * This corresponds to ESMeta's `exists record.Field` expression. *)
Definition record_field_exists
    (base : IRValue)
    (name : FieldName)
    : State_Completion bool :=
  fun state =>
    match base with
    | IR_Address address =>
        match lookup_cell address state.(state_heap) with
        | Some (HC_Record record) =>
            match spec_record_lookup record name with
            | Some _ => RESULT state true
            | None => RESULT state false
            end
        | _ => RESULT state false
        end
    | _ => RESULT state false
    end.

Definition write_record_field
    (base : IRValue)
    (name : FieldName)
    (value : IRValue)
    : State_Completion unit :=
  fun state =>
    match base with
    | IR_Address address =>
        match lookup_cell address state.(state_heap) with
        | Some (HC_Record record) =>
            RESULT
              {|
                state_globals := state.(state_globals);
                state_object_heap := state.(state_object_heap);
                state_heap :=
                  modify_heap
                    address
                    (HC_Record (spec_record_set record name value))
                    state.(state_heap);
                state_next_loc := state.(state_next_loc);
              |}
              tt
        | _ => FAIL
        end
    | _ => FAIL
    end.

Definition record_has_type
    (base : IRValue)
    (name : TypeName)
    : State_Completion bool :=
  fun state =>
    match base with
    | IR_Address address =>
        match lookup_cell address state.(state_heap) with
        | Some (HC_Record record) =>
            RESULT state (String.eqb record.(spec_record_name) name)
        | _ => RESULT state false
        end
    | _ => RESULT state false
    end.
