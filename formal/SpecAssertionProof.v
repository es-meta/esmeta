(** A first ECMA-262 algorithm-assertion proof using preserved function
    annotations.

    ECMA-262 Completion(completionRecord: Completion): Completion begins:
      Assert: completionRecord is a Completion Record.

    [sf_41] is generated from that algorithm.  The first theorem checks that
    the signature and its executable type interpretation survived export.
    The second proves that every runtime argument satisfying the exported
    parameter type makes the actual assertion expression evaluate to true. *)

From Stdlib Require Import String List ZArith Lia FunctionalExtensionality.
From ESMetaFV Require Import Fragment Domain Exec Spec.

Import ListNotations.
Local Open Scope string_scope.

Definition completion_parameter_type : type_annotation :=
  mkTypeAnnotation "Completion" (Some TCompletion).

Example completion_signature_is_preserved :
  f_kind sf_41 = Some FKAbsOp /\
  f_params sf_41 = ["completionRecord"] /\
  f_param_annotations sf_41 =
    [mkParamAnnotation completion_parameter_type false] /\
  f_return_annotation sf_41 = completion_parameter_type /\
  func_annotations_aligned sf_41 = true.
Proof. vm_compute. repeat split; reflexivity. Qed.

Definition completion_assertion : expr :=
  ETypeCheck
    (ERef (RVar (VLocal (LName "completionRecord"))))
    TCompletion.

Definition completion_return : expr :=
  ERef (RVar (VLocal (LName "completionRecord"))).

Definition completion_body : inst :=
  ISeq [IAssert completion_assertion; IReturn completion_return].

(** This guard ties the proof below to the generated ECMA algorithm rather
    than merely to a hand-written copy of its assertion.  If export order or
    the generated body changes, this proof must be revisited. *)
Example completion_body_is_exported :
  f_name sf_41 = "Completion" /\
  f_body sf_41 = completion_body.
Proof. vm_compute. split; reflexivity. Qed.

Definition value_satisfies_type
  (st : xstate) (t : tyexp) (v : val) : Prop :=
  run_heap_query_x st (ty_check_query type_check_fuel t v) =
    Ok (Some true).

Theorem completion_assertion_follows_from_parameter_type :
  forall (st : xstate) (completionRecord : val),
    value_satisfies_type st TCompletion completionRecord ->
    exec_expr st [(LName "completionRecord", completionRecord)]
      completion_assertion =
    Ok (st, VBool true).
Proof.
  intros st completionRecord Htyped.
  unfold value_satisfies_type in Htyped.
  cbn [completion_assertion exec_expr exec_ref read_target_x].
  cbn [obind env_lookup local_eqb].
  cbn [read_target_x env_lookup].
  rewrite local_eqb_refl.
  cbn [obind of_option].
  rewrite Htyped.
  reflexivity.
Qed.

(** The actual generated body neither gets stuck at the assertion nor merely
    carries a return annotation: it returns the same typed value. *)
Theorem completion_body_returns_typed_argument :
  forall (p : prog) (st : xstate) (completionRecord : val),
    value_satisfies_type st TCompletion completionRecord ->
    exec_inst 2 p st [(LName "completionRecord", completionRecord)]
      (f_body sf_41) =
      Ok (st, [(LName "completionRecord", completionRecord)],
        CReturn completionRecord) /\
    value_satisfies_type st TCompletion completionRecord.
Proof.
  intros p st completionRecord Htyped.
  split; [| exact Htyped].
  rewrite (proj2 completion_body_is_exported).
  cbn [completion_body exec_inst].
  rewrite
    (completion_assertion_follows_from_parameter_type
      st completionRecord Htyped).
  vm_compute. reflexivity.
Qed.

(** ** Two more ECMA-262 assertions *)

(** A small structural path language used only to establish that each proof
    targets an instruction occurring in the generated [Spec.v]. *)
Definition inst_child (n : nat) (i : inst) : option inst :=
  match i with
  | ISeq xs => nth_error xs n
  | IIf _ thn els =>
      match n with 0 => Some thn | 1 => Some els | _ => None end
  | IWhile _ body => match n with 0 => Some body | _ => None end
  | _ => None
  end.

Fixpoint inst_at (path : list nat) (i : inst) : option inst :=
  match path with
  | [] => Some i
  | n :: rest =>
      match inst_child n i with
      | Some child => inst_at rest child
      | None => None
      end
  end.

(** The two live record-refinement assertions found during the exporter
    audit are pinned to their actual generated algorithms, rather than only
    to standalone checker examples. *)
Definition get_function_realm_proxy_assertion : inst :=
  IAssert
    (ETypeCheck
      (ERef (RVar (VLocal (LName "proxyTarget"))))
      (TRecordFields "Object" ["Call"])).

Example get_function_realm_proxy_assertion_is_exported :
  f_name sf_176 = "GetFunctionRealm" /\
  inst_at [2; 0; 4] (f_body sf_176) =
    Some get_function_realm_proxy_assertion.
Proof. vm_compute. split; reflexivity. Qed.

Definition super_call_new_target_assertion : inst :=
  IAssert
    (ETypeCheck
      (ERef (RVar (VLocal (LName "newTarget"))))
      (TRecordFields "Object" ["Call"; "Construct"])).

Example super_call_new_target_assertion_is_exported :
  f_name sf_1319 = "SuperCall[0,0].Evaluation" /\
  inst_at [2] (f_body sf_1319) = Some super_call_new_target_assertion.
Proof. vm_compute. split; reflexivity. Qed.

(** ToPrimitive, ECMA-262 sec-toprimitive, steps 1.c.i--iii.  When an
    optional preferred type is present, its exported finite-enum type says
    it is either ~number~ or ~string~.  The string branch handles the latter;
    reaching the else branch therefore makes the assertion of ~number~ safe. *)
Definition tp_pref_ref : ref :=
  RVar (VLocal (LName "preferredType")).

Definition tp_pref : expr := ERef tp_pref_ref.

Definition tp_hint_selection : inst :=
  IIf (EUnary UNot (EExists tp_pref_ref))
    (ISeq [ILet "hint" (EStr (cu "default"))])
    (ISeq [IIf (EBinary BEq tp_pref (EEnum "string"))
      (ISeq [ILet "hint" (EStr (cu "string"))])
      (ISeq [IAssert (EBinary BEq tp_pref (EEnum "number"));
             ILet "hint" (EStr (cu "number"))])]).

Example to_primitive_hint_selection_is_exported :
  f_name sf_103 = "ToPrimitive" /\
  inst_at [0; 0; 4; 0; 0] (f_body sf_103) = Some tp_hint_selection.
Proof. vm_compute. split; reflexivity. Qed.

Example to_primitive_preferred_type_annotation_is_exported :
  nth_error (f_param_annotations sf_103) 1 =
    Some
      (mkParamAnnotation
        (mkTypeAnnotation "Enum[~number~, ~string~]"
          (Some (TEnumNames ["number"; "string"])))
        true).
Proof. vm_compute. reflexivity. Qed.

Lemma name_mem_two (x a b : string) :
  name_mem x [a; b] = true -> x = a \/ x = b.
Proof.
  cbn [name_mem].
  destruct (String.eqb x a) eqn:Ha.
  - intros Hmem. left. now apply String.eqb_eq.
  - destruct (String.eqb x b) eqn:Hb; try discriminate.
    intros Hmem. right. now apply String.eqb_eq.
Qed.

Theorem to_primitive_preferred_type_assertion_safe :
  forall (st : xstate) (preferredType : val),
    value_satisfies_type st
      (TEnumNames ["number"; "string"]) preferredType ->
    exists rho',
      exec_inst 6 (mkProg []) st
        [(LName "preferredType", preferredType)]
        tp_hint_selection = Ok (st, rho', CNormal VUndef).
Proof.
  intros st preferredType Htype.
  unfold value_satisfies_type in Htype.
  destruct preferredType;
    cbn [run_heap_query_x run_heap_query_cached_x
      type_check_fuel ty_check_query
      ty_check_prim] in Htype;
    try discriminate.
  injection Htype as Htype.
  apply name_mem_two in Htype as [-> | ->].
  - eexists. vm_compute. reflexivity.
  - eexists. vm_compute. reflexivity.
Qed.

Theorem to_primitive_absent_preferred_type_skips_assertion :
  forall (st : xstate),
    exists rho',
      exec_inst 6 (mkProg []) st [] tp_hint_selection =
        Ok (st, rho', CNormal VUndef).
Proof. intro st. eexists. vm_compute. reflexivity. Qed.

(** StringLastIndexOf has a relational precondition which its three unary
    parameter annotations cannot express.  We retain the first three actual
    exported instructions so the counterexample exercises the generated
    length computations and [IAssert], not a copied Boolean formula.  This
    refutes safety from the function signature alone; it does not refute the
    assertion under the stronger invariants required at specification call
    sites. *)
Definition seq_prefix (n : nat) (i : inst) : option inst :=
  match i with
  | ISeq xs => Some (ISeq (firstn n xs))
  | _ => None
  end.

Definition local_ref_expr (name : string) : expr :=
  ERef (RVar (VLocal (LName name))).

Definition string_last_index_bound_test : expr :=
  EBinary BLt
    (local_ref_expr "len")
    (EBinary BAdd
      (local_ref_expr "fromIndex")
      (local_ref_expr "searchLen")).

Definition string_last_index_assertion : expr :=
  EUnary UNot string_last_index_bound_test.

Definition string_last_index_prefix : inst :=
  ISeq [
    ILet "len"
      (ESizeOf (ERef (RVar (VLocal (LName "string")))));
    ILet "searchLen"
      (ESizeOf (ERef (RVar (VLocal (LName "searchValue")))));
    IAssert string_last_index_assertion
  ].

Example string_last_index_prefix_is_exported :
  f_name sf_43 = "StringLastIndexOf" /\
  seq_prefix 3 (f_body sf_43) = Some string_last_index_prefix.
Proof. vm_compute. split; reflexivity. Qed.

Lemma exec_local_named_ref
  (st : xstate) (rho : env) (name : string) (v : val) :
  env_lookup rho (LName name) = Some v ->
  exec_expr st rho (local_ref_expr name) = Ok (st, v).
Proof.
  intros Hlookup.
  unfold local_ref_expr.
  cbn [exec_expr exec_ref obind read_target_x].
  rewrite Hlookup. reflexivity.
Qed.

Lemma exec_string_last_index_bound_test :
  forall (st : xstate) (rho : env) (len fromIndex searchLen : Z),
    env_lookup rho (LName "len") = Some (VMath len) ->
    env_lookup rho (LName "fromIndex") = Some (VMath fromIndex) ->
    env_lookup rho (LName "searchLen") = Some (VMath searchLen) ->
    exec_expr st rho string_last_index_bound_test =
      Ok (st, VBool (Z.ltb len (fromIndex + searchLen))).
Proof.
  intros st rho len fromIndex searchLen Hlen Hfrom Hsearch.
  unfold string_last_index_bound_test.
  cbn [exec_expr].
  rewrite (exec_local_named_ref st rho "len" (VMath len) Hlen).
  cbn [obind].
  rewrite
    (exec_local_named_ref st rho "fromIndex" (VMath fromIndex) Hfrom).
  cbn [obind].
  rewrite
    (exec_local_named_ref st rho "searchLen" (VMath searchLen) Hsearch).
  cbn [obind host_bop_query eval_bop of_option].
  reflexivity.
Qed.

Theorem string_last_index_assertion_safe_under_call_bound :
  forall (st : xstate) (rho : env) (len fromIndex searchLen : Z),
    env_lookup rho (LName "len") = Some (VMath len) ->
    env_lookup rho (LName "fromIndex") = Some (VMath fromIndex) ->
    env_lookup rho (LName "searchLen") = Some (VMath searchLen) ->
    (fromIndex + searchLen <= len)%Z ->
    exec_expr st rho string_last_index_assertion = Ok (st, VBool true).
Proof.
  intros st rho len fromIndex searchLen Hlen Hfrom Hsearch Hbound.
  unfold string_last_index_assertion.
  change
    (obind (exec_expr st rho string_last_index_bound_test)
      (fun '(st1, v) =>
        obind (of_option "EUnary" (eval_uop UNot v))
          (fun result => Ok (st1, result))) =
     Ok (st, VBool true)).
  rewrite
    (exec_string_last_index_bound_test
      st rho len fromIndex searchLen Hlen Hfrom Hsearch).
  cbn [obind of_option eval_uop].
  apply Z.ltb_ge in Hbound.
  rewrite Hbound. reflexivity.
Qed.

Example string_last_index_signature_is_exported :
  f_params sf_43 = ["string"; "searchValue"; "fromIndex"] /\
  f_param_annotations sf_43 =
    [mkParamAnnotation (mkTypeAnnotation "String" (Some TStrTy)) false;
     mkParamAnnotation (mkTypeAnnotation "String" (Some TStrTy)) false;
     mkParamAnnotation
       (mkTypeAnnotation "Int[0+]"
         (Some (TMathInt false true true))) false].
Proof. vm_compute. split; reflexivity. Qed.

Theorem string_last_index_signature_counterexample :
  forall (st : xstate),
    exec_inst 5 (mkProg []) st
      [(LName "string", VStr (cu ""));
       (LName "searchValue", VStr (cu "a"));
       (LName "fromIndex", VMath 0)]
      string_last_index_prefix = Stuck "IAssert(false)".
Proof. intro st. vm_compute. reflexivity. Qed.

Theorem string_last_index_counterexample_is_well_typed :
  forall (st : xstate),
    value_satisfies_type st TStrTy (VStr (cu "")) /\
    value_satisfies_type st TStrTy (VStr (cu "a")) /\
    value_satisfies_type st
      (TMathInt false true true) (VMath 0).
Proof. intro st. vm_compute. auto. Qed.

(** ** A path-sensitive assertion proof for [ToString]

    ECMA-262's [ToString] first eliminates String, Symbol, undefined, null,
    both Boolean values, Number, and BigInt.  Only then does it assert that
    its ESValue argument is an Object.  Unlike the small proofs above, the
    theorem below derives the asserted Object type from the exported
    parameter type together with operational evidence that all eight actual
    generated branches fell through. *)

Definition to_string_esvalue_type : tyexp :=
  TUnion [TRecord "Object"; TRecord "Symbol"; TNumberTy; TStrTy;
          TBoolTy; TBigIntTy; TUndefTy; TNullTy].

Definition to_string_argument_ref : expr :=
  ERef (RVar (VLocal (LName "argument"))).

Definition to_string_object_test : expr :=
  ETypeCheck to_string_argument_ref (TRecord "Object").

Definition to_string_object_assertion : inst :=
  IAssert to_string_object_test.

Definition to_string_instructions : list inst :=
  match f_body sf_135 with
  | ISeq instructions => instructions
  | _ => []
  end.

(** This is a control-flow predicate, not an assertion assumption: at each
    generated [IIf] site it records evaluation of that site's condition to
    [false].  The condition and both branches are recovered from [sf_135]
    itself, so a changed export invalidates the extraction lemmas below. *)
Definition falls_through_generated_if
  (st : xstate) (rho : env) (index : nat) : Prop :=
  exists condition then_branch else_branch,
    nth_error to_string_instructions index =
      Some (IIf condition then_branch else_branch) /\
    exec_expr st rho condition = Ok (st, VBool false).

Definition reaches_to_string_object_assertion
  (st : xstate) (rho : env) : Prop :=
  forall index, (index < 8)%nat ->
    falls_through_generated_if st rho index.

Example to_string_signature_and_assertion_are_exported :
  f_name sf_135 = "ToString" /\
  f_params sf_135 = ["argument"] /\
  f_param_annotations sf_135 =
    [mkParamAnnotation
      (mkTypeAnnotation "ESValue" (Some to_string_esvalue_type)) false] /\
  nth_error to_string_instructions 8 = Some to_string_object_assertion.
Proof. vm_compute. repeat split; reflexivity. Qed.

Ltac extract_generated_fallthrough Hreach index Hresult :=
  let H := fresh "Hgenerated" in
  pose proof (Hreach index ltac:(lia)) as H;
  unfold falls_through_generated_if, to_string_instructions in H;
  cbv [sf_135] in H;
  destruct H as
    (?condition & ?then_branch & ?else_branch & ?Hshape & Hresult);
  injection Hshape as <- <- <-.

Lemma to_string_string_case_fell_through st rho :
  reaches_to_string_object_assertion st rho ->
  exec_expr st rho (ETypeCheck to_string_argument_ref TStrTy) =
    Ok (st, VBool false).
Proof.
  intro Hreach.
  extract_generated_fallthrough Hreach 0 Hexec.
  exact Hexec.
Qed.

Lemma to_string_symbol_case_fell_through st rho :
  reaches_to_string_object_assertion st rho ->
  exec_expr st rho
    (ETypeCheck to_string_argument_ref (TRecord "Symbol")) =
    Ok (st, VBool false).
Proof.
  intro Hreach.
  extract_generated_fallthrough Hreach 1 Hexec.
  exact Hexec.
Qed.

Lemma to_string_undefined_case_fell_through st rho :
  reaches_to_string_object_assertion st rho ->
  exec_expr st rho (EBinary BEq to_string_argument_ref EUndef) =
    Ok (st, VBool false).
Proof.
  intro Hreach.
  extract_generated_fallthrough Hreach 2 Hexec.
  exact Hexec.
Qed.

Lemma to_string_null_case_fell_through st rho :
  reaches_to_string_object_assertion st rho ->
  exec_expr st rho (EBinary BEq to_string_argument_ref ENull) =
    Ok (st, VBool false).
Proof.
  intro Hreach.
  extract_generated_fallthrough Hreach 3 Hexec.
  exact Hexec.
Qed.

Lemma to_string_true_case_fell_through st rho :
  reaches_to_string_object_assertion st rho ->
  exec_expr st rho
    (EBinary BEq to_string_argument_ref (EBool true)) =
    Ok (st, VBool false).
Proof.
  intro Hreach.
  extract_generated_fallthrough Hreach 4 Hexec.
  exact Hexec.
Qed.

Lemma to_string_false_case_fell_through st rho :
  reaches_to_string_object_assertion st rho ->
  exec_expr st rho
    (EBinary BEq to_string_argument_ref (EBool false)) =
    Ok (st, VBool false).
Proof.
  intro Hreach.
  extract_generated_fallthrough Hreach 5 Hexec.
  exact Hexec.
Qed.

Lemma to_string_number_case_fell_through st rho :
  reaches_to_string_object_assertion st rho ->
  exec_expr st rho (ETypeCheck to_string_argument_ref TNumberTy) =
    Ok (st, VBool false).
Proof.
  intro Hreach.
  extract_generated_fallthrough Hreach 6 Hexec.
  exact Hexec.
Qed.

Lemma to_string_bigint_case_fell_through st rho :
  reaches_to_string_object_assertion st rho ->
  exec_expr st rho (ETypeCheck to_string_argument_ref TBigIntTy) =
    Ok (st, VBool false).
Proof.
  intro Hreach.
  extract_generated_fallthrough Hreach 7 Hexec.
  exact Hexec.
Qed.

(** The two record targets used by ESValue are roots in the generated record
    hierarchy.  Their checks therefore reduce to one heap read followed by
    a nominal subtype decision; spelling that out avoids normalizing the
    entire generated record-refinement table. *)
Lemma to_string_refinement_object_shape :
  forall fuel stored,
    record_refinement_bindings fuel stored "Object" =
    if TyModel.record_subtype stored "Object" then Some [] else None.
Proof.
  intros [|fuel] stored;
    cbn [record_refinement_bindings TyModel.record_parent];
    destruct (TyModel.record_subtype stored "Object"); reflexivity.
Qed.

Lemma to_string_refinement_symbol_shape :
  forall fuel stored,
    record_refinement_bindings fuel stored "Symbol" =
    if TyModel.record_subtype stored "Symbol" then Some [] else None.
Proof.
  intros [|fuel] stored;
    cbn [record_refinement_bindings TyModel.record_parent];
    destruct (TyModel.record_subtype stored "Symbol"); reflexivity.
Qed.

Lemma to_string_object_constraint_query_shape :
  forall fuel address,
    record_constraint_query (S fuel)
      (TyModel.RFCRecord [("Object", [])]) (VAddr address) =
    HeapRead address (fun object =>
      match object with
      | ORecord stored _ =>
          HeapDone (Some (TyModel.record_subtype stored "Object"))
      | _ => HeapDone (Some false)
      end).
Proof.
  intros fuel address.
  cbn [record_constraint_query].
  f_equal. apply functional_extensionality.
  intros [values | stored fields | entries]; try reflexivity.
  destruct (String.eqb stored "Object") eqn:Heq.
  - apply String.eqb_eq in Heq. subst stored. vm_compute. reflexivity.
  - destruct (TyModel.record_subtype stored "Object") eqn:Hsub.
    + reflexivity.
    + rewrite to_string_refinement_object_shape, Hsub. reflexivity.
Qed.

Lemma to_string_symbol_constraint_query_shape :
  forall fuel address,
    record_constraint_query (S fuel)
      (TyModel.RFCRecord [("Symbol", [])]) (VAddr address) =
    HeapRead address (fun object =>
      match object with
      | ORecord stored _ =>
          HeapDone (Some (TyModel.record_subtype stored "Symbol"))
      | _ => HeapDone (Some false)
      end).
Proof.
  intros fuel address.
  cbn [record_constraint_query].
  f_equal. apply functional_extensionality.
  intros [values | stored fields | entries]; try reflexivity.
  destruct (String.eqb stored "Symbol") eqn:Heq.
  - apply String.eqb_eq in Heq. subst stored. vm_compute. reflexivity.
  - destruct (TyModel.record_subtype stored "Symbol") eqn:Hsub.
    + reflexivity.
    + rewrite to_string_refinement_symbol_shape, Hsub. reflexivity.
Qed.

Lemma to_string_esvalue_address_query_shape :
  forall st address,
    run_heap_query_x st
      (ty_check_query type_check_fuel
        to_string_esvalue_type (VAddr address)) =
    match heap_get st address with
    | Some (ORecord stored _) =>
        Ok (Some
          (orb (TyModel.record_subtype stored "Object")
               (TyModel.record_subtype stored "Symbol")))
    | Some _ => Ok (Some false)
    | None => Stuck "ETypeCheck(heap)"
    end.
Proof.
  intros st address.
  change
    (run_heap_query_x st
      (heap_query_decision_or
        (record_constraint_query 126
          (TyModel.RFCRecord [("Object", [])]) (VAddr address))
        (heap_query_decision_or
          (record_constraint_query 126
            (TyModel.RFCRecord [("Symbol", [])]) (VAddr address))
          (HeapDone (Some false)))) =
     match heap_get st address with
     | Some (ORecord stored _) =>
         Ok (Some
           (orb (TyModel.record_subtype stored "Object")
                (TyModel.record_subtype stored "Symbol")))
     | Some _ => Ok (Some false)
     | None => Stuck "ETypeCheck(heap)"
     end).
  rewrite (to_string_object_constraint_query_shape 125 address).
  rewrite (to_string_symbol_constraint_query_shape 125 address).
  unfold run_heap_query_x.
  cbn [heap_query_decision_or heap_query_bind resolved_lookup].
  destruct (heap_get st address)
    as [[values | stored fields | entries] |] eqn:Hheap;
    cbn [run_heap_query_cached_x heap_query_bind resolved_lookup];
    rewrite ?Hheap;
    cbn [run_heap_query_cached_x heap_query_bind resolved_lookup
      obind of_option decision_or];
    try rewrite Nat.eqb_refl;
    cbn [run_heap_query_cached_x heap_query_bind resolved_lookup
      obind of_option decision_or].
  - reflexivity.
  - destruct (TyModel.record_subtype stored "Object") eqn:Hobject.
    + reflexivity.
    + cbn [run_heap_query_cached_x resolved_lookup].
      rewrite Nat.eqb_refl.
      cbn [heap_query_bind run_heap_query_cached_x].
      destruct (TyModel.record_subtype stored "Symbol"); reflexivity.
  - reflexivity.
  - reflexivity.
Qed.

Lemma to_string_object_address_query_shape :
  forall st address,
    run_heap_query_x st
      (ty_check_query type_check_fuel
        (TRecord "Object") (VAddr address)) =
    match heap_get st address with
    | Some (ORecord stored _) =>
        Ok (Some (TyModel.record_subtype stored "Object"))
    | Some _ => Ok (Some false)
    | None => Stuck "ETypeCheck(heap)"
    end.
Proof.
  intros st address.
  change
    (run_heap_query_x st
      (record_constraint_query 127
        (TyModel.RFCRecord [("Object", [])]) (VAddr address)) =
     match heap_get st address with
     | Some (ORecord stored _) =>
         Ok (Some (TyModel.record_subtype stored "Object"))
     | Some _ => Ok (Some false)
     | None => Stuck "ETypeCheck(heap)"
     end).
  rewrite (to_string_object_constraint_query_shape 126 address).
  unfold run_heap_query_x.
  cbn [run_heap_query_cached_x resolved_lookup].
  destruct (heap_get st address)
    as [[values | stored fields | entries] |] eqn:Hheap;
    rewrite ?Hheap; reflexivity.
Qed.

Lemma to_string_symbol_address_query_shape :
  forall st address,
    run_heap_query_x st
      (ty_check_query type_check_fuel
        (TRecord "Symbol") (VAddr address)) =
    match heap_get st address with
    | Some (ORecord stored _) =>
        Ok (Some (TyModel.record_subtype stored "Symbol"))
    | Some _ => Ok (Some false)
    | None => Stuck "ETypeCheck(heap)"
    end.
Proof.
  intros st address.
  change
    (run_heap_query_x st
      (record_constraint_query 127
        (TyModel.RFCRecord [("Symbol", [])]) (VAddr address)) =
     match heap_get st address with
     | Some (ORecord stored _) =>
         Ok (Some (TyModel.record_subtype stored "Symbol"))
     | Some _ => Ok (Some false)
     | None => Stuck "ETypeCheck(heap)"
     end).
  rewrite (to_string_symbol_constraint_query_shape 126 address).
  unfold run_heap_query_x.
  cbn [run_heap_query_cached_x resolved_lookup].
  destruct (heap_get st address)
    as [[values | stored fields | entries] |] eqn:Hheap;
    rewrite ?Hheap; reflexivity.
Qed.

Definition to_string_nonaddress (value : val) : Prop :=
  match value with
  | VAddr _ => False
  | _ => True
  end.

Definition to_string_esvalue_nonaddress_ok (value : val) : bool :=
  match value with
  | VNumber _ | VBigInt _ | VStr _ | VBool _ | VUndef | VNull => true
  | _ => false
  end.

(** Non-address ESValue classification is entirely primitive.  Keeping this
    reduction in one small opaque lemma prevents the main path proof from
    carrying eight copies of the type-checker's computation term. *)
Lemma to_string_esvalue_nonaddress_query_shape :
  forall st value,
    to_string_nonaddress value ->
    run_heap_query_x st
      (ty_check_query type_check_fuel to_string_esvalue_type value) =
    Ok (Some (to_string_esvalue_nonaddress_ok value)).
Proof.
  intros st value Hnonaddress.
  destruct value; try contradiction; reflexivity.
Qed.

Lemma to_string_typed_nonaddress_is_allowed :
  forall st value,
    to_string_nonaddress value ->
    value_satisfies_type st to_string_esvalue_type value ->
    to_string_esvalue_nonaddress_ok value = true.
Proof.
  intros st value Hnonaddress Htyped.
  unfold value_satisfies_type in Htyped.
  rewrite
    (to_string_esvalue_nonaddress_query_shape st value Hnonaddress)
    in Htyped.
  now injection Htyped.
Qed.

(** Primitive guard evaluations are kept as separate opaque facts so the
    main case split does not embed repeated computation proofs. *)
Lemma to_string_string_guard_is_true :
  forall st text,
    exec_expr st [(LName "argument", VStr text)]
      (ETypeCheck to_string_argument_ref TStrTy) =
    Ok (st, VBool true).
Proof. reflexivity. Qed.

Lemma to_string_undefined_guard_is_true :
  forall st,
    exec_expr st [(LName "argument", VUndef)]
      (EBinary BEq to_string_argument_ref EUndef) =
    Ok (st, VBool true).
Proof. reflexivity. Qed.

Lemma to_string_null_guard_is_true :
  forall st,
    exec_expr st [(LName "argument", VNull)]
      (EBinary BEq to_string_argument_ref ENull) =
    Ok (st, VBool true).
Proof. reflexivity. Qed.

Lemma to_string_true_guard_is_true :
  forall st,
    exec_expr st [(LName "argument", VBool true)]
      (EBinary BEq to_string_argument_ref (EBool true)) =
    Ok (st, VBool true).
Proof. reflexivity. Qed.

Lemma to_string_false_guard_is_true :
  forall st,
    exec_expr st [(LName "argument", VBool false)]
      (EBinary BEq to_string_argument_ref (EBool false)) =
    Ok (st, VBool true).
Proof. reflexivity. Qed.

Lemma to_string_number_guard_is_true :
  forall st number,
    exec_expr st [(LName "argument", VNumber number)]
      (ETypeCheck to_string_argument_ref TNumberTy) =
    Ok (st, VBool true).
Proof. reflexivity. Qed.

Lemma to_string_bigint_guard_is_true :
  forall st bigint,
    exec_expr st [(LName "argument", VBigInt bigint)]
      (ETypeCheck to_string_argument_ref TBigIntTy) =
    Ok (st, VBool true).
Proof. reflexivity. Qed.

Lemma to_string_esvalue_address_components :
  forall st address,
    value_satisfies_type st to_string_esvalue_type (VAddr address) ->
    exists stored fields,
      heap_get st address = Some (ORecord stored fields) /\
      orb (TyModel.record_subtype stored "Object")
          (TyModel.record_subtype stored "Symbol") = true.
Proof.
  intros st address Htyped.
  unfold value_satisfies_type in Htyped.
  rewrite to_string_esvalue_address_query_shape in Htyped.
  destruct (heap_get st address)
    as [[values | stored fields | entries] |] eqn:Hheap;
    try discriminate Htyped.
  exists stored, fields. split; [reflexivity |].
  now injection Htyped.
Qed.

(** Keep the query interpreter opaque while exposing the single outer
    [exec_expr] constructor below.  Otherwise conversion eagerly normalizes
    the 127-step record-refinement query even though the proof uses only its
    already-established result shape. *)
Local Opaque run_heap_query_x ty_check_query.

Lemma argument_typecheck_false_query :
  forall st value expected_type,
    exec_expr st [(LName "argument", value)]
      (ETypeCheck to_string_argument_ref expected_type) =
      Ok (st, VBool false) ->
    run_heap_query_x st
      (ty_check_query type_check_fuel expected_type value) =
      Ok (Some false).
Proof.
  intros st value expected_type Hexec.
  change
    (obind
      (run_heap_query_x st
        (ty_check_query type_check_fuel expected_type value))
      (fun decision =>
        obind (of_option "ETypeCheck(record-refinement)" decision)
          (fun b => Ok (st, VBool b))) =
     Ok (st, VBool false)) in Hexec.
  remember
    (run_heap_query_x st
      (ty_check_query type_check_fuel expected_type value)) as query_result
    eqn:Hresult.
  destruct query_result as [[decision |] | why |];
    cbn [obind of_option] in Hexec; try discriminate.
  destruct decision; cbn [obind of_option] in Hexec; try discriminate.
  reflexivity.
Qed.

Lemma to_string_symbol_guard_false_component :
  forall st address stored fields,
    heap_get st address = Some (ORecord stored fields) ->
    exec_expr st [(LName "argument", VAddr address)]
      (ETypeCheck to_string_argument_ref (TRecord "Symbol")) =
      Ok (st, VBool false) ->
    TyModel.record_subtype stored "Symbol" = false.
Proof.
  intros st address stored fields Hheap Hsymbol.
  pose proof
    (argument_typecheck_false_query
      st (VAddr address) (TRecord "Symbol") Hsymbol) as Hquery.
  rewrite to_string_symbol_address_query_shape, Hheap in Hquery.
  now injection Hquery.
Qed.

Lemma to_string_object_component_satisfies :
  forall st address stored fields,
    heap_get st address = Some (ORecord stored fields) ->
    TyModel.record_subtype stored "Object" = true ->
    value_satisfies_type st (TRecord "Object") (VAddr address).
Proof.
  intros st address stored fields Hheap Hobject.
  unfold value_satisfies_type.
  rewrite to_string_object_address_query_shape, Hheap, Hobject.
  reflexivity.
Qed.

Lemma orb_true_with_false_right :
  forall left right,
    orb left right = true -> right = false -> left = true.
Proof.
  intros [] [] Hor Hright; discriminate || reflexivity.
Qed.

Lemma to_string_address_fallthrough_is_object :
  forall st address,
    value_satisfies_type st to_string_esvalue_type (VAddr address) ->
    exec_expr st [(LName "argument", VAddr address)]
      (ETypeCheck to_string_argument_ref (TRecord "Symbol")) =
      Ok (st, VBool false) ->
    value_satisfies_type st (TRecord "Object") (VAddr address).
Proof.
  intros st address Htyped Hsymbol.
  destruct (to_string_esvalue_address_components st address Htyped)
    as [stored [fields [Hheap Hunion]]].
  apply (to_string_object_component_satisfies
    st address stored fields Hheap).
  apply (orb_true_with_false_right
    (TyModel.record_subtype stored "Object")
    (TyModel.record_subtype stored "Symbol") Hunion).
  exact
    (to_string_symbol_guard_false_component
      st address stored fields Hheap Hsymbol).
Qed.

Lemma to_string_nonaddress_fallthrough_impossible :
  forall st value,
    to_string_nonaddress value ->
    value_satisfies_type st to_string_esvalue_type value ->
    reaches_to_string_object_assertion st [(LName "argument", value)] ->
    False.
Proof.
  intros st value Hnonaddress Htyped Hreach.
  destruct value; try contradiction.
  - abstract (
      pose proof
        (to_string_typed_nonaddress_is_allowed st (VMath z) I Htyped)
        as Hallowed;
      discriminate Hallowed).
  - destruct b.
    + abstract (
        pose proof (to_string_true_case_fell_through _ _ Hreach) as Htrue;
        rewrite to_string_true_guard_is_true in Htrue;
        discriminate).
    + abstract (
        pose proof (to_string_false_case_fell_through _ _ Hreach) as Hfalse;
        rewrite to_string_false_guard_is_true in Hfalse;
        discriminate).
  - abstract (
      pose proof (to_string_string_case_fell_through _ _ Hreach) as Hstring;
      rewrite to_string_string_guard_is_true in Hstring;
      discriminate).
  - abstract (
      pose proof
        (to_string_undefined_case_fell_through _ _ Hreach) as Hundefined;
      rewrite to_string_undefined_guard_is_true in Hundefined;
      discriminate).
  - abstract (
      pose proof (to_string_null_case_fell_through _ _ Hreach) as Hnull;
      rewrite to_string_null_guard_is_true in Hnull;
      discriminate).
  - abstract (
      pose proof
        (to_string_typed_nonaddress_is_allowed st (VEnum name) I Htyped)
        as Hallowed;
      discriminate Hallowed).
  - abstract (
      pose proof
        (to_string_typed_nonaddress_is_allowed st (VClo fn captured) I Htyped)
        as Hallowed;
      discriminate Hallowed).
  - abstract (
      pose proof
        (to_string_typed_nonaddress_is_allowed
          st (VCont fn captured stack) I Htyped)
        as Hallowed;
      discriminate Hallowed).
  - abstract (
      pose proof
        (to_string_typed_nonaddress_is_allowed
          st (VAst origin root rev_path) I Htyped)
        as Hallowed;
      discriminate Hallowed).
  - abstract (
      pose proof (to_string_number_case_fell_through _ _ Hreach) as Hnumber;
      rewrite to_string_number_guard_is_true in Hnumber;
      discriminate).
  - abstract (
      pose proof (to_string_bigint_case_fell_through _ _ Hreach) as Hbigint;
      rewrite to_string_bigint_guard_is_true in Hbigint;
      discriminate).
  - abstract (
      pose proof
        (to_string_typed_nonaddress_is_allowed st (VInfinity pos) I Htyped)
        as Hallowed;
      discriminate Hallowed).
  - abstract (
      pose proof
        (to_string_typed_nonaddress_is_allowed st (VCodeUnit c) I Htyped)
        as Hallowed;
      discriminate Hallowed).
  - abstract (
      pose proof
        (to_string_typed_nonaddress_is_allowed
          st (VGrammarSymbol name params) I Htyped)
        as Hallowed;
      discriminate Hallowed).
Qed.

Lemma to_string_address_or_nonaddress :
  forall value,
    (exists address, value = VAddr address) \/ to_string_nonaddress value.
Proof.
  intros value.
  destruct value; try (right; exact I).
  left. eexists. reflexivity.
Qed.

(** Main classification result: the exported ESValue union supplies the
    exhaustive cases; the eight generated fall-throughs eliminate every
    primitive case and Symbol, leaving Object. *)
Theorem to_string_reached_argument_is_object :
  forall (st : xstate) (argument : val),
    value_satisfies_type st to_string_esvalue_type argument ->
    reaches_to_string_object_assertion st
      [(LName "argument", argument)] ->
    value_satisfies_type st (TRecord "Object") argument.
Proof.
  intros st argument Htyped Hreach.
  destruct (to_string_address_or_nonaddress argument)
    as [[address Haddress] | Hnonaddress].
  - subst argument.
    apply to_string_address_fallthrough_is_object.
    + exact Htyped.
    + exact (to_string_symbol_case_fell_through _ _ Hreach).
  - exfalso.
    exact
      (to_string_nonaddress_fallthrough_impossible
        st argument Hnonaddress Htyped Hreach).
Qed.

(** The actual generated [IAssert] now executes successfully for every
    ESValue-typed argument that reaches its site through the eight real
    dispatch branches.  [p] is arbitrary because this instruction performs
    no calls. *)
Theorem to_string_object_assertion_safe_from_reachability :
  forall (p : prog) (st : xstate) (argument : val),
    value_satisfies_type st to_string_esvalue_type argument ->
    reaches_to_string_object_assertion st
      [(LName "argument", argument)] ->
    exec_inst 2 p st [(LName "argument", argument)]
      to_string_object_assertion =
      Ok (st, [(LName "argument", argument)], CNormal VUndef).
Proof.
  intros p st argument Htyped Hreach.
  pose proof
    (to_string_reached_argument_is_object st argument Htyped Hreach)
    as Hobject.
  unfold value_satisfies_type in Hobject.
  cbn [to_string_object_assertion exec_inst].
  unfold to_string_object_test, to_string_argument_ref.
  cbn [exec_expr exec_ref read_target_x obind of_option env_lookup local_eqb].
  rewrite String.eqb_refl.
  cbn [obind of_option].
  rewrite Hobject. reflexivity.
Qed.

(** Concrete computation below intentionally re-enables the definitions that
    were opaque only to keep the symbolic proof's conversion cost bounded. *)
Local Transparent run_heap_query_x ty_check_query.

(** A concrete non-vacuity witness also runs the complete generated prefix:
    the eight [IIf] nodes followed by instruction 8's real [IAssert]. *)
Definition to_string_object_state : xstate :=
  mkXState [Some (ORecord "Object" [])] [] [] None None [] 0.

Definition to_string_object_env : env :=
  [(LName "argument", VAddr 0)].

Definition to_string_through_object_assertion : inst :=
  ISeq (firstn 9 to_string_instructions).

Example concrete_object_satisfies_exported_esvalue_type :
  value_satisfies_type to_string_object_state
    to_string_esvalue_type (VAddr 0).
Proof. vm_compute. reflexivity. Qed.

Example concrete_object_reaches_generated_assertion :
  reaches_to_string_object_assertion
    to_string_object_state to_string_object_env.
Proof.
  intros index Hindex.
  unfold falls_through_generated_if, to_string_instructions.
  destruct index as [|[|[|[|[|[|[|[|index]]]]]]]]; try lia;
    repeat eexists; vm_compute; reflexivity.
Qed.

Theorem concrete_generated_to_string_object_assertion_succeeds :
  exec_inst 12 (mkProg []) to_string_object_state to_string_object_env
    to_string_through_object_assertion =
  Ok (to_string_object_state, to_string_object_env, CNormal VUndef).
Proof. vm_compute. reflexivity. Qed.
