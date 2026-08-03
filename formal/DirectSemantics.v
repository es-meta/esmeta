(** * ESMetaFV.DirectSemantics — constructor-free direct ITree helpers

    This module is the Rocq ABI consumed by the direct Gallina generator.
    Generated functions build ITree terms from already-evaluated values and
    resolved targets; none of the helpers below accepts an IR [expr], [ref],
    [inst], or [func].  [Semantics.v] remains the generic executable oracle. *)

From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment Domain Events Semantics.

Set Implicit Arguments.

(** Keep [mn] explicit in the generated-code API even when it does not occur
    in a helper's result type. *)
Unset Implicit Arguments.

Local Open Scope string_scope.

Section DIRECT.
  Context `{!crisG Γ Σ α β τ _S _I}.

  Variable mn : string.

  (** A generated instruction body keeps the complete SDO name set explicit
      and threads the local environment exactly like [denote_inst]. *)
  Definition direct_inst_body : Type :=
    list string -> env -> itree crisE (env * completion).

  Definition direct_expr : Type := env -> itree crisE val.
  Definition direct_ref : Type := env -> itree crisE ref_target.
  Definition direct_exprs : Type := env -> itree crisE (list val).
  Definition direct_parse_expr : Type :=
    env -> itree crisE (eval_result val).

  Definition direct_ref_var (variable : var) : direct_ref :=
    fun _ => Ret (TVar variable).

  Definition direct_ref_field
    (base : direct_ref) (field : direct_expr) : direct_ref :=
    fun ρ =>
      target <- base ρ;;
      base_value <- read_target mn ρ target;;
      field_value <- field ρ;;
      Ret (TField base_value field_value).

  Definition direct_read (reference : direct_ref) : direct_expr :=
    fun ρ =>
      target <- reference ρ;;
      read_target mn ρ target.

  Definition direct_unary (op : uop) (operand : direct_expr) : direct_expr :=
    fun ρ =>
      value <- operand ρ;;
      (eval_uop op value)?.

  Definition direct_binary
    (op : bop) (left right : direct_expr) : direct_expr :=
    fun ρ =>
      left_value <- left ρ;;
      right_value <- right ρ;;
      match host_bop_query op left_value right_value with
      | Some query =>
          hosts <- cgetU (hosts_key mn);;
          match typed_host_cache_lookup query hosts with
          | Some result => Ret result
          | None => triggerUB
          end
      | None => (eval_bop op left_value right_value)?
      end.

  (** The right thunk is entered only on the branch that evaluates it. *)
  Definition direct_and (left right : direct_expr) : direct_expr :=
    fun ρ =>
      left_value <- left ρ;;
      match left_value with
      | VBool false => Ret (VBool false)
      | VBool true =>
          right_value <- right ρ;;
          match right_value with
          | VBool result => Ret (VBool result)
          | _ => triggerUB
          end
      | _ => triggerUB
      end.

  Definition direct_or (left right : direct_expr) : direct_expr :=
    fun ρ =>
      left_value <- left ρ;;
      match left_value with
      | VBool true => Ret (VBool true)
      | VBool false =>
          right_value <- right ρ;;
          match right_value with
          | VBool result => Ret (VBool result)
          | _ => triggerUB
          end
      | _ => triggerUB
      end.

  Definition direct_convert (op : cop) (operand : direct_expr) : direct_expr :=
    fun ρ =>
      value <- operand ρ;;
      denote_cop_value mn op value.

  Definition direct_exists_value (ρ : env) (target : ref_target)
    : itree crisE val :=
    match target with
    | TVar (VLocal local) =>
        Ret (VBool
          (match env_lookup ρ local with Some _ => true | None => false end))
    | TVar (VGlobal _) => triggerUB
    | TField (VAddr address) field =>
        object <- get_obj mn address;;
        match object, field with
        | ORecord _ fields, VStr code_units =>
            name <- (ascii_of_cstr code_units)?;;
            Ret (VBool
              (match fields_lookup fields name with
               | Some _ => true | None => false
               end))
        | OMap entries, _ =>
            found <- (map_lookup_partial entries field)?;;
            Ret (VBool
              (match found with Some _ => true | None => false end))
        | OList values, VMath index =>
            Ret (VBool
              (andb (0 <=? index)%Z
                (Nat.ltb (Z.to_nat index) (List.length values))))
        | _, _ => triggerUB
        end
    | TField (VAst _ root path) field =>
        Ret (VBool (ast_cursor_field_exists root path field))
    | _ => triggerUB
    end.

  Definition direct_sizeof_value (value : val) : itree crisE val :=
    match value with
    | VStr code_units => Ret (VMath (Z.of_nat (List.length code_units)))
    | VAddr address =>
        object <- get_obj mn address;;
        size <- (obj_size object)?;;
        Ret (VMath (Z.of_nat size))
    | VAst _ root path =>
        ast_value <- (ast_focus root path)?;;
        Ret (VMath (Z.of_nat (List.length (ast_children ast_value))))
    | _ => triggerUB
    end.

  Definition direct_typeof_value (value : val) : itree crisE val :=
    match value with
    | VAddr address =>
        object <- get_obj mn address;;
        Ret (VStr (cu (typeof_obj object)))
    | _ =>
        name <- (typeof_prim value)?;;
        Ret (VStr (cu name))
    end.

  Definition direct_list_values (values : list val) : itree crisE val :=
    address <- alloc_obj mn (OList values);;
    Ret (VAddr address).

  Definition direct_record_values
    (type_name : string) (fields : list (string * val)) : itree crisE val :=
    address <- alloc_obj mn (ORecord type_name fields);;
    Ret (VAddr address).

  Definition direct_map_values (entries : list (val * val)) : itree crisE val :=
    address <- alloc_obj mn (OMap entries);;
    Ret (VAddr address).

  Definition direct_exists (reference : direct_ref) : direct_expr :=
    fun ρ =>
      target <- reference ρ;;
      direct_exists_value ρ target.

  Definition direct_keys_value (int_sorted : bool) (value : val)
    : itree crisE val :=
    match value with
    | VAddr address =>
        object <- get_obj mn address;;
        keys <-
          match object, int_sorted with
          | ORecord _ fields, _ =>
              match fields with
              | nil => (obj_keys object)?
              | _ :: nil => (obj_keys object)?
              | _ =>
                  known <-
                    (cgetU (record_order_key mn address) : itree crisE bool);;
                  if known then (obj_keys object)? else triggerUB
              end
          | OMap entries, true =>
              hosts <- cgetU (hosts_key mn);;
              (obj_integer_sorted_keys hosts entries)?
          | _, _ => (obj_keys object)?
          end;;
        result <- alloc_obj mn (OList keys);;
        Ret (VAddr result)
    | _ => triggerUB
    end.

  Definition direct_keys (int_sorted : bool) (receiver : direct_expr)
    : direct_expr :=
    fun ρ =>
      value <- receiver ρ;;
      direct_keys_value int_sorted value.

  Definition direct_copy_value (value : val) : itree crisE val :=
    match value with
    | VAddr address =>
        object <- get_obj mn address;;
        result <- alloc_obj mn object;;
        Ret (VAddr result)
    | _ => triggerUB
    end.

  Definition direct_tostr_value (value : val) (radix : option val)
    : itree crisE val :=
    match value with
    | VStr code_units => Ret (VStr code_units)
    | VNumber _ | VBigInt _ =>
        base <-
          match radix with
          | None => Ret 10%Z
          | Some (VMath base) => Ret base
          | Some _ => triggerUB
          end;;
        hosts <- cgetU (hosts_key mn);;
        match typed_host_cache_lookup (HQToStr value base) hosts with
        | Some (VStr code_units) => Ret (VStr code_units)
        | _ => triggerUB
        end
    | _ => triggerUB
    end.

  Definition direct_mathop_values (op : mop) (values : list val)
    : itree crisE val :=
    query <- (host_mathop_query op values)?;;
    hosts <- cgetU (hosts_key mn);;
    match typed_host_cache_lookup query hosts with
    | Some result => Ret result
    | None => triggerUB
    end.

  Definition direct_contains_values (list_value element : val)
    : itree crisE val :=
    match list_value with
    | VAddr address =>
        object <- get_obj mn address;;
        match object with
        | OList values =>
            contained <- (vals_contains_partial element values)?;;
            Ret (VBool contained)
        | _ => triggerUB
        end
    | _ => triggerUB
    end.

  Definition direct_trim_value (value : val) (is_starting : bool)
    : itree crisE val :=
    match value with
    | VStr code_units => Ret (VStr (cstr_trim code_units is_starting))
    | _ => triggerUB
    end.

  (** Direct parse operands use a separate catchable-result type.  The
      generator evaluates them left-to-right and supplies the outcomes to
      this helper; model UB remains distinct from [EvalThrow]. *)
  Definition direct_parse_outcomes
    (code rule : eval_result val) : itree crisE val :=
    match code with
    | EvalThrow => alloc_parse_errors mn
    | EvalValue code_value =>
        match rule with
        | EvalThrow => alloc_parse_errors mn
        | EvalValue rule_value =>
            match rule_value with
            | VGrammarSymbol _ _ =>
                source <- cgetU (src_key mn);;
                cached <- cgetU (cached_key mn);;
                match code_value, rule_value, source, cached with
                | VStr text, VGrammarSymbol name nil, Some source_text,
                    Some ast_value =>
                    if andb (String.eqb name "Script")
                      (cstr_eqb text source_text)
                    then Ret (VAst (AstExported 0) ast_value nil)
                    else
                      match host_parse_query code_value rule_value with
                      | Some query =>
                          hosts <- cgetU (hosts_key mn);;
                          match typed_host_cache_lookup query hosts with
                          | Some (VAst _ root path) =>
                              origin <- alloc_ast_origin mn;;
                              Ret (VAst origin root path)
                          | Some VUndef => alloc_parse_errors mn
                          | _ => triggerUB
                          end
                      | None => alloc_parse_errors mn
                      end
                | _, _, _, _ =>
                    match host_parse_query code_value rule_value with
                    | Some query =>
                        hosts <- cgetU (hosts_key mn);;
                        match typed_host_cache_lookup query hosts with
                        | Some (VAst _ root path) =>
                            origin <- alloc_ast_origin mn;;
                            Ret (VAst origin root path)
                        | Some VUndef => alloc_parse_errors mn
                        | _ => triggerUB
                        end
                    | None => alloc_parse_errors mn
                    end
                end
            | _ => alloc_parse_errors mn
            end
        end
    end.

  Definition direct_parse_results := direct_parse_outcomes.

  Definition direct_parse
    (code rule : direct_parse_expr) : direct_expr :=
    fun ρ =>
      code_result <- code ρ;;
      match code_result with
      | EvalThrow => alloc_parse_errors mn
      | EvalValue _ =>
          rule_result <- rule ρ;;
          direct_parse_outcomes code_result rule_result
      end.

  Definition direct_syntactic_values
    (name : string) (args : list bool) (rhs_index sub_index : nat)
    (child_values : list (option ast))
    (child_names : list string)
    (source_layout : list (option cstr)) : itree crisE val :=
    if existsb
      (fun child => match child with Some _ => true | None => false end)
      child_values
    then triggerUB
    else
      match render_syn_source_raw source_layout child_values with
      | Some parse_source =>
          origin <- alloc_ast_origin mn;;
          Ret
            (VAst origin
              (ASyn name args rhs_index sub_index child_values child_names
                (cstr_java_trim parse_source) parse_source) nil)
      | None => triggerUB
      end.

  Definition direct_source_text_value (value : val) : itree crisE val :=
    match value with
    | VAst _ root path =>
        ast_value <- (ast_focus root path)?;;
        Ret (VStr (ast_src ast_value))
    | _ => triggerUB
    end.

  Definition direct_substring_values
    (string_value from_value : val) (to_value : option val)
    : itree crisE val :=
    (eval_substring string_value from_value to_value)?.

  Definition direct_syntactic
    (name : string) (args : list bool) (rhs_index sub_index : nat)
    (children : env -> itree crisE (list (option ast)))
    (child_names : list string)
    (source_layout : list (option cstr)) : direct_expr :=
    fun ρ =>
      child_values <- children ρ;;
      direct_syntactic_values name args rhs_index sub_index child_values
        child_names source_layout.

  Definition direct_normal (value : val) (fnames : list string) (ρ : env)
    : itree crisE (env * completion) :=
    Ret (ρ, CNormal value).

  Definition direct_nop : direct_inst_body :=
    direct_normal VUndef.

  Definition direct_expr_inst (expression : direct_expr) : direct_inst_body :=
    fun _ ρ => expression ρ;;; Ret (ρ, CNormal VUndef).

  Definition direct_let (name : string) (rhs : direct_expr)
    : direct_inst_body :=
    fun _ ρ =>
      value <- rhs ρ;;
      Ret (env_update (LName name) value ρ, CNormal VUndef).

  (** Binding [target] before [rhs] makes the assignment evaluation order
      structural and prevents the generator from accidentally reversing it. *)
  Definition direct_assign (target : direct_ref) (rhs : direct_expr)
    : direct_inst_body :=
    fun _ ρ =>
      resolved <- target ρ;;
      value <- rhs ρ;;
      ρ' <- write_target mn ρ resolved value;;
      Ret (ρ', CNormal VUndef).

  Definition direct_return (expression : direct_expr) : direct_inst_body :=
    fun _ ρ =>
      value <- expression ρ;;
      Ret (ρ, CReturn value).

  Definition direct_assert (condition : direct_expr) : direct_inst_body :=
    fun _ ρ =>
      value <- condition ρ;;
      match value with
      | VBool true => Ret (ρ, CNormal VUndef)
      | _ => triggerUB
      end.

  (** [IAssert (EYet _)] is an ordered syntactic clause in the generic
      denotation.  The generator selects this helper without compiling the
      unsupported expression. *)
  Definition direct_assert_yet : direct_inst_body := direct_nop.

  Definition direct_push_values
    (ρ : env) (value list_value : val) (front : bool)
    : itree crisE (env * completion) :=
    match list_value with
    | VAddr address =>
        object <- get_obj mn address;;
        match object with
        | OList values =>
            put_obj mn address
              (OList
                (if front then value :: values
                 else (values ++ value :: nil)%list));;;
            Ret (ρ, CNormal VUndef)
        | _ => triggerUB
        end
    | _ => triggerUB
    end.

  Definition direct_push
    (element list_value : direct_expr) (front : bool) : direct_inst_body :=
    fun _ ρ =>
      value <- element ρ;;
      list_result <- list_value ρ;;
      direct_push_values ρ value list_result front.

  Definition direct_pop_value
    (ρ : env) (lhs : local) (list_value : val) (front : bool)
    : itree crisE (env * completion) :=
    match list_value with
    | VAddr address =>
        object <- get_obj mn address;;
        match object with
        | OList values =>
            if front then
              match values with
              | nil => triggerUB
              | value :: rest =>
                  put_obj mn address (OList rest);;;
                  Ret (env_update lhs value ρ, CNormal VUndef)
              end
            else
              match List.rev values with
              | nil => triggerUB
              | value :: reverse_rest =>
                  put_obj mn address (OList (List.rev reverse_rest));;;
                  Ret (env_update lhs value ρ, CNormal VUndef)
              end
        | _ => triggerUB
        end
    | _ => triggerUB
    end.

  Definition direct_pop
    (lhs : local) (list_value : direct_expr) (front : bool)
    : direct_inst_body :=
    fun _ ρ =>
      list_result <- list_value ρ;;
      direct_pop_value ρ lhs list_result front.

  Definition direct_expand_values
    (ρ : env) (base_value field_value : val)
    : itree crisE (env * completion) :=
    match base_value, field_value with
    | VAddr address, VStr code_units =>
        name <- (ascii_of_cstr code_units)?;;
        object <- get_obj mn address;;
        match object with
        | ORecord type_name fields =>
            match fields_lookup fields name with
            | Some _ => Ret (ρ, CNormal VUndef)
            | None =>
                put_obj mn address
                  (ORecord type_name (fields_insert name VUndef fields));;;
                invalidate_record_order mn address;;;
                Ret (ρ, CNormal VUndef)
            end
        | _ => triggerUB
        end
    | _, _ => triggerUB
    end.

  Definition direct_expand
    (base : direct_ref) (field : direct_expr) : direct_inst_body :=
    fun _ ρ =>
      target <- base ρ;;
      base_value <- read_target mn ρ target;;
      field_value <- field ρ;;
      direct_expand_values ρ base_value field_value.

  Definition direct_delete_values
    (ρ : env) (base_value key_value : val)
    : itree crisE (env * completion) :=
    match base_value with
    | VAddr address =>
        object <- get_obj mn address;;
        match object with
        | OMap entries =>
            entries' <- (map_delete_partial key_value entries)?;;
            put_obj mn address (OMap entries');;;
            Ret (ρ, CNormal VUndef)
        | _ => triggerUB
        end
    | _ => triggerUB
    end.

  Definition direct_delete
    (base : direct_ref) (key : direct_expr) : direct_inst_body :=
    fun _ ρ =>
      target <- base ρ;;
      base_value <- read_target mn ρ target;;
      key_value <- key ρ;;
      direct_delete_values ρ base_value key_value.

  (** Argument evaluation is deliberately thunked: SDO target resolution
      precedes argument effects in the generic denotation. *)
  Definition direct_sdo_value
    (fnames : list string) (ρ : env) (lhs : local) (base_value : val)
    (method : string) (arguments : unit -> itree crisE (list val))
    : itree crisE (env * completion) :=
    match base_value with
    | VAst origin root path =>
        match ast_focus root path with
        | None => triggerUB
        | Some ast_value =>
            match ast_value with
            | ALex _ _ _ _ _ =>
                result <- (ast_lex_sdo ast_value method)?;;
                Ret (env_update lhs result ρ, CNormal VUndef)
            | ASyn _ _ _ _ _ _ _ _ =>
                '(resolved_path, function_name) : list nat * string <-
                  (sdo_resolve_cursor fnames root path method)?;;
                values <- arguments tt;;
                result <- ccallU (ir_sig function_name)
                  (nil, VAst origin root resolved_path :: values);;
                Ret (env_update lhs result ρ, CNormal VUndef)
            end
        end
    | _ => triggerUB
    end.

  Definition direct_sdo_call
    (lhs : local) (base : direct_expr) (method : string)
    (arguments : direct_exprs) : direct_inst_body :=
    fun fnames ρ =>
      base_value <- base ρ;;
      direct_sdo_value fnames ρ lhs base_value method
        (fun _ => arguments ρ).

  Definition direct_if
    (condition : direct_expr) (then_body else_body : direct_inst_body)
    : direct_inst_body :=
    fun fnames ρ =>
      value <- condition ρ;;
      match value with
      | VBool true => then_body fnames ρ
      | VBool false => else_body fnames ρ
      | _ => triggerUB
      end.

  Fixpoint direct_seq (bodies : list direct_inst_body) : direct_inst_body :=
    fun fnames ρ =>
      match bodies with
      | nil => Ret (ρ, CNormal VUndef)
      | body :: rest =>
          '(ρ', result) : env * completion <- body fnames ρ;;
          match result with
          | CNormal _ => direct_seq rest fnames ρ'
          | CReturn value => Ret (ρ', CReturn value)
          end
      end.

  Definition direct_while
    (condition : direct_expr) (body : direct_inst_body)
    : direct_inst_body :=
    fun fnames ρ =>
      ITree.iter
        (fun ρ0 : env =>
          value <- condition ρ0;;
          match value with
          | VBool true =>
              '(ρ1, result) : env * completion <- body fnames ρ0;;
              match result with
              | CNormal _ => Ret (inl ρ1)
              | CReturn returned => Ret (inr (ρ1, CReturn returned))
              end
          | VBool false => Ret (inr (ρ0, CNormal VUndef))
          | _ => triggerUB
          end) ρ.

  (** Ordinary and continuation wrappers differ only in argument binding.
      Partially applying [params], [is_main], and [body] produces exactly
      the generated ABI [list string -> ir_arg -> itree crisE val]. *)
  Definition direct_fbody
    (fnames params : list string) (is_main : bool) (body : direct_inst_body)
    : ir_arg -> itree crisE val :=
    fun arg =>
      let '(captured, args) := arg in
      ρ0 <- (init_env params args)?;;
      '(_, result) : env * completion <-
        body fnames (merge_captured_env ρ0 captured);;
      match result with
      | CReturn value => Ret value
      | CNormal _ => if is_main then Ret VUndef else triggerUB
      end.

  Definition direct_cont_fbody
    (fnames params : list string) (is_main : bool) (body : direct_inst_body)
    : ir_arg -> itree crisE val :=
    fun arg =>
      let '(captured, args) := arg in
      let ρ0 := init_cont_env params args in
      '(_, result) : env * completion <-
        body fnames (merge_captured_env ρ0 captured);;
      match result with
      | CReturn value => Ret value
      | CNormal _ => if is_main then Ret VUndef else triggerUB
      end.

  Definition direct_fnsem
    (name : string) (body : ir_arg -> itree crisE val)
    : fname * option (emask * (option fspec_rel * fbody)) :=
    (funid name,
      Some (ir_mask mn, (fsp_none, cfunU (fntyp ir_arg val) body))).

  Definition direct_cont_fnsem := direct_fnsem.

  Definition direct_entry
    (body : ir_arg -> itree crisE val)
    : fname * option (emask * (option fspec_rel * fbody)) :=
    (entry,
      Some (ir_mask mn,
        (fsp_none, cfunU (fntyp unit val) (fun _ => body (nil, nil))))).
End DIRECT.
