(** * ESMetaFV.Semantics — ITree denotation of IR-Core (Milestone 2)

    Implements the layered design of the architecture note §4 (ADR-6):
    - local environments : threaded purely (no events);
    - globals + heap     : CRIS keyed store ([pgE] via [cput]/[cgetU]);
    - control            : an explicit [completion] result type;
    - observable effects : [IO "esmeta.print"] (Events.v) and [callE] calls;
    - stuck states       : [triggerUB] (ADR-7, provisional).

    Fidelity notes (each checked against the Scala interpreter, with the
    source location cited; deviations are listed in the research log):
    - [IAssign] evaluates the reference BEFORE the right-hand side
      (Interpreter.scala:129-133).
    - [RField] evaluates the base reference before the field expression
      (Interpreter.scala:397-402).
    - [BAnd]/[BOr] short-circuit (Interpreter.scala:251-252, 358-365).
    - [ILet] and [IAssign]-to-local are the same unconditional update
      (State.scala:75-77).
    - Call arity must match exactly.  DEVIATION: ESMeta silently ignores
      arity underflow due to a latent unthrown error
      (Interpreter.scala:381); we model strict arity (UB on mismatch) and
      exclude underflow by admissibility.  Recorded in the research log.
    - A non-main function body falling through without [IReturn] is UB,
      mirroring [NoReturnValue] (Interpreter.scala:98).  A main body
      falling through returns [VUndef] (modeling choice; ESMeta leaves the
      RESULT global unset — research log 2026-07-29).
    - [IAssert]: condition must evaluate to [VBool true].  ESMeta evaluates
      assertions through [optional(...)] (Interpreter.scala:147-151), so a
      bare uncompiled metalanguage [EYet] is skipped; every other failure
      or false result remains UB. *)

From CRIS Require Import CRIS.
From stdpp Require Import pretty.
From ESMetaFV Require Import Fragment Domain Events.

Set Implicit Arguments.

Local Open Scope string_scope.

(** Private, typed control calls used only by the closed executable
    projection in [ITreeExec.v].  Capture returns the current explicit
    frame pointer.  Invocation has an empty return type because a
    continuation replaces the current control stack instead of returning
    to its call-site continuation. *)
Definition cont_capture_sig : fnsig_t unit cont_stack :=
  fnsig cont_capture_fn (fntyp unit cont_stack).

Definition cont_invoke_sig : fnsig_t cont_request Empty_set :=
  fnsig cont_invoke_fn (fntyp cont_request Empty_set).

(** The pure semantic domain (completions, operator evaluation,
    environments, heap objects) lives in [Domain.v], shared with the
    executable reference interpreter [Exec.v]. *)

(** ** Store layout (ADR-6, OQ-9)

    All keys of a program module live in its single scope [mn], as required
    by CRIS well-scopedness (SMod.v:21-25 [RF]).  Three key families:
    globals, heap cells, record-key-order provenance, and allocation
    counters.  The [$] separator cannot occur in ESMeta global names or
    produce collisions between families because each family has a
    distinct prefix. *)

Section DENOTE.
  Context `{!crisG Γ Σ α β τ _S _I}.

  Variable mn : string.   (* module (scope) name *)

  Definition glb_key (x : string) : key := (mn, "g$" ++ x).
  Definition heap_key (a : nat) : key := (mn, "h$" ++ pretty (N.of_nat a)).
  Definition record_order_key (a : nat) : key :=
    (mn, "record-order$" ++ pretty (N.of_nat a)).
  Definition alloc_key : key := (mn, "alloc$").
  Definition ast_alloc_key : key := (mn, "ast-alloc$").
  (* Immutable run parameters, mirroring State.scala:17-18.  They live in
     the store rather than as extra parameters because that is where
     ESMeta keeps them: fields of [State]. *)
  Definition src_key : key := (mn, "src$").
  Definition cached_key : key := (mn, "cached$").
  Definition hosts_key : key := (mn, "hosts$").

  Definition get_obj (a : nat) : itree crisE obj := cgetU (heap_key a).
  Definition put_obj (a : nat) (o : obj) : itree crisE unit :=
    cput (heap_key a) o.

  Definition invalidate_record_order (a : nat) : itree crisE unit :=
    cput (record_order_key a) false.

  (** Interpret the shared finite type-check plan in the CRIS keyed store.
      The query builder lives in [Domain.v], so this denotation and
      [Exec.v] cannot drift on recursive Record/List/Map containment. *)
  Fixpoint run_heap_query_cached {A : Type}
    (cache : list (nat * obj)) (query : heap_query A)
    : itree crisE A :=
    match query with
    | HeapDone result => Ret result
    | HeapRead address continue_with =>
        match resolved_lookup cache address with
        | Some object =>
            run_heap_query_cached cache (continue_with object)
        | None =>
            object <- get_obj address;;
            run_heap_query_cached
              ((address, object) :: cache) (continue_with object)
        end
    end.

  Definition run_heap_query {A : Type} (query : heap_query A)
    : itree crisE A :=
    run_heap_query_cached nil query.

  (** Deterministic counter allocation (Heap.scala:62-67). *)
  Definition alloc_obj (o : obj) : itree crisE nat :=
    a <- cgetU alloc_key;;
    cput alloc_key (S a);;;
    put_obj a o;;;
    (* Scala [RecordObj] is backed by a generic mutable HashMap.  Its
       iteration order is not the source-field order represented by
       [ORecord], so a runtime-created/copied record cannot soundly expose
       [EKeys].  Initial records receive exact exported provenance below. *)
    (match o with
     | ORecord _ _ => invalidate_record_order a
     | _ => Ret tt
     end);;;
    Ret a.

  Definition alloc_ast_origin : itree crisE ast_origin :=
    a <- cgetU ast_alloc_key;;
    cput ast_alloc_key (S a);;;
    Ret (AstRuntime a).

  (** ** Reference targets (state/RefTarget.scala) *)

  Variant ref_target : Type :=
  | TVar (x : var)
  | TField (base : val) (field : val).

  Definition read_target (ρ : env) (t : ref_target) : itree crisE val :=
    match t with
    | TVar (VLocal l) => (env_lookup ρ l)?
    | TVar (VGlobal x) => cgetU (glb_key x)
    | TField (VAddr a) k =>
        o <- get_obj a;;
        match o, k with
        | ORecord _ fs, VStr cs =>
            fld <- (ascii_of_cstr cs)?;;    (* field names are ASCII (D-1) *)
            (fields_lookup fs fld)?
        | OList vs, VMath i =>
            if (0 <=? i)%Z then (nth_error vs (Z.to_nat i))? else triggerUB
        | OMap es, _ =>
            found <- (map_lookup_partial es k)?;;
            found?
        | _, _ => triggerUB
        end
    (* Parsed-AST cursor access, including numeric/named children and the
       parser-populated parent relation (State.scala:52, Ast.scala:84-91). *)
    | TField (VAst origin root path) field =>
        cursor <- (ast_cursor_field_get root path field)?;;
        let '(next_root, next_path) := cursor in
        Ret (VAst origin next_root next_path)
    (* Indexing a string yields the code unit at that position
       (State.scala:57-59: [case Math(k) => CodeUnit(str(k.toInt))]).  Any
       non-[Math] field raises WrongStringRef and an out-of-range index
       throws in Scala; both are UB here. *)
    | TField (VStr cs) (VMath i) =>
        let index := scala_to_int32 i in
        if (0 <=? index)%Z
        then c <- (nth_error cs (Z.to_nat index))?;; Ret (VCodeUnit c)
        else triggerUB
    | _ => triggerUB
    end.

  (** [EParse] is the one IR expression whose Scala implementation catches
      exceptions raised while evaluating its operands.  Keep those failures
      explicit until the enclosing parse decides whether to recover them;
      do not reuse [Take False], which also represents model/cache UB. *)
  Definition eval_ret {A : Type} (value : A)
    : itree crisE (eval_result A) :=
    Ret (EvalValue value).

  Definition eval_throw {A : Type} : itree crisE (eval_result A) :=
    Ret EvalThrow.

  Definition eval_bind {A B : Type}
    (result : itree crisE (eval_result A))
    (continue_with : A -> itree crisE (eval_result B))
    : itree crisE (eval_result B) :=
    outcome <- result;;
    match outcome with
    | EvalValue value => continue_with value
    | EvalThrow => eval_throw
    end.

  Definition eval_of_option {A : Type} (value : option A)
    : itree crisE (eval_result A) :=
    match value with
    | Some result => eval_ret result
    | None => eval_throw
    end.

  (** Unlike [cgetU], a failed dynamic downcast here is an ESMeta lookup
      exception and remains catchable by [EParse].  Model-level host-cache
      accesses continue to use [cgetU] and therefore remain UB on corruption. *)
  Definition eval_cget {A : Type} (key : key)
    : itree crisE (eval_result A) :=
    boxed <- trigger (SGet key);;
    eval_of_option (@Any.downcast A boxed).

  Definition eval_get_obj (address : nat)
    : itree crisE (eval_result obj) :=
    eval_cget (heap_key address).

  Definition eval_read_target (ρ : env) (target : ref_target)
    : itree crisE (eval_result val) :=
    match target with
    | TVar (VLocal local) => eval_of_option (env_lookup ρ local)
    | TVar (VGlobal global) => eval_cget (glb_key global)
    | TField (VAddr address) field =>
        eval_bind (eval_get_obj address) (fun object =>
          match object, field with
          | ORecord _ fields, VStr code_units =>
              eval_bind (eval_of_option (ascii_of_cstr code_units))
                (fun name => eval_of_option (fields_lookup fields name))
          | OList values, VMath index =>
              if (0 <=? index)%Z
              then eval_of_option (nth_error values (Z.to_nat index))
              else eval_throw
          | OMap entries, _ =>
              match map_lookup_partial entries field with
              | Some found => eval_of_option found
              | None => triggerUB
              end
          | _, _ => eval_throw
          end)
    | TField (VAst origin root path) field =>
        eval_bind
          (eval_of_option (ast_cursor_field_get root path field))
          (fun cursor =>
            let '(next_root, next_path) := cursor in
            eval_ret (VAst origin next_root next_path))
    | TField (VStr code_units) (VMath index) =>
        let wrapped_index := scala_to_int32 index in
        if (0 <=? wrapped_index)%Z
        then
          eval_bind
            (eval_of_option
              (nth_error code_units (Z.to_nat wrapped_index)))
            (fun code_unit => eval_ret (VCodeUnit code_unit))
        else eval_throw
    | _ => eval_throw
    end.

  (** Catchable evaluator for the operand fragment admitted by the exporter
      under [EParse].  Every currently generated ECMA-262 parse operand is a
      literal, a reference, or [ESourceText] over such a value.  Unsupported
      syntax remains model UB and is rejected by the Scala exporter rather
      than being mistaken for an ordinary parse error. *)
  Fixpoint denote_parse_operand (e : expr) (ρ : env) {struct e}
    : itree crisE (eval_result val) :=
    match e with
    | EMath z => eval_ret (VMath z)
    | EBool b => eval_ret (VBool b)
    | EStr s => eval_ret (VStr s)
    | EUndef => eval_ret VUndef
    | ENull => eval_ret VNull
    | EEnum name => eval_ret (VEnum name)
    | ENumber number => eval_ret (VNumber number)
    | EBigInt integer => eval_ret (VBigInt integer)
    | EInfinity positive => eval_ret (VInfinity positive)
    | ECodeUnit code_unit => eval_ret (VCodeUnit code_unit)
    | EGrammarSymbol name params =>
        eval_ret (VGrammarSymbol name params)
    | EYet _ => eval_throw
    | ERef reference =>
        eval_bind (denote_parse_ref reference ρ) (eval_read_target ρ)
    | EList expressions =>
        eval_bind
          ((fix go (rest : list expr)
              : itree crisE (eval_result (list val)) :=
              match rest with
              | nil => eval_ret nil
              | head :: tail =>
                  eval_bind (denote_parse_operand head ρ) (fun value =>
                    eval_bind (go tail) (fun values =>
                      eval_ret (value :: values)))
              end) expressions)
          (fun values =>
            address <- alloc_obj (OList values);;
            eval_ret (VAddr address))
    | ESourceText inner =>
        eval_bind (denote_parse_operand inner ρ) (fun value =>
          match value with
          | VAst _ root path =>
              eval_bind (eval_of_option (ast_focus root path)) (fun ast =>
                eval_ret (VStr (ast_src ast)))
          | _ => eval_throw
          end)
    | _ => triggerUB
    end

  with denote_parse_ref (r : ref) (ρ : env) {struct r}
    : itree crisE (eval_result ref_target) :=
    match r with
    | RVar variable => eval_ret (TVar variable)
    | RField base field =>
        eval_bind (denote_parse_ref base ρ) (fun base_target =>
          eval_bind (eval_read_target ρ base_target) (fun base_value =>
            eval_bind (denote_parse_operand field ρ) (fun field_value =>
              eval_ret (TField base_value field_value))))
    end.

  Definition alloc_parse_errors : itree crisE val :=
    errors <- alloc_obj (OList nil);;
    Ret (VAddr errors).

  (** Writing.  Returns the (possibly updated) local environment; store
      writes happen as events.  Locals/globals update unconditionally
      (State.scala:75-77); record-field write inserts-or-updates
      (Obj.scala:29-30, OQ-11 resolved); list update requires the index
      in bounds (Obj.scala:32-36 throws otherwise). *)
  Definition write_target (ρ : env) (t : ref_target) (v : val)
    : itree crisE env :=
    match t with
    | TVar (VLocal l) => Ret (env_update l v ρ)
    | TVar (VGlobal x) => cput (glb_key x) v;;; Ret ρ
    | TField (VAddr a) k =>
        o <- get_obj a;;
        match o, k with
        | ORecord tn fs, VStr cs =>
            fld <- (ascii_of_cstr cs)?;;
            match fields_lookup fs fld with
            | Some _ =>
                (* Updating an existing HashMap binding preserves the
                   already-exported iteration order. *)
                put_obj a (ORecord tn (fields_insert fld v fs));;;
                Ret ρ
            | None =>
                put_obj a (ORecord tn (fields_insert fld v fs));;;
                invalidate_record_order a;;;
                Ret ρ
            end
        | OList vs, VMath i =>
            if (0 <=? i)%Z
            then
              vs' <- (list_update (Z.to_nat i) v vs)?;;
              put_obj a (OList vs');;; Ret ρ
            else triggerUB
        | OMap es, _ =>
            es' <- (map_insert_partial k v es)?;;
            put_obj a (OMap es');;; Ret ρ
        | _, _ => triggerUB
        end
    | _ => triggerUB
    end.

  (** Capture for [EClo]: each captured name must be bound in the current
      locals (Interpreter.scala:279-281: absent name throws). *)
  Fixpoint capture (ρ : env) (xs : list string)
    : itree crisE (list (string * val)) :=
    match xs with
    | nil => Ret nil
    | x :: tl =>
        v <- (env_lookup ρ (LName x))?;;
        cs <- capture ρ tl;;
        Ret (captured_normalize ((x, v) :: cs))
    end.

  Definition denote_cop_value (op : cop) (v : val) : itree crisE val :=
    match host_cop_query op v with
    | Some query =>
        hosts <- cgetU hosts_key;;
        match typed_host_cache_lookup query hosts with
        | Some result => Ret result
        | None => triggerUB
        end
    | None => (eval_cop op v)?
    end.

  Definition denote_prepared_math_operand
    (operand : prepared_number_math_operand) : itree crisE val :=
    match operand with
    | PNMOFiniteNumber f => denote_cop_value CToMath (VNumber f)
    | PNMOMath v => Ret v
    end.

  (** Execute the already-evaluated operands of one generated
      Number -> Math -> Number composite.  Two Number operands cross the
      exact typed host boundary; every other case follows the original
      conversion/operator pipeline without re-evaluating either operand. *)
  Definition denote_number_math_values
    (op : number_math_op) (math_op : bop) (result_op : cop)
    (left right : prepared_number_math_operand) : itree crisE val :=
    match left, right with
    | PNMOFiniteNumber lf, PNMOFiniteNumber rf =>
        hosts <- cgetU hosts_key;;
        match typed_host_cache_lookup (HQNumberMathOp op lf rf) hosts with
        | Some result => Ret result
        | None => triggerUB
        end
    | _, _ =>
        lm <- denote_prepared_math_operand left;;
        rm <- denote_prepared_math_operand right;;
        result <- (eval_bop math_op lm rm)?;;
        denote_cop_value result_op result
    end.

  Definition denote_number_sin_value (v : val) : itree crisE val :=
    match v with
    | VNumber f =>
        if PrimFloat.is_finite f then
          hosts <- cgetU hosts_key;;
          match typed_host_cache_lookup (HQNumberSin f) hosts with
          | Some result => Ret result
          | None => triggerUB
          end
        else triggerUB
    | _ =>
        mv <- denote_cop_value CToMath v;;
        query <- (host_mathop_query MSin [mv])?;;
        hosts <- cgetU hosts_key;;
        match typed_host_cache_lookup query hosts with
        | Some math_result => denote_cop_value CToApproxNumber math_result
        | None => triggerUB
        end
    end.

  Definition denote_number_math_comparison
    (op : bop) (left right : prepared_number_math_operand)
    : itree crisE val :=
    match number_math_comparison_query op left right with
    | Some query =>
        hosts <- cgetU hosts_key;;
        match typed_host_cache_lookup query hosts with
        | Some result => Ret result
        | None => triggerUB
        end
    | None => (eval_number_math_comparison_pure op left right)?
    end.

  (** ** Expression denotation

      Mutual over [expr]/[ref]; list arguments handled by nested fixes
      (rose-tree pattern).  Expression evaluation can allocate ([EList])
      and read state, mirroring the Scala evaluator's effectfulness. *)

  Fixpoint denote_expr (e : expr) (ρ : env) {struct e} : itree crisE val :=
    match e with
    | EMath z => Ret (VMath z)
    | EBool b => Ret (VBool b)
    | EStr s => Ret (VStr s)
    | EUndef => Ret VUndef
    | ENull => Ret VNull
    | EEnum n => Ret (VEnum n)
    | ERef r =>
        t <- denote_ref r ρ;;
        read_target ρ t
    | EUnary op e1 =>
        v <- denote_expr e1 ρ;;
        (eval_uop op v)?
    | EBinary BAnd e1 e2 =>
        v1 <- denote_expr e1 ρ;;
        match v1 with
        | VBool false => Ret (VBool false)
        | VBool true =>
            v2 <- denote_expr e2 ρ;;
            match v2 with
            | VBool b => Ret (VBool b)
            | _ => triggerUB
            end
        | _ => triggerUB
        end
    | EBinary BOr e1 e2 =>
        v1 <- denote_expr e1 ρ;;
        match v1 with
        | VBool true => Ret (VBool true)
        | VBool false =>
            v2 <- denote_expr e2 ρ;;
            match v2 with
            | VBool b => Ret (VBool b)
            | _ => triggerUB
            end
        | _ => triggerUB
        end
    | EBinary (BLt as op) (EConvert CToMath lhs) (EConvert CToMath rhs)
    | EBinary (BEqual as op) (EConvert CToMath lhs) (EConvert CToMath rhs) =>
        lv <- denote_expr lhs ρ;;
        lp <- (prepare_number_math_operand lv)?;;
        rv <- denote_expr rhs ρ;;
        rp <- (prepare_number_math_operand rv)?;;
        denote_number_math_comparison op lp rp
    | EBinary op e1 e2 =>
        v1 <- denote_expr e1 ρ;;
        v2 <- denote_expr e2 ρ;;
        match host_bop_query op v1 v2 with
        | Some query =>
            hosts <- cgetU hosts_key;;
            match typed_host_cache_lookup query hosts with
            | Some result => Ret result
            | _ => triggerUB
            end
        | None => (eval_bop op v1 v2)?
        end
    | EClo fn captured =>
        cs <- capture ρ captured;;
        Ret (VClo fn cs)
    | ECont fn =>
        stack <- ccallU cont_capture_sig tt;;
        Ret (VCont fn (capture_named_env_map ρ) stack)
    | EList es =>
        vs <- (fix go (l : list expr) : itree crisE (list val) :=
                 match l with
                 | nil => Ret nil
                 | e1 :: tl =>
                     v <- denote_expr e1 ρ;;
                     vs <- go tl;;
                     Ret (v :: vs)
                 end) es;;
        a <- alloc_obj (OList vs);;
        Ret (VAddr a)
    | ESizeOf e1 =>
        (* Obj.size is lists only (state/Obj.scala:50-52); ESMeta also
           accepts strings and ASTs (Interpreter.scala:317-321).  With D-1
           the string case is exact: length in UTF-16 code units. *)
        v <- denote_expr e1 ρ;;
        match v with
        | VStr cs => Ret (VMath (Z.of_nat (List.length cs)))
        | VAddr a =>
            o <- get_obj a;;
            n <- (obj_size o)?;;
            Ret (VMath (Z.of_nat n))
        (* ast.children.size — Interpreter.scala:321 *)
        | VAst _ root path =>
            a <- (ast_focus root path)?;;
            Ret (VMath (Z.of_nat (List.length (ast_children a))))
        | _ => triggerUB
        end
    | ERecord tname fields =>
        (* Fields evaluate left-to-right and update the insertion-ordered
           record as they go (Interpreter.scala:337-338).  Thus a duplicate
           field keeps its first position but its last value. *)
        fs <- (fix go (l : list (string * expr))
                        (acc : list (string * val))
                 : itree crisE (list (string * val)) :=
                 match l with
                 | nil => Ret acc
                 | (f, e1) :: tl =>
                     v <- denote_expr e1 ρ;;
                     go tl (fields_insert f v acc)
                 end) fields nil;;
        a <- alloc_obj (ORecord tname fs);;
        Ret (VAddr a)
    | EExists r =>
        t <- denote_ref r ρ;;
        (* Bool(st.exists(...)) — Interpreter.scala:296 *)
        match t with
        | TVar (VLocal l) =>
            Ret (VBool (match env_lookup ρ l with Some _ => true | None => false end))
        | TVar (VGlobal x) =>
            (* store lookups cannot fail at the event level, so existence of a
               global is not observable in this model: UB rather than a guess *)
            triggerUB
        | TField (VAddr a) k =>
            o <- get_obj a;;
            match o, k with
            | ORecord _ fs, VStr cs =>
                fld <- (ascii_of_cstr cs)?;;
                Ret (VBool (match fields_lookup fs fld with
                            | Some _ => true | None => false end))
            | OMap es, _ =>
                found <- (map_lookup_partial es k)?;;
                Ret (VBool (match found with
                            | Some _ => true | None => false end))
            | OList vs, VMath i =>
                Ret (VBool (andb (0 <=? i)%Z
                              (Nat.ltb (Z.to_nat i) (List.length vs))))
            | _, _ => triggerUB
            end
        | TField (VAst _ root path) field =>
            Ret (VBool (ast_cursor_field_exists root path field))
        | _ => triggerUB
        end
    | ETypeOf e1 =>
        v <- denote_expr e1 ρ;;
        match v with
        | VAddr a => o <- get_obj a;; Ret (VStr (cu (typeof_obj o)))
        | _ => s0 <- (typeof_prim v)?;; Ret (VStr (cu s0))
        end
    | ETypeCheck e1 t =>
        v <- denote_expr e1 ρ;;
        decision <- run_heap_query (ty_check_query type_check_fuel t v);;
        b <- decision?;;
        Ret (VBool b)
    | EYet _ => triggerUB      (* NotSupported — Interpreter.scala:231 *)
    | EMap pairs =>
        es <- (fix go (l : list (expr * expr))
                        (acc : list (val * val))
                 : itree crisE (list (val * val)) :=
                 match l with
                 | nil => Ret acc
                 | (ke, ve) :: tl =>
                     kv <- denote_expr ke ρ;;
                     vv <- denote_expr ve ρ;;
                     acc' <- (map_insert_partial kv vv acc)?;;
                     go tl acc'
                 end) pairs nil;;
        a <- alloc_obj (OMap es);;
        Ret (VAddr a)
    | EKeys m intSorted =>
        v <- denote_expr m ρ;;
        match v with
        | VAddr a =>
            o <- get_obj a;;
            ks <-
              match o, intSorted with
              | ORecord _ fields, _ =>
                  match fields with
                  | nil => (obj_keys o)?
                  | _ :: nil => (obj_keys o)?
                  | _ =>
                      known <-
                        (cgetU (record_order_key a) : itree crisE bool);;
                      if known then (obj_keys o)? else triggerUB
                  end
              | OMap entries, true =>
                  hosts <- cgetU hosts_key;;
                  (obj_integer_sorted_keys hosts entries)?
              | _, _ => (obj_keys o)?
              end;;
            a2 <- alloc_obj (OList ks);;
            Ret (VAddr a2)
        | _ => triggerUB
        end
    | ECopy e1 =>
        v <- denote_expr e1 ρ;;
        match v with
        | VAddr a =>
            o <- get_obj a;;
            a2 <- alloc_obj o;;
            Ret (VAddr a2)
        | _ => triggerUB
        end
    | ENumber f => Ret (VNumber f)
    | EBigInt z => Ret (VBigInt z)
    | EInfinity p => Ret (VInfinity p)
    | ECodeUnit c => Ret (VCodeUnit c)
    | EConvert CToNumber
        (EBinary BAdd (EConvert CToMath lhs) (EConvert CToMath rhs)) =>
        lv <- denote_expr lhs ρ;;
        lp <- (prepare_number_math_operand lv)?;;
        rv <- denote_expr rhs ρ;;
        rp <- (prepare_number_math_operand rv)?;;
        denote_number_math_values NMAdd BAdd CToNumber lp rp
    | EConvert CToNumber
        (EBinary BMul (EConvert CToMath lhs) (EConvert CToMath rhs)) =>
        lv <- denote_expr lhs ρ;;
        lp <- (prepare_number_math_operand lv)?;;
        rv <- denote_expr rhs ρ;;
        rp <- (prepare_number_math_operand rv)?;;
        denote_number_math_values NMMul BMul CToNumber lp rp
    | EConvert CToNumber
        (EBinary BDiv (EConvert CToMath lhs) (EConvert CToMath rhs)) =>
        lv <- denote_expr lhs ρ;;
        lp <- (prepare_number_math_operand lv)?;;
        rv <- denote_expr rhs ρ;;
        rp <- (prepare_number_math_operand rv)?;;
        denote_number_math_values NMDiv BDiv CToNumber lp rp
    | EConvert CToApproxNumber
        (EBinary BPow (EConvert CToMath lhs) (EConvert CToMath rhs)) =>
        lv <- denote_expr lhs ρ;;
        lp <- (prepare_number_math_operand lv)?;;
        rv <- denote_expr rhs ρ;;
        rp <- (prepare_number_math_operand rv)?;;
        denote_number_math_values NMPow BPow CToApproxNumber lp rp
    | EConvert CToApproxNumber
        (EMathOp MSin [EConvert CToMath inner]) =>
        v <- denote_expr inner ρ;;
        denote_number_sin_value v
    | EConvert op e1 =>
        v <- denote_expr e1 ρ;;
        denote_cop_value op v
    (* [Str] is already a string and, exactly as Interpreter.scala:279,
       does not evaluate the optional radix.  Number and BigInt formatting
       are trusted host primitives: operands/radix are evaluated here, then
       an exact typed query is looked up. *)
    | EToStr e1 radix =>
        v <- denote_expr e1 ρ;;
        match v with
        | VStr cs => Ret (VStr cs)
        | VNumber _ =>
            z <- match radix with
                 | None => Ret 10%Z
                 | Some e2 =>
                     rv <- denote_expr e2 ρ;;
                     match rv with
                     | VMath z => Ret z
                     | _ => triggerUB
                     end
                 end;;
            hosts <- cgetU hosts_key;;
            match typed_host_cache_lookup (HQToStr v z) hosts with
            | Some (VStr cs) => Ret (VStr cs)
            | _ => triggerUB
            end
        | VBigInt _ =>
            z <- match radix with
                 | None => Ret 10%Z
                 | Some e2 =>
                     rv <- denote_expr e2 ρ;;
                     match rv with
                     | VMath z => Ret z
                     | _ => triggerUB
                     end
                 end;;
            hosts <- cgetU hosts_key;;
            match typed_host_cache_lookup (HQToStr v z) hosts with
            | Some (VStr cs) => Ret (VStr cs)
            | _ => triggerUB
            end
        | _ => triggerUB
        end
    (* Arguments evaluate left-to-right (Interpreter.scala:257-258). *)
    | EVariadic op es =>
        vs <- (fix go (l : list expr) : itree crisE (list val) :=
                 match l with
                 | nil => Ret nil
                 | e1 :: tl =>
                     v <- denote_expr e1 ρ;;
                     vs <- go tl;;
                     Ret (v :: vs)
                 end) es;;
        (eval_vop op vs)?
    (* Deterministic host math: evaluate arguments left-to-right, enforce
       exact arity and [Math] operands, then accept only a typed cache hit. *)
    | EMathOp op args =>
        vs <- (fix go (l : list expr) : itree crisE (list val) :=
                 match l with
                 | nil => Ret nil
                 | e1 :: tl =>
                     v <- denote_expr e1 ρ;;
                     rest <- go tl;;
                     Ret (v :: rest)
                 end) args;;
        query <- (host_mathop_query op vs)?;;
        hosts <- cgetU hosts_key;;
        match typed_host_cache_lookup query hosts with
        | Some result => Ret result
        | None => triggerUB
        end
    (* [asList] then Scala [==] membership (Interpreter.scala:233-236);
       a non-list receiver throws NoList/NoAddr, hence UB. *)
    | EContains lst e1 =>
        lv <- denote_expr lst ρ;;
        ev <- denote_expr e1 ρ;;
        match lv with
        | VAddr a =>
            o <- get_obj a;;
            match o with
            | OList vs =>
                contained <- (vals_contains_partial ev vs)?;;
                Ret (VBool contained)
            | _ => triggerUB
            end
        | _ => triggerUB
        end
    | ETrim e1 isStarting =>
        v <- denote_expr e1 ρ;;
        match v with
        | VStr cs => Ret (VStr (cstr_trim cs isStarting))
        | _ => triggerUB
        end
    (* ESyntactic evaluates present children left-to-right, checks every
       one is an AST, then combines them with exporter-supplied grammar
       metadata.  Source rendering stays pure and exact in [Domain]. *)
    | ESyntactic nm args rhsIdx subIdx children child_names source_layout =>
        cs <- (fix go (l : list (option expr))
                    : itree crisE (list (option ast)) :=
                 match l with
                 | nil => Ret nil
                 | None :: tl =>
                     rest <- go tl;;
                     Ret (None :: rest)
                 | Some e1 :: tl =>
                     v <- denote_expr e1 ρ;;
                     match v with
                     | VAst _ a _ =>
                         rest <- go tl;;
                         Ret (Some a :: rest)
                     | _ => triggerUB
                     end
                 end) children;;
        if existsb (fun c => match c with Some _ => true | None => false end) cs
        then triggerUB
        else
          match render_syn_source_raw source_layout cs with
          | Some parse_src =>
              origin <- alloc_ast_origin;;
              Ret
                (VAst
                  origin
                  (ASyn nm args rhsIdx subIdx cs child_names
                    (cstr_java_trim parse_src) parse_src)
                  nil)
          | None => triggerUB
          end
    | EGrammarSymbol nm ps => Ret (VGrammarSymbol nm ps)
    | EInstanceOf e1 tgt =>
        v <- denote_expr e1 ρ;;
        t <- denote_expr tgt ρ;;
        Ret (eval_instanceof v t)
    (* [to] is evaluated only when present (Interpreter.scala:240). *)
    | ESourceText e1 =>
        v <- denote_expr e1 ρ;;
        match v with
        | VAst _ root path =>
            a <- (ast_focus root path)?;;
            Ret (VStr (ast_src a))
        | _ => triggerUB
        end
    (* Parsing itself remains outside the IR semantics.  The initial
       Script fast path mirrors Interpreter.scala:206-209; covered-grammar
       and runtime parses use exact results exported from the same trusted
       parser boundary.  A cache miss is UB, never an approximation. *)
    | EParse code rule =>
        code_outcome <- denote_parse_operand code ρ;;
        match code_outcome with
        | EvalThrow => alloc_parse_errors
        | EvalValue cv =>
            rule_outcome <- denote_parse_operand rule ρ;;
            match rule_outcome with
            | EvalThrow => alloc_parse_errors
            | EvalValue rv =>
                (* Scala performs [asGrammarSymbol] before reading the
                   cached source/AST fields. *)
                match rv with
                | VGrammarSymbol _ _ =>
                    src <- cgetU src_key;;
                    cached <- cgetU cached_key;;
                    match cv, rv, src, cached with
                    | VStr x, VGrammarSymbol nm nil, Some y, Some a =>
                        if andb (String.eqb nm "Script") (cstr_eqb x y)
                        then Ret (VAst (AstExported 0) a nil)
                        else
                          match host_parse_query cv rv with
                          | Some query =>
                              hosts <- cgetU hosts_key;;
                              match typed_host_cache_lookup query hosts with
                              | Some (VAst _ root path) =>
                                  origin <- alloc_ast_origin;;
                                  Ret (VAst origin root path)
                              | Some VUndef => alloc_parse_errors
                              | _ => triggerUB
                              end
                          | None => alloc_parse_errors
                          end
                    | _, _, _, _ =>
                        match host_parse_query cv rv with
                        | Some query =>
                            hosts <- cgetU hosts_key;;
                            match typed_host_cache_lookup query hosts with
                            | Some (VAst _ root path) =>
                                origin <- alloc_ast_origin;;
                                Ret (VAst origin root path)
                            | Some VUndef => alloc_parse_errors
                            | _ => triggerUB
                            end
                        | None => alloc_parse_errors
                        end
                    end
                | _ => alloc_parse_errors
                end
            end
        end
    | ESubstring e1 from to =>
        sv <- denote_expr e1 ρ;;
        fv <- denote_expr from ρ;;
        match to with
        | None => (eval_substring sv fv None)?
        | Some e2 =>
            tv <- denote_expr e2 ρ;;
            (eval_substring sv fv (Some tv))?
        end
    | EOptField recv fld =>
        (* SYNTHETIC (ADR-9): receiver once; nullish guard; no heap
           access on the nullish branch *)
        v <- denote_expr recv ρ;;
        match v with
        | VNull | VUndef => Ret VUndef
        | _ => read_target ρ (TField v (VStr (cu fld)))
        end
    end

  (** Reference denotation: base reference is dereferenced before the
      field expression is evaluated (Interpreter.scala:397-402). *)
  with denote_ref (r : ref) (ρ : env) {struct r} : itree crisE ref_target :=
    match r with
    | RVar x => Ret (TVar x)
    | RField b f =>
        tb <- denote_ref b ρ;;
        bv <- read_target ρ tb;;
        fv <- denote_expr f ρ;;
        Ret (TField bv fv)
    end.

  (** Regression: the right [EList] would begin with an allocation-counter
      read.  A non-finite left Number reaches UB during operand preparation,
      before the denotation can emit that right-hand allocation event. *)
  Example number_math_nonfinite_left_prevents_right_allocation :
    denote_expr
      (EConvert CToNumber
        (EBinary BAdd
          (EConvert CToMath (ENumber PrimFloat.infinity))
          (EConvert CToMath (EList [EMath 1%Z])))) nil = triggerUB.
  Proof. cbn [prepare_number_math_operand]. grind. Qed.

  (** Standalone monadic map for argument lists (reused by [ICall]). *)
  Fixpoint denote_exprs (es : list expr) (ρ : env)
    : itree crisE (list val) :=
    match es with
    | nil => Ret nil
    | e :: tl =>
        v <- denote_expr e ρ;;
        vs <- denote_exprs tl ρ;;
        Ret (v :: vs)
    end.

  (** ** Calls

      Uniform CRIS signature of every denoted IR function: it receives the
      closure's captured environment and the argument values, and returns
      the callee's result value (OQ-10 resolved: values cross the [Any.t]
      boundary as [val]). *)

  Definition ir_arg : Type := (list (string * val)) * (list val).

  Definition ir_sig (fn : string) : fnsig_t ir_arg val :=
    fnsig fn (fntyp ir_arg val).

  (** ** Instruction denotation *)

  Fixpoint denote_inst (fnames : list string) (i : inst) (ρ : env)
      {struct i} : itree crisE (env * completion) :=
    match i with
    | INop => Ret (ρ, CNormal VUndef)
    | ISeq insts =>
        (fix go (l : list inst) (ρ0 : env) : itree crisE (env * completion) :=
           match l with
           | nil => Ret (ρ0, CNormal VUndef)
           | i1 :: tl =>
               '(ρ1, k) : env * completion <- denote_inst fnames i1 ρ0;;
               match k with
               | CNormal _ => go tl ρ1
               | CReturn v => Ret (ρ1, CReturn v)
               end
           end) insts ρ
    | IExpr e =>
        denote_expr e ρ;;;
        Ret (ρ, CNormal VUndef)
    | ILet x e =>
        v <- denote_expr e ρ;;
        Ret (env_update (LName x) v ρ, CNormal VUndef)
    | IAssign r e =>
        t <- denote_ref r ρ;;
        v <- denote_expr e ρ;;
        ρ' <- write_target ρ t v;;
        Ret (ρ', CNormal VUndef)
    | IIf c thn els =>
        cv <- denote_expr c ρ;;
        match cv with
        | VBool true => denote_inst fnames thn ρ
        | VBool false => denote_inst fnames els ρ
        | _ => triggerUB
        end
    | IWhile c body =>
        ITree.iter
          (fun ρ0 : env =>
             cv <- denote_expr c ρ0;;
             match cv with
             | VBool true =>
                 '(ρ1, k) : env * completion <- denote_inst fnames body ρ0;;
                 match k with
                 | CNormal _ => Ret (inl ρ1)
                 | CReturn v => Ret (inr (ρ1, CReturn v))
                 end
             | VBool false => Ret (inr (ρ0, CNormal VUndef))
             | _ => triggerUB
             end) ρ
    | ICall lhs f args =>
        fv <- denote_expr f ρ;;
        match fv with
        | VClo fn captured =>
            vs <- denote_exprs args ρ;;
            rv <- ccallU (ir_sig fn) (captured, vs);;
            Ret (env_update lhs rv ρ, CNormal VUndef)
        | VCont fn captured stack =>
            vs <- denote_exprs args ρ;;
            impossible <-
              ccallU cont_invoke_sig
                (mkContRequest fn captured vs stack);;
            match impossible with end
        | _ => triggerUB
        end
    | IReturn e =>
        v <- denote_expr e ρ;;
        Ret (ρ, CReturn v)
    (* Interpreter.scala:147-151 wraps assertion evaluation in
       [optional(...)].  A bare [EYet] is ESMeta's
       NotSupported(Metalanguage) case and is therefore skipped, exactly as
       in Exec.v.  Match syntactically so no other semantic failure is
       swallowed. *)
    | IAssert (EYet _) =>
        Ret (ρ, CNormal VUndef)
    | IAssert e =>
        cv <- denote_expr e ρ;;
        match cv with
        | VBool true => Ret (ρ, CNormal VUndef)
        | _ => triggerUB
        end
    | IPrint e =>
        v <- denote_expr e ρ;;
        log_val v;;;
        Ret (ρ, CNormal VUndef)
    | IPush elem lst front =>
        v <- denote_expr elem ρ;;
        lv <- denote_expr lst ρ;;
        match lv with
        | VAddr a =>
            o <- get_obj a;;
            match o with
            | OList vs =>
                put_obj a (OList (if front then v :: vs else (vs ++ (v :: nil))%list));;;
                Ret (ρ, CNormal VUndef)
            | _ => triggerUB
            end
        | _ => triggerUB
        end
    | IPop lhs lst front =>
        lv <- denote_expr lst ρ;;
        match lv with
        | VAddr a =>
            o <- get_obj a;;
            match o with
            | OList vs =>
                if front
                then
                  match vs with
                  | nil => triggerUB
                  | v :: tl => put_obj a (OList tl);;;
                               Ret (env_update lhs v ρ, CNormal VUndef)
                  end
                else
                  match List.rev vs with
                  | nil => triggerUB
                  | v :: rtl => put_obj a (OList (List.rev rtl));;;
                                Ret (env_update lhs v ρ, CNormal VUndef)
                  end
            | _ => triggerUB
            end
        | _ => triggerUB
        end
    | IExpand base fld =>
        t <- denote_ref base ρ;;
        bv <- read_target ρ t;;
        fv <- denote_expr fld ρ;;
        match bv, fv with
        | VAddr a, VStr cs =>
            f <- (ascii_of_cstr cs)?;;
            o <- get_obj a;;
            match o with
            | ORecord tn fs =>
                (* add with undefined if absent, else keep — Obj.scala:55-58 *)
                match fields_lookup fs f with
                | Some _ => Ret (ρ, CNormal VUndef)
                | None => put_obj a (ORecord tn (fields_insert f VUndef fs));;;
                          invalidate_record_order a;;;
                          Ret (ρ, CNormal VUndef)
                end
            | _ => triggerUB
            end
        | _, _ => triggerUB
        end
    | IDelete base key =>
        t <- denote_ref base ρ;;
        bv <- read_target ρ t;;
        kv <- denote_expr key ρ;;
        match bv with
        | VAddr a =>
            o <- get_obj a;;
            match o with
            | OMap es =>
                es' <- (map_delete_partial kv es)?;;
                put_obj a (OMap es');;;
                Ret (ρ, CNormal VUndef)
            | _ => triggerUB     (* Obj.delete only supports maps *)
            end
        | _ => triggerUB
        end
    (* Syntax-directed dispatch (Interpreter.scala:177-192): resolve the
       target through the production chain, prepend the receiver AST as the
       first argument, and call.  Lexical receivers are dispatched to
       Scala-implemented value parsers (Interpreter.scala:192-193, 521-542);
       D-3 has the exporter precompute those, so a lexical receiver is a
       table lookup that returns immediately, with NO call frame — exactly
       ESMeta's [setCallResult]. *)
    | ISdoCall lhs base method args =>
        bv <- denote_expr base ρ;;
        match bv with
        | VAst origin root path =>
            match ast_focus root path with
            | None => triggerUB
            | Some a => match a with
            | ALex _ _ _ _ _ =>
                rv <- (ast_lex_sdo a method)?;;
                Ret (env_update lhs rv ρ, CNormal VUndef)
            | ASyn _ _ _ _ _ _ _ _ =>
                '(path0, fname) : list nat * string <-
                  (sdo_resolve_cursor fnames root path method)?;;
                vs <- denote_exprs args ρ;;
                rv <- ccallU (ir_sig fname) (nil, VAst origin root path0 :: vs);;
                Ret (env_update lhs rv ρ, CNormal VUndef)
            end
            end
        | _ => triggerUB
        end
    end.

  (** ** Function bodies *)

  Definition denote_fbody (fnames : list string) (f : func) (arg : ir_arg)
    : itree crisE val :=
    let '(captured, args) := arg in
    ρ0 <- (init_env (f_params f) args)?;;
    '(_, k) : env * completion
      <- denote_inst fnames (f_body f)
           (merge_captured_env ρ0 captured);;
    match k with
    | CReturn v => Ret v
    | CNormal _ => if f_main f then Ret VUndef else triggerUB
    end.

  (** Continuation entry differs at exactly one call-boundary rule:
      surplus arguments are ignored.  It otherwise executes the same
      [denote_inst] tree.  [merge_captured_env] implements ESMeta's
      [getLocals(...) ++ captured], including right-hand overwrite and
      duplicate collapse. *)
  Definition denote_cont_fbody
    (fnames : list string) (f : func) (arg : ir_arg) : itree crisE val :=
    let '(captured, args) := arg in
    let ρ0 := init_cont_env (f_params f) args in
    '(_, k) : env * completion
      <- denote_inst fnames (f_body f)
           (merge_captured_env ρ0 captured);;
    match k with
    | CReturn v => Ret v
    | CNormal _ => if f_main f then Ret VUndef else triggerUB
    end.

  (** ** CRIS module packaging *)

  Definition ir_mask : emask := msk_scp (mn :: nil) msk_true.

  Definition ir_fnsem (fnames : list string) (f : func)
    : fname * option (emask * (option fspec_rel * fbody)) :=
    (funid (f_name f),
     Some (ir_mask,
           (fsp_none, cfunU (fntyp ir_arg val) (denote_fbody fnames f)))).

  (** The distinguished [entry] function runs main with no captured
      environment and no arguments ([RF]: standalone-IR mains are
      nullary in all of tests/ir). *)
  Definition ir_entry (fnames : list string) (f : func)
    : fname * option (emask * (option fspec_rel * fbody)) :=
    (entry,
     Some (ir_mask,
           (fsp_none,
            cfunU (fntyp unit val)
              (fun _ => denote_fbody fnames f (nil, nil))))).

  Definition prog_fnames (p : prog) : list string :=
    List.map f_name (p_funcs p).

  Definition ir_fnsems (p : prog) : fnsemmap :=
    let fns := prog_fnames p in
    list_to_map
      (List.map (ir_fnsem fns) (p_funcs p) ++
       match List.find f_main (p_funcs p) with
       | Some f => ir_entry fns f :: nil
       | None => nil
       end).

  (** Runtime-only alternate entries for [VCont] invocation.  They are
      intentionally not installed in [ir_smod]: proof-facing module calls
      continue to use ordinary function semantics, while [ITreeExec.v]
      selects this map only after intercepting the private nonlocal-jump
      ABI. *)
  Definition ir_cont_fnsem (fnames : list string) (f : func)
    : fname * option (emask * (option fspec_rel * fbody)) :=
    (funid (f_name f),
     Some (ir_mask,
           (fsp_none,
            cfunU (fntyp ir_arg val) (denote_cont_fbody fnames f)))).

  Definition ir_cont_fnsems (p : prog) : fnsemmap :=
    let fns := prog_fnames p in
    list_to_map (List.map (ir_cont_fnsem fns) (p_funcs p)).

  (** The exported initial state (Initialize.scala:29-40) as store
      entries: one key per global and one per heap address.  Addresses are
      list positions (ADR-16), so the allocation counter starts at the
      exported heap's length and freshly allocated objects continue from
      there, exactly as ESMeta's [Heap.size] counter does. *)
  Definition glb_kvs (p : prog) : list (key * option Any.t) :=
    List.map (fun xv => (glb_key (fst xv), Some ((snd xv)↑))) (p_globals p).

  (* An unmapped slot simply has no key: [cgetU] on a missing key is UB,
     which is what ESMeta's UnknownAddr amounts to. *)
  Fixpoint mapped_slots (l : list (nat * option obj)) : list (nat * obj) :=
    match l with
    | nil => nil
    | (i, Some o) :: tl => (i, o) :: mapped_slots tl
    | (_, None) :: tl => mapped_slots tl
    end.

  Definition heap_kvs (p : prog) : list (key * option Any.t) :=
    List.map (fun ia => (heap_key (fst ia), Some ((snd ia)↑)))
      (mapped_slots (combine (seq 0 (List.length (p_heap p))) (p_heap p))).

  (** The exporter serializes each initial RecordObj in the exact key
      iteration order observed in ESMeta.  Mark every mapped initial
      address as trusted; only record addresses consult this key. *)
  Definition record_order_kvs (p : prog) : list (key * option Any.t) :=
    List.map (fun ia => (record_order_key (fst ia), Some (true↑)))
      (mapped_slots (combine (seq 0 (List.length (p_heap p))) (p_heap p))).

  Definition ir_initial_st (p : prog) : gmap key (option Any.t) :=
    <[ src_key := Some ((p_source p)↑) ]>
      (<[ cached_key := Some ((p_cached p)↑) ]>
         (<[ hosts_key := Some ((p_hosts p)↑) ]>
            (<[ alloc_key := Some ((List.length (p_heap p) : nat)↑) ]>
               (<[ ast_alloc_key := Some ((0 : nat)↑) ]>
                  (list_to_map
                    (glb_kvs p ++ heap_kvs p ++ record_order_kvs p)))))).


  (** Every key of the exported initial store lives in scope [mn]: the
      three fixed keys by construction, and the globals/heap keys because
      [glb_key]/[heap_key] build them that way. *)
  Lemma initial_st_scope (p : prog) (k : key) :
    k ∈ dom (ir_initial_st p) -> fst k = mn.
  Proof.
    unfold ir_initial_st. rewrite !dom_insert_L.
    rewrite !elem_of_union !elem_of_singleton. intros Hk.
    destruct Hk as [E|[E|[E|[E|[E|Hk]]]]];
      [ rewrite E; unfold src_key; reflexivity
      | rewrite E; unfold cached_key; reflexivity
      | rewrite E; unfold hosts_key; reflexivity
      | rewrite E; unfold alloc_key; reflexivity
      | rewrite E; unfold ast_alloc_key; reflexivity
      | ].
    apply elem_of_dom in Hk. destruct Hk as [v Hv].
    apply elem_of_list_to_map_2, elem_of_list_In in Hv.
    rewrite !in_app_iff in Hv.
    destruct Hv as [Hv|[Hv|Hv]]; apply in_map_iff in Hv;
      destruct Hv as (z & Heq & _); inversion Heq; reflexivity.
  Qed.

  (** The packaged CRIS module of an IR-Core program.  The well-formedness
      obligations hold for EVERY program because all function entries use
      the module-scoped mask [ir_mask] and the initial store is the
      singleton allocation counter. *)
  Program Definition ir_smod (p : prog) : SMod.t := {|
    SMod.scopes := mn :: nil;
    SMod.fnsems := ir_fnsems p;
    SMod.initial_st := ir_initial_st p;
  |}.
  Next Obligation.
  Proof. intros; repeat constructor. Qed.
  Next Obligation.
  Proof.
    intros p. apply map_Forall_lookup. intros fn mb H.
    rewrite lookup_omap in H.
    unfold ir_fnsems in H.
    destruct (list_to_map _ !! fn) as [[b|]|] eqn:Hf; simpl in H;
      simplify_eq.
    apply elem_of_list_to_map_2, elem_of_list_In in Hf.
    apply List.in_app_iff in Hf.
    destruct mb as [msk fb].
    assert (Hmask : msk = ir_mask).
    { destruct Hf as [Hf|Hf].
      - apply List.in_map_iff in Hf.
        destruct Hf as (f & Hf & _).
        unfold ir_fnsem in Hf. by simplify_eq.
      - destruct (List.find f_main (p_funcs p)) as [f0|]; simpl in Hf;
          [|by destruct Hf].
        destruct Hf as [Hf|Hf]; [|by destruct Hf].
        unfold ir_entry in Hf. by simplify_eq. }
    subst msk.
    split.
    - intros k' v' Hmsk. unfold ir_mask, msk_scp in Hmsk. cbn in Hmsk.
      by apply bool_decide_eq_true in Hmsk.
    - intros k' Hmsk. unfold ir_mask, msk_scp in Hmsk. cbn in Hmsk.
      by apply bool_decide_eq_true in Hmsk.
  Qed.
  Next Obligation.
  Proof.
    intros p x Hx. apply elem_of_map in Hx.
    destruct Hx as (k' & Hxk & Hk').
    apply initial_st_scope in Hk'. rewrite Hxk Hk'. set_solver.
  Qed.
  Next Obligation.
  Proof.
    intros p _. unfold ir_initial_st.
    repeat (apply map_Forall_insert_2; [by eexists|]).
    apply map_Forall_lookup. intros k v Hv.
    apply elem_of_list_to_map_2, elem_of_list_In in Hv.
    rewrite !in_app_iff in Hv.
    destruct Hv as [Hv|[Hv|Hv]]; apply in_map_iff in Hv;
      destruct Hv as (z & Heq & _); inversion Heq; by eexists.
  Qed.

  Definition ir_mod (p : prog) : Mod.t := SMod.to_mod ∅ (ir_smod p).

  (** ** First denotation facts (PO-004: completion preservation)

      [ISeq] denotation unfolds one instruction at a time... *)

  Lemma denote_seq_cons (fnames : list string) (i : inst)
      (rest : list inst) (ρ : env) :
    denote_inst fnames (ISeq (i :: rest)) ρ =
    '(ρ1, k) : env * completion <- denote_inst fnames i ρ;;
    match k with
    | CNormal _ => denote_inst fnames (ISeq rest) ρ1
    | CReturn v => Ret (ρ1, CReturn v)
    end.
  Proof. reflexivity. Qed.

  (** ... and an early [CReturn] short-circuits the remaining
      instructions: nothing after a returning instruction is executed,
      mirroring ESMeta's [retVal]-then-[ExitCursor] discipline. *)

  Lemma denote_seq_return_shortcircuit (fnames : list string)
      (i : inst) (rest : list inst) (ρ ρ1 : env) (v : val)
      (H : denote_inst fnames i ρ = Ret (ρ1, CReturn v)) :
    denote_inst fnames (ISeq (i :: rest)) ρ = Ret (ρ1, CReturn v).
  Proof. rewrite denote_seq_cons H bind_ret_l. reflexivity. Qed.

End DENOTE.
