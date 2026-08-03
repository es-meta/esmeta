(** * ESMetaFV.Exec — executable reference interpreter for IR-Core

    A fuel-based, stdlib-only interpreter that mirrors the ITree
    denotation ([Semantics.v]) clause by clause:

      Semantics.v                      Exec.v
      -----------                      ------
      itree crisE                      out (fuel monad: Ok/Stuck/OOF)
      triggerUB                        Stuck
      cgetU/cput keyed store           x_heap / x_globals in xstate
      log_val (IO event)               x_out append
      ccallU (callE)                   exec_call via the program table
      ITree.iter (IWhile)              fuel-indexed recursion

    ROLE AND TRUST STATUS.  This interpreter exists for VALIDATION
    (PO-011): it is vm_compute-executable inside Rocq, so corpus programs
    can be run and compared against ESMeta's interpreter.  Its agreement
    with the ITree denotation is currently an ENGINEERING ASSUMPTION
    established by clause-by-clause parallelism (each clause below cites
    its Semantics.v counterpart implicitly by identical structure); a
    formal correspondence proof is PO-013 (deferred).  Nothing in the M4
    equivalence theorem depends on this file. *)

From Stdlib Require Import String ZArith List Bool.
Import ListNotations.

From ESMetaFV Require Import Fragment Domain.

Set Implicit Arguments.

Local Open Scope string_scope.

(** ** Outcome monad *)

(** [Stuck] carries a REASON.  A run of the whole specification that ends
    stuck is otherwise undiagnosable: the earlier way to find out why was
    to instrument ESMeta and compare reachable-function sets, which only
    ever finds missing functions, never undefined behaviour inside one that
    is present.  The string is diagnostic only — every [Stuck _] is equally
    undefined behaviour, and no semantics branches on it. *)
Variant out (A : Type) : Type :=
| Ok (a : A)          (* successful execution *)
| Stuck (why : string)(* undefined behavior / interpreter failure *)
| OOF.                (* out of fuel — inconclusive, raise the fuel bound *)
Arguments Ok {A} a.
Arguments Stuck {A} why.
Arguments OOF {A}.

Definition obind {A B} (m : out A) (k : A -> out B) : out B :=
  match m with
  | Ok a => k a
  | Stuck w => Stuck w
  | OOF => OOF
  end.

Definition of_option {A} (why : string) (m : option A) : out A :=
  match m with
  | Some a => Ok a
  | None => Stuck why
  end.

Declare Scope exec_scope.
Notation "x <- m ;; k" := (obind m (fun x => k))
  (at level 62, m at level 61, right associativity) : exec_scope.
Notation "' pat <- m ;; k" := (obind m (fun pat => k))
  (at level 62, pat pattern at level 0, m at level 61,
   right associativity) : exec_scope.
Local Open Scope exec_scope.

(** ** Execution state

    Heap addresses are list indices; allocation appends, so addresses are
    assigned by a deterministic counter and never reused, exactly like
    ESMeta (Heap.scala:62-67) and the [alloc$] counter of the denotation.
    [x_out] is the print log in program order — the executable image of
    the [IO "esmeta.print"] trace events. *)

Record xstate : Type := mkXState {
  x_heap : list (option obj);
  x_globals : list (string * val);
  x_out : list val;
  (* immutable run parameters, mirroring State.scala:17-18 *)
  x_source : option cstr;
  x_cached : option ast;
  x_hosts : list host_cache_entry;
  x_ast_next : nat;
}.

(** The exported initial state (Initialize.scala:29-40): heap objects at
    their exported addresses, initial globals, empty print log. *)
Definition init_xstate (p : prog) : xstate :=
  mkXState
    (p_heap p) (p_globals p) nil
    (p_source p) (p_cached p) (p_hosts p) 0.

(* [None] both for an out-of-range address and for a slot that exists but
   is unmapped; ESMeta throws UnknownAddr for the latter (Heap.scala:19). *)
Definition heap_get (st : xstate) (a : nat) : option obj :=
  match nth_error (x_heap st) a with
  | Some (Some o) => Some o
  | _ => None
  end.

(** Interpret the same finite recursive type-check plan that
    [Semantics.v] runs through CRIS store reads. *)
Fixpoint run_heap_query_cached_x {A : Type}
  (st : xstate) (cache : list (nat * obj)) (query : heap_query A)
  : out A :=
  match query with
  | HeapDone result => Ok result
  | HeapRead address continue_with =>
      match resolved_lookup cache address with
      | Some object =>
          run_heap_query_cached_x st cache (continue_with object)
      | None =>
          object <- of_option "ETypeCheck(heap)" (heap_get st address);;
          run_heap_query_cached_x st
            ((address, object) :: cache) (continue_with object)
      end
  end.

Definition run_heap_query_x {A : Type}
  (st : xstate) (query : heap_query A) : out A :=
  run_heap_query_cached_x st nil query.

Definition heap_set (st : xstate) (a : nat) (o : obj) : option xstate :=
  option_map
    (fun h =>
       mkXState h (x_globals st) (x_out st)
         (x_source st) (x_cached st) (x_hosts st) (x_ast_next st))
    (list_update a (Some o) (x_heap st)).

Definition heap_alloc (st : xstate) (o : obj) : xstate * nat :=
  (mkXState (x_heap st ++ [Some o]) (x_globals st) (x_out st)
     (x_source st) (x_cached st) (x_hosts st) (x_ast_next st),
   List.length (x_heap st)).

Definition globals_set (st : xstate) (x : string) (v : val) : xstate :=
  mkXState (x_heap st) (fields_insert x v (x_globals st)) (x_out st)
    (x_source st) (x_cached st) (x_hosts st) (x_ast_next st).

Definition out_print (st : xstate) (v : val) : xstate :=
  mkXState (x_heap st) (x_globals st) (x_out st ++ [v])
    (x_source st) (x_cached st) (x_hosts st) (x_ast_next st).

Definition ast_origin_alloc_x (st : xstate) : xstate * ast_origin :=
  (mkXState (x_heap st) (x_globals st) (x_out st)
     (x_source st) (x_cached st) (x_hosts st) (S (x_ast_next st)),
   AstRuntime (x_ast_next st)).

(** ** Reference targets (mirrors Semantics.v [ref_target]) *)

Variant xtarget : Type :=
| XVar (x : var)
| XField (base : val) (field : val).

Definition read_target_x (st : xstate) (ρ : env) (t : xtarget) : out val :=
  match t with
  | XVar (VLocal l) => of_option "XVar" (env_lookup ρ l)
  | XVar (VGlobal x) => of_option "XVar" (fields_lookup (x_globals st) x)
  | XField (VAddr a) k =>
      o <- of_option "XField" (heap_get st a);;
      match o, k with
      | ORecord _ fs, VStr cs =>
          fld <- of_option "XField" (ascii_of_cstr cs);;
          of_option "XField" (fields_lookup fs fld)
      | OList vs, VMath i =>
          if (0 <=? i)%Z then of_option "XField" (nth_error vs (Z.to_nat i)) else Stuck "XField"
      | OMap es, _ =>
          found <- of_option "XField(map-key-equality)" (map_lookup_partial es k);;
          of_option "XField" found
      | _, _ => Stuck "XField"
      end
  | XField (VAst origin root path) field =>
      cursor <- of_option "XField(ast-field)"
        (ast_cursor_field_get root path field);;
      let '(next_root, next_path) := cursor in
      Ok (VAst origin next_root next_path)
  (* String indexing -> code unit (State.scala:57-59). *)
  | XField (VStr cs) (VMath i) =>
      let index := scala_to_int32 i in
      if (0 <=? index)%Z
      then c <- of_option "XField" (nth_error cs (Z.to_nat index));;
           Ok (VCodeUnit c)
      else Stuck "XField"
  | _ => Stuck "XField"
  end.

(** State-preserving catchable failures used only for [EParse] operands.
    Ordinary model gaps still use [Stuck]; [EvalThrow] denotes an exception
    that Scala's local parse [try/catch] observes. *)
Definition x_eval_ret {A : Type} (st : xstate) (value : A)
  : out (xstate * eval_result A) :=
  Ok (st, EvalValue value).

Definition x_eval_throw {A : Type} (st : xstate)
  : out (xstate * eval_result A) :=
  Ok (st, EvalThrow).

Definition x_eval_bind {A B : Type}
  (result : out (xstate * eval_result A))
  (continue_with : xstate -> A -> out (xstate * eval_result B))
  : out (xstate * eval_result B) :=
  match result with
  | Ok (st, EvalValue value) => continue_with st value
  | Ok (st, EvalThrow) => x_eval_throw st
  | Stuck reason => Stuck reason
  | OOF => OOF
  end.

Definition x_eval_of_option {A : Type}
  (st : xstate) (value : option A) : out (xstate * eval_result A) :=
  match value with
  | Some result => x_eval_ret st result
  | None => x_eval_throw st
  end.

Definition x_eval_read_target
  (st : xstate) (ρ : env) (target : xtarget)
  : out (xstate * eval_result val) :=
  match target with
  | XVar (VLocal local) => x_eval_of_option st (env_lookup ρ local)
  | XVar (VGlobal global) =>
      x_eval_of_option st (fields_lookup (x_globals st) global)
  | XField (VAddr address) field =>
      x_eval_bind (x_eval_of_option st (heap_get st address))
        (fun st1 object =>
          match object, field with
          | ORecord _ fields, VStr code_units =>
              x_eval_bind (x_eval_of_option st1 (ascii_of_cstr code_units))
                (fun st2 name =>
                  x_eval_of_option st2 (fields_lookup fields name))
          | OList values, VMath index =>
              if (0 <=? index)%Z
              then x_eval_of_option st1 (nth_error values (Z.to_nat index))
              else x_eval_throw st1
          | OMap entries, _ =>
              match map_lookup_partial entries field with
              | Some found => x_eval_of_option st1 found
              | None => Stuck "EParse(map-key-equality)"
              end
          | _, _ => x_eval_throw st1
          end)
  | XField (VAst origin root path) field =>
      x_eval_bind
        (x_eval_of_option st (ast_cursor_field_get root path field))
        (fun st1 cursor =>
          let '(next_root, next_path) := cursor in
          x_eval_ret st1 (VAst origin next_root next_path))
  | XField (VStr code_units) (VMath index) =>
      let wrapped_index := scala_to_int32 index in
      if (0 <=? wrapped_index)%Z
      then
        x_eval_bind
          (x_eval_of_option st
            (nth_error code_units (Z.to_nat wrapped_index)))
          (fun st1 code_unit => x_eval_ret st1 (VCodeUnit code_unit))
      else x_eval_throw st
  | _ => x_eval_throw st
  end.

Fixpoint exec_parse_operand
  (st : xstate) (ρ : env) (e : expr) {struct e}
  : out (xstate * eval_result val) :=
  match e with
  | EMath integer => x_eval_ret st (VMath integer)
  | EBool boolean => x_eval_ret st (VBool boolean)
  | EStr string_value => x_eval_ret st (VStr string_value)
  | EUndef => x_eval_ret st VUndef
  | ENull => x_eval_ret st VNull
  | EEnum name => x_eval_ret st (VEnum name)
  | ENumber number => x_eval_ret st (VNumber number)
  | EBigInt integer => x_eval_ret st (VBigInt integer)
  | EInfinity positive => x_eval_ret st (VInfinity positive)
  | ECodeUnit code_unit => x_eval_ret st (VCodeUnit code_unit)
  | EGrammarSymbol name params =>
      x_eval_ret st (VGrammarSymbol name params)
  | EYet _ => x_eval_throw st
  | ERef reference =>
      x_eval_bind (exec_parse_ref st ρ reference)
        (fun st1 target => x_eval_read_target st1 ρ target)
  | EList expressions =>
      x_eval_bind
        ((fix go (rest : list expr) (st0 : xstate)
            : out (xstate * eval_result (list val)) :=
            match rest with
            | nil => x_eval_ret st0 nil
            | head :: tail =>
                x_eval_bind (exec_parse_operand st0 ρ head)
                  (fun st1 value =>
                    x_eval_bind (go tail st1)
                      (fun st2 values =>
                        x_eval_ret st2 (value :: values)))
            end) expressions st)
        (fun st1 values =>
          let '(st2, address) := heap_alloc st1 (OList values) in
          x_eval_ret st2 (VAddr address))
  | ESourceText inner =>
      x_eval_bind (exec_parse_operand st ρ inner) (fun st1 value =>
        match value with
        | VAst _ root path =>
            x_eval_bind (x_eval_of_option st1 (ast_focus root path))
              (fun st2 ast => x_eval_ret st2 (VStr (ast_src ast)))
        | _ => x_eval_throw st1
        end)
  | _ => Stuck "EParse(unsupported-operand)"
  end

with exec_parse_ref
  (st : xstate) (ρ : env) (r : ref) {struct r}
  : out (xstate * eval_result xtarget) :=
  match r with
  | RVar variable => x_eval_ret st (XVar variable)
  | RField base field =>
      x_eval_bind (exec_parse_ref st ρ base) (fun st1 base_target =>
        x_eval_bind (x_eval_read_target st1 ρ base_target)
          (fun st2 base_value =>
            x_eval_bind (exec_parse_operand st2 ρ field)
              (fun st3 field_value =>
                x_eval_ret st3 (XField base_value field_value))))
  end.

Definition x_alloc_parse_errors (st : xstate) : out (xstate * val) :=
  let '(next, address) := heap_alloc st (OList nil) in
  Ok (next, VAddr address).

Definition write_target_x (st : xstate) (ρ : env) (t : xtarget) (v : val)
  : out (xstate * env) :=
  match t with
  | XVar (VLocal l) => Ok (st, env_update l v ρ)
  | XVar (VGlobal x) => Ok (globals_set st x v, ρ)
  | XField (VAddr a) k =>
      o <- of_option "XField" (heap_get st a);;
      match o, k with
      | ORecord tn fs, VStr cs =>
          fld <- of_option "XField" (ascii_of_cstr cs);;
          st' <- of_option "XField" (heap_set st a (ORecord tn (fields_insert fld v fs)));;
          Ok (st', ρ)
      | OList vs, VMath i =>
          if (0 <=? i)%Z
          then
            vs' <- of_option "XField" (list_update (Z.to_nat i) v vs);;
            st' <- of_option "XField" (heap_set st a (OList vs'));;
            Ok (st', ρ)
          else Stuck "XField"
      | OMap es, _ =>
          es' <- of_option "XField(map-key-equality)"
            (map_insert_partial k v es);;
          st' <- of_option "XField" (heap_set st a (OMap es'));;
          Ok (st', ρ)
      | _, _ => Stuck "XField"
      end
  | _ => Stuck "XField"
  end.

Fixpoint capture_x (ρ : env) (xs : list string)
  : out (list (string * val)) :=
  match xs with
  | nil => Ok nil
  | x :: tl =>
      v <- of_option "XField" (env_lookup ρ (LName x));;
      cs <- capture_x ρ tl;;
      Ok (captured_normalize ((x, v) :: cs))
  end.

Definition exec_cop_value_x (st : xstate) (op : cop) (v : val) : out val :=
  match host_cop_query op v with
  | Some query =>
      match typed_host_cache_lookup query (x_hosts st) with
      | Some result => Ok result
      | None => Stuck "EConvert(host)"
      end
  | None => of_option "EConvert" (eval_cop op v)
  end.

Definition exec_prepared_math_operand_x
  (st : xstate) (operand : prepared_number_math_operand) : out val :=
  match operand with
  | PNMOFiniteNumber f => exec_cop_value_x st CToMath (VNumber f)
  | PNMOMath v => Ok v
  end.

Definition exec_number_math_values_x
  (st : xstate) (op : number_math_op) (math_op : bop) (result_op : cop)
  (left right : prepared_number_math_operand) : out val :=
  match left, right with
  | PNMOFiniteNumber lf, PNMOFiniteNumber rf =>
      match typed_host_cache_lookup (HQNumberMathOp op lf rf) (x_hosts st) with
      | Some result => Ok result
      | None => Stuck "EConvert(number-math-host)"
      end
  | _, _ =>
      lm <- exec_prepared_math_operand_x st left;;
      rm <- exec_prepared_math_operand_x st right;;
      result <- of_option "EBinary(number-math-fallback)"
        (eval_bop math_op lm rm);;
      exec_cop_value_x st result_op result
  end.

Definition exec_number_sin_value_x (st : xstate) (v : val) : out val :=
  match v with
  | VNumber f =>
      if PrimFloat.is_finite f then
        match typed_host_cache_lookup (HQNumberSin f) (x_hosts st) with
        | Some result => Ok result
        | None => Stuck "EConvert(number-sin-host)"
        end
      else Stuck "EConvert(number-sin-input)"
  | _ =>
      mv <- exec_cop_value_x st CToMath v;;
      query <- of_option "EMathOp(arguments)" (host_mathop_query MSin [mv]);;
      math_result <-
        match typed_host_cache_lookup query (x_hosts st) with
        | Some result => Ok result
        | None => Stuck "EMathOp(host)"
        end;;
      exec_cop_value_x st CToApproxNumber math_result
  end.

Definition exec_number_math_comparison_x
  (st : xstate) (op : bop)
  (left right : prepared_number_math_operand) : out val :=
  match number_math_comparison_query op left right with
  | Some query =>
      match typed_host_cache_lookup query (x_hosts st) with
      | Some result => Ok result
      | None => Stuck "EBinary(number-math-comparison-host)"
      end
  | None => of_option "EBinary(number-math-comparison)"
      (eval_number_math_comparison_pure op left right)
  end.

(** ** Expression evaluation (structural; expressions contain no calls) *)

Fixpoint exec_expr (st : xstate) (ρ : env) (e : expr) {struct e}
  : out (xstate * val) :=
  match e with
  | EMath z => Ok (st, VMath z)
  | EBool b => Ok (st, VBool b)
  | EStr s => Ok (st, VStr s)
  | EUndef => Ok (st, VUndef)
  | ENull => Ok (st, VNull)
  | EEnum n => Ok (st, VEnum n)
  | ERef r =>
      '(st1, t) <- exec_ref st ρ r;;
      v <- read_target_x st1 ρ t;;
      Ok (st1, v)
  | EUnary op e1 =>
      '(st1, v) <- exec_expr st ρ e1;;
      r <- of_option "EUnary" (eval_uop op v);;
      Ok (st1, r)
  | EBinary BAnd e1 e2 =>
      '(st1, v1) <- exec_expr st ρ e1;;
      match v1 with
      | VBool false => Ok (st1, VBool false)
      | VBool true =>
          '(st2, v2) <- exec_expr st1 ρ e2;;
          match v2 with
          | VBool b => Ok (st2, VBool b)
          | _ => Stuck "EBinary"
          end
      | _ => Stuck "EBinary"
      end
  | EBinary BOr e1 e2 =>
      '(st1, v1) <- exec_expr st ρ e1;;
      match v1 with
      | VBool true => Ok (st1, VBool true)
      | VBool false =>
          '(st2, v2) <- exec_expr st1 ρ e2;;
          match v2 with
          | VBool b => Ok (st2, VBool b)
          | _ => Stuck "EBinary"
          end
      | _ => Stuck "EBinary"
      end
  | EBinary (BLt as op) (EConvert CToMath lhs) (EConvert CToMath rhs)
  | EBinary (BEqual as op) (EConvert CToMath lhs) (EConvert CToMath rhs) =>
      '(st1, lv) <- exec_expr st ρ lhs;;
      lp <- of_option "EConvert(number-compare-left)"
        (prepare_number_math_operand lv);;
      '(st2, rv) <- exec_expr st1 ρ rhs;;
      rp <- of_option "EConvert(number-compare-right)"
        (prepare_number_math_operand rv);;
      result <- exec_number_math_comparison_x st2 op lp rp;;
      Ok (st2, result)
  | EBinary op e1 e2 =>
      '(st1, v1) <- exec_expr st ρ e1;;
      '(st2, v2) <- exec_expr st1 ρ e2;;
      match host_bop_query op v1 v2 with
      | Some query =>
          match typed_host_cache_lookup query (x_hosts st2) with
          | Some result => Ok (st2, result)
          | _ => Stuck "EBinary(host-pow)"
          end
      | None =>
          r <- of_option "EBinary" (eval_bop op v1 v2);;
          Ok (st2, r)
      end
  | EClo fn captured =>
      cs <- capture_x ρ captured;;
      Ok (st, VClo fn cs)
  | ECont _ => Stuck "ECont(control-only)"
  | EList es =>
      '(st1, vs) <-
        ((fix go (l : list expr) (st0 : xstate)
            : out (xstate * list val) :=
            match l with
            | nil => Ok (st0, nil)
            | e1 :: tl =>
                '(st1, v) <- exec_expr st0 ρ e1;;
                '(st2, vs) <- go tl st1;;
                Ok (st2, v :: vs)
            end) es st);;
      let '(st2, a) := heap_alloc st1 (OList vs) in
      Ok (st2, VAddr a)
  | ESizeOf e1 =>
      '(st1, v) <- exec_expr st ρ e1;;
      match v with
      | VStr cs => Ok (st1, VMath (Z.of_nat (List.length cs)))
      | VAddr a =>
          o <- of_option "ESizeOf" (heap_get st1 a);;
          n <- of_option "ESizeOf" (obj_size o);;
          Ok (st1, VMath (Z.of_nat n))
      | VAst _ root path =>
          a <- of_option "ESizeOf(ast-cursor)" (ast_focus root path);;
          Ok (st1, VMath (Z.of_nat (List.length (ast_children a))))
      | _ => Stuck "ESizeOf"
      end
  | ERecord tname fields =>
      '(st1, fs) <-
        ((fix go (l : list (string * expr)) (st0 : xstate)
                 (acc : list (string * val))
            : out (xstate * list (string * val)) :=
            match l with
            | nil => Ok (st0, acc)
            | (f, e1) :: tl =>
                '(st1, v) <- exec_expr st0 ρ e1;;
                go tl st1 (fields_insert f v acc)
            end) fields st nil);;
      let '(st2, a) := heap_alloc st1 (ORecord tname fs) in
      Ok (st2, VAddr a)
  | EExists r =>
      '(st1, t) <- exec_ref st ρ r;;
      match t with
      | XVar (VLocal l) =>
          Ok (st1, VBool (match env_lookup ρ l with Some _ => true | None => false end))
      | XVar (VGlobal _) => Stuck "XVar"
      | XField (VAddr a) k =>
          o <- of_option "XField" (heap_get st1 a);;
          match o, k with
          | ORecord _ fs, VStr cs =>
              fld <- of_option "XField" (ascii_of_cstr cs);;
              Ok (st1, VBool (match fields_lookup fs fld with
                              | Some _ => true | None => false end))
          | OMap es, _ =>
              found <- of_option "XField(map-key-equality)"
                (map_lookup_partial es k);;
              Ok (st1, VBool (match found with
                              | Some _ => true | None => false end))
          | OList vs, VMath i =>
              Ok (st1, VBool (andb (0 <=? i)%Z
                                (Nat.ltb (Z.to_nat i) (List.length vs))))
          | _, _ => Stuck "XField"
          end
      | XField (VAst _ root path) field =>
          Ok (st1, VBool (ast_cursor_field_exists root path field))
      | _ => Stuck "XField"
      end
  | ETypeOf e1 =>
      '(st1, v) <- exec_expr st ρ e1;;
      match v with
      | VAddr a =>
          o <- of_option "ETypeOf" (heap_get st1 a);;
          Ok (st1, VStr (cu (typeof_obj o)))
      | _ =>
          s0 <- of_option "ETypeOf" (typeof_prim v);;
          Ok (st1, VStr (cu s0))
      end
  | ETypeCheck e1 t =>
      '(st1, v) <- exec_expr st ρ e1;;
      decision <-
        run_heap_query_x st1 (ty_check_query type_check_fuel t v);;
      b <- of_option "ETypeCheck(record-refinement)" decision;;
      Ok (st1, VBool b)
  | EYet _ => Stuck "EYet"
  | EMap pairs =>
      '(st1, es) <-
        ((fix go (l : list (expr * expr)) (st0 : xstate)
                 (acc : list (val * val))
            : out (xstate * list (val * val)) :=
            match l with
            | nil => Ok (st0, acc)
            | (ke, ve) :: tl =>
                '(st1, kv) <- exec_expr st0 ρ ke;;
                '(st2, vv) <- exec_expr st1 ρ ve;;
                acc' <- of_option "EMap(map-key-equality)"
                  (map_insert_partial kv vv acc);;
                go tl st2 acc'
            end) pairs st nil);;
      let '(st2, a) := heap_alloc st1 (OMap es) in
      Ok (st2, VAddr a)
  | EKeys m intSorted =>
      '(st1, v) <- exec_expr st ρ m;;
      match v with
      | VAddr a =>
          o <- of_option "EKeys" (heap_get st1 a);;
          ks <-
            match o, intSorted with
            | ORecord _ fields, _ =>
                match fields with
                | nil => of_option "EKeys" (obj_keys o)
                | _ :: nil => of_option "EKeys" (obj_keys o)
                | _ => Stuck "EKeys(record-order)"
                end
            | OMap entries, true =>
                of_option "EKeys(integer-sorted)"
                  (obj_integer_sorted_keys (x_hosts st1) entries)
            | _, _ => of_option "EKeys" (obj_keys o)
            end;;
          let '(st2, a2) := heap_alloc st1 (OList ks) in
          Ok (st2, VAddr a2)
      | _ => Stuck "EKeys"
      end
  | ECopy e1 =>
      '(st1, v) <- exec_expr st ρ e1;;
      match v with
      | VAddr a =>
          o <- of_option "ECopy" (heap_get st1 a);;
          let '(st2, a2) := heap_alloc st1 o in
          Ok (st2, VAddr a2)
      | _ => Stuck "ECopy"
      end
  | ENumber f => Ok (st, VNumber f)
  | EBigInt z => Ok (st, VBigInt z)
  | EInfinity p => Ok (st, VInfinity p)
  | ECodeUnit c => Ok (st, VCodeUnit c)
  | EConvert CToNumber
      (EBinary BAdd (EConvert CToMath lhs) (EConvert CToMath rhs)) =>
      '(st1, lv) <- exec_expr st ρ lhs;;
      lp <- of_option "EConvert(number-math-left)"
        (prepare_number_math_operand lv);;
      '(st2, rv) <- exec_expr st1 ρ rhs;;
      rp <- of_option "EConvert(number-math-right)"
        (prepare_number_math_operand rv);;
      result <- exec_number_math_values_x st2 NMAdd BAdd CToNumber lp rp;;
      Ok (st2, result)
  | EConvert CToNumber
      (EBinary BMul (EConvert CToMath lhs) (EConvert CToMath rhs)) =>
      '(st1, lv) <- exec_expr st ρ lhs;;
      lp <- of_option "EConvert(number-math-left)"
        (prepare_number_math_operand lv);;
      '(st2, rv) <- exec_expr st1 ρ rhs;;
      rp <- of_option "EConvert(number-math-right)"
        (prepare_number_math_operand rv);;
      result <- exec_number_math_values_x st2 NMMul BMul CToNumber lp rp;;
      Ok (st2, result)
  | EConvert CToNumber
      (EBinary BDiv (EConvert CToMath lhs) (EConvert CToMath rhs)) =>
      '(st1, lv) <- exec_expr st ρ lhs;;
      lp <- of_option "EConvert(number-math-left)"
        (prepare_number_math_operand lv);;
      '(st2, rv) <- exec_expr st1 ρ rhs;;
      rp <- of_option "EConvert(number-math-right)"
        (prepare_number_math_operand rv);;
      result <- exec_number_math_values_x st2 NMDiv BDiv CToNumber lp rp;;
      Ok (st2, result)
  | EConvert CToApproxNumber
      (EBinary BPow (EConvert CToMath lhs) (EConvert CToMath rhs)) =>
      '(st1, lv) <- exec_expr st ρ lhs;;
      lp <- of_option "EConvert(number-math-left)"
        (prepare_number_math_operand lv);;
      '(st2, rv) <- exec_expr st1 ρ rhs;;
      rp <- of_option "EConvert(number-math-right)"
        (prepare_number_math_operand rv);;
      result <- exec_number_math_values_x st2 NMPow BPow CToApproxNumber lp rp;;
      Ok (st2, result)
  | EConvert CToApproxNumber
      (EMathOp MSin [EConvert CToMath inner]) =>
      '(st1, v) <- exec_expr st ρ inner;;
      result <- exec_number_sin_value_x st1 v;;
      Ok (st1, result)
  | EConvert op e1 =>
      '(st1, v) <- exec_expr st ρ e1;;
      result <- exec_cop_value_x st1 op v;;
      Ok (st1, result)
  | EToStr e1 radix =>
      '(st1, v) <- exec_expr st ρ e1;;
      match v with
      | VStr cs => Ok (st1, VStr cs)
      | VNumber _ =>
          '(st2, z) <-
            match radix with
            | None => Ok (st1, 10%Z)
            | Some e2 =>
                '(st2, rv) <- exec_expr st1 ρ e2;;
                match rv with
                | VMath z => Ok (st2, z)
                | _ => Stuck "EToStr(radix)"
                end
            end;;
          match typed_host_cache_lookup (HQToStr v z) (x_hosts st2) with
          | Some (VStr cs) => Ok (st2, VStr cs)
          | _ => Stuck "EToStr(host)"
          end
      | VBigInt _ =>
          '(st2, z) <-
            match radix with
            | None => Ok (st1, 10%Z)
            | Some e2 =>
                '(st2, rv) <- exec_expr st1 ρ e2;;
                match rv with
                | VMath z => Ok (st2, z)
                | _ => Stuck "EToStr(radix)"
                end
            end;;
          match typed_host_cache_lookup (HQToStr v z) (x_hosts st2) with
          | Some (VStr cs) => Ok (st2, VStr cs)
          | _ => Stuck "EToStr(host)"
          end
      | _ => Stuck "EToStr(input)"
      end
  | EVariadic op es =>
      '(st1, vs) <-
        ((fix go (l : list expr) (st0 : xstate) : out (xstate * list val) :=
            match l with
            | nil => Ok (st0, nil)
            | e1 :: tl =>
                '(sta, v) <- exec_expr st0 ρ e1;;
                '(stb, vs) <- go tl sta;;
                Ok (stb, v :: vs)
            end) es st);;
      r <- of_option "EVariadic" (eval_vop op vs);;
      Ok (st1, r)
  | EMathOp op args =>
      '(st1, vs) <-
        ((fix go (l : list expr) (st0 : xstate) : out (xstate * list val) :=
            match l with
            | nil => Ok (st0, nil)
            | e1 :: tl =>
                '(sta, v) <- exec_expr st0 ρ e1;;
                '(stb, rest) <- go tl sta;;
                Ok (stb, v :: rest)
            end) args st);;
      query <- of_option "EMathOp(arguments)" (host_mathop_query op vs);;
      match typed_host_cache_lookup query (x_hosts st1) with
      | Some result => Ok (st1, result)
      | None => Stuck "EMathOp(host)"
      end
  | EContains lst e1 =>
      '(st1, lv) <- exec_expr st ρ lst;;
      '(st2, ev) <- exec_expr st1 ρ e1;;
      match lv with
      | VAddr a =>
          o <- of_option "EContains" (heap_get st2 a);;
          match o with
          | OList vs =>
              contained <- of_option "EContains(value-equality)"
                (vals_contains_partial ev vs);;
              Ok (st2, VBool contained)
          | _ => Stuck "EContains"
          end
      | _ => Stuck "EContains"
      end
  | ETrim e1 isStarting =>
      '(st1, v) <- exec_expr st ρ e1;;
      match v with
      | VStr cs => Ok (st1, VStr (cstr_trim cs isStarting))
      | _ => Stuck "ETrim"
      end
  | ESyntactic nm args rhsIdx subIdx children child_names source_layout =>
      '(st1, cs) <-
        ((fix go (l : list (option expr)) (st0 : xstate)
            : out (xstate * list (option ast)) :=
            match l with
            | nil => Ok (st0, nil)
            | None :: tl =>
                '(st1, rest) <- go tl st0;;
                Ok (st1, None :: rest)
            | Some e1 :: tl =>
                '(st1, v) <- exec_expr st0 ρ e1;;
                match v with
                | VAst _ a _ =>
                    '(st2, rest) <- go tl st1;;
                    Ok (st2, Some a :: rest)
                | _ => Stuck "ESyntactic(child)"
                end
            end) children st);;
      if existsb (fun c => match c with Some _ => true | None => false end) cs
      then Stuck "ESyntactic(parent-alias)"
      else
        parse_src <- of_option "ESyntactic(source)"
          (render_syn_source_raw source_layout cs);;
        let '(st2, origin) := ast_origin_alloc_x st1 in
        Ok
          (st2,
            VAst
              origin
              (ASyn nm args rhsIdx subIdx cs child_names
                (cstr_java_trim parse_src) parse_src)
              nil)
  | EGrammarSymbol nm ps => Ok (st, VGrammarSymbol nm ps)
  | EInstanceOf e1 tgt =>
      '(st1, v) <- exec_expr st ρ e1;;
      '(st2, t) <- exec_expr st1 ρ tgt;;
      Ok (st2, eval_instanceof v t)
  | ESourceText e1 =>
      '(st1, v) <- exec_expr st ρ e1;;
      match v with
      | VAst _ root path =>
          a <- of_option "ESourceText(ast-cursor)" (ast_focus root path);;
          Ok (st1, VStr (ast_src a))
      | _ => Stuck "ESourceText"
      end
  (* Exact exported parser results extend the initial Script fast path.
     A cache miss remains [Stuck] rather than inventing a parse. *)
  | EParse code rule =>
      match exec_parse_operand st ρ code with
      | Ok (st1, EvalThrow) => x_alloc_parse_errors st1
      | Ok (st1, EvalValue cv) =>
          match exec_parse_operand st1 ρ rule with
          | Ok (st2, EvalThrow) => x_alloc_parse_errors st2
          | Ok (st2, EvalValue rv) =>
              match rv with
              | VGrammarSymbol _ _ =>
                  match cv, rv, x_source st2, x_cached st2 with
                  | VStr x, VGrammarSymbol nm nil, Some y, Some a =>
                      if andb (String.eqb nm "Script") (cstr_eqb x y)
                      then Ok (st2, VAst (AstExported 0) a nil)
                      else
                        match host_parse_query cv rv with
                        | Some query =>
                            match
                              typed_host_cache_lookup query (x_hosts st2)
                            with
                            | Some (VAst _ root path) =>
                                let '(st3, origin) :=
                                  ast_origin_alloc_x st2 in
                                Ok (st3, VAst origin root path)
                            | Some VUndef => x_alloc_parse_errors st2
                            | Some _ => Stuck "EParse(host-result)"
                            | None => Stuck "EParse(host-cache-miss)"
                            end
                        | None => x_alloc_parse_errors st2
                        end
                  | _, _, _, _ =>
                      match host_parse_query cv rv with
                      | Some query =>
                          match typed_host_cache_lookup query (x_hosts st2) with
                          | Some (VAst _ root path) =>
                              let '(st3, origin) := ast_origin_alloc_x st2 in
                              Ok (st3, VAst origin root path)
                          | Some VUndef => x_alloc_parse_errors st2
                          | Some _ => Stuck "EParse(host-result)"
                          | None => Stuck "EParse(host-cache-miss)"
                          end
                      | None => x_alloc_parse_errors st2
                      end
                  end
              | _ => x_alloc_parse_errors st2
              end
          | Stuck reason => Stuck reason
          | OOF => OOF
          end
      | Stuck reason => Stuck reason
      | OOF => OOF
      end
  | ESubstring e1 from to =>
      '(st1, sv) <- exec_expr st ρ e1;;
      '(st2, fv) <- exec_expr st1 ρ from;;
      match to with
      | None =>
          r <- of_option "ESubstring" (eval_substring sv fv None);;
          Ok (st2, r)
      | Some e2 =>
          '(st3, tv) <- exec_expr st2 ρ e2;;
          r <- of_option "ESubstring" (eval_substring sv fv (Some tv));;
          Ok (st3, r)
      end
  | EOptField recv fld =>
      '(st1, v) <- exec_expr st ρ recv;;
      match v with
      | VNull | VUndef => Ok (st1, VUndef)
      | _ =>
          rv <- read_target_x st1 ρ (XField v (VStr (cu fld)));;
          Ok (st1, rv)
      end
  end

with exec_ref (st : xstate) (ρ : env) (r : ref) {struct r}
  : out (xstate * xtarget) :=
  match r with
  | RVar x => Ok (st, XVar x)
  | RField b f =>
      '(st1, tb) <- exec_ref st ρ b;;
      bv <- read_target_x st1 ρ tb;;
      '(st2, fv) <- exec_expr st1 ρ f;;
      Ok (st2, XField bv fv)
  end.

Fixpoint exec_exprs (st : xstate) (ρ : env) (es : list expr)
  : out (xstate * list val) :=
  match es with
  | nil => Ok (st, nil)
  | e :: tl =>
      '(st1, v) <- exec_expr st ρ e;;
      '(st2, vs) <- exec_exprs st1 ρ tl;;
      Ok (st2, v :: vs)
  end.

(** ** Instructions and calls (fuel-indexed) *)

Fixpoint exec_inst (fuel : nat) (p : prog) (st : xstate) (ρ : env)
    (i : inst) {struct fuel} : out (xstate * env * completion) :=
  match fuel with
  | O => OOF
  | S fuel =>
      match i with
      | INop => Ok (st, ρ, CNormal VUndef)
      | ISeq insts =>
          (fix go (l : list inst) (st0 : xstate) (ρ0 : env)
             : out (xstate * env * completion) :=
             match l with
             | nil => Ok (st0, ρ0, CNormal VUndef)
             | i1 :: tl =>
                 '(st1, ρ1, k) <- exec_inst fuel p st0 ρ0 i1;;
                 match k with
                 | CNormal _ => go tl st1 ρ1
                 | CReturn v => Ok (st1, ρ1, CReturn v)
                 end
             end) insts st ρ
      | IExpr e =>
          '(st1, _) <- exec_expr st ρ e;;
          Ok (st1, ρ, CNormal VUndef)
      | ILet x e =>
          '(st1, v) <- exec_expr st ρ e;;
          Ok (st1, env_update (LName x) v ρ, CNormal VUndef)
      | IAssign r e =>
          '(st1, t) <- exec_ref st ρ r;;
          '(st2, v) <- exec_expr st1 ρ e;;
          '(st3, ρ') <- write_target_x st2 ρ t v;;
          Ok (st3, ρ', CNormal VUndef)
      | IIf c thn els =>
          '(st1, cv) <- exec_expr st ρ c;;
          match cv with
          | VBool true => exec_inst fuel p st1 ρ thn
          | VBool false => exec_inst fuel p st1 ρ els
          | _ => Stuck "IIf"
          end
      | IWhile c body =>
          '(st1, cv) <- exec_expr st ρ c;;
          match cv with
          | VBool true =>
              '(st2, ρ1, k) <- exec_inst fuel p st1 ρ body;;
              match k with
              | CNormal _ => exec_inst fuel p st2 ρ1 (IWhile c body)
              | CReturn v => Ok (st2, ρ1, CReturn v)
              end
          | VBool false => Ok (st1, ρ, CNormal VUndef)
          | _ => Stuck "IWhile"
          end
      | ICall lhs f args =>
          '(st1, fv) <- exec_expr st ρ f;;
          match fv with
          | VClo fn captured =>
              '(st2, vs) <- exec_exprs st1 ρ args;;
              '(st3, rv) <- exec_call fuel p st2 fn captured vs;;
              Ok (st3, env_update lhs rv ρ, CNormal VUndef)
          | VCont _ _ _ => Stuck "ICall(cont-control-only)"
          | _ => Stuck "ICall"
          end
      | IReturn e =>
          '(st1, v) <- exec_expr st ρ e;;
          Ok (st1, ρ, CReturn v)
      (* Interpreter.scala:147-151 evaluates the asserted expression inside
         `optional(...)`, so a THROW during evaluation SKIPS the assertion
         ("skip not yet compiled assertions").  Running the specification
         needs this: `var x = 1;` reaches `assert (yet "If the caller will
         not be overridden ...")` twice, and without the skip the whole run
         is stuck [VF].

         We mirror it only for [EYet], which is precisely ESMeta's own
         `NotSupported(Metalanguage)` throw (Interpreter.scala:231-232) and
         therefore a DEFINED behaviour rather than a gap in our model.  Any
         other stuck stays propagated, because ours also means "the model
         has no case here" and swallowing those would hide exactly the gaps
         this development exists to find (ADR-14).  The divergence is
         one-sided: where ESMeta swallows something else, the model gets
         stuck and the harness reports a mismatch — it never silently
         produces a wrong answer.

         One caveat, recorded rather than glossed: an expression that
         allocated before failing would keep those effects in ESMeta, while
         we resume from the pre-assert state.  The assertion that actually
         occurs is a bare [EYet], which allocates nothing. *)
      | IAssert e =>
          match exec_expr st ρ e with
          | Ok (st1, VBool true) => Ok (st1, ρ, CNormal VUndef)
          | Ok (_, _) => Stuck "IAssert(false)"
          | Stuck "EYet" => Ok (st, ρ, CNormal VUndef)
          | Stuck w => Stuck w
          | OOF => OOF
          end
      | IPrint e =>
          '(st1, v) <- exec_expr st ρ e;;
          Ok (out_print st1 v, ρ, CNormal VUndef)
      | IPush elem lst front =>
          '(st1, v) <- exec_expr st ρ elem;;
          '(st2, lv) <- exec_expr st1 ρ lst;;
          match lv with
          | VAddr a =>
              o <- of_option "IPush" (heap_get st2 a);;
              match o with
              | OList vs =>
                  st3 <- of_option "IPush" (heap_set st2 a
                           (OList (if front then v :: vs
                                   else (vs ++ (v :: nil))%list)));;
                  Ok (st3, ρ, CNormal VUndef)
              | _ => Stuck "IPush"
              end
          | _ => Stuck "IPush"
          end
      | IPop lhs lst front =>
          '(st1, lv) <- exec_expr st ρ lst;;
          match lv with
          | VAddr a =>
              o <- of_option "IPop" (heap_get st1 a);;
              match o with
              | OList vs =>
                  if front
                  then match vs with
                       | nil => Stuck "IPop"
                       | v :: tl =>
                           st2 <- of_option "IPop" (heap_set st1 a (OList tl));;
                           Ok (st2, env_update lhs v ρ, CNormal VUndef)
                       end
                  else match List.rev vs with
                       | nil => Stuck "IPop"
                       | v :: rtl =>
                           st2 <- of_option "IPop"
                                    (heap_set st1 a (OList (List.rev rtl)));;
                           Ok (st2, env_update lhs v ρ, CNormal VUndef)
                       end
              | _ => Stuck "IPop"
              end
          | _ => Stuck "IPop"
          end
      | IExpand base fld =>
          '(st1, t) <- exec_ref st ρ base;;
          bv <- read_target_x st1 ρ t;;
          '(st2, fv) <- exec_expr st1 ρ fld;;
          match bv, fv with
          | VAddr a, VStr cs =>
              f <- of_option "IExpand" (ascii_of_cstr cs);;
              o <- of_option "IExpand" (heap_get st2 a);;
              match o with
              | ORecord tn fs =>
                  match fields_lookup fs f with
                  | Some _ => Ok (st2, ρ, CNormal VUndef)
                  | None =>
                      st3 <- of_option "IExpand" (heap_set st2 a
                               (ORecord tn (fields_insert f VUndef fs)));;
                      Ok (st3, ρ, CNormal VUndef)
                  end
              | _ => Stuck "IExpand"
              end
          | _, _ => Stuck "IExpand"
          end
      | IDelete base key =>
          '(st1, t) <- exec_ref st ρ base;;
          bv <- read_target_x st1 ρ t;;
          '(st2, kv) <- exec_expr st1 ρ key;;
          match bv with
          | VAddr a =>
              o <- of_option "IDelete" (heap_get st2 a);;
              match o with
              | OMap es =>
                  es' <- of_option "IDelete(map-key-equality)"
                    (map_delete_partial kv es);;
                  st3 <- of_option "IDelete" (heap_set st2 a (OMap es'));;
                  Ok (st3, ρ, CNormal VUndef)
              | _ => Stuck "IDelete"
              end
          | _ => Stuck "IDelete"
          end
      | ISdoCall lhs base method args =>
          '(st1, bv) <- exec_expr st ρ base;;
          match bv with
          | VAst origin root path =>
              match ast_focus root path with
              | None => Stuck "ISdoCall(ast-cursor)"
              | Some a => match a with
              (* D-3: exporter-precomputed lexical SDO, returned with no
                 call frame (Interpreter.scala:192-193). *)
              | ALex _ _ _ _ _ =>
                  rv <- of_option "ISdoCall" (ast_lex_sdo a method);;
                  Ok (st1, env_update lhs rv ρ, CNormal VUndef)
              | ASyn _ _ _ _ _ _ _ _ =>
                  '(path0, fname) <-
                    of_option "ISdoCall"
                      (sdo_resolve_cursor (List.map f_name (p_funcs p))
                        root path method);;
                  '(st2, vs) <- exec_exprs st1 ρ args;;
                  '(st3, rv) <-
                    exec_call fuel p st2 fname nil
                      (VAst origin root path0 :: vs);;
                  Ok (st3, env_update lhs rv ρ, CNormal VUndef)
              end
              end
          | _ => Stuck "ISdoCall"
          end
      end
  end

with exec_call (fuel : nat) (p : prog) (st : xstate) (fn : irname)
    (captured : list (string * val)) (args : list val) {struct fuel}
  : out (xstate * val) :=
  match fuel with
  | O => OOF
  | S fuel =>
      match List.find (fun f => String.eqb (f_name f) fn) (p_funcs p) with
      | None => Stuck "ISdoCall"
      | Some f =>
          ρ0 <- of_option "ISdoCall" (init_env (f_params f) args);;
          '(st1, _, k) <- exec_inst fuel p st
                            (merge_captured_env ρ0 captured)
                            (f_body f);;
          match k with
          | CReturn v => Ok (st1, v)
          | CNormal _ => if f_main f then Ok (st1, VUndef) else Stuck "ISdoCall"
          end
      end
  end.

(** ** Whole programs

    Runs the main function with no arguments from the empty state,
    mirroring the denotation's [entry] convention. *)

Definition exec_prog (fuel : nat) (p : prog) : out (xstate * val) :=
  match List.find f_main (p_funcs p) with
  | Some f => exec_call fuel p (init_xstate p) (f_name f) nil nil
  | None => Stuck "ISdoCall"
  end.

(** Observable image of a run: the result value and the print log in
    order — the executable counterpart of a [Tr.done]-terminated trace. *)
Definition run (fuel : nat) (p : prog) : out (val * list val) :=
  match exec_prog fuel p with
  | Ok (st, v) => Ok (v, x_out st)
  | Stuck w => Stuck w
  | OOF => OOF
  end.
