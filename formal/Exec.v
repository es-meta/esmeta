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

Variant out (A : Type) : Type :=
| Ok (a : A)      (* successful execution *)
| Stuck           (* undefined behavior / interpreter failure *)
| OOF.            (* out of fuel — inconclusive, raise the fuel bound *)
Arguments Ok {A} a.
Arguments Stuck {A}.
Arguments OOF {A}.

Definition obind {A B} (m : out A) (k : A -> out B) : out B :=
  match m with
  | Ok a => k a
  | Stuck => Stuck
  | OOF => OOF
  end.

Definition of_option {A} (m : option A) : out A :=
  match m with
  | Some a => Ok a
  | None => Stuck
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
}.

(** The exported initial state (Initialize.scala:29-40): heap objects at
    their exported addresses, initial globals, empty print log. *)
Definition init_xstate (p : prog) : xstate :=
  mkXState (p_heap p) (p_globals p) nil (p_source p) (p_cached p).

(* [None] both for an out-of-range address and for a slot that exists but
   is unmapped; ESMeta throws UnknownAddr for the latter (Heap.scala:19). *)
Definition heap_get (st : xstate) (a : nat) : option obj :=
  match nth_error (x_heap st) a with
  | Some (Some o) => Some o
  | _ => None
  end.

Definition heap_set (st : xstate) (a : nat) (o : obj) : option xstate :=
  option_map
    (fun h => mkXState h (x_globals st) (x_out st) (x_source st) (x_cached st))
    (list_update a (Some o) (x_heap st)).

Definition heap_alloc (st : xstate) (o : obj) : xstate * nat :=
  (mkXState (x_heap st ++ [Some o]) (x_globals st) (x_out st)
     (x_source st) (x_cached st),
   List.length (x_heap st)).

Definition globals_set (st : xstate) (x : string) (v : val) : xstate :=
  mkXState (x_heap st) (fields_insert x v (x_globals st)) (x_out st)
    (x_source st) (x_cached st).

Definition out_print (st : xstate) (v : val) : xstate :=
  mkXState (x_heap st) (x_globals st) (x_out st ++ [v])
    (x_source st) (x_cached st).

(** ** Reference targets (mirrors Semantics.v [ref_target]) *)

Variant xtarget : Type :=
| XVar (x : var)
| XField (base : val) (field : val).

Definition read_target_x (st : xstate) (ρ : env) (t : xtarget) : out val :=
  match t with
  | XVar (VLocal l) => of_option (env_lookup ρ l)
  | XVar (VGlobal x) => of_option (fields_lookup (x_globals st) x)
  | XField (VAddr a) k =>
      o <- of_option (heap_get st a);;
      match o, k with
      | ORecord _ fs, VStr cs =>
          fld <- of_option (ascii_of_cstr cs);;
          of_option (fields_lookup fs fld)
      | OList vs, VMath i =>
          if (0 <=? i)%Z then of_option (nth_error vs (Z.to_nat i)) else Stuck
      | OMap es, _ => of_option (map_lookup es k)
      | _, _ => Stuck
      end
  | XField (VAst a) (VMath i) =>
      if (0 <=? i)%Z
      then
        c <- of_option (nth_error (ast_children a) (Z.to_nat i));;
        c0 <- of_option c;;
        Ok (VAst c0)
      else Stuck
  (* String indexing -> code unit (State.scala:57-59). *)
  | XField (VStr cs) (VMath i) =>
      if (0 <=? i)%Z
      then c <- of_option (nth_error cs (Z.to_nat i));; Ok (VCodeUnit c)
      else Stuck
  | _ => Stuck
  end.

Definition write_target_x (st : xstate) (ρ : env) (t : xtarget) (v : val)
  : out (xstate * env) :=
  match t with
  | XVar (VLocal l) => Ok (st, env_update l v ρ)
  | XVar (VGlobal x) => Ok (globals_set st x v, ρ)
  | XField (VAddr a) k =>
      o <- of_option (heap_get st a);;
      match o, k with
      | ORecord tn fs, VStr cs =>
          fld <- of_option (ascii_of_cstr cs);;
          st' <- of_option (heap_set st a (ORecord tn (fields_insert fld v fs)));;
          Ok (st', ρ)
      | OList vs, VMath i =>
          if (0 <=? i)%Z
          then
            vs' <- of_option (list_update (Z.to_nat i) v vs);;
            st' <- of_option (heap_set st a (OList vs'));;
            Ok (st', ρ)
          else Stuck
      | OMap es, _ =>
          st' <- of_option (heap_set st a (OMap (map_insert k v es)));;
          Ok (st', ρ)
      | _, _ => Stuck
      end
  | _ => Stuck
  end.

Fixpoint capture_x (ρ : env) (xs : list string)
  : out (list (string * val)) :=
  match xs with
  | nil => Ok nil
  | x :: tl =>
      v <- of_option (env_lookup ρ (LName x));;
      cs <- capture_x ρ tl;;
      Ok ((x, v) :: cs)
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
      r <- of_option (eval_uop op v);;
      Ok (st1, r)
  | EBinary BAnd e1 e2 =>
      '(st1, v1) <- exec_expr st ρ e1;;
      match v1 with
      | VBool false => Ok (st1, VBool false)
      | VBool true =>
          '(st2, v2) <- exec_expr st1 ρ e2;;
          match v2 with
          | VBool b => Ok (st2, VBool b)
          | _ => Stuck
          end
      | _ => Stuck
      end
  | EBinary BOr e1 e2 =>
      '(st1, v1) <- exec_expr st ρ e1;;
      match v1 with
      | VBool true => Ok (st1, VBool true)
      | VBool false =>
          '(st2, v2) <- exec_expr st1 ρ e2;;
          match v2 with
          | VBool b => Ok (st2, VBool b)
          | _ => Stuck
          end
      | _ => Stuck
      end
  | EBinary op e1 e2 =>
      '(st1, v1) <- exec_expr st ρ e1;;
      '(st2, v2) <- exec_expr st1 ρ e2;;
      r <- of_option (eval_bop op v1 v2);;
      Ok (st2, r)
  | EClo fn captured =>
      cs <- capture_x ρ captured;;
      Ok (st, VClo fn cs)
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
          o <- of_option (heap_get st1 a);;
          n <- of_option (obj_size o);;
          Ok (st1, VMath (Z.of_nat n))
      | VAst a => Ok (st1, VMath (Z.of_nat (List.length (ast_children a))))
      | _ => Stuck
      end
  | ERecord tname fields =>
      '(st1, fs) <-
        ((fix go (l : list (string * expr)) (st0 : xstate)
            : out (xstate * list (string * val)) :=
            match l with
            | nil => Ok (st0, nil)
            | (f, e1) :: tl =>
                '(st1, v) <- exec_expr st0 ρ e1;;
                '(st2, vs) <- go tl st1;;
                Ok (st2, (f, v) :: vs)
            end) fields st);;
      let '(st2, a) := heap_alloc st1 (ORecord tname fs) in
      Ok (st2, VAddr a)
  | EExists r =>
      '(st1, t) <- exec_ref st ρ r;;
      match t with
      | XVar (VLocal l) =>
          Ok (st1, VBool (match env_lookup ρ l with Some _ => true | None => false end))
      | XVar (VGlobal _) => Stuck
      | XField (VAddr a) k =>
          o <- of_option (heap_get st1 a);;
          match o, k with
          | ORecord _ fs, VStr cs =>
              fld <- of_option (ascii_of_cstr cs);;
              Ok (st1, VBool (match fields_lookup fs fld with
                              | Some _ => true | None => false end))
          | OMap es, _ =>
              Ok (st1, VBool (match map_lookup es k with
                              | Some _ => true | None => false end))
          | OList vs, VMath i =>
              Ok (st1, VBool (andb (0 <=? i)%Z
                                (Nat.ltb (Z.to_nat i) (List.length vs))))
          | _, _ => Stuck
          end
      | _ => Stuck
      end
  | ETypeOf e1 =>
      '(st1, v) <- exec_expr st ρ e1;;
      s0 <- of_option (typeof_prim v);;
      Ok (st1, VStr (cu s0))
  | ETypeCheck e1 t =>
      '(st1, v) <- exec_expr st ρ e1;;
      match v with
      | VAddr a =>
          o <- of_option (heap_get st1 a);;
          (* TAbrupt also needs the `Value` field's object (OQ-12) *)
          if ty_needs_value_obj t
          then
            match value_field_addr o with
            | Some b =>
                ob <- of_option (heap_get st1 b);;
                Ok (st1, VBool (ty_check_obj t o (Some ob)))
            | None => Ok (st1, VBool (ty_check_obj t o None))
            end
          else Ok (st1, VBool (ty_check_obj t o None))
      | _ => Ok (st1, VBool (ty_check_prim t v))
      end
  | EYet _ => Stuck
  | EMap pairs =>
      '(st1, es) <-
        ((fix go (l : list (expr * expr)) (st0 : xstate)
            : out (xstate * list (val * val)) :=
            match l with
            | nil => Ok (st0, nil)
            | (ke, ve) :: tl =>
                '(st1, kv) <- exec_expr st0 ρ ke;;
                '(st2, vv) <- exec_expr st1 ρ ve;;
                '(st3, rest) <- go tl st2;;
                Ok (st3, (kv, vv) :: rest)
            end) pairs st);;
      let '(st2, a) := heap_alloc st1 (OMap es) in
      Ok (st2, VAddr a)
  | EKeys m intSorted =>
      '(st1, v) <- exec_expr st ρ m;;
      if intSorted then Stuck else
      match v with
      | VAddr a =>
          o <- of_option (heap_get st1 a);;
          ks <- of_option (obj_keys o);;
          let '(st2, a2) := heap_alloc st1 (OList ks) in
          Ok (st2, VAddr a2)
      | _ => Stuck
      end
  | ECopy e1 =>
      '(st1, v) <- exec_expr st ρ e1;;
      match v with
      | VAddr a =>
          o <- of_option (heap_get st1 a);;
          let '(st2, a2) := heap_alloc st1 o in
          Ok (st2, VAddr a2)
      | _ => Stuck
      end
  | ENumber f => Ok (st, VNumber f)
  | EBigInt z => Ok (st, VBigInt z)
  | EInfinity p => Ok (st, VInfinity p)
  | ECodeUnit c => Ok (st, VCodeUnit c)
  | EConvert op e1 =>
      '(st1, v) <- exec_expr st ρ e1;;
      r <- of_option (eval_cop op v);;
      Ok (st1, r)
  | EToStr _ _ => Stuck
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
      r <- of_option (eval_vop op vs);;
      Ok (st1, r)
  | EContains lst e1 =>
      '(st1, lv) <- exec_expr st ρ lst;;
      '(st2, ev) <- exec_expr st1 ρ e1;;
      match lv with
      | VAddr a =>
          o <- of_option (heap_get st2 a);;
          match o with
          | OList vs => Ok (st2, VBool (existsb (val_eqb ev) vs))
          | _ => Stuck
          end
      | _ => Stuck
      end
  | EGrammarSymbol nm ps => Ok (st, VGrammarSymbol nm ps)
  | EInstanceOf e1 tgt =>
      '(st1, v) <- exec_expr st ρ e1;;
      '(st2, t) <- exec_expr st1 ρ tgt;;
      Ok (st2, eval_instanceof v t)
  | ESourceText e1 =>
      '(st1, v) <- exec_expr st ρ e1;;
      match v with
      | VAst a => Ok (st1, VStr (ast_src a))
      | _ => Stuck
      end
  (* Only the cached-AST fast path (Interpreter.scala:206-209); a real
     parse needs ESMeta's Scala parser, so everything else is UB. *)
  | EParse code rule =>
      '(st1, cv) <- exec_expr st ρ code;;
      '(st2, rv) <- exec_expr st1 ρ rule;;
      match cv, rv, x_source st2, x_cached st2 with
      | VStr x, VGrammarSymbol nm nil, Some y, Some a =>
          if andb (String.eqb nm "Script") (val_eqb (VStr x) (VStr y))
          then Ok (st2, VAst a) else Stuck
      | _, _, _, _ => Stuck
      end
  | ESubstring e1 from to =>
      '(st1, sv) <- exec_expr st ρ e1;;
      '(st2, fv) <- exec_expr st1 ρ from;;
      match to with
      | None =>
          r <- of_option (eval_substring sv fv None);;
          Ok (st2, r)
      | Some e2 =>
          '(st3, tv) <- exec_expr st2 ρ e2;;
          r <- of_option (eval_substring sv fv (Some tv));;
          Ok (st3, r)
      end
  | EOptField recv fld =>
      '(st1, v) <- exec_expr st ρ recv;;
      if orb (val_eqb v VNull) (val_eqb v VUndef)
      then Ok (st1, VUndef)
      else
        rv <- read_target_x st1 ρ (XField v (VStr (cu fld)));;
        Ok (st1, rv)
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
          | _ => Stuck
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
          | _ => Stuck
          end
      | ICall lhs f args =>
          '(st1, fv) <- exec_expr st ρ f;;
          match fv with
          | VClo fn captured =>
              '(st2, vs) <- exec_exprs st1 ρ args;;
              '(st3, rv) <- exec_call fuel p st2 fn captured vs;;
              Ok (st3, env_update lhs rv ρ, CNormal VUndef)
          | _ => Stuck
          end
      | IReturn e =>
          '(st1, v) <- exec_expr st ρ e;;
          Ok (st1, ρ, CReturn v)
      | IAssert e =>
          '(st1, cv) <- exec_expr st ρ e;;
          match cv with
          | VBool true => Ok (st1, ρ, CNormal VUndef)
          | _ => Stuck
          end
      | IPrint e =>
          '(st1, v) <- exec_expr st ρ e;;
          Ok (out_print st1 v, ρ, CNormal VUndef)
      | IPush elem lst front =>
          '(st1, v) <- exec_expr st ρ elem;;
          '(st2, lv) <- exec_expr st1 ρ lst;;
          match lv with
          | VAddr a =>
              o <- of_option (heap_get st2 a);;
              match o with
              | OList vs =>
                  st3 <- of_option (heap_set st2 a
                           (OList (if front then v :: vs
                                   else (vs ++ (v :: nil))%list)));;
                  Ok (st3, ρ, CNormal VUndef)
              | _ => Stuck
              end
          | _ => Stuck
          end
      | IPop lhs lst front =>
          '(st1, lv) <- exec_expr st ρ lst;;
          match lv with
          | VAddr a =>
              o <- of_option (heap_get st1 a);;
              match o with
              | OList vs =>
                  if front
                  then match vs with
                       | nil => Stuck
                       | v :: tl =>
                           st2 <- of_option (heap_set st1 a (OList tl));;
                           Ok (st2, env_update lhs v ρ, CNormal VUndef)
                       end
                  else match List.rev vs with
                       | nil => Stuck
                       | v :: rtl =>
                           st2 <- of_option
                                    (heap_set st1 a (OList (List.rev rtl)));;
                           Ok (st2, env_update lhs v ρ, CNormal VUndef)
                       end
              | _ => Stuck
              end
          | _ => Stuck
          end
      | IExpand base fld =>
          '(st1, t) <- exec_ref st ρ base;;
          bv <- read_target_x st1 ρ t;;
          '(st2, fv) <- exec_expr st1 ρ fld;;
          match bv, fv with
          | VAddr a, VStr cs =>
              f <- of_option (ascii_of_cstr cs);;
              o <- of_option (heap_get st2 a);;
              match o with
              | ORecord tn fs =>
                  match fields_lookup fs f with
                  | Some _ => Ok (st2, ρ, CNormal VUndef)
                  | None =>
                      st3 <- of_option (heap_set st2 a
                               (ORecord tn (fields_insert f VUndef fs)));;
                      Ok (st3, ρ, CNormal VUndef)
                  end
              | _ => Stuck
              end
          | _, _ => Stuck
          end
      | IDelete base key =>
          '(st1, t) <- exec_ref st ρ base;;
          bv <- read_target_x st1 ρ t;;
          '(st2, kv) <- exec_expr st1 ρ key;;
          match bv with
          | VAddr a =>
              o <- of_option (heap_get st2 a);;
              match o with
              | OMap es =>
                  st3 <- of_option (heap_set st2 a (OMap (map_delete kv es)));;
                  Ok (st3, ρ, CNormal VUndef)
              | _ => Stuck
              end
          | _ => Stuck
          end
      | ISdoCall lhs base method args =>
          '(st1, bv) <- exec_expr st ρ base;;
          match bv with
          | VAst a =>
              match a with
              (* D-3: exporter-precomputed lexical SDO, returned with no
                 call frame (Interpreter.scala:192-193). *)
              | ALex _ _ _ _ =>
                  rv <- of_option (ast_lex_sdo a method);;
                  Ok (st1, env_update lhs rv ρ, CNormal VUndef)
              | ASyn _ _ _ _ _ _ =>
                  '(a0, fname) <-
                    of_option (sdo_resolve (List.map f_name (p_funcs p))
                                 a method);;
                  '(st2, vs) <- exec_exprs st1 ρ args;;
                  '(st3, rv) <- exec_call fuel p st2 fname nil (VAst a0 :: vs);;
                  Ok (st3, env_update lhs rv ρ, CNormal VUndef)
              end
          | _ => Stuck
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
      | None => Stuck
      | Some f =>
          ρ0 <- of_option (init_env (f_params f) args);;
          '(st1, _, k) <- exec_inst fuel p st
                            ((ρ0 ++ captured_env captured)%list)
                            (f_body f);;
          match k with
          | CReturn v => Ok (st1, v)
          | CNormal _ => if f_main f then Ok (st1, VUndef) else Stuck
          end
      end
  end.

(** ** Whole programs

    Runs the main function with no arguments from the empty state,
    mirroring the denotation's [entry] convention. *)

Definition exec_prog (fuel : nat) (p : prog) : out (xstate * val) :=
  match List.find f_main (p_funcs p) with
  | Some f => exec_call fuel p (init_xstate p) (f_name f) nil nil
  | None => Stuck
  end.

(** Observable image of a run: the result value and the print log in
    order — the executable counterpart of a [Tr.done]-terminated trace. *)
Definition run (fuel : nat) (p : prog) : out (val * list val) :=
  match exec_prog fuel p with
  | Ok (st, v) => Ok (v, x_out st)
  | Stuck => Stuck
  | OOF => OOF
  end.
