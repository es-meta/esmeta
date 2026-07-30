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
    - [IAssert]: condition must evaluate to [VBool true]; everything else
      is UB.  DEVIATION: ESMeta silently skips assertions whose condition
      evaluation itself crashes (Interpreter.scala:147-151); such programs
      are excluded by admissibility (M0 OQ-5). *)

From CRIS Require Import CRIS.
From stdpp Require Import pretty.
From ESMetaFV Require Import Fragment Domain Events.

Set Implicit Arguments.

Local Open Scope string_scope.

(** The pure semantic domain (completions, operator evaluation,
    environments, heap objects) lives in [Domain.v], shared with the
    executable reference interpreter [Exec.v]. *)

(** ** Store layout (ADR-6, OQ-9)

    All keys of a program module live in its single scope [mn], as required
    by CRIS well-scopedness (SMod.v:21-25 [RF]).  Three key families:
    globals, heap cells, and the allocation counter.  The [$] separator
    cannot occur in ESMeta global names or produce collisions between
    families because each family has a distinct prefix. *)

Section DENOTE.
  Context `{!crisG Γ Σ α β τ _S _I}.

  Variable mn : string.   (* module (scope) name *)

  Definition glb_key (x : string) : key := (mn, "g$" ++ x).
  Definition heap_key (a : nat) : key := (mn, "h$" ++ pretty (N.of_nat a)).
  Definition alloc_key : key := (mn, "alloc$").
  (* Immutable run parameters, mirroring State.scala:17-18.  They live in
     the store rather than as extra parameters because that is where
     ESMeta keeps them: fields of [State]. *)
  Definition src_key : key := (mn, "src$").
  Definition cached_key : key := (mn, "cached$").

  Definition get_obj (a : nat) : itree crisE obj := cgetU (heap_key a).
  Definition put_obj (a : nat) (o : obj) : itree crisE unit :=
    cput (heap_key a) o.

  (** Deterministic counter allocation (Heap.scala:62-67). *)
  Definition alloc_obj (o : obj) : itree crisE nat :=
    a <- cgetU alloc_key;;
    cput alloc_key (S a);;;
    put_obj a o;;;
    Ret a.

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
        | OMap es, _ => (map_lookup es k)?
        | _, _ => triggerUB
        end
    (* AST child access, e.g. `this[0]` (State.scala:52, Ast.scala:90-91).
       Named-field and "parent" access need the grammar / parent pointers
       (Ast.scala:84-89), which the model does not carry: UB, not guessed. *)
    | TField (VAst a) (VMath i) =>
        if (0 <=? i)%Z
        then
          c <- (nth_error (ast_children a) (Z.to_nat i))?;;
          c0 <- c?;;
          Ret (VAst c0)
        else triggerUB
    (* Indexing a string yields the code unit at that position
       (State.scala:57-59: [case Math(k) => CodeUnit(str(k.toInt))]).  Any
       non-[Math] field raises WrongStringRef and an out-of-range index
       throws in Scala; both are UB here. *)
    | TField (VStr cs) (VMath i) =>
        if (0 <=? i)%Z
        then c <- (nth_error cs (Z.to_nat i))?;; Ret (VCodeUnit c)
        else triggerUB
    | _ => triggerUB
    end.

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
            put_obj a (ORecord tn (fields_insert fld v fs));;; Ret ρ
        | OList vs, VMath i =>
            if (0 <=? i)%Z
            then
              vs' <- (list_update (Z.to_nat i) v vs)?;;
              put_obj a (OList vs');;; Ret ρ
            else triggerUB
        | OMap es, _ => put_obj a (OMap (map_insert k v es));;; Ret ρ
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
        Ret ((x, v) :: cs)
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
    | EBinary op e1 e2 =>
        v1 <- denote_expr e1 ρ;;
        v2 <- denote_expr e2 ρ;;
        (eval_bop op v1 v2)?
    | EClo fn captured =>
        cs <- capture ρ captured;;
        Ret (VClo fn cs)
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
        | VAst a => Ret (VMath (Z.of_nat (List.length (ast_children a))))
        | _ => triggerUB
        end
    | ERecord tname fields =>
        (* fields evaluate left-to-right (Interpreter.scala:337-338) *)
        fs <- (fix go (l : list (string * expr))
                 : itree crisE (list (string * val)) :=
                 match l with
                 | nil => Ret nil
                 | (f, e1) :: tl =>
                     v <- denote_expr e1 ρ;;
                     vs <- go tl;;
                     Ret ((f, v) :: vs)
                 end) fields;;
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
                Ret (VBool (match map_lookup es k with
                            | Some _ => true | None => false end))
            | OList vs, VMath i =>
                Ret (VBool (andb (0 <=? i)%Z
                              (Nat.ltb (Z.to_nat i) (List.length vs))))
            | _, _ => triggerUB
            end
        | _ => triggerUB
        end
    | ETypeOf e1 =>
        v <- denote_expr e1 ρ;;
        (* addresses need ObjectT/SymbolT containment (not modelled) *)
        s0 <- (typeof_prim v)?;;
        Ret (VStr (cu s0))
    | ETypeCheck e1 t =>
        v <- denote_expr e1 ρ;;
        match v with
        | VAddr a =>
            o <- get_obj a;;
            (* [TAbrupt] alone needs the `Value` field's object as well
               (OQ-12); every other test is decided by the receiver, and
               reading nothing extra keeps their event traces unchanged. *)
            if ty_needs_value_obj t
            then
              match value_field_addr o with
              | Some b =>
                  ob <- get_obj b;;
                  Ret (VBool (ty_check_obj t o (Some ob)))
              | None => Ret (VBool (ty_check_obj t o None))
              end
            else Ret (VBool (ty_check_obj t o None))
        | _ => Ret (VBool (ty_check_prim t v))
        end
    | EYet _ => triggerUB      (* NotSupported — Interpreter.scala:231 *)
    | EMap pairs =>
        es <- (fix go (l : list (expr * expr))
                 : itree crisE (list (val * val)) :=
                 match l with
                 | nil => Ret nil
                 | (ke, ve) :: tl =>
                     kv <- denote_expr ke ρ;;
                     vv <- denote_expr ve ρ;;
                     rest <- go tl;;
                     Ret ((kv, vv) :: rest)
                 end) pairs;;
        a <- alloc_obj (OMap es);;
        Ret (VAddr a)
    | EKeys m intSorted =>
        v <- denote_expr m ρ;;
        if intSorted then triggerUB else
        match v with
        | VAddr a =>
            o <- get_obj a;;
            ks <- (obj_keys o)?;;
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
    | EConvert op e1 =>
        v <- denote_expr e1 ρ;;
        (eval_cop op v)?
    (* COp.ToStr needs toStringHelper / ESValueParser (Scala): UB (L-11) *)
    | EToStr _ _ => triggerUB
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
    (* [asList] then Scala [==] membership (Interpreter.scala:233-236);
       a non-list receiver throws NoList/NoAddr, hence UB. *)
    | EContains lst e1 =>
        lv <- denote_expr lst ρ;;
        ev <- denote_expr e1 ρ;;
        match lv with
        | VAddr a =>
            o <- get_obj a;;
            match o with
            | OList vs => Ret (VBool (existsb (val_eqb ev) vs))
            | _ => triggerUB
            end
        | _ => triggerUB
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
        | VAst a => Ret (VStr (ast_src a))
        | _ => triggerUB
        end
    (* Only the cached-AST fast path (Interpreter.scala:206-209); a real
       parse needs ESMeta's Scala parser, so everything else is UB. *)
    | EParse code rule =>
        cv <- denote_expr code ρ;;
        rv <- denote_expr rule ρ;;
        src <- cgetU src_key;;
        cached <- cgetU cached_key;;
        match cv, rv, src, cached with
        | VStr x, VGrammarSymbol nm nil, Some y, Some a =>
            if andb (String.eqb nm "Script") (val_eqb (VStr x) (VStr y))
            then Ret (VAst a) else triggerUB
        | _, _, _, _ => triggerUB
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
        if orb (val_eqb v VNull) (val_eqb v VUndef)
        then Ret VUndef
        else read_target ρ (TField v (VStr (cu fld)))
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
        | _ => triggerUB
        end
    | IReturn e =>
        v <- denote_expr e ρ;;
        Ret (ρ, CReturn v)
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
            | OMap es => put_obj a (OMap (map_delete kv es));;;
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
        | VAst a =>
            match a with
            | ALex _ _ _ _ =>
                rv <- (ast_lex_sdo a method)?;;
                Ret (env_update lhs rv ρ, CNormal VUndef)
            | ASyn _ _ _ _ _ _ =>
                '(a0, fname) : ast * string <- (sdo_resolve fnames a method)?;;
                vs <- denote_exprs args ρ;;
                rv <- ccallU (ir_sig fname) (nil, VAst a0 :: vs);;
                Ret (env_update lhs rv ρ, CNormal VUndef)
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
      <- denote_inst fnames (f_body f) (ρ0 ++ captured_env captured)%list;;
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

  Definition ir_initial_st (p : prog) : gmap key (option Any.t) :=
    <[ src_key := Some ((p_source p)↑) ]>
      (<[ cached_key := Some ((p_cached p)↑) ]>
         (<[ alloc_key := Some ((List.length (p_heap p) : nat)↑) ]>
            (list_to_map (glb_kvs p ++ heap_kvs p)))).


  (** Every key of the exported initial store lives in scope [mn]: the
      three fixed keys by construction, and the globals/heap keys because
      [glb_key]/[heap_key] build them that way. *)
  Lemma initial_st_scope (p : prog) (k : key) :
    k ∈ dom (ir_initial_st p) -> fst k = mn.
  Proof.
    unfold ir_initial_st. rewrite !dom_insert_L.
    rewrite !elem_of_union !elem_of_singleton. intros Hk.
    destruct Hk as [E|[E|[E|Hk]]];
      [ rewrite E; unfold src_key; reflexivity
      | rewrite E; unfold cached_key; reflexivity
      | rewrite E; unfold alloc_key; reflexivity
      | ].
    apply elem_of_dom in Hk. destruct Hk as [v Hv].
    apply elem_of_list_to_map_2, elem_of_list_In, in_app_iff in Hv.
    destruct Hv as [Hv|Hv]; apply in_map_iff in Hv;
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
    apply elem_of_list_to_map_2, elem_of_list_In, in_app_iff in Hv.
    destruct Hv as [Hv|Hv]; apply in_map_iff in Hv;
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
