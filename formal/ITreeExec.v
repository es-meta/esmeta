(** * ESMetaFV.ITreeExec — close the ITree denotation for execution

    [Semantics.v] denotes each IR-Core function to [itree crisE].  CRIS
    already provides the operational interpreters that:

    - turn the keyed [pgE] store into concrete state;
    - resolve [callE] through a closed program's function table; and
    - leave only primitive [coreE] events (notably [IO] and UB's
      [Take False]).

    This module assembles those pieces without going through [Exec.v].
    Consequently every instruction executed through [exec_itree] comes
    from the ITree denotation in [Semantics.v].

    The proof-oriented [SMod]/[Mod] packaging carries masks, resource
    algebras, and well-scopedness proofs.  None of those data affect the
    closed deterministic executions used for differential testing, so the
    executable assembly below builds the corresponding [LMod] directly
    from the same [denote_fbody] definitions and exported initial store. *)

From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment Domain Events Semantics.

Import ListNotations.

Set Implicit Arguments.

Local Open Scope string_scope.

(** No program denotation in this development emits an [agE] resource
    event.  The empty GRA is therefore sufficient to instantiate [crisE]
    for execution. *)
Definition execΣ : GRA := #[].

(** Test262 consumes only deterministic return/tau steps and named IO.
    Function-entry/exit IO events make unsupported-effect reports useful
    without assigning executable meaning to CRIS's proof-level
    [Take]/[Choose] events. *)
Definition trace_enter_fn : string := "esmeta.trace.enter".
Definition trace_exit_fn : string := "esmeta.trace.exit".
Definition trace_inst_prefix : string := "$ESMetaFV.trace.inst:".

(** Deep instruction tracing is a pure, opt-in program transformation used
    only by the diagnostic Test262 tree.  It inserts a reserved enum-valued
    print immediately before each instruction in one selected function.
    The OCaml runner recognizes and removes those markers from the
    observable print stream, retaining only the last path when UB occurs.

    Keeping this transformation outside [Semantics.v] is important:
    proof-facing modules and normal Test262 trees use the original program
    unchanged.  A residual generic [Take] can therefore be localized
    without weakening [Take]/[Choose] or perturbing the formal semantics. *)
Definition inst_kind (i : inst) : string :=
  match i with
  | INop => "INop"
  | ISeq _ => "ISeq"
  | IExpr _ => "IExpr"
  | ILet _ _ => "ILet"
  | IAssign _ _ => "IAssign"
  | IIf _ _ _ => "IIf"
  | IWhile _ _ => "IWhile"
  | ICall _ _ _ => "ICall"
  | IReturn _ => "IReturn"
  | IAssert _ => "IAssert"
  | IPrint _ => "IPrint"
  | IPush _ _ _ => "IPush"
  | IPop _ _ _ => "IPop"
  | IExpand _ _ => "IExpand"
  | IDelete _ _ => "IDelete"
  | ISdoCall _ _ _ _ => "ISdoCall"
  end.

Definition trace_inst_marker (path : string) (i : inst) : inst :=
  IPrint (EEnum (trace_inst_prefix ++ path ++ ":" ++ inst_kind i)).

Definition trace_child_path (path suffix : string) : string :=
  (path ++ "." ++ suffix)%string.

Fixpoint trace_inst (path : string) (i : inst) {struct i} : inst :=
  let traced :=
    match i with
    | ISeq insts =>
        ISeq
          ((fix go (index : nat) (rest : list inst) : list inst :=
              match rest with
              | nil => nil
              | next :: tail =>
                  trace_inst
                    (trace_child_path path (nat_str index))
                    next ::
                  go (S index) tail
              end) 0 insts)
    | IIf c thn els =>
        IIf c
          (trace_inst (trace_child_path path "then") thn)
          (trace_inst (trace_child_path path "else") els)
    | IWhile c body =>
        IWhile c (trace_inst (trace_child_path path "body") body)
    | _ => i
    end in
  ISeq (trace_inst_marker path i :: traced :: nil).

Definition trace_func (target : string) (f : func) : func :=
  if String.eqb target (f_name f)
  then
    func_with_body f (trace_inst "body" (f_body f))
  else f.

Definition trace_prog_func (target : string) (p : prog) : prog :=
  mkProgFull
    (List.map (trace_func target) (p_funcs p))
    (p_source p)
    (p_cached p)
    (p_hosts p)
    (p_globals p)
    (p_heap p).

Definition trace_fname (name : fname) : string :=
  match name with
  | funid fn => fn
  | entry => "<entry>"
  end.

Definition trace_fnsem
  (name : fname) (body : @fbody execΣ) : @fbody execΣ :=
  fun arg =>
    trigger (IO (I := unit) trace_enter_fn (trace_fname name));;;
    result <- body arg;;
    trigger (IO (I := unit) trace_exit_fn (trace_fname name));;;
    Ret result.

(** Erase only the proof-facing mask and optional function specification,
    then apply CRIS's standard [crisE] to [lmodE] interpreter.  Projecting
    [ir_fnsems] rather than rebuilding its list is important: execution
    and contextual-refinement proofs share exactly the same function
    bodies and entry selection by construction. *)
Definition project_fnsem
  (traced : bool)
  (name : fname)
  (x : option
    (@emask execΣ * (option (@fspec_rel execΣ) * @fbody execΣ)))
  : option (Any.t -> itree lmodE Any.t) :=
  match x with
  | Some (_, (_, body)) =>
      let executable_body := if traced then trace_fnsem name body else body in
      Some (@ModTr.trans_fnsem execΣ executable_body)
  | None => None
  end.

Definition exec_ir_fnsems_with_trace
  (traced : bool) (mn : string) (p : prog)
  : gmap fname (Any.t -> itree lmodE Any.t) :=
  map_imap (project_fnsem traced) (@ir_fnsems execΣ mn p).

Definition exec_ir_fnsems (mn : string) (p : prog)
  : gmap fname (Any.t -> itree lmodE Any.t) :=
  exec_ir_fnsems_with_trace false mn p.

(** Alternate function entries used only when a [VCont] performs a
    nonlocal jump.  They share the same instruction denotation but use
    ESMeta's continuation-specific argument rule (surplus arguments are
    ignored). *)
Definition exec_ir_cont_fnsems_with_trace
  (traced : bool) (mn : string) (p : prog)
  : gmap fname (Any.t -> itree lmodE Any.t) :=
  map_imap (project_fnsem traced) (@ir_cont_fnsems execΣ mn p).

Definition exec_ir_cont_fnsems (mn : string) (p : prog)
  : gmap fname (Any.t -> itree lmodE Any.t) :=
  exec_ir_cont_fnsems_with_trace false mn p.

Definition exec_lmod_with_trace
  (traced : bool) (mn : string) (p : prog) : LMod.t := {|
  LMod.fnsems := exec_ir_fnsems_with_trace traced mn p;
  LMod.initial_st := (ir_initial_st mn p, tt↑)
|}.

Definition exec_lmod (mn : string) (p : prog) : LMod.t :=
  exec_lmod_with_trace false mn p.

(** ** Explicit call-frame machine

    CRIS's standard closed-module interpreter implements an ordinary call
    by replacing the current tree with [body arg >>= caller_continuation].
    That representation is ideal for ordinary calls, but the caller
    continuation is then hidden inside [bind], so ESMeta's first-class
    continuation cannot discard it or restore an older call stack.

    The Test262 executor therefore keeps exactly the same [lmodE] trees and
    store interpreter while making the call stack explicit.  A frame is an
    immutable caller continuation plus its parent pointer and a liveness
    bit.  Frames live in an append-only finite map: a [VCont] can safely
    retain a stable pointer, and invoking a continuation clones every saved
    frame just as [CallContext.copied] does in ESMeta.

    ESMeta's retained [CallContext] points at a mutable CFG context.  After
    an ordinary return that context may advance and mutate before an older
    continuation is invoked.  The structured ITree continuation below has
    no explicit CFG cursor that could receive those mutations.  Reusing its
    capture-time closure would therefore invent behavior, so an ordinarily
    restored frame is retired.

    Retirement is checked only when control actually returns into that
    frame.  Continuation invocation still clones retired frames, preserving
    [ef_live = false] as a poison marker.  This lets the continuation body
    run and then discard the saved chain through another nonlocal jump, as
    the generator algorithms do.  If it instead returns normally into the
    stale caller, the [RetF] branch below raises UB before applying the old
    closure.  Missing/cyclic chains remain invocation-time UB.  Live chains
    remain exact and reusable through fresh clones.

    This machine interprets only [callE] control.  Store events pass to
    CRIS's standard [interp_stateE], core events pass to the OCaml boundary,
    and no executable meaning is assigned to [Take] or [Choose]. *)

Record exec_frame : Type := mkExecFrame {
  ef_parent : cont_stack;
  ef_continue : Any.t -> itree lmodE Any.t;
  ef_live : bool;
}.

Record exec_machine : Type := mkExecMachine {
  em_tree : itree lmodE Any.t;
  em_stack : cont_stack;
  em_next_frame : nat;
  em_frames : gmap nat exec_frame;
}.

Definition retire_exec_frame
  (frame_id : nat) (frame : exec_frame) (frames : gmap nat exec_frame)
  : gmap nat exec_frame :=
  <[frame_id :=
      mkExecFrame (ef_parent frame) (ef_continue frame) false]> frames.

Record exec_stack_clone : Type := mkExecStackClone {
  esc_stack : cont_stack;
  esc_next_frame : nat;
  esc_frames : gmap nat exec_frame;
}.

(** Clone a saved chain before a nonlocal jump.  Parent identifiers
    are always older than their children, so the number of allocated
    frames is sufficient fuel even in the presence of malformed external
    data.  Missing or cyclic frames are rejected.  A retired frame is
    cloned as retired: its continuation closure is never applied unless
    execution later attempts to return into it, where [handle_exec_callE]
    raises UB. *)
Fixpoint clone_exec_stack
  (fuel : nat)
  (stack : cont_stack)
  (next_frame : nat)
  (frames : gmap nat exec_frame)
  : option exec_stack_clone :=
  match stack with
  | None => Some (mkExecStackClone None next_frame frames)
  | Some frame_id =>
      match fuel with
      | O => None
      | S fuel' =>
          match frames !! frame_id with
          | Some frame =>
              match
                clone_exec_stack
                  fuel' (ef_parent frame) next_frame frames
              with
              | Some parent_clone =>
                  let cloned_id := esc_next_frame parent_clone in
                  let cloned_frame :=
                    mkExecFrame
                      (esc_stack parent_clone)
                      (ef_continue frame)
                      (ef_live frame) in
                  Some
                    (mkExecStackClone
                      (Some cloned_id)
                      (S cloned_id)
                      (<[cloned_id := cloned_frame]>
                         (esc_frames parent_clone)))
              | None => None
              end
          | None => None
          end
      end
  end.

Definition replace_tree (m : exec_machine) (tree : itree lmodE Any.t)
  : exec_machine :=
  mkExecMachine tree (em_stack m) (em_next_frame m) (em_frames m).

Definition handle_exec_callE
  (prog cont_prog : string -> option (Any.t -> itree lmodE Any.t))
  : exec_machine ->
    itreeV (lstateE +' coreE) (exec_machine + Any.t) :=
  fun m =>
    match observe (em_tree m) with
    | RetF rv =>
        match em_stack m with
        | None => itreeV_nvis (Ret (inr rv))
        | Some frame_id =>
            itreeV_nvis
              (frame <- ((em_frames m) !! frame_id)?;;
               if ef_live frame
               then
                 Ret (inl
                   (mkExecMachine
                      ((ef_continue frame) rv)
                      (ef_parent frame)
                      (em_next_frame m)
                      (retire_exec_frame
                        frame_id frame (em_frames m))))
               else triggerUB)
        end
    | TauF tree' =>
        itreeV_nvis (Ret (inl (replace_tree m tree')))
    | VisF (inr1 e) k =>
        itreeV_vis (subevent _ e)
          (fun v => Ret (inl (replace_tree m (k v))))
    | VisF (inl1 e) k =>
        itreeV_nvis
          (match e in callE T return (T -> _) -> _ with
           | Call fn arg =>
               fun k =>
                 if String.eqb fn cont_capture_fn
                 then
                   match @Any.downcast unit arg with
                   | Some _ =>
                       Ret (inl
                         (replace_tree m (k ((em_stack m)↑))))
                   | None => triggerUB
                   end
                 else if String.eqb fn cont_invoke_fn
                 then
                   match @Any.downcast cont_request arg with
                   | Some request =>
                       body <- (cont_prog (cr_fn request))?;;
                       match
                         clone_exec_stack
                           (em_next_frame m)
                           (cr_stack request)
                           (em_next_frame m)
                           (em_frames m)
                       with
                       | Some cloned =>
                           Ret (inl
                             (mkExecMachine
                                (body
                                  ((cr_captured request,
                                    cr_args request)↑))
                                (esc_stack cloned)
                                (esc_next_frame cloned)
                                (esc_frames cloned)))
                       | None => triggerUB
                       end
                   | None => triggerUB
                   end
                 else
                   body <- (prog fn)?;;
                   let frame_id := em_next_frame m in
                   let frame :=
                     mkExecFrame (em_stack m) k true in
                   Ret (inl
                     (mkExecMachine
                        (body arg)
                        (Some frame_id)
                        (S frame_id)
                        (<[frame_id := frame]> (em_frames m))))
           | Spawn _ _ => fun _ => triggerUB
           | Yield _ => fun _ => triggerUB
           | GetTid => fun _ => triggerUB
           end k)
    end.

Definition interp_exec_calls
  (prog cont_prog : string -> option (Any.t -> itree lmodE Any.t))
  (tree : itree lmodE Any.t) : itree (lstateE +' coreE) Any.t :=
  iterV (handle_exec_callE prog cont_prog)
    (mkExecMachine tree None 0 ∅).

Definition exec_trans
  (prog cont_prog : string -> option (Any.t -> itree lmodE Any.t))
  (tree : itree lmodE Any.t) (st : lstateT)
  : itree coreE (lstateT * Any.t) :=
  LModTr.interp_stateE Any.t (interp_exec_calls prog cont_prog tree) st.

(** [LMod.compile] hides the final store because CRIS contextual
    refinement observes only the returned value and [coreE] trace.  The
    explicit-state interpreter naturally returns both, which is useful for
    diagnostics even though Test262's observable result is the entry
    function's returned [val]. *)
Definition compile_full
  (ms : LMod.t)
  (cont_fnsems : gmap fname (Any.t -> itree lmodE Any.t))
  (arg : Any.t)
  : itree coreE (lstateT * Any.t) :=
  body <- ((LMod.fnsems ms) !! entry)? ;;
  exec_trans
    (LMod.prog ms)
    (fun fn => cont_fnsems !! (funid fn))
    (body arg)
    (LMod.initial_st ms).

Definition entry_result (ret : Any.t) : option val :=
  @Any.downcast val ret.

(** A closed program tree.  Its remaining visible events are exactly the
    primitive CRIS [coreE] events:

    - [IO "esmeta.print" v] for the observable print trace;
    - [Take False] when the denotation reaches undefined behavior.

    The OCaml boundary handles these events and supplies fuel. *)
Definition exec_itree (mn : string) (p : prog) : itree coreE val :=
  sr <-
    compile_full
      (exec_lmod mn p)
      (exec_ir_cont_fnsems mn p)
      tt↑ ;;
  v <- (entry_result (snd sr))?;;
  Ret v.

(** Same denotation with function-boundary IO instrumentation.  This is
    the Test262 runner's diagnostic projection: it does not interpret or
    resolve any other [coreE] constructor. *)
Definition exec_itree_traced (mn : string) (p : prog) : itree coreE val :=
  sr <-
    compile_full
      (exec_lmod_with_trace true mn p)
      (exec_ir_cont_fnsems_with_trace true mn p)
      tt↑ ;;
  v <- (entry_result (snd sr))?;;
  Ret v.

(** Function-boundary tracing plus instruction paths for one caller-chosen
    function.  This is constructed lazily by the extracted runner only
    under [--trace-func]; normal differential execution remains untouched. *)
Definition exec_itree_trace_func
  (mn : string) (p : prog) (target : string) : itree coreE val :=
  exec_itree_traced mn (trace_prog_func target p).
