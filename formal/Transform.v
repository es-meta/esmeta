(** * ESMetaFV.Transform — T-1: fresh-temporary introduction

    The proof-of-concept transpilation pass (architecture note §10).
    Every call instruction

      call lhs = f(args...)

    becomes

      call %k = f(args...) ; lhs = %k

    where [%k] ([LTemp k]) is fresh for the enclosing function body.
    Because the temporary is dead immediately after the copy, a single
    fresh index [k] per function suffices for all call sites.

    Correctness obligations (why this is not syntactic identity):
    - the call must still happen exactly once, at the same program point;
    - argument evaluation order must be unchanged;
    - the callee's effects (prints, state, further calls) must interleave
      with the caller's exactly as before — including when [f] is an
      UNKNOWN function supplied by the linking context;
    - the extra temporary must not disturb any other variable.

    The preconditions are syntactic and decidable ([temp_fresh_*]); the
    designated index produced by [fresh_temp] always satisfies them
    ([fresh_temp_is_fresh] — proved, no assumptions). *)

From Stdlib Require Import String ZArith List Bool Lia.
Import ListNotations.

From ESMetaFV Require Import Fragment.

Set Implicit Arguments.

(** ** Occurrence of a temporary in fragment syntax

    [LTemp k] can occur: as a variable inside references (read or write
    position) and as the destination of a call. *)

Definition temp_fresh_local (k : nat) (l : local) : bool :=
  match l with
  | LTemp n => negb (Nat.eqb n k)
  | LName _ => true
  end.

Definition temp_fresh_var (k : nat) (x : var) : bool :=
  match x with
  | VLocal l => temp_fresh_local k l
  | VGlobal _ => true
  end.

Fixpoint temp_fresh_expr (k : nat) (e : expr) {struct e} : bool :=
  match e with
  | EMath _ | EBool _ | EStr _ | EUndef | ENull | EEnum _ => true
  | ERef r => temp_fresh_ref k r
  | EUnary _ e1 => temp_fresh_expr k e1
  | EBinary _ e1 e2 => andb (temp_fresh_expr k e1) (temp_fresh_expr k e2)
  | EClo _ _ => true    (* captured lists name only [LName]s *)
  | EList es =>
      (fix go (l : list expr) : bool :=
         match l with
         | nil => true
         | e1 :: tl => andb (temp_fresh_expr k e1) (go tl)
         end) es
  | ESizeOf e1 => temp_fresh_expr k e1
  | ERecord _ fields =>
      (fix go (l : list (string * expr)) : bool :=
         match l with
         | nil => true
         | (_, e1) :: tl => andb (temp_fresh_expr k e1) (go tl)
         end) fields
  | EExists r => temp_fresh_ref k r
  | ETypeOf e1 => temp_fresh_expr k e1
  | ETypeCheck e1 _ => temp_fresh_expr k e1
  | EYet _ => true
  | EMap pairs =>
      (fix go (l : list (expr * expr)) : bool :=
         match l with
         | nil => true
         | (ke, ve) :: tl =>
             andb (temp_fresh_expr k ke)
               (andb (temp_fresh_expr k ve) (go tl))
         end) pairs
  | EKeys m _ => temp_fresh_expr k m
  | ECopy e1 => temp_fresh_expr k e1
  | EOptField recv _ => temp_fresh_expr k recv
  end

with temp_fresh_ref (k : nat) (r : ref) {struct r} : bool :=
  match r with
  | RVar x => temp_fresh_var k x
  | RField b f => andb (temp_fresh_ref k b) (temp_fresh_expr k f)
  end.

Fixpoint temp_fresh_inst (k : nat) (i : inst) {struct i} : bool :=
  match i with
  | INop => true
  | ISeq insts =>
      (fix go (l : list inst) : bool :=
         match l with
         | nil => true
         | i1 :: tl => andb (temp_fresh_inst k i1) (go tl)
         end) insts
  | IExpr e => temp_fresh_expr k e
  | ILet _ e => temp_fresh_expr k e
  | IAssign r e => andb (temp_fresh_ref k r) (temp_fresh_expr k e)
  | IIf c t e =>
      andb (temp_fresh_expr k c)
        (andb (temp_fresh_inst k t) (temp_fresh_inst k e))
  | IWhile c b => andb (temp_fresh_expr k c) (temp_fresh_inst k b)
  | ICall lhs f args =>
      andb (temp_fresh_local k lhs)
        (andb (temp_fresh_expr k f)
           ((fix go (l : list expr) : bool :=
               match l with
               | nil => true
               | e1 :: tl => andb (temp_fresh_expr k e1) (go tl)
               end) args))
  | IReturn e => temp_fresh_expr k e
  | IAssert e => temp_fresh_expr k e
  | IPrint e => temp_fresh_expr k e
  | IPush elem lst _ =>
      andb (temp_fresh_expr k elem) (temp_fresh_expr k lst)
  | IPop lhs lst _ =>
      andb (temp_fresh_local k lhs) (temp_fresh_expr k lst)
  | IExpand base fld =>
      andb (temp_fresh_ref k base) (temp_fresh_expr k fld)
  | IDelete base key =>
      andb (temp_fresh_ref k base) (temp_fresh_expr k key)
  | ISdoCall lhs base _ args =>
      andb (temp_fresh_local k lhs)
        (andb (temp_fresh_expr k base)
           ((fix go (l : list expr) : bool :=
               match l with
               | nil => true
               | e1 :: tl => andb (temp_fresh_expr k e1) (go tl)
               end) args))
  end.

(** ** A designated fresh temporary: one above every occurring index *)

Definition temp_bound_local (l : local) : nat :=
  match l with
  | LTemp n => S n
  | LName _ => 0
  end.

Definition temp_bound_var (x : var) : nat :=
  match x with
  | VLocal l => temp_bound_local l
  | VGlobal _ => 0
  end.

Fixpoint temp_bound_expr (e : expr) {struct e} : nat :=
  match e with
  | EMath _ | EBool _ | EStr _ | EUndef | ENull | EEnum _ => 0
  | ERef r => temp_bound_ref r
  | EUnary _ e1 => temp_bound_expr e1
  | EBinary _ e1 e2 => Nat.max (temp_bound_expr e1) (temp_bound_expr e2)
  | EClo _ _ => 0
  | EList es =>
      (fix go (l : list expr) : nat :=
         match l with
         | nil => 0
         | e1 :: tl => Nat.max (temp_bound_expr e1) (go tl)
         end) es
  | ESizeOf e1 => temp_bound_expr e1
  | ERecord _ fields =>
      (fix go (l : list (string * expr)) : nat :=
         match l with
         | nil => 0
         | (_, e1) :: tl => Nat.max (temp_bound_expr e1) (go tl)
         end) fields
  | EExists r => temp_bound_ref r
  | ETypeOf e1 => temp_bound_expr e1
  | ETypeCheck e1 _ => temp_bound_expr e1
  | EYet _ => 0
  | EMap pairs =>
      (fix go (l : list (expr * expr)) : nat :=
         match l with
         | nil => 0
         | (ke, ve) :: tl =>
             Nat.max (temp_bound_expr ke)
               (Nat.max (temp_bound_expr ve) (go tl))
         end) pairs
  | EKeys m _ => temp_bound_expr m
  | ECopy e1 => temp_bound_expr e1
  | EOptField recv _ => temp_bound_expr recv
  end

with temp_bound_ref (r : ref) {struct r} : nat :=
  match r with
  | RVar x => temp_bound_var x
  | RField b f => Nat.max (temp_bound_ref b) (temp_bound_expr f)
  end.

Fixpoint temp_bound_inst (i : inst) {struct i} : nat :=
  match i with
  | INop => 0
  | ISeq insts =>
      (fix go (l : list inst) : nat :=
         match l with
         | nil => 0
         | i1 :: tl => Nat.max (temp_bound_inst i1) (go tl)
         end) insts
  | IExpr e => temp_bound_expr e
  | ILet _ e => temp_bound_expr e
  | IAssign r e => Nat.max (temp_bound_ref r) (temp_bound_expr e)
  | IIf c t e =>
      Nat.max (temp_bound_expr c)
        (Nat.max (temp_bound_inst t) (temp_bound_inst e))
  | IWhile c b => Nat.max (temp_bound_expr c) (temp_bound_inst b)
  | ICall lhs f args =>
      Nat.max (temp_bound_local lhs)
        (Nat.max (temp_bound_expr f)
           ((fix go (l : list expr) : nat :=
               match l with
               | nil => 0
               | e1 :: tl => Nat.max (temp_bound_expr e1) (go tl)
               end) args))
  | IReturn e => temp_bound_expr e
  | IAssert e => temp_bound_expr e
  | IPrint e => temp_bound_expr e
  | IPush elem lst _ => Nat.max (temp_bound_expr elem) (temp_bound_expr lst)
  | IPop lhs lst _ =>
      Nat.max (temp_bound_local lhs) (temp_bound_expr lst)
  | IExpand base fld => Nat.max (temp_bound_ref base) (temp_bound_expr fld)
  | IDelete base key => Nat.max (temp_bound_ref base) (temp_bound_expr key)
  | ISdoCall lhs base _ args =>
      Nat.max (temp_bound_local lhs)
        (Nat.max (temp_bound_expr base)
           ((fix go (l : list expr) : nat :=
               match l with
               | nil => 0
               | e1 :: tl => Nat.max (temp_bound_expr e1) (go tl)
               end) args))
  end.

Definition fresh_temp (i : inst) : nat := temp_bound_inst i.

(** ** The transformation *)

Definition t1_temp_ref (k : nat) : expr := ERef (RVar (VLocal (LTemp k))).

Fixpoint t1_inst (k : nat) (i : inst) {struct i} : inst :=
  match i with
  | ICall lhs f args =>
      ISeq (ICall (LTemp k) f args ::
            IAssign (RVar (VLocal lhs)) (t1_temp_ref k) :: nil)
  | ISeq insts =>
      ISeq ((fix go (l : list inst) : list inst :=
               match l with
               | nil => nil
               | i1 :: tl => t1_inst k i1 :: go tl
               end) insts)
  | IIf c t e => IIf c (t1_inst k t) (t1_inst k e)
  | IWhile c b => IWhile c (t1_inst k b)
  | _ => i    (* other instructions contain no call to split *)
  end.

Definition t1_func (f : func) : func :=
  mkFunc (f_main f) (f_name f) (f_params f)
    (t1_inst (fresh_temp (f_body f)) (f_body f)).

Definition t1_prog (p : prog) : prog :=
  mkProg (List.map t1_func (p_funcs p)).

(** ** The designated temporary is always fresh (no `Admitted`)

    Anything ≥ the bound is fresh; [fresh_temp] itself is the instance
    [n = bound]. *)

Lemma temp_fresh_local_bound (k : nat) (l : local)
  (H : (temp_bound_local l <= k)%nat) : temp_fresh_local k l = true.
Proof.
  destruct l as [x|n]; simpl in *; [reflexivity|].
  apply negb_true_iff, Nat.eqb_neq. lia.
Qed.

Lemma temp_fresh_var_bound (k : nat) (x : var)
  (H : (temp_bound_var x <= k)%nat) : temp_fresh_var k x = true.
Proof.
  destruct x as [g|l]; simpl in *;
    [reflexivity|apply temp_fresh_local_bound; exact H].
Qed.

(** Nested-structure syntax needs a hand-rolled mutual induction; this is
    the first consumer of a PO-001-style scheme, done inline with nested
    fixes in proof form (Fixpoint-style lemma). *)

Fixpoint temp_fresh_expr_bound (k : nat) (e : expr) {struct e} :
    (temp_bound_expr e <= k)%nat -> temp_fresh_expr k e = true

with temp_fresh_ref_bound (k : nat) (r : ref) {struct r} :
    (temp_bound_ref r <= k)%nat -> temp_fresh_ref k r = true.
Proof.
  - destruct e; simpl; intros H; try reflexivity;
      try (apply temp_fresh_expr_bound; exact H);
      try (apply temp_fresh_ref_bound; exact H).
    + apply andb_true_intro; split;
        [apply temp_fresh_expr_bound | apply temp_fresh_expr_bound]; lia.
    + induction es as [|e1 tl IH]; simpl in *; [reflexivity|].
      apply andb_true_intro; split;
        [apply temp_fresh_expr_bound; lia | apply IH; lia].
    + induction fields as [|[f e1] tl IH]; simpl in *; [reflexivity|].
      apply andb_true_intro; split;
        [apply temp_fresh_expr_bound; lia | apply IH; lia].
    + induction pairs as [|[ke ve] tl IH]; simpl in *; [reflexivity|].
      apply andb_true_intro; split; [apply temp_fresh_expr_bound; lia|].
      apply andb_true_intro; split;
        [apply temp_fresh_expr_bound; lia | apply IH; lia].
  - destruct r; simpl; intros H.
    + apply temp_fresh_var_bound; exact H.
    + apply andb_true_intro; split;
        [apply temp_fresh_ref_bound | apply temp_fresh_expr_bound]; lia.
Qed.

Fixpoint temp_fresh_inst_bound (k : nat) (i : inst) {struct i} :
    (temp_bound_inst i <= k)%nat -> temp_fresh_inst k i = true.
Proof.
  destruct i; simpl; intros H;
    try (apply temp_fresh_expr_bound; exact H);
    try reflexivity.
  - (* ISeq *)
    induction insts as [|i1 tl IH]; simpl in *; [reflexivity|].
    apply andb_true_intro; split;
      [apply temp_fresh_inst_bound; lia | apply IH; lia].
  - (* IAssign *)
    apply andb_true_intro; split;
      [apply temp_fresh_ref_bound | apply temp_fresh_expr_bound]; lia.
  - (* IIf *)
    apply andb_true_intro; split; [apply temp_fresh_expr_bound; lia|].
    apply andb_true_intro; split; apply temp_fresh_inst_bound; lia.
  - (* IWhile *)
    apply andb_true_intro; split;
      [apply temp_fresh_expr_bound; lia | apply temp_fresh_inst_bound; lia].
  - (* ICall *)
    apply andb_true_intro; split; [apply temp_fresh_local_bound; lia|].
    apply andb_true_intro; split; [apply temp_fresh_expr_bound; lia|].
    induction args as [|e1 tl IH]; simpl in *; [reflexivity|].
    apply andb_true_intro; split;
      [apply temp_fresh_expr_bound; lia | apply IH; lia].
  - (* IPush *)
    apply andb_true_intro; split; apply temp_fresh_expr_bound; lia.
  - (* IPop *)
    apply andb_true_intro; split;
      [apply temp_fresh_local_bound; lia | apply temp_fresh_expr_bound; lia].
  - (* IExpand *)
    apply andb_true_intro; split;
      [apply temp_fresh_ref_bound; lia | apply temp_fresh_expr_bound; lia].
  - (* IDelete *)
    apply andb_true_intro; split;
      [apply temp_fresh_ref_bound; lia | apply temp_fresh_expr_bound; lia].
  - (* ISdoCall *)
    apply andb_true_intro; split; [apply temp_fresh_local_bound; lia|].
    apply andb_true_intro; split; [apply temp_fresh_expr_bound; lia|].
    induction args as [|e1 tl IH]; simpl in *; [reflexivity|].
    apply andb_true_intro; split;
      [apply temp_fresh_expr_bound; lia | apply IH; lia].
Qed.

Theorem fresh_temp_is_fresh (i : inst) :
  temp_fresh_inst (fresh_temp i) i = true.
Proof. apply temp_fresh_inst_bound. unfold fresh_temp. lia. Qed.

(** ** T-2: optional-field-access desugaring (ADR-9)

    Desugars the synthetic [EOptField] into guarded IR-Core:

      lhs := recv?.fld
        ⇒
      %k := recv ;
      if (%k = null || %k = undefined) lhs := undefined
      else lhs := %k.fld

    Correctness obligations (why this is not syntactic identity):
    - the heap is NOT touched on the nullish branch (a desugaring that
      accessed the field unconditionally would be UB where the source
      yields undefined — the equivalence proof would fail);
    - the receiver is evaluated exactly once, before the test (enforced
      syntactically; within IR-Core, expression re-evaluation is not
      independently observable — no getters — so this clause carries no
      separate observable obligation here; in full JS it does, which is
      exactly what the spec-level [FW] extension would add);
    - surrounding effect order (calls, prints) is unchanged.

    RESTRICTION (checked by [t2_ok_inst]): [EOptField] may appear only
    as the immediate right-hand side of [ILet] or of [IAssign] to a
    local variable, with an [EOptField]-free receiver.  Nested
    occurrences are out of scope for T-2. *)

Definition nullish_test (e : expr) : expr :=
  EBinary BOr (EBinary BEq e ENull) (EBinary BEq e EUndef).

Definition t2_desugar (k : nat) (lhs : local) (recv : expr) (fld : string)
  : inst :=
  ISeq (IAssign (RVar (VLocal (LTemp k))) recv ::
        IIf (nullish_test (t1_temp_ref k))
            (IAssign (RVar (VLocal lhs)) EUndef)
            (IAssign (RVar (VLocal lhs))
               (ERef (RField (RVar (VLocal (LTemp k))) (EStr fld))))
        :: nil).

Fixpoint t2_inst (k : nat) (i : inst) {struct i} : inst :=
  match i with
  | ILet x (EOptField recv fld) => t2_desugar k (LName x) recv fld
  | IAssign (RVar (VLocal l)) (EOptField recv fld) => t2_desugar k l recv fld
  | ISeq insts =>
      ISeq ((fix go (l : list inst) : list inst :=
               match l with
               | nil => nil
               | i1 :: tl => t2_inst k i1 :: go tl
               end) insts)
  | IIf c t e => IIf c (t2_inst k t) (t2_inst k e)
  | IWhile c b => IWhile c (t2_inst k b)
  | _ => i
  end.

Definition t2_func (f : func) : func :=
  mkFunc (f_main f) (f_name f) (f_params f)
    (t2_inst (fresh_temp (f_body f)) (f_body f)).

Definition t2_prog (p : prog) : prog :=
  mkProg (List.map t2_func (p_funcs p)).

(** Admissibility for T-2: [EOptField] only at binding positions. *)

Fixpoint opt_free_expr (e : expr) {struct e} : bool :=
  match e with
  | EMath _ | EBool _ | EStr _ | EUndef | ENull | EEnum _ | EClo _ _ => true
  | ERef r => opt_free_ref r
  | EUnary _ e1 => opt_free_expr e1
  | EBinary _ e1 e2 => andb (opt_free_expr e1) (opt_free_expr e2)
  | EList es =>
      (fix go (l : list expr) : bool :=
         match l with
         | nil => true
         | e1 :: tl => andb (opt_free_expr e1) (go tl)
         end) es
  | ESizeOf e1 => opt_free_expr e1
  | ERecord _ fields =>
      (fix go (l : list (string * expr)) : bool :=
         match l with
         | nil => true
         | (_, e1) :: tl => andb (opt_free_expr e1) (go tl)
         end) fields
  | EExists r => opt_free_ref r
  | ETypeOf e1 => opt_free_expr e1
  | ETypeCheck e1 _ => opt_free_expr e1
  | EYet _ => true
  | EMap pairs =>
      (fix go (l : list (expr * expr)) : bool :=
         match l with
         | nil => true
         | (ke, ve) :: tl =>
             andb (opt_free_expr ke) (andb (opt_free_expr ve) (go tl))
         end) pairs
  | EKeys m _ => opt_free_expr m
  | ECopy e1 => opt_free_expr e1
  | EOptField _ _ => false
  end

with opt_free_ref (r : ref) {struct r} : bool :=
  match r with
  | RVar _ => true
  | RField b f => andb (opt_free_ref b) (opt_free_expr f)
  end.

Definition t2_rhs_ok (e : expr) : bool :=
  match e with
  | EOptField recv _ => opt_free_expr recv
  | _ => opt_free_expr e
  end.

Fixpoint t2_ok_inst (i : inst) {struct i} : bool :=
  match i with
  | INop => true
  | ISeq insts =>
      (fix go (l : list inst) : bool :=
         match l with
         | nil => true
         | i1 :: tl => andb (t2_ok_inst i1) (go tl)
         end) insts
  | IExpr e => opt_free_expr e
  | ILet _ e => t2_rhs_ok e
  | IAssign (RVar (VLocal _)) e => t2_rhs_ok e
  | IAssign r e => andb (opt_free_ref r) (opt_free_expr e)
  | IIf c t e =>
      andb (opt_free_expr c) (andb (t2_ok_inst t) (t2_ok_inst e))
  | IWhile c b => andb (opt_free_expr c) (t2_ok_inst b)
  | ICall lhs f args =>
      andb (opt_free_expr f)
        ((fix go (l : list expr) : bool :=
            match l with
            | nil => true
            | e1 :: tl => andb (opt_free_expr e1) (go tl)
            end) args)
  | IReturn e => opt_free_expr e
  | IAssert e => opt_free_expr e
  | IPrint e => opt_free_expr e
  | IPush elem lst _ => andb (opt_free_expr elem) (opt_free_expr lst)
  | IPop _ lst _ => opt_free_expr lst
  | IExpand base fld => andb (opt_free_ref base) (opt_free_expr fld)
  | IDelete base key => andb (opt_free_ref base) (opt_free_expr key)
  | ISdoCall _ base _ args =>
      andb (opt_free_expr base)
        ((fix go (l : list expr) : bool :=
            match l with
            | nil => true
            | e1 :: tl => andb (opt_free_expr e1) (go tl)
            end) args)
  end.
