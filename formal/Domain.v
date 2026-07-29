(** * ESMetaFV.Domain — pure semantic domain of IR-Core

    Everything here is framework-agnostic (Coq stdlib only): completions,
    pure operator evaluation, local environments, heap objects, and the
    pure helpers shared by the ITree denotation ([Semantics.v]) and the
    executable reference interpreter ([Exec.v]).

    Fidelity notes are attached to each definition; the authoritative
    catalogue is the header of [Semantics.v] and the research log. *)

From Stdlib Require Import String ZArith List Bool.
Import ListNotations.

From ESMetaFV Require Import Fragment TyModel.

Set Implicit Arguments.

Local Open Scope string_scope.

(** ** Completions (architecture note §4, layer 4)

    IR-Core's only instruction-level abrupt control is [IReturn]
    (ECMAScript-level throw is completion-record *data* in ESMeta, not
    control [RF]).  Break/Continue are not IR constructs. *)

Variant completion : Type :=
| CNormal (v : val)
| CReturn (v : val).

(** ** Pure value operations *)

Definition eval_uop (op : uop) (v : val) : option val :=
  match op, v with
  | UNeg, VMath z => Some (VMath (- z))
  | UNot, VBool b => Some (VBool (negb b))
  | UAbs, VMath z => Some (VMath (Z.abs z))
  | UFloor, VMath z => Some (VMath z)   (* integers: floor is identity *)
  | _, _ => None
  end.

(** Structural equality on parse trees (needed by [val_eqb]). *)
Fixpoint ast_eqb (a1 a2 : ast) {struct a1} : bool :=
  match a1, a2 with
  | ALex n1 s1, ALex n2 s2 => andb (String.eqb n1 n2) (String.eqb s1 s2)
  | ASyn n1 g1 r1 b1 c1, ASyn n2 g2 r2 b2 c2 =>
      andb (String.eqb n1 n2)
        (andb (Nat.eqb r1 r2)
           (andb (Nat.eqb b1 b2)
              (andb ((fix bs (l1 l2 : list bool) : bool :=
                        match l1, l2 with
                        | nil, nil => true
                        | x :: t1, y :: t2 => andb (Bool.eqb x y) (bs t1 t2)
                        | _, _ => false
                        end) g1 g2)
                 ((fix cs (l1 l2 : list (option ast)) : bool :=
                     match l1, l2 with
                     | nil, nil => true
                     | None :: t1, None :: t2 => cs t1 t2
                     | Some x :: t1, Some y :: t2 =>
                         andb (ast_eqb x y) (cs t1 t2)
                     | _, _ => false
                     end) c1 c2))))
  | _, _ => false
  end.

Definition ast_name (a : ast) : string :=
  match a with ASyn n _ _ _ _ => n | ALex n _ => n end.

Definition ast_children (a : ast) : list (option ast) :=
  match a with ASyn _ _ _ _ cs => cs | ALex _ _ => nil end.

(** Production chains (Ast.scala:38-44): the node itself, then, while a
    node has exactly one present child, that child — the fall-through used
    by SDO lookup.  Fuel is the tree size, which bounds the chain length,
    because the single-present-child projection is not a structural
    subterm the guard checker can follow. *)

Fixpoint ast_size (a : ast) : nat :=
  match a with
  | ALex _ _ => 1
  | ASyn _ _ _ _ cs =>
      S ((fix go (l : list (option ast)) : nat :=
            match l with
            | nil => 0
            | None :: t => go t
            | Some c :: t => ast_size c + go t
            end) cs)
  end.

Definition single_present (l : list (option ast)) : option ast :=
  match List.filter (fun o => match o with Some _ => true | None => false end) l with
  | Some c :: nil => Some c
  | _ => None
  end.

Fixpoint ast_chain_fuel (n : nat) (a : ast) : list ast :=
  a :: match n with
       | O => nil
       | S n' =>
           match single_present (ast_children a) with
           | Some c => ast_chain_fuel n' c
           | None => nil
           end
       end.

Definition ast_chain (a : ast) : list ast := ast_chain_fuel (ast_size a) a.

(** Decimal rendering of a natural number, shared by the denotation and
    the executable interpreter so the two build identical SDO names. *)
Fixpoint nat_to_dec (fuel n : nat) (acc : string) : string :=
  match fuel with
  | O => acc
  | S fuel' =>
      let d := Nat.modulo n 10 in
      let ch := match d with
                | 0 => "0" | 1 => "1" | 2 => "2" | 3 => "3" | 4 => "4"
                | 5 => "5" | 6 => "6" | 7 => "7" | 8 => "8" | _ => "9"
                end in
      let acc' := (ch ++ acc)%string in
      let q := Nat.div n 10 in
      match q with O => acc' | _ => nat_to_dec fuel' q acc' end
  end.

Definition nat_str (n : nat) : string := nat_to_dec (S n) n "".

(** SDO target resolution (Ast.scala:102-113): for each node in the chain
    try `Name[rhsIdx,subIdx].Method`, else `DEFAULT:Method`; first hit
    wins.  Existence is decided against the program's function names. *)
Definition sdo_candidate (a : ast) (m : string) : string :=
  match a with
  | ASyn n _ r b _ =>
      (n ++ "[" ++ nat_str r ++ "," ++ nat_str b ++ "]." ++ m)%string
  | ALex n _ => (n ++ "[0,0]." ++ m)%string
  end.

Fixpoint name_mem (x : string) (l : list string) : bool :=
  match l with
  | nil => false
  | y :: tl => if String.eqb x y then true else name_mem x tl
  end.

Definition sdo_resolve (fnames : list string) (a : ast) (m : string)
  : option (ast * string) :=
  let dflt := ("DEFAULT:" ++ m)%string in
  (fix go (l : list ast) : option (ast * string) :=
     match l with
     | nil => None
     | a0 :: tl =>
         let c := sdo_candidate a0 m in
         if name_mem c fnames then Some (a0, c)
         else if name_mem dflt fnames then Some (a0, dflt)
         else go tl
     end) (ast_chain a).

(** Structural equality, mirroring Scala case-class equality on the
    fragment's value forms (BOp.Eq, Interpreter.scala:566ff). *)
Fixpoint val_eqb (v1 v2 : val) {struct v1} : bool :=
  match v1, v2 with
  | VMath z1, VMath z2 => Z.eqb z1 z2
  | VBool b1, VBool b2 => Bool.eqb b1 b2
  | VStr s1, VStr s2 => String.eqb s1 s2
  | VUndef, VUndef => true
  | VNull, VNull => true
  | VEnum n1, VEnum n2 => String.eqb n1 n2
  | VAddr a1, VAddr a2 => Nat.eqb a1 a2
  | VAst a1, VAst a2 => ast_eqb a1 a2
  | VClo f1 c1, VClo f2 c2 =>
      andb (String.eqb f1 f2)
        ((fix go (l1 l2 : list (string * val)) : bool :=
            match l1, l2 with
            | nil, nil => true
            | (x1, u1) :: t1, (x2, u2) :: t2 =>
                andb (String.eqb x1 x2) (andb (val_eqb u1 u2) (go t1 t2))
            | _, _ => false
            end) c1 c2)
  | _, _ => false
  end.

(** Strict binary operators.  [BAnd]/[BOr] are handled (short-circuit) by
    the interpreters and must not reach here. *)
Definition eval_bop (op : bop) (v1 v2 : val) : option val :=
  match op, v1, v2 with
  | BAdd, VMath z1, VMath z2 => Some (VMath (z1 + z2))
  | BSub, VMath z1, VMath z2 => Some (VMath (z1 - z2))
  | BMul, VMath z1, VMath z2 => Some (VMath (z1 * z2))
  | BLt, VMath z1, VMath z2 => Some (VBool (Z.ltb z1 z2))
  | BEq, _, _ => Some (VBool (val_eqb v1 v2))
  (* Numeric equality; on the fragment's integer Math values this
     coincides with structural equality (Interpreter.scala BOp.Equal). *)
  | BEqual, VMath z1, VMath z2 => Some (VBool (Z.eqb z1 z2))
  (* ESMeta's Math division rounds to DECIMAL128 (Interpreter.scala:584);
     to avoid modelling that artifact (ADR-5) we admit division only when
     it is exact on integers, and leave it undefined otherwise. *)
  | BDiv, VMath z1, VMath z2 =>
      if Z.eqb z2 0 then None
      else if Z.eqb (Z.rem z1 z2) 0 then Some (VMath (Z.quot z1 z2)) else None
  | BMod, VMath z1, VMath z2 =>
      if Z.eqb z2 0 then None else Some (VMath (Z.modulo z1 z2))
  | _, _, _ => None
  end.

(** ** Local environments (pure; ADR-6)

    Mirrors the flat, unscoped [MMap[Local, Value]] of a call context
    (state/Context.scala:10-53): lookup fails on absent locals (read of an
    unknown variable is UB), update defines unconditionally. *)

Definition env : Type := list (local * val).

Fixpoint env_lookup (ρ : env) (l : local) : option val :=
  match ρ with
  | nil => None
  | (l', v) :: tl => if local_eqb l l' then Some v else env_lookup tl l
  end.

Fixpoint env_update (l : local) (v : val) (ρ : env) : env :=
  match ρ with
  | nil => (l, v) :: nil
  | (l', v') :: tl =>
      if local_eqb l l' then (l, v) :: tl else (l', v') :: env_update l v tl
  end.

Lemma local_eqb_refl (l : local) : local_eqb l l = true.
Proof. apply local_eqb_eq. reflexivity. Qed.

(** Reading back the variable just written, in an arbitrary environment —
    the key environment fact behind T-1's fresh-temporary copy. *)
Lemma env_lookup_update_same (ρ : env) (l : local) (v : val) :
  env_lookup (env_update l v ρ) l = Some v.
Proof.
  induction ρ as [|[l' v'] tl IH]; simpl.
  - rewrite local_eqb_refl. reflexivity.
  - destruct (local_eqb l l') eqn:Heq; simpl.
    + rewrite local_eqb_refl. reflexivity.
    + rewrite Heq. exact IH.
Qed.

(** ** Heap objects (fragment of state/Obj.scala) *)

Variant obj : Type :=
| OList (vs : list val)                          (* ListObj *)
| ORecord (tname : string) (fields : list (string * val)) (* RecordObj *)
| OMap (entries : list (val * val)).             (* MapObj — insertion-ordered
    (state/Obj.scala:129 uses a LinkedHashMap; EKeys depends on that order) *)

Fixpoint map_lookup (es : list (val * val)) (k : val) : option val :=
  match es with
  | nil => None
  | (k', v) :: tl => if val_eqb k k' then Some v else map_lookup tl k
  end.

(* insertion-ordered update: existing key keeps its position, new key
   appends (mirrors `m.map += key -> value`) *)
Fixpoint map_insert (k : val) (v : val) (es : list (val * val))
  : list (val * val) :=
  match es with
  | nil => (k, v) :: nil
  | (k', v') :: tl =>
      if val_eqb k k' then (k, v) :: tl else (k', v') :: map_insert k v tl
  end.

Fixpoint map_delete (k : val) (es : list (val * val)) : list (val * val) :=
  match es with
  | nil => nil
  | (k', v') :: tl =>
      if val_eqb k k' then tl else (k', v') :: map_delete k tl
  end.

Fixpoint fields_lookup (fs : list (string * val)) (x : string) : option val :=
  match fs with
  | nil => None
  | (x', v) :: tl => if String.eqb x x' then Some v else fields_lookup tl x
  end.

(* Record-field write is an unconditional insert-or-update, mirroring
   [r.map += field -> value] (state/Obj.scala:29-30) — OQ-11 resolved
   2026-07-29: ESMeta creates the field if absent. *)
Fixpoint fields_insert (x : string) (v : val) (fs : list (string * val))
  : list (string * val) :=
  match fs with
  | nil => (x, v) :: nil
  | (x', v') :: tl =>
      if String.eqb x x'
      then (x, v) :: tl
      else (x', v') :: fields_insert x v tl
  end.

(** ** Type tests for [ETypeCheck] (ADR-11)

    ESMeta evaluates `(? x : T)` as [T.contains(value, state)]
    (Interpreter.scala:315-316).  On record types containment follows the
    type model's hierarchy AND the declared field refinements:
    `AbruptCompletion extends CompletionRecord { Type : Enum[~break~,
    ~continue~, ~return~, ~throw~] }` while `NormalCompletion` refines
    `Type : Enum[~normal~]` — and at runtime a completion is stored as a
    record whose own tname may be the base `CompletionRecord`
    (state/State.scala:169-175).  We therefore decide Abrupt/Normal by the
    `Type` field, and other record tests by the exported subtyping
    relation ([TyModel.v], generated from esmeta.ty.TyModel.parentOf).

    Field refinements beyond Completion's `Type` are NOT modelled; the
    exporter only emits [tyexp]s in this grammar, so anything else is
    reported rather than silently mis-modelled.  Validation of these tests
    against ESMeta is by the differential harness. *)

Definition completion_type (fs : list (string * val)) : option string :=
  match fields_lookup fs "Type" with
  | Some (VEnum n) => Some n
  | _ => None
  end.

Definition ty_check_obj (t : tyexp) (o : obj) : bool :=
  match o, t with
  | OList _, TList => true
  | OMap _, TMapTy => true
  | ORecord tn _, TRecord want => record_subtype tn want
  | ORecord tn _, TCompletion => record_subtype tn "CompletionRecord"
  | ORecord tn fs, TAbrupt =>
      andb (record_subtype tn "CompletionRecord")
        (match completion_type fs with
         | Some n => negb (String.eqb n "normal")
         | None => false
         end)
  | ORecord tn fs, TNormal =>
      andb (record_subtype tn "CompletionRecord")
        (match completion_type fs with
         | Some n => String.eqb n "normal"
         | None => false
         end)
  | _, _ => false
  end.

Definition ty_check_prim (t : tyexp) (v : val) : bool :=
  match v, t with
  | VStr _, TStrTy => true
  | VBool _, TBoolTy => true
  | VMath _, TMathTy => true
  | VUndef, TUndefTy => true
  | VNull, TNullTy => true
  | VEnum _, TEnumTy => true
  | VClo _ _, TCloTy => true
  | _, _ => false
  end.

(** [ETypeOf] (Interpreter.scala:297-309).  The fragment has no Number or
    BigInt values, and Math/Enum/Clo are not contained in ObjectT or
    SymbolT, so they yield "SpecType" exactly as ESMeta does.  Addresses
    need ObjectT/SymbolT containment, which depends on field refinements
    we do not model: those are left to the caller as UB (see Semantics.v),
    NOT guessed. *)
Definition typeof_prim (v : val) : option string :=
  match v with
  | VStr _ => Some "String"
  | VBool _ => Some "Boolean"
  | VUndef => Some "Undefined"
  | VNull => Some "Null"
  | VMath _ | VEnum _ | VClo _ _ | VAst _ => Some "SpecType"
  | VAddr _ => None
  end.

(** Keys of a record/map as a list of values (EKeys, state/Obj.scala:88-99).
    The [intSorted] variant filters and numerically sorts map keys; that
    path is not modelled (the caller raises UB). *)
Definition obj_keys (o : obj) : option (list val) :=
  match o with
  | ORecord _ fs => Some (List.map (fun p => VStr (fst p)) fs)
  | OMap es => Some (List.map fst es)
  | OList _ => None
  end.

Definition obj_size (o : obj) : option nat :=
  match o with
  | OList vs => Some (List.length vs)
  | _ => None    (* Obj.size throws InvalidSizeOf for non-lists *)
  end.

Fixpoint list_update {A} (n : nat) (a : A) (l : list A) : option (list A) :=
  match l, n with
  | nil, _ => None
  | _ :: tl, O => Some (a :: tl)
  | hd :: tl, S n' => option_map (fun tl' => hd :: tl') (list_update n' a tl)
  end.

(** ** Call-boundary helpers *)

Fixpoint init_env (params : list string) (args : list val) : option env :=
  match params, args with
  | nil, nil => Some nil
  | p :: ps, a :: aas =>
      option_map (fun ρ => (LName p, a) :: ρ) (init_env ps aas)
  | _, _ => None    (* strict arity; see Semantics.v fidelity notes *)
  end.

Definition captured_env (cs : list (string * val)) : env :=
  List.map (fun '(x, v) => (LName x, v)) cs.
