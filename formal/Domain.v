(** * ESMetaFV.Domain — pure semantic domain of IR-Core

    Everything here is framework-agnostic (Coq stdlib only): completions,
    pure operator evaluation, local environments, heap objects, and the
    pure helpers shared by the ITree denotation ([Semantics.v]) and the
    executable reference interpreter ([Exec.v]).

    Fidelity notes are attached to each definition; the authoritative
    catalogue is the header of [Semantics.v] and the research log. *)

From Stdlib Require Import String ZArith List Bool.
Import ListNotations.

From ESMetaFV Require Import Fragment.

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
  | _, _ => None
  end.

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
| ORecord (tname : string) (fields : list (string * val)). (* RecordObj *)

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
