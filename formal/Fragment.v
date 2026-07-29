(** * ESMetaFV.Fragment — IR-Core fragment syntax

    A small, hand-mirrored fragment of the ESMeta IR
    (ESMeta 0.7.3, ecma262 submodule 84b38ad8 / es2025).

    Mirrored from (Scala sources, repository facts):
    - [src/main/scala/esmeta/ir/Inst.scala]  (16 constructors; we take 11)
    - [src/main/scala/esmeta/ir/Expr.scala]  (39 constructors; we take 12)
    - [src/main/scala/esmeta/ir/Ref.scala]   (4 constructors; we take all 4)
    - [src/main/scala/esmeta/ir/Op.scala]    (52 operator cases; we take 9)
    - [src/main/scala/esmeta/state/Value.scala] (16 constructors; we take 8)

    Deliberate deviations from ESMeta, each recorded as an ADR or
    limitation in [docs/formal-verification/itree-transpiler-plan.md]:
    - Mathematical values are restricted to integers ([Z]) — ADR-5.
      ESMeta's [Math] is an unbounded-precision decimal; on the included
      operators (+, -, *, <, =) integer arithmetic is exact in both.
    - Parameter/return type annotations and [FuncKind] are dropped;
      the fragment is untyped (limitation L-3).
    - Excluded instruction/expression forms (continuations, AST values,
      IEEE-754 numbers, maps, parsing, sdo-calls, push/pop, expand/delete)
      are NOT claimed to be supported.

    This file is intentionally framework-agnostic: it depends only on the
    Coq standard library, so the syntax can be reused even if the proof
    framework changes.  Semantics lives in later files (Milestone 2). *)

From Stdlib Require Import String ZArith List.
Import ListNotations.

Set Implicit Arguments.

Local Open Scope string_scope.

(** ** Identifiers *)

(* Function names.  Named [irname] (not [fname]) to avoid shadowing the
   CRIS framework's [Fn.fname] in files importing both. *)
Definition irname := string.

(** Locals: named variables and compiler temporaries.
    Mirrors [Local = Name | Temp] (ir/Ref.scala:15-16). *)
Inductive local : Type :=
| LName (x : string)
| LTemp (n : nat).

(** Variables: globals and locals.  Mirrors [Var = Global | Local]. *)
Inductive var : Type :=
| VGlobal (x : string)
| VLocal (l : local).

(** ** Operators (subset of ir/Op.scala) *)

Inductive uop : Type :=
| UNeg   (* Neg  : arithmetic negation *)
| UNot.  (* Not  : boolean negation *)

Inductive bop : Type :=
| BAdd   (* Add *)
| BSub   (* Sub *)
| BMul   (* Mul *)
| BLt    (* Lt  : Math comparison *)
| BEq    (* Eq  : structural equality *)
| BAnd   (* And : strict boolean conjunction (non-short-circuit) *)
| BOr.   (* Or  : strict boolean disjunction (non-short-circuit) *)

(** NOTE (repository fact): the ESMeta interpreter short-circuits [And]/[Or]
    at expression-evaluation level (Interpreter.scala:251-252, 358-365).
    Whether the fragment adopts short-circuit or strict evaluation is
    Open Question OQ-7 in the architecture note; the denotation (M2) must
    pick the interpreter's behavior.  Syntax is unaffected. *)

(** ** Expressions and references (mutual)

    Mirrors the [Expr]/[Ref] split of ir/Expr.scala and ir/Ref.scala. *)

Inductive expr : Type :=
| EMath (z : Z)                                 (* EMath — ADR-5: Z only *)
| EBool (b : bool)                              (* EBool *)
| EStr (s : string)                             (* EStr *)
| EUndef                                        (* EUndef *)
| ENull                                         (* ENull *)
| EEnum (name : string)                         (* EEnum, e.g. ~empty~ *)
| ERef (r : ref)                                (* ERef *)
| EUnary (op : uop) (e : expr)                  (* EUnary *)
| EBinary (op : bop) (e1 e2 : expr)             (* EBinary *)
| EClo (fn : irname) (captured : list string)   (* EClo: clo<"f">(...) *)
| EList (es : list expr)                        (* EList: list allocation *)
| ESizeOf (e : expr)                            (* ESizeOf *)
| ERecord (tname : string) (fields : list (string * expr))
    (* ERecord — mirrored from ir/Expr.scala:56; fields evaluate
       left-to-right in declaration order (Interpreter.scala:337-338),
       allocation like lists (Heap.scala:50-53; RecordObj.apply inserts
       exactly the given pairs, Obj.scala:113-121) *)
| EOptField (recv : expr) (fld : string)
    (* SYNTHETIC (ADR-9) — NOT an ESMeta IR construct.  "recv?.fld":
       evaluate the receiver once; if it is Null or Undef, yield Undef
       WITHOUT touching the heap; otherwise read the record field.
       Exists as the source form of the T-2 desugaring proof
       (Transform.v, T2Proof.v).  The Scala exporter never produces it;
       ESMeta cannot execute it — its semantics is defined here and
       validated only inside the model (see ADR-9 for the honesty
       boundary). *)

with ref : Type :=
| RVar (x : var)                                (* Global / Name / Temp *)
| RField (base : ref) (field : expr).           (* Field: base[expr] *)

(** ** Instructions

    Mirrors ir/Inst.scala.  [ISeq] keeps the tree shape; the CFG layer of
    ESMeta is deliberately NOT mirrored (ADR-4: we denote the tree IR and
    connect to ESMeta's CFG interpreter by differential testing first). *)

Inductive inst : Type :=
| INop                                          (* INop *)
| ISeq (insts : list inst)                      (* ISeq *)
| IExpr (e : expr)                              (* IExpr *)
| ILet (x : string) (e : expr)                  (* ILet (lhs is a Name) *)
| IAssign (r : ref) (e : expr)                  (* IAssign *)
| IIf (c : expr) (thn els : inst)               (* IIf *)
| IWhile (c : expr) (body : inst)               (* IWhile *)
| ICall (lhs : local) (f : expr) (args : list expr) (* ICall *)
| IReturn (e : expr)                            (* IReturn *)
| IAssert (e : expr)                            (* IAssert — ADR-7 *)
| IPrint (e : expr).                            (* IPrint — the Log effect *)

(** ** Functions and programs

    Mirrors ir/Func.scala with [kind], [retTy], and per-parameter types
    dropped (limitation L-3).  Parameters are Names, non-optional. *)

Record func : Type := mkFunc {
  f_main   : bool;
  f_name   : irname;
  f_params : list string;
  f_body   : inst;
}.

Record prog : Type := mkProg {
  p_funcs : list func;
}.

(** ** Semantic values (domain of the M2 denotation)

    Mirrors state/Value.scala restricted to the fragment.  Addresses are
    naturals, matching ESMeta's deterministic-counter allocation
    (state/Heap.scala:62-67, addresses never reused).  A closure carries
    its defining function's name and captured environment, mirroring
    [Clo(func, captured)] (state/Value.scala:73) with the function
    identified by name rather than by CFG reference. *)

Inductive val : Type :=
| VMath (z : Z)
| VBool (b : bool)
| VStr (s : string)
| VUndef
| VNull
| VEnum (name : string)
| VAddr (a : nat)
| VClo (fn : irname) (captured : list (string * val)).

(** [val] nests [list (string * val)]; the auto-generated induction
    principle is too weak for closure environments.  A proper mutual
    induction principle is PO-000 groundwork and will be added with the
    first proof that needs it (Milestone 2). *)

(** ** Basic decidable equalities (needed pervasively from M2 on) *)

Definition local_eqb (l1 l2 : local) : bool :=
  match l1, l2 with
  | LName x1, LName x2 => String.eqb x1 x2
  | LTemp n1, LTemp n2 => Nat.eqb n1 n2
  | _, _ => false
  end.

Lemma local_eqb_eq (l1 l2 : local) : local_eqb l1 l2 = true <-> l1 = l2.
Proof.
  destruct l1 as [x1|n1], l2 as [x2|n2]; simpl; split; intro H;
    try discriminate.
  - f_equal. apply String.eqb_eq. exact H.
  - inversion H; subst. apply String.eqb_refl.
  - f_equal. apply Nat.eqb_eq. exact H.
  - inversion H; subst. apply Nat.eqb_refl.
Qed.
