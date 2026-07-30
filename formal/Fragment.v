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

From Stdlib Require Import String ZArith List Floats Uint63.
Import ListNotations.

Set Implicit Arguments.

Local Open Scope string_scope.

(** ** ECMAScript strings as UTF-16 code units (D-1)

    ESMeta's [Str] wraps a Scala [String], i.e. a sequence of UTF-16 code
    units, and [CodeUnit] is one such unit (state/Value.scala:129, 149).
    Byte strings would make [ESizeOf]/[ESubstring] wrong for non-ASCII, so
    string VALUES carry code units.

    Record type names and record FIELD names stay Coq [string]: they are
    spec-internal identifiers.  Verified over the compiled spec: of 157
    distinct quoted field names and 474 distinct dotted field names, **zero**
    are non-ASCII (the 48 non-ASCII string literals in the spec are all
    [EYet] prose).  Arbitrary ECMAScript strings reach the heap only as
    *values* — object properties live in [MapObj] keyed by values, not in
    record field names. *)

Definition cunit := Z.           (* one UTF-16 code unit, 0..65535 *)
Definition cstr := list cunit.   (* an ECMAScript string value *)

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
| UNeg   (* Neg   : arithmetic negation *)
| UNot   (* Not   : boolean negation *)
| UAbs   (* Abs   : mathematical absolute value *)
| UFloor (* Floor : mathematical floor *)
| UBNot. (* BNot  : bitwise negation on integral Math *)

Inductive bop : Type :=
| BAdd   (* Add *)
| BSub   (* Sub *)
| BMul   (* Mul *)
| BLt    (* Lt  : Math comparison *)
| BEq    (* Eq  : structural equality *)
| BAnd   (* And   : boolean conjunction (short-circuit, see Semantics) *)
| BOr    (* Or    : boolean disjunction (short-circuit) *)
| BDiv   (* Div   : mathematical division (integers: exact only) *)
| BMod   (* Mod   : mathematical modulo *)
| BEqual (* Equal  : numeric equality *)
| BPow   (* Pow    : exponentiation *)
| BBAnd  (* BAnd   : bitwise and   on integral Math *)
| BBOr   (* BOr    : bitwise or    on integral Math *)
| BBXOr  (* BXOr   : bitwise xor   on integral Math *)
| BLShift(* LShift : left shift    on integral Math *)
| BRShift. (* RShift: right shift   on integral Math *)

(** NOTE (repository fact): the ESMeta interpreter short-circuits [And]/[Or]
    at expression-evaluation level (Interpreter.scala:251-252, 358-365).
    Whether the fragment adopts short-circuit or strict evaluation is
    Open Question OQ-7 in the architecture note; the denotation (M2) must
    pick the interpreter's behavior.  Syntax is unaffected. *)

(** ** Value conversions ([EConvert], Interpreter.scala:263-289) *)

Inductive cop : Type :=
| CToApproxNumber | CToNumber | CToBigInt | CToMath | CToCodeUnit.

(** ** Variadic operators ([EVariadic], ir/Op.scala:35-39)

    [VOp.Min], [VOp.Max], [VOp.Concat]; the interpreter's transition is
    Interpreter.scala:669-693. *)

Inductive vop : Type := VoMin | VoMax | VoConcat.

(** ** ECMAScript parse trees as values (mirrors es/Ast.scala)

    [ASyn name args rhsIdx subIdx children] mirrors
    [Syntactic(name, args, rhsIdx, children)]; [subIdx] is ESMeta's
    [Ast.subIdx], which is derived from the grammar (Ast.scala:116-128) and
    is therefore PRECOMPUTED BY THE EXPORTER rather than recomputed here —
    the model does not carry the grammar.  [ALex] mirrors [Lexical].

    [src] is the node's printed source text, i.e. ESMeta's
    [ast.toString(grammar = Some(grammar)).trim] (Interpreter.scala:227-230)
    — again grammar-derived, so again PRECOMPUTED BY THE EXPORTER.  It is a
    *derived* field: two nodes equal on the other fields have equal [src],
    which is why [ast_eqb] ignores it (ESMeta compares case-class fields
    only).  ASTs are immutable in ESMeta — [State.update] on an AST base
    throws (State.scala:78-80) — so precomputing cannot go stale. *)

(** Values a lexical SDO can produce.  ESMeta dispatches a lexical
    receiver straight to Scala (Interpreter.scala:192-193 returns
    [Interpreter.eval(lex, method)] with no call frame; that function,
    lines 521-542, is a pure function of the node's name, its lexeme and
    the method name, implemented by [ESValueParser]).  D-3 therefore has
    the exporter evaluate it and ship the answers.  The result type is a
    closed set so that [ast] need not be mutually inductive with [val];
    the exporter rejects any lexical value outside it rather than
    approximating. *)

Inductive lexval : Type :=
| LVStr (cs : cstr)
| LVMath (z : Z)
| LVNumber (f : float)
| LVBigInt (z : Z).

Inductive ast : Type :=
| ASyn (name : string) (args : list bool) (rhsIdx subIdx : nat)
       (children : list (option ast)) (src : cstr)
(** [sdos] maps a lexical SDO name ("StringValue", "NumericValue", "MV",
    "SV", "TV", "TRV" — the six of Interpreter.scala:525-536) to the value
    ESMeta computes for THIS lexeme.  A method absent from the table is UB,
    which is exactly ESMeta's [InvalidAstField]. *)
| ALex (name : string) (str : string) (src : cstr)
       (sdos : list (string * lexval)).

(** ** Restricted type expressions (for [ETypeCheck], ADR-11)

    ESMeta's [Type] wraps the full [esmeta.ty] language (unions, record
    field maps, …).  The compiled spec uses type *tests* far more simply:
    overwhelmingly `(? x: Completion)` / `(? x: Abrupt)` plus a handful of
    value-kind and record-name tests.  We mirror exactly that much; the
    exporter rejects any [Ty] outside this grammar, so unsupported tests
    are reported rather than silently mis-modelled. *)

Inductive tyexp : Type :=
| TRecord (tname : string)   (* record with this type name (exact) *)
| TCompletion                (* a CompletionRecord *)
| TAbrupt                    (* CompletionRecord whose Type <> ~normal~ *)
| TNormal                    (* CompletionRecord whose Type  = ~normal~ *)
| TList                      (* list object *)
| TMapTy                     (* map object *)
| TStrTy | TBoolTy | TMathTy | TUndefTy | TNullTy | TEnumTy | TCloTy
| TNumberTy | TBigIntTy | TCodeUnitTy | TInfinityTy | TAstTy
(* AstTy.Simple / AstTy.Detail (ty/AstTy.scala:76-81).  [TAstNames] matches
   against the node's [types], the names down its single-child chain
   (es/Ast.scala:46-53), not just its own name. *)
| TAstNames (ns : list string)
| TAstDetail (n : string) (idx : nat)
(* [ValueTy] is a product of per-kind lattices and [contains] dispatches on
   the value's kind (ty/ValueTy.scala:167-188), so a type with several
   non-bottom components is exactly a disjunction. *)
| TUnion (ts : list tyexp)
(* ListTy.Elem (ty/ListTy.scala:57-60): EVERY element satisfies [t].  The
   exporter only emits this when [t] itself needs no heap resolution, so
   one level of element lookup suffices; see [ty_addrs_needed]. *)
| TListOf (t : tyexp).

(** ** Expressions and references (mutual)

    Mirrors the [Expr]/[Ref] split of ir/Expr.scala and ir/Ref.scala. *)

Inductive expr : Type :=
| EMath (z : Z)                                 (* EMath — ADR-5: Z only *)
| EBool (b : bool)                              (* EBool *)
| EStr (cs : cstr)                              (* EStr — D-1 *)
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
| EExists (r : ref)                             (* EExists: ref present? *)
| ETypeOf (e : expr)                            (* ETypeOf: kind string *)
| ETypeCheck (e : expr) (t : tyexp)             (* ETypeCheck (ADR-11) *)
| EYet (msg : string)                           (* EYet: unimplemented -> UB *)
| EMap (pairs : list (expr * expr))             (* EMap: map allocation *)
| EKeys (m : expr) (intSorted : bool)           (* EKeys: key list *)
| ECopy (e : expr)                              (* ECopy: shallow copy *)
| ENumber (f : float)                           (* ENumber: IEEE-754 double *)
| EBigInt (z : Z)                               (* EBigInt *)
| EInfinity (pos : bool)                        (* EInfinity: extended Math *)
| ECodeUnit (c : cunit)                         (* ECodeUnit *)
| EConvert (op : cop) (e : expr)                (* EConvert, flat cases *)
| EToStr (e : expr) (radix : option expr)       (* EConvert with COp.ToStr *)
| EVariadic (op : vop) (es : list expr)         (* EVariadic *)
| EContains (lst : expr) (e : expr)             (* EContains *)
| EGrammarSymbol (name : string) (params : list bool)
| EInstanceOf (e : expr) (target : expr)        (* EInstanceOf *)
| ESubstring (e : expr) (from : expr) (to : option expr)
| ESourceText (e : expr)                        (* ESourceText *)
| EParse (code : expr) (rule : expr)            (* EParse — cached case *)
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
| IPrint (e : expr)                             (* IPrint — the Log effect *)
| IPush (elem : expr) (lst : expr) (front : bool)   (* IPush *)
| IPop (lhs : local) (lst : expr) (front : bool)    (* IPop *)
| IExpand (base : ref) (fld : expr)                 (* IExpand *)
| IDelete (base : ref) (key : expr)                 (* IDelete *)
| ISdoCall (lhs : local) (base : expr) (method : string) (args : list expr).
    (* ISdoCall — syntax-directed dispatch on an AST value
       (ir/Inst.scala:43, Interpreter.scala:177-192) *)

(** ** Functions and programs

    Mirrors ir/Func.scala with [kind], [retTy], and per-parameter types
    dropped (limitation L-3).  Parameters are Names, non-optional. *)

Record func : Type := mkFunc {
  f_main   : bool;
  f_name   : irname;
  f_params : list string;
  f_body   : inst;
}.

(** A program plus the two immutable *run parameters* ESMeta keeps on
    [State]: the source text being executed and the AST it was already
    parsed into (state/State.scala:17-18).  Both are `val`s — nothing ever
    writes them — and [EParse]'s fast path compares against exactly these
    (Interpreter.scala:198-209).  [mkProg] keeps the old one-argument
    shape for the many programs that have neither. *)


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
| VStr (cs : cstr)
| VUndef
| VNull
| VEnum (name : string)
| VAddr (a : nat)
| VClo (fn : irname) (captured : list (string * val))
| VAst (a : ast)                 (* AstValue — state/Value.scala:83 *)
| VNumber (f : float)            (* Number   — state/Value.scala:143 *)
| VBigInt (z : Z)                (* BigInt   — state/Value.scala:146 *)
| VInfinity (pos : bool)         (* Infinity — state/Value.scala:123 *)
| VCodeUnit (c : cunit)          (* CodeUnit — state/Value.scala:129 *)
| VGrammarSymbol (name : string) (params : list bool).
                                 (* GrammarSymbol — state/Value.scala:86 *)

(** [val] nests [list (string * val)]; the auto-generated induction
    principle is too weak for closure environments.  A proper mutual
    induction principle is PO-000 groundwork and will be added with the
    first proof that needs it (Milestone 2). *)

(** ** Heap objects (fragment of state/Obj.scala)

    Defined here rather than in [Domain.v] because [prog] carries an
    exported initial heap. *)

Variant obj : Type :=
| OList (vs : list val)                          (* ListObj *)
| ORecord (tname : string) (fields : list (string * val)) (* RecordObj *)
| OMap (entries : list (val * val)).             (* MapObj — insertion-ordered
    (state/Obj.scala:129 uses a LinkedHashMap; EKeys depends on that order) *)

Record prog : Type := mkProgFull {
  p_funcs  : list func;
  p_source : option cstr;
  p_cached : option ast;
  (* Exported initial state (Initialize.scala:29-40).  [p_heap] is indexed
     by address; ESMeta's initial heap uses only NamedAddrs, which the
     exporter renumbers to list positions (ADR-16).  A slot is [None] when
     the address is REFERENCED but not mapped — ESMeta's initial globals
     include such an address (#CandidateExecution), and dereferencing it
     throws UnknownAddr (Heap.scala:19).  The slot must still exist so that
     allocation, which appends, cannot later hand out that index. *)
  p_globals : list (string * val);
  p_heap    : list (option obj);
}.

Definition mkProg (fs : list func) : prog :=
  mkProgFull fs None None nil nil.

(** ASCII convenience for hand-written programs and the exporter's ASCII
    fast path: turn a Coq byte string into code units.  Only sound for
    ASCII; the exporter emits explicit code units for anything else. *)
Definition cu (s : string) : cstr :=
  List.map (fun c => Z.of_nat (Ascii.nat_of_ascii c)) (list_ascii_of_string s).

(** Inverse for record-field lookup: a [VStr] key must be matched against a
    field name, which is a Coq [string].  Defined only on ASCII, which the
    empirical check above justifies; non-ASCII yields [None] and the caller
    raises UB rather than guessing. *)
Fixpoint ascii_of_cstr (cs : cstr) : option string :=
  match cs with
  | nil => Some EmptyString
  | z :: tl =>
      if andb (0 <=? z)%Z (z <? 128)%Z
      then option_map (String (Ascii.ascii_of_nat (Z.to_nat z)))
             (ascii_of_cstr tl)
      else None
  end.

(** All-ASCII test, and the round trip that record-field lookup relies on:
    encoding an ASCII field name to code units and decoding it back is the
    identity.  Proved once so proofs and tactics do not have to unfold the
    character encoding. *)
Fixpoint ascii_str (s : string) : bool :=
  match s with
  | EmptyString => true
  | String c t => andb (Nat.ltb (Ascii.nat_of_ascii c) 128) (ascii_str t)
  end.

Lemma ascii_of_cstr_cu (s : string) :
  ascii_str s = true -> ascii_of_cstr (cu s) = Some s.
Proof.
  induction s as [|c t IH]; simpl; intros H; [reflexivity|].
  apply Bool.andb_true_iff in H as [Hc Ht].
  apply Nat.ltb_lt in Hc.
  assert (Hz : ((0 <=? Z.of_nat (Ascii.nat_of_ascii c))%Z
                && (Z.of_nat (Ascii.nat_of_ascii c) <? 128)%Z)%bool = true).
  { apply Bool.andb_true_iff; split.
    - apply Z.leb_le. apply Nat2Z.is_nonneg.
    - apply Z.ltb_lt. change 128%Z with (Z.of_nat 128).
      apply Nat2Z.inj_lt. exact Hc. }
  unfold cu in *; simpl. rewrite Hz.
  rewrite Nat2Z.id, Ascii.ascii_nat_embedding.
  rewrite IH by exact Ht. reflexivity.
Qed.

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
