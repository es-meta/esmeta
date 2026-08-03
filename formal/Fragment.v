(** * ESMetaFV.Fragment — executable ESMeta IR syntax

    This file began as a small IR-Core fragment and now contains the broad
    executable mirror used by the generated ECMA-262 specification and the
    Test262 ITree runner (ESMeta 0.7.3, ecma262 es2025).  The inductives below,
    together with [FVSpecScan], are the source of truth for current coverage;
    historical constructor counts in the original architecture plan are not.

    Mirrored from (Scala sources, repository facts):
    - [src/main/scala/esmeta/ir/Inst.scala]
    - [src/main/scala/esmeta/ir/Expr.scala]
    - [src/main/scala/esmeta/ir/Ref.scala]
    - [src/main/scala/esmeta/ir/Op.scala]
    - [src/main/scala/esmeta/state/Value.scala]

    Deliberate deviations from ESMeta, each recorded as an ADR or
    limitation in [formal/docs/formal-verification/itree-transpiler-plan.md]:
    - Mathematical values are restricted to integers ([Z]) — ADR-5.
      ESMeta's [Math] is an unbounded-precision decimal; on the included
      operators (+, -, *, <, =) integer arithmetic is exact in both.
    - Parameter/return annotations and [FuncKind] are retained as proof
      metadata.  The executable semantics remains dynamically typed; an
      annotation carries an executable [tyexp] only when the exporter can
      translate it exactly, and otherwise keeps the original ESMeta text.
    - Unsupported cases are rejected or represented explicitly as [EYet]; the
      exporter and coverage scans report them rather than silently
      approximating ESMeta behavior.

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

(** Mathematical host operators ([EMathOp], ir/Op.scala:43-46).

    Their transcendental behavior is deliberately not approximated in
    Rocq.  The semantics evaluates and validates operands, then consults
    the deterministic typed host cache. *)
Inductive mop : Type :=
| MExpm1 | MLog10 | MLog2 | MCos | MCbrt | MExp | MCosh | MSinh | MTanh
| MAcos | MAcosh | MAsinh | MAtanh | MAsin | MAtan2 | MAtan | MLog1p
| MLog | MSin | MSqrt | MTan.

(** Closed operation tag for the generated Number -> Math -> Number
    composites whose result is computed by ESMeta's current BigDecimal
    path.  Keeping this distinct from [bop] prevents an accidental raw
    binary64 fallback. *)
Inductive number_math_op : Type :=
| NMAdd | NMMul | NMDiv | NMPow.

(** Closed tags for the generated [Lt]/[Equal] comparisons after [ToMath].
    Mixed Number/integral-Math comparisons use ESMeta's
    [BigDecimal(Double, UNLIMITED)] conversion at the typed host boundary. *)
Inductive number_math_compare_op : Type :=
| NMCLt | NMCEqual.

Inductive number_math_compare_direction : Type :=
| NMCNumberLeft | NMCNumberRight.

(** ** ECMAScript parse trees as values (mirrors es/Ast.scala)

    [ASyn name args rhsIdx subIdx children child_names src parse_src] mirrors
    [Syntactic(name, args, rhsIdx, children)]; [subIdx] is ESMeta's
    [Ast.subIdx], which is derived from the grammar (Ast.scala:116-128) and
    is therefore PRECOMPUTED BY THE EXPORTER rather than recomputed here —
    the model does not carry the grammar.  [child_names] is the aligned list
    of RHS nonterminal names used by [Ast.get] (Ast.scala:84-91); it too is
    grammar-derived and precomputed.  [ALex] mirrors [Lexical].

    [src] is [ESourceText]'s result, i.e. ESMeta's
    [ast.toString(grammar = Some(grammar)).trim] (Interpreter.scala:227-230)
    while [parse_src] is the untrimmed string passed to [EParse] when its
    source is an AST (Interpreter.scala:216-221).  Both are grammar-derived
    and PRECOMPUTED BY THE EXPORTER.  They are derived fields, so [ast_eqb]
    ignores them (ESMeta compares case-class fields only).  ASTs are
    immutable in ESMeta — [State.update] on an AST base throws
    (State.scala:78-80) — so precomputing cannot go stale. *)

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
| LVBigInt (z : Z)
| LVUndef.

Inductive ast : Type :=
| ASyn (name : string) (args : list bool) (rhsIdx subIdx : nat)
       (children : list (option ast)) (child_names : list string)
       (src parse_src : cstr)
(** [sdos] maps a lexical SDO name ("StringValue", "NumericValue", "MV",
    "SV", "TV", "TRV" — the six of Interpreter.scala:525-536) to the value
    ESMeta computes for THIS lexeme.  A method absent from the table is UB.
    This covers both ESMeta's [InvalidAstField] and a fail-closed exporter
    omission when the method's exact result lies outside [lexval] (currently a
    fractional decimal Math value).  [LVUndef] is required by the [TV] of an
    invalid escape in a tagged template.  Other representable methods on the
    same AST remain usable; no decimal is rounded to binary64. *)
| ALex (name : string) (str : string) (src parse_src : cstr)
       (sdos : list (string * lexval)).

(** ** Restricted type expressions (for [ETypeCheck], ADR-11)

    ESMeta's [Type] wraps the full [esmeta.ty] language (unions, record
    field maps, …).  The compiled spec uses type *tests* far more simply:
    overwhelmingly `(? x: Completion)` / `(? x: Abrupt)` plus a handful of
    value-kind and record-name tests.  We mirror exactly that much; the
    exporter rejects any [Ty] outside this grammar, so unsupported tests
    are reported rather than silently mis-modelled. *)

Inductive tyexp : Type :=
| TRecord (tname : string)   (* unrefined named record test *)
(* A record target whose inline [FieldMap] consists only of required,
   otherwise-unconstrained fields ([Binding.Exist]).  This is the exact IR
   type generated for typed iteration over anonymous records such as
   [Record[{ Key, Value }]]. *)
| TRecordFields (tname : string) (fields : list string)
| TCompletion                (* a CompletionRecord *)
| TAbrupt                    (* CompletionRecord whose Type <> ~normal~ *)
| TNormal                    (* CompletionRecord whose Type  = ~normal~ *)
| TList                      (* list object *)
| TMapTy                     (* map object *)
| TStrTy | TBoolTy | TMathTy | TUndefTy | TNullTy | TEnumTy | TCloTy
| TStrSet (values : list cstr)
| TBoolSet (allow_false allow_true : bool)
| TMathIntSet (values : list Z)
| TInfinity (allow_neg allow_pos : bool)
| TEnumNames (names : list string)
| TNumberTy | TBigIntTy | TCodeUnitTy | TInfinityTy | TAstTy
(* MathIntTy(IntSignTy sign).  Every [VMath] in this executable fragment is
   integral (ADR-5); these bits retain the remaining sign refinement exactly. *)
| TMathInt (neg zero pos : bool)
(* NumberIntTy(IntSignTy(sign), hasNaN).  The three sign bits are ordered
   [negative, zero, positive].  Keeping the NaN bit explicit mirrors
   NumberTy.contains instead of baking in the currently common [false]. *)
| TNumberInt (neg zero pos hasNaN : bool)
(* AstTy.Simple / AstTy.Detail (ty/AstTy.scala:76-81).  [TAstNames] matches
   against the node's [types], the names down its single-child chain
   (es/Ast.scala:46-53), not just its own name. *)
| TAstNames (ns : list string)
| TAstDetail (n : string) (idx : nat)
(* [ValueTy] is a product of per-kind lattices and [contains] dispatches on
   the value's kind (ty/ValueTy.scala:167-188), so a type with several
   non-bottom components is exactly a disjunction. *)
| TUnion (ts : list tyexp)
(* ListTy.Elem (ty/ListTy.scala:57-60): EVERY element satisfies [t].
   Address-valued elements are checked by the recursive lazy heap-query
   checker, including nested list and structural record refinements. *)
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
| ECont (fn : irname)                            (* ECont: cont<"f"> *)
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
| EMathOp (op : mop) (args : list expr)         (* EMathOp *)
| EContains (lst : expr) (e : expr)             (* EContains *)
| ETrim (e : expr) (isStarting : bool)           (* ETrim *)
| ESyntactic
    (name : string) (args : list bool) (rhsIdx subIdx : nat)
    (children : list (option expr))
    (child_names : list string)
    (source_layout : list (option cstr))
    (* Runtime construction of a Syntactic AST.  [child_names] and
       [subIdx] are grammar-derived metadata supplied by the exporter.
       [source_layout] is the grammar RHS with terminals as [Some text]
       and nonterminal slots as [None]; evaluating it against the runtime
       children reproduces ESMeta's grammar-aware AST source printer. *)
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
       (Transform.v, attic/T2Proof.v).  The Scala exporter never produces it;
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

    The runtime still binds parameters positionally by name, exactly as the
    IR interpreter does.  In parallel we retain the source annotations for
    proof work.  [ta_check = None] is explicit: the annotation is preserved
    but lies outside the currently executable [tyexp] subset, so no theorem
    may silently treat it as [Any]. *)

Inductive func_kind : Type :=
| FKAbsOp | FKNumMeth | FKSynDirOp | FKConcMeth | FKInternalMeth
| FKBuiltin | FKClo | FKCont | FKAux.

Record type_annotation : Type := mkTypeAnnotation {
  ta_source : string;
  ta_check : option tyexp;
}.

Record param_annotation : Type := mkParamAnnotation {
  pa_type : type_annotation;
  pa_optional : bool;
}.

Definition unknown_type_annotation : type_annotation :=
  mkTypeAnnotation "unknown" None.

Fixpoint default_param_annotations
  (params : list string) : list param_annotation :=
  match params with
  | nil => nil
  | _ :: rest =>
      mkParamAnnotation unknown_type_annotation false ::
      default_param_annotations rest
  end.

Record func : Type := mkFuncData {
  f_main   : bool;
  f_kind   : option func_kind;
  f_name   : irname;
  f_params : list string;
  f_param_annotations : list param_annotation;
  f_return_annotation : type_annotation;
  f_body   : inst;
}.

(** Compatibility constructor for handwritten regression programs. *)
Definition mkFunc
  (main : bool) (name : irname) (params : list string) (body : inst) : func :=
  mkFuncData main None name params
    (default_param_annotations params) unknown_type_annotation body.

(** Constructor used by the ESMeta exporter. *)
Definition mkTypedFunc
  (main : bool) (kind : func_kind) (name : irname)
  (params : list string) (param_types : list param_annotation)
  (return_type : type_annotation) (body : inst) : func :=
  mkFuncData main (Some kind) name params param_types return_type body.

Definition func_with_body (f : func) (body : inst) : func :=
  mkFuncData (f_main f) (f_kind f) (f_name f) (f_params f)
    (f_param_annotations f) (f_return_annotation f) body.

Definition func_annotations_aligned (f : func) : bool :=
  Nat.eqb (List.length (f_params f))
    (List.length (f_param_annotations f)).

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

(** A continuation stores ESMeta's current call stack.  The executable
    ITree call machine represents an immutable stack by a stable frame-table
    identifier; [None] is the empty stack.  This stays data (rather than a
    Rocq function) so continuations remain comparable and extractable. *)
Definition cont_stack : Type := option nat.

(** Stable AST reference identity.  Exported identities name roots already
    present in the initial state; runtime identities come from a separate
    deterministic counter, so the two namespaces cannot collide. *)
Inductive ast_origin : Type :=
| AstExported (id : nat)
| AstRuntime (id : nat).

Inductive val : Type :=
| VMath (z : Z)
| VBool (b : bool)
| VStr (cs : cstr)
| VUndef
| VNull
| VEnum (name : string)
| VAddr (a : nat)
| VClo (fn : irname) (captured : list (string * val))
| VCont (fn : irname) (captured : list (string * val))
        (stack : cont_stack)
| VAst (origin : ast_origin) (root : ast) (rev_path : list nat)
    (* Parsed AstValue cursor.  [rev_path] stores child indices from the
       focused node back toward [root].  [origin] is stable across cursor
       movement and distinguishes separately allocated but structurally
       equal trees. *)
| VNumber (f : float)            (* Number   — state/Value.scala:143 *)
| VBigInt (z : Z)                (* BigInt   — state/Value.scala:146 *)
| VInfinity (pos : bool)         (* Infinity — state/Value.scala:123 *)
| VCodeUnit (c : cunit)          (* CodeUnit — state/Value.scala:129 *)
| VGrammarSymbol (name : string) (params : list bool).
                                 (* GrammarSymbol — state/Value.scala:86 *)

(** Private call-effect ABI between [Semantics.v] and the closed Test262
    executor.  These names are not IR functions: [ECont] asks the executor
    for the current frame pointer, while invoking a [VCont] transfers
    control to [cr_fn] with the saved pointer and never returns to the
    invoking continuation.  Encoding the ABI in [callE] keeps CRIS
    [Take]/[Choose] proof effects completely uninterpreted. *)
Definition cont_capture_fn : string := "$ESMetaFV.control.capture".
Definition cont_invoke_fn : string := "$ESMetaFV.control.invoke".

Record cont_request : Type := mkContRequest {
  cr_fn : irname;
  cr_captured : list (string * val);
  cr_args : list val;
  cr_stack : cont_stack;
}.

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

(** Some IR operations deliberately cross into ESMeta's Scala host:
    [EParse] calls the ECMAScript parser, string-to-Number/BigInt conversion
    calls [ESValueParser], and Number exponentiation / [COp.ToStr] call JVM
    numeric primitives.  They are not control effects and must not be
    confused with CRIS [Take]/[Choose].

    Test execution records only these primitive calls while obtaining the
    ESMeta oracle result.  The Rocq model still evaluates every operand,
    branch, call and heap operation itself, then performs an exact typed
    query lookup.  A missing query is UB; no result is guessed and the
    final Test262 verdict is never cached. *)
Inductive host_query : Type :=
| HQParseText (parse_text : cstr) (rule_name : string)
    (effective_params : list bool)
| HQToStr (input : val) (radix : Z)
| HQStrToNumber (input : cstr)
| HQNumberPow (left right : float)
| HQDoubleToLongChecked (input : float)
| HQStrToBigInt (input : cstr)
| HQMathOp (op : mop) (args : list Z)
| HQMathToNumber (input : Z)
| HQNumberMathOp (op : number_math_op) (left right : float)
| HQNumberSin (input : float)
| HQNumberMathCompare
    (op : number_math_compare_op)
    (direction : number_math_compare_direction)
    (number : float)
    (integer : Z)
| HQNumberToMath (input : float).

Record host_cache_entry : Type := mkHostCacheEntry {
  hc_query : host_query;
  hc_result : val;
}.

Record prog : Type := mkProgFull {
  p_funcs  : list func;
  p_source : option cstr;
  p_cached : option ast;
  p_hosts  : list host_cache_entry;
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
  mkProgFull fs None None nil nil nil.

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
