(** * ESMetaFV.Domain — pure semantic domain of IR-Core

    Everything here is framework-agnostic (Coq stdlib only): completions,
    pure operator evaluation, local environments, heap objects, and the
    pure helpers shared by the ITree denotation ([Semantics.v]) and the
    executable reference interpreter ([Exec.v]).

    Fidelity notes are attached to each definition; the authoritative
    catalogue is the header of [Semantics.v] and the research log. *)

From Stdlib Require Import String ZArith List Bool Floats Uint63.
From Stdlib Require Import Floats.FloatOps.
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

(** A catchable ESMeta evaluator failure.  This is deliberately distinct
    from CRIS undefined behaviour: [EvalThrow] represents an exception that
    the Scala interpreter's local [try/catch] can observe (currently
    [EParse]), while missing host-cache entries and unsupported model cases
    remain [triggerUB]/[Stuck] in the two evaluators. *)
Variant eval_result (A : Type) : Type :=
| EvalValue (value : A)
| EvalThrow.

Arguments EvalValue {A} value.
Arguments EvalThrow {A}.

(** ** Pure value operations *)

(** Scala/JVM [Int] coercion used by several ESMeta numeric operators.

    For the integer-only [VMath] fragment, [BigDecimal.toInt] and
    [BigInt.toInt] both keep the low 32 bits and interpret them as a signed
    two's-complement integer.  Keeping this coercion explicit prevents Coq's
    unbounded [Z] shift and complement operations from silently accepting a
    different operand. *)
Definition scala_int_modulus : Z := 4294967296.
Definition scala_int_min : Z := -2147483648.
Definition scala_int_max : Z := 2147483647.
(** [scala.math.BigDecimal.pow] delegates to
    [java.math.BigDecimal.pow], whose exponent contract is
    [0, 999999999].  ESMeta's preceding [isValidInt] guard is therefore
    necessary but not sufficient to guarantee a successful operation. *)
Definition scala_bigdecimal_pow_max : Z := 999999999.

(** The JVM permits BigInteger magnitudes close to [2^31] bits, but
    operations near that implementation ceiling can deterministically
    throw and are not practical executable test cases.  Keep a smaller,
    explicit conservative boundary for the extracted runner.  Every
    accepted result is far inside the JVM range; larger operations become
    UB rather than an invented unbounded-Coq success. *)
Definition scala_bigint_exec_max_bits : Z := 1048576.

Definition z_magnitude_bits (z : Z) : Z :=
  if Z.eqb z 0 then 0 else (Z.log2 (Z.abs z) + 1)%Z.

Definition scala_shift_result_safe
  (left_shift : bool) (z count : Z) : bool :=
  if Z.eqb z 0 then true
  else
    let bits := z_magnitude_bits z in
    let growth :=
      if left_shift then Z.max 0 count else Z.max 0 (- count) in
    andb
      (bits <=? scala_bigint_exec_max_bits)%Z
      ((bits + growth <=? scala_bigint_exec_max_bits)%Z).

Definition scala_pow_result_safe (base exponent : Z) : bool :=
  let bits := z_magnitude_bits base in
  andb
    (bits <=? scala_bigint_exec_max_bits)%Z
    ((bits * exponent <=? scala_bigint_exec_max_bits)%Z).

(** Avoid evaluating enormous powers even when their result is trivial.
    Callers establish [0 <= exponent] before using this helper. *)
Definition bounded_z_pow (base exponent : Z) : option Z :=
  if Z.eqb exponent 0 then Some 1%Z
  else if Z.eqb base 0 then Some 0%Z
  else if Z.eqb base 1 then Some 1%Z
  else if Z.eqb base (-1)
       then Some (if Z.even exponent then 1%Z else (-1)%Z)
       else if scala_pow_result_safe base exponent
            then Some (Z.pow base exponent)
            else None.

Definition scala_to_int32 (z : Z) : Z :=
  let low := Z.modulo z scala_int_modulus in
  if (low <=? scala_int_max)%Z then low else (low - scala_int_modulus)%Z.

Definition scala_is_valid_int (z : Z) : bool :=
  andb (scala_int_min <=? z)%Z (z <=? scala_int_max)%Z.

(** A shift by [Int.MinValue] is deliberately outside the executable
    fragment.  JVM [BigInteger] handles the two directions asymmetrically
    at that count (one direction can raise "overflow supported range"), so
    returning [None] avoids inventing a successful ESMeta execution. *)
Definition scala_shift_count (z : Z) : option Z :=
  let count := scala_to_int32 z in
  if Z.eqb count scala_int_min then None else Some count.

(** ** Number equality — ESMeta uses TWO different notions

    [BOp.Eq] on Numbers uses [doubleEquals] (util/DoubleEquals.scala:7-12,
    BaseUtils.scala:197-201): NaN equals NaN, and -0.0 differs from +0.0.
    [BOp.Equal] on Numbers uses plain [==] (Interpreter.scala:645), i.e.
    IEEE semantics: NaN differs from NaN and -0.0 equals +0.0.  Modelling
    them with one comparison would be wrong either way. *)

Definition is_negzero (f : float) : bool :=
  andb (PrimFloat.is_zero f) (PrimFloat.get_sign f).

(** [BOp.Eq] / case-class equality on [Number]. *)
Definition num_struct_eqb (x y : float) : bool :=
  if andb (PrimFloat.is_nan x) (PrimFloat.is_nan y) then true
  else if xorb (is_negzero x) (is_negzero y) then false
  else PrimFloat.eqb x y.

(** [BOp.Equal] on [Number] — plain IEEE equality. *)
Definition num_ieee_eqb (x y : float) : bool := PrimFloat.eqb x y.

(** Small list equalities reused by AST equality and semantic host-query
    keys. *)
Fixpoint bool_list_eqb (l1 l2 : list bool) : bool :=
  match l1, l2 with
  | nil, nil => true
  | x :: t1, y :: t2 => andb (Bool.eqb x y) (bool_list_eqb t1 t2)
  | _, _ => false
  end.

Fixpoint cstr_eqb (l1 l2 : cstr) : bool :=
  match l1, l2 with
  | nil, nil => true
  | x :: t1, y :: t2 => andb (Z.eqb x y) (cstr_eqb t1 t2)
  | _, _ => false
  end.

Fixpoint z_list_eqb (l1 l2 : list Z) : bool :=
  match l1, l2 with
  | nil, nil => true
  | x :: t1, y :: t2 => andb (Z.eqb x y) (z_list_eqb t1 t2)
  | _, _ => false
  end.

(** Exact equality for all 21 [MOp] constructors. *)
Definition mop_eqb (op1 op2 : mop) : bool :=
  match op1, op2 with
  | MExpm1, MExpm1 | MLog10, MLog10 | MLog2, MLog2 | MCos, MCos
  | MCbrt, MCbrt | MExp, MExp | MCosh, MCosh | MSinh, MSinh
  | MTanh, MTanh | MAcos, MAcos | MAcosh, MAcosh | MAsinh, MAsinh
  | MAtanh, MAtanh | MAsin, MAsin | MAtan2, MAtan2 | MAtan, MAtan
  | MLog1p, MLog1p | MLog, MLog | MSin, MSin | MSqrt, MSqrt
  | MTan, MTan => true
  | _, _ => false
  end.

(** Equality for the stable root identity and cursor path carried by an
    AST value. *)
Definition ast_origin_eqb (x y : ast_origin) : bool :=
  match x, y with
  | AstExported n, AstExported m
  | AstRuntime n, AstRuntime m => Nat.eqb n m
  | _, _ => false
  end.

Fixpoint nat_list_eqb (l1 l2 : list nat) : bool :=
  match l1, l2 with
  | nil, nil => true
  | x :: t1, y :: t2 => andb (Nat.eqb x y) (nat_list_eqb t1 t2)
  | _, _ => false
  end.

(** ESMeta's [BOp.Eq] special-cases AST values with Scala reference
    identity.  The immutable tree representation names that reference by
    its allocation origin plus cursor path.  Keep this separate from
    [val_eqb_partial]: ordinary Scala value equality (for example
    [EContains]) compares the focused AST case-class payload structurally. *)
Definition ast_ref_eqb
  (origin1 : ast_origin) (path1 : list nat)
  (origin2 : ast_origin) (path2 : list nat) : bool :=
  andb (ast_origin_eqb origin1 origin2) (nat_list_eqb path1 path2).

(** Structural equality on parse-tree payloads.  Ordinary Scala value
    equality uses this helper; [BOp.Eq] deliberately bypasses it for ASTs
    because ESMeta special-cases that operator with reference identity. *)
Fixpoint ast_eqb (a1 a2 : ast) {struct a1} : bool :=
  match a1, a2 with
  (* [subIdx], child names and both source strings are derived.  Scala's
     case-class equality compares only name/args/rhsIdx/children. *)
  | ALex n1 s1 _ _ _, ALex n2 s2 _ _ _ =>
      andb (String.eqb n1 n2) (String.eqb s1 s2)
  | ASyn n1 g1 r1 _ c1 _ _ _, ASyn n2 g2 r2 _ c2 _ _ _ =>
      andb (String.eqb n1 n2)
        (andb (Nat.eqb r1 r2)
           (andb (bool_list_eqb g1 g2)
              ((fix cs (l1 l2 : list (option ast)) : bool :=
                  match l1, l2 with
                  | nil, nil => true
                  | None :: t1, None :: t2 => cs t1 t2
                  | Some x :: t1, Some y :: t2 =>
                      andb (ast_eqb x y) (cs t1 t2)
                  | _, _ => false
                  end) c1 c2)))
  | _, _ => false
  end.

Definition ast_name (a : ast) : string :=
  match a with ASyn n _ _ _ _ _ _ _ => n | ALex n _ _ _ _ => n end.

Definition ast_args (a : ast) : list bool :=
  match a with ASyn _ args _ _ _ _ _ _ => args | ALex _ _ _ _ _ => nil end.

(** Printed source text (ESourceText, Interpreter.scala:227-230):
    exporter-precomputed, see Fragment.v. *)
(** [Ast.idx] (es/Ast.scala:25-27): the rhs index, 0 for a lexical node. *)
Definition ast_idx (a : ast) : nat :=
  match a with ASyn _ _ r _ _ _ _ _ => r | ALex _ _ _ _ _ => 0%nat end.

Definition ast_src (a : ast) : cstr :=
  match a with ASyn _ _ _ _ _ _ s _ => s | ALex _ _ s _ _ => s end.

Definition ast_parse_src (a : ast) : cstr :=
  match a with
  | ASyn _ _ _ _ _ _ _ parse_src => parse_src
  | ALex _ _ _ parse_src _ => parse_src
  end.

(** Grammar-aware source rendering for runtime [ESyntactic].

    ESMeta's AST stringifier walks the RHS left-to-right, appending every
    terminal or present child followed by one ASCII space, then applies
    Java/Scala [String.trim] to the complete result
    (es/util/Stringifier.scala:44-66).  The exporter erases the grammar to
    a compact layout: [Some terminal] emits that terminal and [None]
    consumes one aligned optional child. *)
Definition cunit_is_java_trim (c : cunit) : bool := (c <=? 32)%Z.

Fixpoint cstr_drop_java_trim (cs : cstr) : cstr :=
  match cs with
  | nil => nil
  | c :: tl =>
      if cunit_is_java_trim c then cstr_drop_java_trim tl else cs
  end.

Definition cstr_java_trim (cs : cstr) : cstr :=
  List.rev (cstr_drop_java_trim (List.rev (cstr_drop_java_trim cs))).

(** ECMAScript [StrWhiteSpaceChar] for [ETrim].  ESMeta's parser lists the
    Unicode Space_Separator code points explicitly and adds TAB, VT, FF,
    ZWNBSP plus the four line terminators (UnicodeParsers.scala:15-39).
    Every member is in the BMP, so testing the UTF-16 code units stored by
    [cstr] is identical to ESMeta's code-point walk at either string edge. *)
Definition ecma_trim_code_units : list Z :=
  9%Z :: 10%Z :: 11%Z :: 12%Z :: 13%Z :: 32%Z :: 160%Z :: 5760%Z ::
  8192%Z :: 8193%Z :: 8194%Z :: 8195%Z :: 8196%Z :: 8197%Z ::
  8198%Z :: 8199%Z :: 8200%Z :: 8201%Z :: 8202%Z :: 8232%Z ::
  8233%Z :: 8239%Z :: 8287%Z :: 12288%Z :: 65279%Z :: nil.

Definition cunit_is_ecma_trim (c : cunit) : bool :=
  existsb (Z.eqb c) ecma_trim_code_units.

Fixpoint cstr_drop_ecma_trim (cs : cstr) : cstr :=
  match cs with
  | nil => nil
  | c :: tl =>
      if cunit_is_ecma_trim c then cstr_drop_ecma_trim tl else cs
  end.

Definition cstr_trim (cs : cstr) (isStarting : bool) : cstr :=
  if isStarting
  then cstr_drop_ecma_trim cs
  else List.rev (cstr_drop_ecma_trim (List.rev cs)).

Fixpoint render_syn_source_raw
  (layout : list (option cstr)) (children : list (option ast))
  : option cstr :=
  match layout with
  | nil => Some nil
  | Some terminal :: tl =>
      option_map
        (fun rest => List.app terminal ((32%Z) :: rest))
        (render_syn_source_raw tl children)
  | None :: tl =>
      match children with
      | nil => None
      | None :: cs => render_syn_source_raw tl cs
      | Some child :: cs =>
          option_map
            (fun rest => List.app (ast_src child) ((32%Z) :: rest))
            (render_syn_source_raw tl cs)
      end
  end.

Definition render_syn_source
  (layout : list (option cstr)) (children : list (option ast))
  : option cstr :=
  option_map cstr_java_trim (render_syn_source_raw layout children).

(** D-3: the exporter-precomputed answer of a lexical SDO
    (Interpreter.scala:521-542).  [None] mirrors [InvalidAstField]. *)
Definition lexval_to_val (l : lexval) : val :=
  match l with
  | LVStr cs => VStr cs
  | LVMath z => VMath z
  | LVNumber f => VNumber f
  | LVBigInt z => VBigInt z
  | LVUndef => VUndef
  end.

Fixpoint lex_lookup (tbl : list (string * lexval)) (m : string)
  : option lexval :=
  match tbl with
  | nil => None
  | (k, v) :: tl => if String.eqb k m then Some v else lex_lookup tl m
  end.

Definition ast_lex_sdo (a : ast) (m : string) : option val :=
  match a with
  | ALex _ _ _ _ tbl => option_map lexval_to_val (lex_lookup tbl m)
  | ASyn _ _ _ _ _ _ _ _ => None
  end.

Definition ast_children (a : ast) : list (option ast) :=
  match a with ASyn _ _ _ _ cs _ _ _ => cs | ALex _ _ _ _ _ => nil end.

(** Named child access is grammar-directed in ESMeta: [Ast.get] asks the
    selected RHS for the first nonterminal whose name matches the field,
    then reads the child at the same position.  [child_names] is that RHS
    nonterminal list, exported alongside each node, so this is an exact
    lookup without carrying the full ECMAScript grammar in the model. *)
Fixpoint named_child_lookup_indexed
  (names : list string) (children : list (option ast)) (field : string)
  (idx : nat) : option (nat * ast) :=
  match names, children with
  | name :: name_rest, child :: child_rest =>
      if String.eqb name field
      then option_map (fun a => (idx, a)) child
      else named_child_lookup_indexed name_rest child_rest field (S idx)
  | _, _ => None
  end.

Definition ast_named_child_indexed (a : ast) (field : string)
  : option (nat * ast) :=
  match a with
  | ASyn _ _ _ _ children child_names _ _ =>
      named_child_lookup_indexed child_names children field 0
  | ALex _ _ _ _ _ => None
  end.

Definition ast_child_indexed (a : ast) (idx : nat) : option (nat * ast) :=
  match nth_error (ast_children a) idx with
  | Some (Some child) => Some (idx, child)
  | _ => None
  end.

Fixpoint ast_follow_path (a : ast) (path : list nat) : option ast :=
  match path with
  | nil => Some a
  | idx :: tl =>
      match ast_child_indexed a idx with
      | Some (_, child) => ast_follow_path child tl
      | None => None
      end
  end.

Definition ast_focus (root : ast) (rev_path : list nat) : option ast :=
  ast_follow_path root (List.rev rev_path).

Definition ast_cursor_field_get
  (root : ast) (rev_path : list nat) (field : val)
  : option (ast * list nat) :=
  match field with
  | VStr cs =>
      match ascii_of_cstr cs with
      | Some "parent" =>
          match ast_focus root rev_path, rev_path with
          | Some _, _ :: parent_path => Some (root, parent_path)
          | _, _ => None
          end
      | Some name =>
          match ast_focus root rev_path with
          | Some a =>
              match ast_named_child_indexed a name with
              | Some (idx, _) => Some (root, idx :: rev_path)
              | None => None
              end
          | None => None
          end
      | None => None
      end
  | VMath i =>
      if (0 <=? i)%Z
      then
        match ast_focus root rev_path with
        | Some a =>
            match ast_child_indexed a (Z.to_nat i) with
            | Some (idx, _) => Some (root, idx :: rev_path)
            | None => None
            end
        | None => None
        end
      else None
  | _ => None
  end.

(** Reads and existence checks share the cursor lookup above.  Keeping the
    reverse path in the value represents parser-populated parent links
    without making the inductive AST cyclic. *)
Definition ast_cursor_field_exists
  (root : ast) (rev_path : list nat) (field : val) : bool :=
  match ast_cursor_field_get root rev_path field with
  | Some _ => true
  | None => false
  end.

(** Production chains (Ast.scala:38-44): the node itself, then, while a
    node has exactly one present child, that child — the fall-through used
    by SDO lookup.  Fuel is the tree size, which bounds the chain length,
    because the single-present-child projection is not a structural
    subterm the guard checker can follow. *)

Fixpoint ast_size (a : ast) : nat :=
  match a with
  | ALex _ _ _ _ _ => 1
  | ASyn _ _ _ _ cs _ _ _ =>
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

Definition single_present_indexed (a : ast) : option (nat * ast) :=
  let fix go (cs : list (option ast)) (idx count : nat)
          (found : option (nat * ast)) :=
      match cs with
      | nil => if Nat.eqb count 1 then found else None
      | None :: tl => go tl (S idx) count found
      | Some child :: tl => go tl (S idx) (S count) (Some (idx, child))
      end
  in go (ast_children a) 0 0 None.

Fixpoint ast_cursor_chain_fuel
  (fuel : nat) (root current : ast) (rev_path : list nat)
  : list (ast * list nat) :=
  (current, rev_path) ::
  match fuel with
  | O => nil
  | S fuel' =>
      match single_present_indexed current with
      | Some (idx, child) =>
          ast_cursor_chain_fuel fuel' root child (idx :: rev_path)
      | None => nil
      end
  end.

Definition ast_cursor_chain (root : ast) (rev_path : list nat)
  : option (list (ast * list nat)) :=
  match ast_focus root rev_path with
  | Some a => Some (ast_cursor_chain_fuel (ast_size a) root a rev_path)
  | None => None
  end.

(** [Ast.types] (es/Ast.scala:46-53): this node's name plus the names down
    its single-present-child chain — the same chain [ast_chain] walks for
    SDO dispatch, since [children.flatten match { case Vector(child) => .. }]
    is exactly "exactly one present child". *)
Definition ast_types (a : ast) : list string :=
  List.map ast_name (ast_chain a).

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
  | ASyn n _ r b _ _ _ _ =>
      (n ++ "[" ++ nat_str r ++ "," ++ nat_str b ++ "]." ++ m)%string
  | ALex n _ _ _ _ => (n ++ "[0,0]." ++ m)%string
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

Definition sdo_resolve_cursor
  (fnames : list string) (root : ast) (rev_path : list nat) (m : string)
  : option (list nat * string) :=
  match ast_cursor_chain root rev_path with
  | None => None
  | Some chain =>
      let dflt := ("DEFAULT:" ++ m)%string in
      (fix go (l : list (ast * list nat)) : option (list nat * string) :=
         match l with
         | nil => None
         | (a0, path0) :: tl =>
             let c := sdo_candidate a0 m in
             if name_mem c fnames then Some (path0, c)
             else if name_mem dflt fnames then Some (path0, dflt)
             else go tl
         end) chain
  end.

(** Captured environments are Scala immutable [Map] values, although the
    extracted representation is a list.  Construction with [toMap] keeps the
    last occurrence of a duplicate key, and Map equality ignores iteration
    order.  These helpers expose precisely that finite-map view. *)
Fixpoint captured_lookup (x : string) (cs : list (string * val))
  : option val :=
  match cs with
  | nil => None
  | (y, v) :: tl =>
      match captured_lookup x tl with
      | Some v' => Some v'
      | None => if String.eqb x y then Some v else None
      end
  end.

Fixpoint captured_remove (x : string) (cs : list (string * val))
  : list (string * val) :=
  match cs with
  | nil => nil
  | (y, v) :: tl =>
      if String.eqb x y
      then captured_remove x tl
      else (y, v) :: captured_remove x tl
  end.

Fixpoint captured_normalize (cs : list (string * val))
  : list (string * val) :=
  match cs with
  | nil => nil
  | (x, v) :: tl =>
      let tl' := captured_normalize tl in
      match captured_lookup x tl' with
      | Some _ => tl'
      | None => (x, v) :: tl'
      end
  end.

(** Partial structural equality, mirroring ordinary Scala case-class
    equality on the fragment's value forms.  A nonempty continuation stack
    is represented only by its current frame-table identifier, while the
    underlying Scala [List[CallContext]] is mutable.  Equal identifiers are
    enough to recognize the same represented stack, but unequal identifiers
    cannot soundly decide inequality after either context list has mutated.
    [None] exposes precisely that ambiguity as UB to semantic callers.

    [BOp.Eq] applies its AST-specific reference rule separately in
    [eval_bop]. *)
Fixpoint val_eqb_partial (v1 v2 : val) {struct v1} : option bool :=
  match v1, v2 with
  | VMath z1, VMath z2 => Some (Z.eqb z1 z2)
  | VBool b1, VBool b2 => Some (Bool.eqb b1 b2)
  | VStr c1, VStr c2 =>
      Some (cstr_eqb c1 c2)
  (* case-class equality on Number is doubleEquals (see above); MapObj keys
     are compared with Scala == , so this is the right notion there. *)
  | VNumber f1, VNumber f2 => Some (num_struct_eqb f1 f2)
  | VBigInt z1, VBigInt z2 => Some (Z.eqb z1 z2)
  | VInfinity p1, VInfinity p2 => Some (Bool.eqb p1 p2)
  | VCodeUnit c1, VCodeUnit c2 => Some (Z.eqb c1 c2)
  | VGrammarSymbol n1 p1, VGrammarSymbol n2 p2 =>
      Some (andb (String.eqb n1 n2) (bool_list_eqb p1 p2))
  | VUndef, VUndef => Some true
  | VNull, VNull => Some true
  | VEnum n1, VEnum n2 => Some (String.eqb n1 n2)
  | VAddr a1, VAddr a2 => Some (Nat.eqb a1 a2)
  | VAst _ root1 path1, VAst _ root2 path2 =>
      Some
        (match ast_focus root1 path1, ast_focus root2 path2 with
         | Some ast1, Some ast2 => ast_eqb ast1 ast2
         | _, _ => false
         end)
  | VClo f1 c1, VClo f2 c2 =>
      if String.eqb f1 f2 then
        (fix go (l1 l2 : list (string * val)) : option bool :=
           match l1 with
           | nil =>
               match l2 with nil => Some true | _ => Some false end
           | (x1, u1) :: t1 =>
               match captured_lookup x1 t1 with
               | Some _ => go t1 l2
               | None =>
                   match captured_lookup x1 l2 with
                   | None => Some false
                   | Some u2 =>
                       match val_eqb_partial u1 u2 with
                       | Some true => go t1 (captured_remove x1 l2)
                       | Some false => Some false
                       | None => None
                       end
                   end
               end
           end) c1 c2
      else Some false
  | VCont f1 c1 s1, VCont f2 c2 s2 =>
      if String.eqb f1 f2 then
        match
          (fix go (l1 l2 : list (string * val)) : option bool :=
             match l1 with
             | nil =>
                 match l2 with nil => Some true | _ => Some false end
             | (x1, u1) :: t1 =>
                 match captured_lookup x1 t1 with
                 | Some _ => go t1 l2
                 | None =>
                     match captured_lookup x1 l2 with
                     | None => Some false
                     | Some u2 =>
                         match val_eqb_partial u1 u2 with
                         | Some true => go t1 (captured_remove x1 l2)
                         | Some false => Some false
                         | None => None
                         end
                     end
                 end
             end) c1 c2
        with
        | Some true =>
            match s1, s2 with
            | None, None => Some true
            | Some n1, Some n2 =>
                if Nat.eqb n1 n2 then Some true else None
            | _, _ => Some false
            end
        | Some false => Some false
        | None => None
        end
      else Some false
  | _, _ => Some false
  end.

(** Total compatibility projection for trusted keys and diagnostic
    comparison only.  Semantic equality/list/map operations below use
    [val_eqb_partial] directly so ambiguity is never mistaken for false. *)
Definition val_eqb (v1 v2 : val) : bool :=
  match val_eqb_partial v1 v2 with
  | Some b => b
  | None => false
  end.

(** Scala [List.contains] short-circuits at the first equality.  Therefore
    a definite earlier match succeeds without inspecting a later ambiguous
    continuation comparison. *)
Fixpoint vals_contains_partial (needle : val) (vs : list val)
  : option bool :=
  match vs with
  | nil => Some false
  | v :: tl =>
      match val_eqb_partial needle v with
      | Some true => Some true
      | Some false => vals_contains_partial needle tl
      | None => None
      end
  end.

(** Typed equality and exact lookup for the trusted host-operation
    boundary.  Parser keys contain only what Scala's parser observes:
    untrimmed input text, rule name, and effective grammar parameters. *)
Definition host_query_eqb (q1 q2 : host_query) : bool :=
  match q1, q2 with
  | HQParseText text1 rule1 params1, HQParseText text2 rule2 params2 =>
      andb (cstr_eqb text1 text2)
        (andb (String.eqb rule1 rule2) (bool_list_eqb params1 params2))
  | HQToStr input1 radix1, HQToStr input2 radix2 =>
      andb
        (match input1, input2 with
         | VNumber f1, VNumber f2 => num_struct_eqb f1 f2
         | VBigInt z1, VBigInt z2 => Z.eqb z1 z2
         | _, _ => false
         end)
        (Z.eqb radix1 radix2)
  | HQStrToNumber input1, HQStrToNumber input2 =>
      cstr_eqb input1 input2
  | HQStrToBigInt input1, HQStrToBigInt input2 =>
      cstr_eqb input1 input2
  | HQNumberPow left1 right1, HQNumberPow left2 right2 =>
      andb (num_struct_eqb left1 left2) (num_struct_eqb right1 right2)
  | HQDoubleToLongChecked input1, HQDoubleToLongChecked input2 =>
      num_struct_eqb input1 input2
  | HQMathOp op1 args1, HQMathOp op2 args2 =>
      andb (mop_eqb op1 op2) (z_list_eqb args1 args2)
  | HQMathToNumber input1, HQMathToNumber input2 => Z.eqb input1 input2
  | HQNumberMathOp op1 left1 right1, HQNumberMathOp op2 left2 right2 =>
      andb
        (match op1, op2 with
         | NMAdd, NMAdd | NMMul, NMMul | NMDiv, NMDiv | NMPow, NMPow => true
         | _, _ => false
         end)
        (andb (num_struct_eqb left1 left2) (num_struct_eqb right1 right2))
  | HQNumberSin input1, HQNumberSin input2 =>
      num_struct_eqb input1 input2
  | HQNumberMathCompare op1 direction1 number1 integer1,
    HQNumberMathCompare op2 direction2 number2 integer2 =>
      andb
        (match op1, op2 with
         | NMCLt, NMCLt | NMCEqual, NMCEqual => true
         | _, _ => false
         end)
        (andb
          (match direction1, direction2 with
           | NMCNumberLeft, NMCNumberLeft
           | NMCNumberRight, NMCNumberRight => true
           | _, _ => false
           end)
          (andb (num_struct_eqb number1 number2) (Z.eqb integer1 integer2)))
  | HQNumberToMath input1, HQNumberToMath input2 =>
      num_struct_eqb input1 input2
  | _, _ => false
  end.

Fixpoint host_cache_lookup
  (query : host_query) (entries : list host_cache_entry) : option val :=
  match entries with
  | nil => None
  | entry :: rest =>
      if host_query_eqb query (hc_query entry)
      then Some (hc_result entry)
      else host_cache_lookup query rest
  end.

(** One result-shape contract for both the ITree semantics and executable
    diagnostics.  This prevents the two evaluators from drifting when a
    new trusted primitive is added. *)
Definition host_result_well_typed (query : host_query) (result : val) : bool :=
  match query, result with
  | HQParseText _ _ _, VAst _ _ _ => true
  | HQParseText _ _ _, VUndef => true
  | HQToStr _ _, VStr _ => true
  | HQStrToNumber _, VNumber _ => true
  | HQStrToBigInt _, VBigInt _ => true
  | HQStrToBigInt _, VUndef => true
  | HQNumberPow _ _, VNumber _ => true
  | HQDoubleToLongChecked _, VUndef => true
  | HQDoubleToLongChecked _, VMath z =>
      andb ((- (Z.pow 2 63) <=? z)%Z) ((z <? Z.pow 2 63)%Z)
  | HQMathOp _ _, VMath _ => true
  | HQMathOp _ _, VInfinity _ => true
  | HQMathToNumber _, VNumber _ => true
  | HQNumberMathOp _ _ _, VNumber _ => true
  | HQNumberSin _, VNumber _ => true
  | HQNumberMathCompare _ _ _ _, VBool _ => true
  | HQNumberToMath input, VMath _ => PrimFloat.is_finite input
  | _, _ => false
  end.

Definition typed_host_cache_lookup
  (query : host_query) (entries : list host_cache_entry) : option val :=
  match host_cache_lookup query entries with
  | Some result =>
      if host_result_well_typed query result then Some result else None
  | None => None
  end.

(** Normalize [EParse]'s evaluated operands to the exact semantic host key.
    An explicit grammar-parameter list wins; an empty list on an AST source
    inherits that source node's arguments (Interpreter.scala:216-221). *)
Definition host_parse_query (source rule : val) : option host_query :=
  match source, rule with
  | VStr text, VGrammarSymbol name params =>
      Some (HQParseText text name params)
  | VAst _ root path, VGrammarSymbol name params =>
      match ast_focus root path with
      | Some a =>
          Some
            (HQParseText
              (ast_parse_src a)
              name
              (match params with nil => ast_args a | _ => params end))
      | None => None
      end
  | _, _ => None
  end.

(** Pure dispatch tables identify exactly which otherwise ordinary IR
    operators cross the host boundary.  Extending Number [mod] or another
    Scala primitive later changes this table once, not both evaluators. *)
Definition host_bop_query (op : bop) (v1 v2 : val) : option host_query :=
  match op, v1, v2 with
  | BPow, VNumber lf, VNumber rf => Some (HQNumberPow lf rf)
  | _, _, _ => None
  end.

(** Binary64 represents every integer through [2^53] exactly.  Math-to-
    Number conversion stays pure in that closed interval.  Outside that
    guaranteed-exact interval, delegate to ESMeta's [BigDecimal.toDouble];
    some larger integers remain exactly representable, while others round. *)
Definition max_exact_binary64_integer : Z := 9007199254740992.

Definition math_to_number_needs_host (z : Z) : bool :=
  negb (Z.abs z <=? max_exact_binary64_integer)%Z.

Definition host_cop_query (op : cop) (v : val) : option host_query :=
  match op, v with
  | CToMath, VNumber input =>
      if PrimFloat.is_finite input then Some (HQNumberToMath input) else None
  | CToNumber, VStr input => Some (HQStrToNumber input)
  | CToBigInt, VStr input => Some (HQStrToBigInt input)
  | CToApproxNumber, VMath z
  | CToNumber, VMath z =>
      if math_to_number_needs_host z
      then Some (HQMathToNumber z)
      else None
  | _, _ => None
  end.

(** Validate [EMathOp] operands without assigning any transcendental
    meaning in Rocq.  [Atan2] alone is binary; every other operator has
    exactly one [Math] argument. *)
Definition host_mathop_query (op : mop) (vs : list val)
  : option host_query :=
  match op, vs with
  (* ESMeta's interpreter rejects these three as [NotSupported], even
     though they are present in the IR operator enum. *)
  | MAcosh, _ | MAsinh, _ | MAtanh, _ => None
  | MAtan2, VMath x :: VMath y :: nil => Some (HQMathOp op [x; y])
  | MAtan2, _ => None
  | _, VMath x :: nil => Some (HQMathOp op [x])
  | _, _ => None
  end.

(** ** Strict binary operators (Interpreter.scala:566-666)

    [BAnd]/[BOr] on booleans are handled (short-circuit) by the
    interpreters and do not reach here.  [None] means ESMeta throws
    [InvalidBinaryOp], i.e. UB in this model — including the cases ESMeta
    itself leaves out (e.g. INF * 0). *)

(** Exact [Z] to double, used by [CToNumber] on Math.  Restricted to the
    range where doubles are exact; beyond it ESMeta's [BigDecimal.toDouble]
    rounds, and we do not model that rounding (limitation, not a guess). *)
Definition float_of_Z (z : Z) : option float :=
  if (Z.abs z <=? max_exact_binary64_integer)%Z
  then
    let m := PrimFloat.of_uint63 (Uint63.of_Z (Z.abs z)) in
    Some (if (z <? 0)%Z then PrimFloat.opp m else m)
  else None.

(** Exact Binary64-to-integer conversion for the integer-only [VMath]
    fragment.  [frshiftexp] gives [f = fraction * 2^exponent], and
    [normfr_mantissa fraction] gives the 53-bit integer mantissa.  We
    accept precisely those finite doubles whose fractional denominator
    divides that mantissa; non-integral doubles remain outside this
    fragment instead of being rounded or guessed. *)
Definition float_to_Z_exact (f : float) : option Z :=
  if PrimFloat.is_finite f then
    if PrimFloat.is_zero f then Some 0%Z
    else
      let '(fraction, encoded_exp) := PrimFloat.frshiftexp f in
      let mantissa :=
        Uint63.to_Z (PrimFloat.normfr_mantissa fraction) in
      let exponent :=
        (Uint63.to_Z encoded_exp - FloatOps.shift - FloatOps.prec)%Z in
      let magnitude :=
        if (0 <=? exponent)%Z then
          Some (mantissa * Z.pow 2 exponent)%Z
        else
          let divisor := Z.pow 2 (- exponent) in
          if Z.eqb (Z.modulo mantissa divisor) 0
          then Some (Z.div mantissa divisor)
          else None in
      option_map
        (fun z => if PrimFloat.get_sign f then (- z)%Z else z)
        magnitude
  else None.

(** Truncate a finite Binary64 value toward zero using its exact binary
    decomposition.  Unlike [float_to_Z_exact], a fractional denominator
    need not divide the mantissa: division of the positive magnitude drops
    the fractional bits, and the sign is applied only afterwards. *)
Definition float_to_Z_trunc (f : float) : option Z :=
  if PrimFloat.is_finite f then
    if PrimFloat.is_zero f then Some 0%Z
    else
      let '(fraction, encoded_exp) := PrimFloat.frshiftexp f in
      let mantissa :=
        Uint63.to_Z (PrimFloat.normfr_mantissa fraction) in
      let exponent :=
        (Uint63.to_Z encoded_exp - FloatOps.shift - FloatOps.prec)%Z in
      let magnitude :=
        if (0 <=? exponent)%Z then
          (mantissa * Z.pow 2 exponent)%Z
        else
          Z.div mantissa (Z.pow 2 (- exponent)) in
      Some (if PrimFloat.get_sign f then (- magnitude)%Z else magnitude)
  else None.

(** Unary operators that need exact Binary64/integer conversion live after
    [float_of_Z] and [float_to_Z_exact].  ESMeta accepts arbitrary Numbers
    for [BNot] through JVM [Double.toInt]; the executable fragment admits the
    exact signed-int32 subset reached after the spec's [ToInt32] call and
    conservatively rejects fractional or saturated raw-Number cases. *)
Definition eval_uop (op : uop) (v : val) : option val :=
  match op, v with
  | UNeg, VNumber f => Some (VNumber (PrimFloat.opp f))
  | UNeg, VMath z => Some (VMath (- z))
  | UNeg, VInfinity p => Some (VInfinity (negb p))
  | UNeg, VBigInt z => Some (VBigInt (- z))
  | UNot, VBool b => Some (VBool (negb b))
  | UAbs, VMath z => Some (VMath (Z.abs z))
  | UFloor, VMath z => Some (VMath z)   (* integers: floor is identity *)
  | UBNot, VMath z => Some (VMath (Z.lnot (scala_to_int32 z)))
  | UBNot, VNumber f =>
      match float_to_Z_exact f with
      | Some z =>
          if scala_is_valid_int z
          then option_map VNumber (float_of_Z (Z.lnot z))
          else None
      | None => None
      end
  | UBNot, VBigInt z => Some (VBigInt (Z.lnot z))
  | _, _ => None
  end.

(** Exact membership for ESMeta's [NumberIntTy(IntSignTy sign, hasNaN)].
    [NumberTy.contains] first handles NaN, then requires [Double.isWhole]
    and tests the integer's sign.  [float_to_Z_exact] is the corresponding
    exact finite/integral check; inspecting the resulting [Z] avoids the
    lossy machine-[Int] conversion while preserving the sign abstraction. *)
Definition number_int_sign_ok
  (f : float) (neg zero pos hasNaN : bool) : bool :=
  if PrimFloat.is_nan f then hasNaN
  else
    match float_to_Z_exact f with
    | None => false
    | Some z =>
        if (z <? 0)%Z then neg
        else if Z.eqb z 0 then zero
        else pos
      end.

(** Exact membership for [MathIntTy(IntSignTy sign)].  [VMath] stores a [Z],
    so integrality is already guaranteed and only the sign abstraction remains. *)
Definition math_int_sign_ok
  (z : Z) (neg zero pos : bool) : bool :=
  if (z <? 0)%Z then neg else if Z.eqb z 0 then zero else pos.

Definition eval_bop (op : bop) (v1 v2 : val) : option val :=
  match op, v1, v2 with
  (* --- IEEE-754 doubles --- *)
  | BAdd, VNumber l, VNumber r => Some (VNumber (PrimFloat.add l r))
  | BSub, VNumber l, VNumber r => Some (VNumber (PrimFloat.sub l r))
  | BMul, VNumber l, VNumber r => Some (VNumber (PrimFloat.mul l r))
  | BDiv, VNumber l, VNumber r => Some (VNumber (PrimFloat.div l r))
  (* ESMeta returns Bool(true) for -0.0 < 0.0 (Interpreter.scala:576-577),
     which IEEE does not; model that special case explicitly. *)
  | BLt, VNumber l, VNumber r =>
      if andb (is_negzero l) (andb (PrimFloat.is_zero r)
                                (negb (PrimFloat.get_sign r)))
      then Some (VBool true)
      else Some (VBool (PrimFloat.ltb l r))
  (* Pow and Mod on doubles use math.pow / a Scala helper; PrimFloat has no
     primitive for either, so they are UB rather than approximated. *)

  (* --- mathematical values (integers here, ADR-5) --- *)
  | BAdd, VMath z1, VMath z2 => Some (VMath (z1 + z2))
  | BSub, VMath z1, VMath z2 => Some (VMath (z1 - z2))
  | BMul, VMath z1, VMath z2 => Some (VMath (z1 * z2))
  | BLt, VMath z1, VMath z2 => Some (VBool (Z.ltb z1 z2))
  | BDiv, VMath z1, VMath z2 =>
      (* ESMeta rounds Math division to DECIMAL128 (Interpreter.scala:584);
         admit only the exact case (ADR-5). *)
      if Z.eqb z2 0 then None
      else if Z.eqb (Z.rem z1 z2) 0 then Some (VMath (Z.quot z1 z2)) else None
  (* %% is floored modulo (interpreter/package.scala:23-26): the sign
     follows the divisor, which is exactly Coq's Z.modulo. *)
  | BMod, VMath z1, VMath z2 =>
      if Z.eqb z2 0 then None else Some (VMath (Z.modulo z1 z2))
  | BPow, VMath z1, VMath z2 =>
      (* [BigDecimal.pow] is selected only for a nonnegative [isValidInt]
         exponent, and the JVM primitive additionally rejects exponents
         above 999999999.  The floating fallback cannot in general be
         represented exactly by ADR-5's integer-only [VMath], so it
         remains UB. *)
      if andb
           (scala_is_valid_int z2)
           (andb (0 <=? z2)%Z (z2 <=? scala_bigdecimal_pow_max)%Z)
      then option_map VMath (bounded_z_pow z1 z2)
      else None
  | BBAnd, VMath z1, VMath z2 => Some (VMath (Z.land z1 z2))
  | BBOr, VMath z1, VMath z2 => Some (VMath (Z.lor z1 z2))
  | BBXOr, VMath z1, VMath z2 => Some (VMath (Z.lxor z1 z2))
  | BLShift, VMath z1, VMath z2 =>
      match scala_shift_count z2 with
      | Some count =>
          if scala_shift_result_safe true z1 count
          then Some (VMath (Z.shiftl z1 count))
          else None
      | None => None
      end
  | BRShift, VMath z1, VMath z2 =>
      match scala_shift_count z2 with
      | Some count =>
          if scala_shift_result_safe false z1 count
          then Some (VMath (Z.shiftr z1 count))
          else None
      | None => None
      end

  (* --- extended mathematical values (Interpreter.scala:600-630) --- *)
  | BAdd, VInfinity p, VMath _ => Some (VInfinity p)
  | BAdd, VMath _, VInfinity p => Some (VInfinity p)
  | BAdd, VInfinity p1, VInfinity p2 =>
      if Bool.eqb p1 p2 then Some (VInfinity p1) else None
  | BSub, VInfinity p, VMath _ => Some (VInfinity p)
  | BSub, VMath _, VInfinity p => Some (VInfinity (negb p))
  | BSub, VInfinity p1, VInfinity p2 =>
      if Bool.eqb p1 p2 then None else Some (VInfinity p1)
  | BMul, VInfinity p, VMath z =>
      if Z.eqb z 0 then None else Some (VInfinity (xorb p (z <? 0)%Z))
  | BMul, VMath z, VInfinity p =>
      if Z.eqb z 0 then None else Some (VInfinity (xorb p (z <? 0)%Z))
  | BMul, VInfinity p1, VInfinity p2 => Some (VInfinity (xorb p1 (negb p2)))
  | BLt, VInfinity p, VMath _ => Some (VBool (negb p))
  | BLt, VMath _, VInfinity p => Some (VBool p)
  | BLt, VInfinity p1, VInfinity p2 =>
      Some (VBool (andb (negb p1) p2))

  (* --- booleans (Interpreter.scala:633-635) --- *)
  | BAnd, VBool b1, VBool b2 => Some (VBool (andb b1 b2))
  | BOr, VBool b1, VBool b2 => Some (VBool (orb b1 b2))

  (* --- structural/reference equality (Interpreter.scala:638-640) --- *)
  (* AstValue-vs-AstValue alone uses REFERENCE equality (`l eq r`);
     all other values use ordinary Scala equality. *)
  | BEq, VAst origin1 _ path1, VAst origin2 _ path2 =>
      Some (VBool (ast_ref_eqb origin1 path1 origin2 path2))
  | BEq, _, _ => option_map VBool (val_eqb_partial v1 v2)

  (* --- numeric equality (Interpreter.scala:643-651) --- *)
  | BEqual, VMath z1, VMath z2 => Some (VBool (Z.eqb z1 z2))
  | BEqual, VInfinity p1, VInfinity p2 => Some (VBool (Bool.eqb p1 p2))
  | BEqual, VNumber f1, VNumber f2 => Some (VBool (num_ieee_eqb f1 f2))
  | BEqual, VBigInt z1, VBigInt z2 => Some (VBool (Z.eqb z1 z2))
  | BEqual, VInfinity _, VMath _ => Some (VBool false)
  | BEqual, VMath _, VInfinity _ => Some (VBool false)

  (* --- big integers (Interpreter.scala:654-665) --- *)
  | BAdd, VBigInt z1, VBigInt z2 => Some (VBigInt (z1 + z2))
  | BSub, VBigInt z1, VBigInt z2 => Some (VBigInt (z1 - z2))
  | BMul, VBigInt z1, VBigInt z2 => Some (VBigInt (z1 * z2))
  | BDiv, VBigInt z1, VBigInt z2 =>
      if Z.eqb z2 0 then None else Some (VBigInt (Z.quot z1 z2))
  | BMod, VBigInt z1, VBigInt z2 =>
      if Z.eqb z2 0 then None else Some (VBigInt (Z.modulo z1 z2))
  | BLt, VBigInt z1, VBigInt z2 => Some (VBool (Z.ltb z1 z2))
  | BBAnd, VBigInt z1, VBigInt z2 => Some (VBigInt (Z.land z1 z2))
  | BBOr, VBigInt z1, VBigInt z2 => Some (VBigInt (Z.lor z1 z2))
  | BBXOr, VBigInt z1, VBigInt z2 => Some (VBigInt (Z.lxor z1 z2))
  | BLShift, VBigInt z1, VBigInt z2 =>
      match scala_shift_count z2 with
      | Some count =>
          if scala_shift_result_safe true z1 count
          then Some (VBigInt (Z.shiftl z1 count))
          else None
      | None => None
      end
  | BRShift, VBigInt z1, VBigInt z2 =>
      match scala_shift_count z2 with
      | Some count =>
          if scala_shift_result_safe false z1 count
          then Some (VBigInt (Z.shiftr z1 count))
          else None
      | None => None
      end
  | BPow, VBigInt z1, VBigInt z2 =>
      let exponent := scala_to_int32 z2 in
      if (0 <=? exponent)%Z
      then option_map VBigInt (bounded_z_pow z1 exponent)
      else None

  | _, _, _ => None
  end.


(** ** Conversions ([EConvert], Interpreter.scala:263-289)

    Pure conversions stay here.  String-to-Number/BigInt and formatting use
    the typed host cache in [Semantics]/[Exec], because they call Scala/JVM
    helpers rather than IR operational semantics. *)
Definition eval_cop (op : cop) (v : val) : option val :=
  match op, v with
  | CToMath, VCodeUnit c => Some (VMath c)
  | CToCodeUnit, VMath z => Some (VCodeUnit (Z.modulo z 65536))  (* n.toChar *)
  | CToNumber, VInfinity true => Some (VNumber PrimFloat.infinity)
  | CToNumber, VInfinity false => Some (VNumber PrimFloat.neg_infinity)
  | CToApproxNumber, VInfinity true => Some (VNumber PrimFloat.infinity)
  | CToApproxNumber, VInfinity false => Some (VNumber PrimFloat.neg_infinity)
  | CToApproxNumber, VMath z => option_map VNumber (float_of_Z z)
  | CToNumber, VMath z => option_map VNumber (float_of_Z z)
  | CToBigInt, VMath z => Some (VBigInt z)
  | CToBigInt, VNumber f =>
      (** [BigDecimal.exact(f).toBigInt] in ESMeta truncates the exact finite
          Binary64 value toward zero; it is not restricted to the safe-integer
          interval. *)
      option_map VBigInt (float_to_Z_trunc f)
  | CToMath, VMath z => Some (VMath z)
  | CToApproxNumber, VNumber f => Some (VNumber f)
  | CToNumber, VNumber f => Some (VNumber f)
  | CToMath, VBigInt z => Some (VMath z)
  | _, _ => None
  end.

(** A generated Number -> Math -> Number composite must validate its left
    conversion before evaluating the right expression.  A finite Number is
    deliberately kept raw here: [VMath] currently models integers only, while
    two finite Numbers are handled exactly by [HQNumberMathOp]. *)
Variant prepared_number_math_operand : Type :=
| PNMOFiniteNumber (f : float)
| PNMOMath (v : val).

Definition prepare_number_math_operand
  (v : val) : option prepared_number_math_operand :=
  match v with
  | VNumber f =>
      if PrimFloat.is_finite f then Some (PNMOFiniteNumber f) else None
  | _ => option_map PNMOMath (eval_cop CToMath v)
  end.

(** Mixed comparisons cannot use the exact binary value of the Number:
    ESMeta first constructs a Scala BigDecimal from its decimal rendering.
    Identify precisely those cases for the typed host boundary. *)
Definition number_math_comparison_query
  (op : bop) (left right : prepared_number_math_operand)
  : option host_query :=
  match op, left, right with
  | BLt, PNMOFiniteNumber number, PNMOMath (VMath integer) =>
      Some (HQNumberMathCompare NMCLt NMCNumberLeft number integer)
  | BLt, PNMOMath (VMath integer), PNMOFiniteNumber number =>
      Some (HQNumberMathCompare NMCLt NMCNumberRight number integer)
  | BEqual, PNMOFiniteNumber number, PNMOMath (VMath integer) =>
      Some (HQNumberMathCompare NMCEqual NMCNumberLeft number integer)
  | BEqual, PNMOMath (VMath integer), PNMOFiniteNumber number =>
      Some (HQNumberMathCompare NMCEqual NMCNumberRight number integer)
  | _, _, _ => None
  end.

(** Pure cases remain local: Number/Number uses Binary64 ordering/equality,
    while fully materialized Math/Math follows [eval_bop]. *)
Definition eval_number_math_comparison_pure
  (op : bop) (left right : prepared_number_math_operand) : option val :=
  match op, left, right with
  | BLt, PNMOFiniteNumber lf, PNMOFiniteNumber rf =>
      Some (VBool (PrimFloat.ltb lf rf))
  | BEqual, PNMOFiniteNumber lf, PNMOFiniteNumber rf =>
      Some (VBool (num_ieee_eqb lf rf))
  | BLt, PNMOMath lv, PNMOMath rv
  | BEqual, PNMOMath lv, PNMOMath rv => eval_bop op lv rv
  | _, _, _ => None
  end.

(** ** Variadic operators ([EVariadic], Interpreter.scala:669-693)

    [Min]: if any argument is [-inf] the result is [-inf]; otherwise drop
    every [+inf] and, if nothing is left, return [+inf]; otherwise every
    remaining argument must be a [Math] ([asMath] throws otherwise) and
    the result is their minimum.  [Max] is the mirror image.  [Concat]
    maps [Str s |-> s] and [CodeUnit c |-> c.toString] (a one-code-unit
    string under D-1) and concatenates; any other value raises [NoString].
    An empty argument list raises [InvalidVariadicOp] in every case.
    ESMeta's [Math] is a [BigDecimal]; ADR-5 restricts us to [Z]. *)

Definition is_inf (p : bool) (v : val) : bool :=
  match v with VInfinity q => Bool.eqb p q | _ => false end.

Fixpoint maths_of (vs : list val) : option (list Z) :=
  match vs with
  | nil => Some nil
  | VMath z :: t => option_map (cons z) (maths_of t)
  | _ :: _ => None
  end.

(** Scala's [reduce]: fold the tail onto the head; undefined when empty. *)
Definition reduce_z (f : Z -> Z -> Z) (zs : list Z) : option Z :=
  match zs with nil => None | z :: t => Some (fold_left f t z) end.

Definition vop_extremum (f : Z -> Z -> Z) (absorb drop : bool)
    (vs : list val) : option val :=
  if existsb (is_inf absorb) vs then Some (VInfinity absorb)
  else
    match filter (fun v => negb (is_inf drop v)) vs with
    | nil => Some (VInfinity drop)
    | kept =>
        match maths_of kept with
        | None => None
        | Some zs => option_map VMath (reduce_z f zs)
        end
    end.

Fixpoint concat_vals (vs : list val) : option cstr :=
  match vs with
  | nil => Some nil
  | VStr cs :: t => option_map (app cs) (concat_vals t)
  | VCodeUnit c :: t => option_map (app [c]) (concat_vals t)
  | _ :: _ => None
  end.

Definition eval_vop (op : vop) (vs : list val) : option val :=
  match vs with
  | nil => None                       (* InvalidVariadicOp *)
  | _ :: _ =>
      match op with
      | VoMin => vop_extremum Z.min false true vs
      | VoMax => vop_extremum Z.max true false vs
      | VoConcat => option_map VStr (concat_vals vs)
      end
  end.

(** ** [EInstanceOf] (Interpreter.scala:310-314)

    Total: a non-AST value or a non-GrammarSymbol target is [false], not an
    error.  The first clause is a wildcard grammar symbol matching any
    *syntactic* node; a lexical node still has to match by name. *)

Definition eval_instanceof (v t : val) : val :=
  match v, t with
  | VAst _ root path, VGrammarSymbol nm _ =>
      match ast_focus root path with
      | Some a =>
          match a with
          | ASyn _ _ _ _ _ _ _ _ =>
              if String.eqb nm "" then VBool true
              else VBool (String.eqb (ast_name a) nm)
          | ALex _ _ _ _ _ => VBool (String.eqb (ast_name a) nm)
          end
      | None => VBool false
      end
  | _, _ => VBool false
  end.

(** ** [ESubstring] (Interpreter.scala:237-243)

    [asStr]/[asInt] throw on the wrong shape, and [java.lang.String.substring]
    throws unless [0 <= from <= to <= length]; all of those are UB here.
    Under D-1 a [cstr] is exactly Java's UTF-16 code-unit sequence, so the
    index arithmetic is the same arithmetic.  The one non-obvious clause is
    ESMeta's own: an upper bound *strictly greater* than the length is not
    an error, it degrades to [substring(from)]. *)

Definition substr_from (cs : cstr) (f : Z) : option cstr :=
  if andb (0 <=? f)%Z (f <=? Z.of_nat (List.length cs))%Z
  then Some (skipn (Z.to_nat f) cs) else None.

Definition substr_range (cs : cstr) (f t : Z) : option cstr :=
  if andb (0 <=? f)%Z
       (andb (f <=? t)%Z (t <=? Z.of_nat (List.length cs))%Z)
  then Some (firstn (Z.to_nat (t - f)) (skipn (Z.to_nat f) cs))
  else None.

Definition eval_substring (sv fv : val) (tv : option val) : option val :=
  match sv, fv with
  | VStr cs, VMath f =>
      match tv with
      | None => option_map VStr (substr_from cs f)
      | Some (VMath t) =>
          if (Z.of_nat (List.length cs) <? t)%Z
          then option_map VStr (substr_from cs f)
          else option_map VStr (substr_range cs f t)
      | Some _ => None
      end
  | _, _ => None
  end.

(** ** Local environments (pure; ADR-6)

    Mirrors the flat, unscoped [MMap[Local, Value]] of a call context
    (state/Context.scala:10-53): lookup fails on absent locals (read of an
    unknown variable is UB), update defines unconditionally. *)

Definition env : Type := list (local * val).

(** [ECont] captures every named local and no temporary, matching
    [Interpreter.scala:327-330].  [env_update] maintains at most one
    binding per local, so this list is a faithful finite-map image. *)
Fixpoint capture_named_env (ρ : env) : list (string * val) :=
  match ρ with
  | nil => nil
  | (LName x, v) :: tl => (x, v) :: capture_named_env tl
  | (LTemp _, _) :: tl => capture_named_env tl
  end.

Definition capture_named_env_map (ρ : env) : list (string * val) :=
  captured_normalize (capture_named_env ρ).

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

(** ** Heap objects — the type lives in [Fragment.v] because [prog] must
    carry an exported initial heap; the operations stay here. *)

(** A result of [None] from these helpers is equality ambiguity; successful
    lookup absence is represented separately as [Some None]. *)
Fixpoint map_lookup_partial (es : list (val * val)) (k : val)
  : option (option val) :=
  match es with
  | nil => Some None
  | (k', v) :: tl =>
      match val_eqb_partial k k' with
      | Some true => Some (Some v)
      | Some false => map_lookup_partial tl k
      | None => None
      end
  end.

(* insertion-ordered update: existing key keeps its position, new key
   appends (mirrors `m.map += key -> value`).  A continuation key would
   also have a hash derived from mutable CallContext data in Scala, so it is
   conservatively unsupported even when no existing comparison is needed. *)
Fixpoint val_hash_stable (v : val) : bool :=
  match v with
  | VCont _ _ _ => false
  | VClo _ captured =>
      (fix go (xs : list (string * val)) : bool :=
         match xs with
         | nil => true
         | (_, u) :: tl => andb (val_hash_stable u) (go tl)
         end) captured
  | _ => true
  end.

Fixpoint map_insert_scan_partial
  (k : val) (v : val) (es : list (val * val))
  : option (list (val * val)) :=
  match es with
  | nil => Some ((k, v) :: nil)
  | (k', v') :: tl =>
      match val_eqb_partial k k' with
      | Some true => Some ((k, v) :: tl)
      | Some false =>
          option_map (cons (k', v')) (map_insert_scan_partial k v tl)
      | None => None
      end
  end.

Definition map_insert_partial
  (k : val) (v : val) (es : list (val * val))
  : option (list (val * val)) :=
  if val_hash_stable k then map_insert_scan_partial k v es else None.

Fixpoint map_delete_partial (k : val) (es : list (val * val))
  : option (list (val * val)) :=
  match es with
  | nil => Some nil
  | (k', v') :: tl =>
      match val_eqb_partial k k' with
      | Some true => Some tl
      | Some false =>
          option_map (cons (k', v')) (map_delete_partial k tl)
      | None => None
      end
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
    record whose own tname is the base `CompletionRecord`.  Record tests
    use the exported subtyping relation ([TyModel.v], generated from
    esmeta.ty.TyModel.parentOf); Abrupt/Normal additionally check the field
    refinement, exactly, as set out below (OQ-12).

    Field refinements on OTHER record types are still not modelled; the
    exporter only emits [tyexp]s in this grammar, so anything else is
    reported rather than silently mis-modelled.  Validation of these tests
    against ESMeta is by the differential harness. *)

Definition completion_type (fs : list (string * val)) : option string :=
  match fields_lookup fs "Type" with
  | Some (VEnum n) => Some n
  | _ => None
  end.

(** *** Completion type tests: the exact field-map refinement (OQ-12)

    `(? x: Abrupt)` and `(? x: Normal)` are 3355 of the specification's
    7760 type tests, so getting them approximately right is not an option.

    ESMeta decides them with [RecordTy.contains] (RecordTy.scala:157-168).
    The runtime tname is always `CompletionRecord` — the spec allocates no
    other completion record (9 [ERecord] sites, none of them
    Normal/Throw/…, and none in the initial heap) [VF] — so the
    `isStrictSubTy` and `l == r` branches never fire and the third one
    decides: [lcaOf(CompletionRecord, AbruptCompletion) = CompletionRecord]
    and [diffOf(lca, r) = FieldMap(upper ++ ownFieldsOf(r))]
    (TyModel.scala:86-93). Measured from ESMeta [VF]:

<<
  ownFieldsOf(AbruptCompletion) =
    Type   : Enum[~break~, ~continue~, ~return~, ~throw~]
    Value  : ESValue | Enum[~empty~]
    Target : Enum[~empty~] | String
  ownFieldsOf(NormalCompletion) =
    Type   : Enum[~normal~]
    Value  : (unconstrained, but present)
    Target : Enum[~empty~]
>>

    [FieldMap.contains] applies [Binding.contains] to every field
    (FieldMap.scala:71-73), and that function has two clauses that are easy
    to get wrong (Binding.scala:62-65):

      case Some(Undef) => true   // "Undef represents uninitialized value"
      case None        => this.absent

    So a field holding [Undef] satisfies ANY binding, and an ABSENT field
    satisfies only a binding that admits absence — which none of these six
    do. Both are modelled below.

    The previous model tested only `Type`, which over-approximates: it
    ignored `Value` and `Target` entirely. `FVExport.rocqTy` now re-derives
    these two field maps from ESMeta at export time and refuses to emit
    [TAbrupt]/[TNormal] if they differ from what is written here, so this
    transcription cannot drift silently. *)

Definition enum_in (v : val) (names : list string) : bool :=
  match v with
  | VEnum n => existsb (String.eqb n) names
  | _ => false
  end.

(** [Binding.contains] for a binding that does not admit absence. *)
Definition binding_ok (fs : list (string * val)) (f : string)
    (p : val -> bool) : bool :=
  match fields_lookup fs f with
  | None => false               (* absent, and absent is not admitted *)
  | Some VUndef => true         (* Undef satisfies any binding *)
  | Some v => p v
  end.

(** [ESValue = ObjectT || ESPrimT] (ty/package.scala:63-78): a record
    subtyped from `Object` or from `Symbol`, or a primitive.  Notably NOT
    Math, Enum, CodeUnit, Infinity, Clo, Ast or GrammarSymbol.  [vobj] is
    the heap object the value points at, resolved by the caller because
    this file is pure. *)
Definition esvalue_ok (vobj : option obj) (v : val) : bool :=
  match v with
  | VNumber _ | VBigInt _ | VStr _ | VBool _ | VUndef | VNull => true
  | VAddr _ =>
      match vobj with
      | Some (ORecord tn _) =>
          orb (record_subtype tn "Object") (record_subtype tn "Symbol")
      | _ => false
      end
  | _ => false
  end.

Definition abrupt_fields_ok (fs : list (string * val)) (vobj : option obj)
  : bool :=
  andb
    (binding_ok fs "Type"
       (fun v => enum_in v ("break" :: "continue" :: "return" :: "throw"
                            :: nil)))
    (andb
       (binding_ok fs "Value"
          (fun v => orb (enum_in v ("empty" :: nil)) (esvalue_ok vobj v)))
       (binding_ok fs "Target"
          (fun v => orb (enum_in v ("empty" :: nil))
                      (match v with VStr _ => true | _ => false end)))).

Definition normal_fields_ok (fs : list (string * val)) : bool :=
  andb (binding_ok fs "Type" (fun v => enum_in v ("normal" :: nil)))
    (andb (binding_ok fs "Value" (fun _ => true))
       (binding_ok fs "Target" (fun v => enum_in v ("empty" :: nil)))).

(** Lookup in the objects already resolved by the lazy heap-query cache. *)
Fixpoint resolved_lookup (r : list (nat * obj)) (a : nat) : option obj :=
  match r with
  | nil => None
  | (b, o) :: tl => if Nat.eqb a b then Some o else resolved_lookup tl a
  end.

Fixpoint ty_check_prim (t : tyexp) (v : val) : bool :=
  match v, t with
  | _, TUnion ts => existsb (fun t' => ty_check_prim t' v) ts
  | VStr _, TStrTy => true
  | _, TStrSet values =>
      match v with
      | VStr cs => existsb (fun allowed => cstr_eqb cs allowed) values
      | _ => false
      end
  | VBool _, TBoolTy => true
  | _, TBoolSet allow_false allow_true =>
      match v with
      | VBool false => allow_false
      | VBool true => allow_true
      | _ => false
      end
  | VMath _, TMathTy => true
  | VMath z, TMathInt neg zero pos => math_int_sign_ok z neg zero pos
  | _, TMathIntSet values =>
      match v with
      | VMath z => existsb (fun allowed => Z.eqb z allowed) values
      | _ => false
      end
  | VUndef, TUndefTy => true
  | VNull, TNullTy => true
  | VEnum _, TEnumTy => true
  | VEnum name, TEnumNames names => name_mem name names
  | VClo _ _, TCloTy => true
  | VNumber _, TNumberTy => true
  | VNumber f, TNumberInt neg zero pos hasNaN =>
      number_int_sign_ok f neg zero pos hasNaN
  | VBigInt _, TBigIntTy => true
  | VCodeUnit _, TCodeUnitTy => true
  | VInfinity _, TInfinityTy => true
  | _, TInfinity allow_neg allow_pos =>
      match v with
      | VInfinity false => allow_neg
      | VInfinity true => allow_pos
      | _ => false
      end
  | VAst _ root path, TAstTy =>
      match ast_focus root path with Some _ => true | None => false end
  (* AstTy.Simple: some listed name is among the node's [types] *)
  | VAst _ root path, TAstNames ns =>
      match ast_focus root path with
      | Some a => existsb (fun n => name_mem n (ast_types a)) ns
      | None => false
      end
  (* AstTy.Detail: exact production name and rhs index *)
  | VAst _ root path, TAstDetail n i =>
      match ast_focus root path with
      | Some a => andb (String.eqb (ast_name a) n) (Nat.eqb (ast_idx a) i)
      | None => false
      end
  | _, _ => false
  end.

(** ESMeta's [RecordTy.contains] has a structural descendant branch:
    an ancestor-tagged record can satisfy a requested descendant when the
    descendant-side field-map difference accepts the runtime fields.

    The generated model now retains every direct binding's absence bit,
    finite-enum discriminator, and primitive/address kind.  This is the
    important distinction between, for example, [NormalCompletion] and
    [ReturnCompletion]: both have [Type]/[Value]/[Target], but their [Type]
    constraints are disjoint.

    Recursive heap-container refinements are deliberately projected to the
    address kind by [FVTyModel].  Their deeper shape remains the exported
    state's typing invariant; type tests stay local and do not traverse the
    whole mutable ECMAScript heap. *)

(** Child declarations override an inherited binding with the same field,
    matching [TyModel.diffOf]'s [upper ++ ownFieldsOf(child)] map union. *)
Fixpoint record_binding_put
  (b : record_field_binding) (bs : list record_field_binding)
  : list record_field_binding :=
  match bs with
  | nil => b :: nil
  | x :: tl =>
      if String.eqb (rfb_name x) (rfb_name b)
      then b :: tl
      else x :: record_binding_put b tl
  end.

Fixpoint record_bindings_override
  (own inherited : list record_field_binding)
  : list record_field_binding :=
  match own with
  | nil => inherited
  | b :: tl =>
      record_bindings_override tl (record_binding_put b inherited)
  end.

(** Reconstruct the target-side binding difference from the least common
    ancestor.  [stored <: target] is RecordTy.contains's nominal branch and
    needs no structural constraints. *)
Fixpoint record_refinement_bindings
  (fuel : nat) (stored target : string)
  : option (list record_field_binding) :=
  if record_subtype stored target then Some nil else
  match fuel with
  | O => None
  | S fuel' =>
      match record_parent target with
      | None => None
      | Some parent =>
          if record_subtype stored parent
          then Some (record_own_bindings target)
          else
            option_map
              (fun inherited =>
                 record_bindings_override
                   (record_own_bindings target) inherited)
              (record_refinement_bindings fuel' stored parent)
      end
  end.

Fixpoint record_constraint_ok
  (c : record_field_constraint) (v : val) : bool :=
  match c with
  | RFCAny => true
  | RFCNever => false
  | RFCUnion cs => existsb (fun c' => record_constraint_ok c' v) cs
  | RFCEnum names => enum_in v names
  | RFCEnumAny =>
      match v with VEnum _ => true | _ => false end
  | RFCStr =>
      match v with VStr _ => true | _ => false end
  | RFCStrSet values =>
      match v with
      | VStr cs => existsb (fun allowed => cstr_eqb cs allowed) values
      | _ => false
      end
  | RFCBool allow_false allow_true =>
      match v with
      | VBool false => allow_false
      | VBool true => allow_true
      | _ => false
      end
  | RFCMath =>
      match v with VMath _ => true | _ => false end
  | RFCMathSign allow_neg allow_zero allow_pos
  | RFCMathIntSign allow_neg allow_zero allow_pos =>
      match v with
      | VMath z =>
          if (z <? 0)%Z then allow_neg
          else if Z.eqb z 0 then allow_zero
          else allow_pos
      | _ => false
      end
  | RFCMathSet values
  | RFCMathIntSet values =>
      match v with
      | VMath z => existsb (fun allowed => Z.eqb z allowed) values
      | _ => false
      end
  | RFCInfinity allow_neg allow_pos =>
      match v with
      | VInfinity false => allow_neg
      | VInfinity true => allow_pos
      | _ => false
      end
  | RFCNumber =>
      match v with VNumber _ => true | _ => false end
  | RFCBigInt =>
      match v with VBigInt _ => true | _ => false end
  | RFCUndef =>
      match v with VUndef => true | _ => false end
  | RFCNull =>
      match v with VNull => true | _ => false end
  | RFCCodeUnit =>
      match v with VCodeUnit _ => true | _ => false end
  | RFCClo =>
      match v with VClo _ _ => true | _ => false end
  | RFCCloNames names =>
      match v with
      | VClo fn _ => name_mem fn names
      | _ => false
      end
  | RFCCont =>
      match v with VCont _ _ _ => true | _ => false end
  | RFCAst =>
      match v with VAst _ _ _ => true | _ => false end
  | RFCAstNames names =>
      match v with
      | VAst _ root path =>
          match ast_focus root path with
          | Some a => existsb (fun name => name_mem name (ast_types a)) names
          | None => false
          end
      | _ => false
      end
  | RFCAstDetail name idx =>
      match v with
      | VAst _ root path =>
          match ast_focus root path with
          | Some a =>
              andb (String.eqb (ast_name a) name) (Nat.eqb (ast_idx a) idx)
          | None => false
          end
      | _ => false
      end
  | RFCGrammarSymbol =>
      match v with VGrammarSymbol _ _ => true | _ => false end
  (* These constructors need the heap object behind an address.  The pure
     fallback never guesses their result; [record_constraint_query] below
     performs the exact, lazy heap reads. *)
  | RFCRecordTop | RFCRecord _
  | RFCMapTop | RFCMap _ _
  | RFCListTop | RFCList _
  | RFCUnsupported | RFCAddr => false
  end.

(** Binding.contains has one unusual but important rule: a present [Undef]
    represents an uninitialized slot and satisfies every binding, even
    [Binding.Bot]. *)
Definition record_binding_ok
  (fs : list (string * val)) (b : record_field_binding) : bool :=
  match fields_lookup fs (rfb_name b) with
  | None => rfb_absent b
  | Some VUndef => true
  | Some v => record_constraint_ok (rfb_constraint b) v
  end.

Fixpoint record_bindings_ok
  (fs : list (string * val)) (bs : list record_field_binding) : bool :=
  match bs with
  | nil => true
  | b :: tl =>
      andb (record_binding_ok fs b) (record_bindings_ok fs tl)
  end.

(** An inline [Binding.Exist] accepts every present value, including the
    special uninitialized [VUndef], and rejects only absence. *)
Fixpoint required_record_fields_ok
  (fs : list (string * val)) (required : list string) : bool :=
  match required with
  | nil => true
  | field :: rest =>
      andb
        (match fields_lookup fs field with Some _ => true | None => false end)
        (required_record_fields_ok fs rest)
  end.

(** Pure projection of [RecordTy.contains] for an inline field map made only
    of [Binding.Exist].  The query-based execution path below performs the
    same test through [RFCRecord]. *)
Definition record_fields_accepts
  (stored : string) (fs : list (string * val))
  (target : string) (required : list string) : bool :=
  if String.eqb stored target
  then required_record_fields_ok fs required
  else if record_subtype stored target
       then true
       else
         match record_refinement_bindings 113 stored target with
         | Some inherited =>
             andb
               (record_bindings_ok fs inherited)
               (required_record_fields_ok fs required)
         | None => false
         end.

Definition record_accepts
  (stored : string) (fs : list (string * val)) (target : string) : bool :=
  match record_refinement_bindings 113 stored target with
  | Some required => record_bindings_ok fs required
  | None => false
  end.

(** Three-valued refinement checks prevent the exported [RFCAddr]
    projection from becoming a silent proof that an arbitrary heap object
    has the recursively constrained type expected by ESMeta.  [None] means
    that the local model has reached precisely that unexported heap-shape
    obligation; callers turn it into UB rather than guessing [true] or
    [false]. *)
Definition decision_or (left right : option bool) : option bool :=
  match left, right with
  | Some true, _ | _, Some true => Some true
  | Some false, Some false => Some false
  | _, _ => None
  end.

Definition decision_and (left right : option bool) : option bool :=
  match left, right with
  | Some false, _ | _, Some false => Some false
  | Some true, Some true => Some true
  | _, _ => None
  end.

Fixpoint decisions_any (xs : list (option bool)) : option bool :=
  match xs with
  | nil => Some false
  | x :: tl => decision_or x (decisions_any tl)
  end.

Fixpoint decisions_all (xs : list (option bool)) : option bool :=
  match xs with
  | nil => Some true
  | x :: tl => decision_and x (decisions_all tl)
  end.

Fixpoint record_constraint_decide
  (c : record_field_constraint) (v : val) : option bool :=
  match c with
  | RFCUnion cs =>
      decisions_any (List.map (fun c' => record_constraint_decide c' v) cs)
  | RFCRecordTop | RFCRecord _
  | RFCMapTop | RFCMap _ _
  | RFCListTop | RFCList _
  | RFCAddr =>
      match v with
      | VAddr _ => None
      | _ => Some false
      end
  | RFCUnsupported => None
  | _ => Some (record_constraint_ok c v)
  end.

Definition record_binding_decide
  (fs : list (string * val)) (b : record_field_binding) : option bool :=
  match fields_lookup fs (rfb_name b) with
  | None => Some (rfb_absent b)
  | Some VUndef => Some true
  | Some v => record_constraint_decide (rfb_constraint b) v
  end.

Fixpoint record_bindings_decide
  (fs : list (string * val)) (bs : list record_field_binding)
  : option bool :=
  match bs with
  | nil => Some true
  | b :: tl =>
      decision_and
        (record_binding_decide fs b)
        (record_bindings_decide fs tl)
  end.

Definition record_accepts_decide
  (stored : string) (fs : list (string * val)) (target : string)
  : option bool :=
  if record_subtype stored target then Some true else
  match record_refinement_bindings 113 stored target with
  | Some required => record_bindings_decide fs required
  | None => Some false
  end.

(** This legacy pure helper does not support [TListOf]; the live path uses
    the recursive lazy [ty_check_query] checker below. *)
Fixpoint ty_check_obj (t : tyexp) (o : obj) (r : list (nat * obj)) : bool :=
  match o, t with
  | _, TUnion ts => existsb (fun t' => ty_check_obj t' o r) ts
  | OList _, TList => true
  | OMap _, TMapTy => true
  | ORecord tn fs, TRecord want => record_accepts tn fs want
  | ORecord tn fs, TRecordFields want required =>
      record_fields_accepts tn fs want required
  | ORecord tn _, TCompletion => record_subtype tn "CompletionRecord"
  | ORecord tn fs, TAbrupt =>
      if record_subtype tn "AbruptCompletion" then true
      else if record_subtype tn "CompletionRecord"
           then abrupt_fields_ok fs
                  (match fields_lookup fs "Value" with
                   | Some (VAddr b) => resolved_lookup r b
                   | _ => None
                   end)
           else false
  | ORecord tn fs, TNormal =>
      if record_subtype tn "NormalCompletion" then true
      else if record_subtype tn "CompletionRecord"
           then normal_fields_ok fs
           else false
  | _, _ => false
  end.

(** Exact when the exported type carries enough information, and [None]
    only for recursively constrained address refinements projected to
    [RFCAddr]. *)
Fixpoint ty_check_obj_decide
  (t : tyexp) (o : obj) (r : list (nat * obj)) : option bool :=
  match o, t with
  | _, TUnion ts =>
      decisions_any (List.map (fun t' => ty_check_obj_decide t' o r) ts)
  | ORecord tn fs, TRecord want => record_accepts_decide tn fs want
  | ORecord tn fs, TRecordFields want required =>
      if String.eqb tn want
      then Some (required_record_fields_ok fs required)
      else if record_subtype tn want
           then Some true
           else
             match record_refinement_bindings 113 tn want with
             | Some inherited =>
                 decision_and
                   (record_bindings_decide fs inherited)
                   (Some (required_record_fields_ok fs required))
             | None => Some false
             end
  | OList vs, TListOf t' =>
      decisions_all
        (List.map
          (fun v =>
             match v with
             | VAddr b =>
                 match resolved_lookup r b with
                 | Some o' => ty_check_obj_decide t' o' nil
                 | None => Some false
                 end
             | _ => Some (ty_check_prim t' v)
             end) vs)
  | _, _ => Some (ty_check_obj t o r)
  end.

(** *** One shared, lazy heap-query plan for recursive type containment

    ESMeta's [ValueTy.contains] recursively follows only the heap edges
    demanded by the active Record/List/Map constraint.  Reimplementing that
    traversal once in [Semantics.v] and once in [Exec.v] would create a new
    semantic drift surface, so the pure domain instead builds this finite
    free read tree.  The ITree denotation interprets [HeapRead] with
    [get_obj]; the executable reference interpreter uses [heap_get].

    [option bool] remains deliberate: [None] is a conservative boundary
    (fuel exhaustion or the legacy opaque [RFCAddr]), never a guessed
    success.  Missing heap addresses are not represented here; each
    interpreter maps a failed read to its ordinary UB/Stuck behavior. *)

Inductive heap_query (A : Type) : Type :=
| HeapDone (result : A)
| HeapRead (address : nat) (continue_with : obj -> heap_query A).

Arguments HeapDone {A} _.
Arguments HeapRead {A} _ _.

Fixpoint heap_query_bind {A B}
  (query : heap_query A) (next : A -> heap_query B) : heap_query B :=
  match query with
  | HeapDone result => next result
  | HeapRead address continue_with =>
      HeapRead address
        (fun object => heap_query_bind (continue_with object) next)
  end.

Definition heap_query_decision_or
  (left right : heap_query (option bool)) : heap_query (option bool) :=
  heap_query_bind left (fun left_result =>
    match left_result with
    | Some true => HeapDone (Some true)
    | Some false => right
    | None =>
        heap_query_bind right (fun right_result =>
          HeapDone (decision_or None right_result))
    end).

Definition heap_query_decision_and
  (left right : heap_query (option bool)) : heap_query (option bool) :=
  heap_query_bind left (fun left_result =>
    match left_result with
    | Some false => HeapDone (Some false)
    | Some true => right
    | None =>
        heap_query_bind right (fun right_result =>
          HeapDone (decision_and None right_result))
    end).

(** [record_constraint_query] is the exact executable image of the
    container cases in [ValueTy.contains]:

    - record targets are a disjunction;
    - map/list elements are universally quantified;
    - field bindings use ESMeta's special present-[Undef] rule;
    - a strict nominal record subtype succeeds without inspecting the
      target's inline refinement, exactly matching RecordTy.scala's first
      branch;
    - otherwise the generated hierarchy difference and inline [FieldMap]
      are both checked.

    Fuel bounds only recursive heap depth.  Traversing siblings in a list,
    map, union, or field map does not consume additional depth. *)
Fixpoint record_constraint_query
  (fuel : nat) (constraint : record_field_constraint) (value : val)
  {struct fuel} : heap_query (option bool) :=
  match fuel with
  | O => HeapDone None
  | S fuel' =>
      match constraint with
      | RFCUnion constraints =>
          (fix check_any
             (rest : list record_field_constraint)
             : heap_query (option bool) :=
             match rest with
             | nil => HeapDone (Some false)
             | head :: tail =>
                 heap_query_decision_or
                   (record_constraint_query fuel' head value)
                   (check_any tail)
             end) constraints
      | RFCRecordTop =>
          match value with
          | VAddr address =>
              HeapRead address (fun object =>
                match object with
                | ORecord _ _ => HeapDone (Some true)
                | _ => HeapDone (Some false)
                end)
          | _ => HeapDone (Some false)
          end
      | RFCRecord targets =>
          match value with
          | VAddr address =>
              HeapRead address (fun object =>
                match object with
                | ORecord stored fields =>
                    (fix check_targets
                       (rest :
                         list
                           (string *
                             list
                               (string *
                                 (bool * record_field_constraint))))
                       : heap_query (option bool) :=
                       match rest with
                       | nil => HeapDone (Some false)
                       | (target, inline_bindings) :: tail =>
                           let check_inline :=
                             (fix check_bindings
                                (bindings :
                                  list
                                    (string *
                                      (bool * record_field_constraint)))
                                : heap_query (option bool) :=
                                match bindings with
                                | nil => HeapDone (Some true)
                                | (field, (absent, field_constraint)) :: more =>
                                    let current :=
                                      match fields_lookup fields field with
                                      | None => HeapDone (Some absent)
                                      | Some VUndef => HeapDone (Some true)
                                      | Some field_value =>
                                          record_constraint_query
                                            fuel' field_constraint field_value
                                      end in
                                    heap_query_decision_and
                                      current (check_bindings more)
                                end) inline_bindings in
                           let current :=
                             if String.eqb stored target
                             then check_inline
                             else if record_subtype stored target
                             then HeapDone (Some true)
                             else
                               match
                                 record_refinement_bindings
                                   113 stored target
                               with
                               | None => HeapDone (Some false)
                               | Some required =>
                                   let check_required :=
                                     (fix check_bindings
                                        (bindings :
                                          list record_field_binding)
                                        : heap_query (option bool) :=
                                        match bindings with
                                        | nil => HeapDone (Some true)
                                        | binding :: more =>
                                            let current :=
                                              match
                                                fields_lookup fields
                                                  (rfb_name binding)
                                              with
                                              | None =>
                                                  HeapDone
                                                    (Some
                                                      (rfb_absent binding))
                                              | Some VUndef =>
                                                  HeapDone (Some true)
                                              | Some field_value =>
                                                  record_constraint_query
                                                    fuel'
                                                    (rfb_constraint binding)
                                                    field_value
                                              end in
                                            heap_query_decision_and
                                              current
                                              (check_bindings more)
                                        end) required in
                                   heap_query_decision_and
                                     check_required check_inline
                               end in
                           heap_query_decision_or
                             current (check_targets tail)
                       end) targets
                | _ => HeapDone (Some false)
                end)
          | _ => HeapDone (Some false)
          end
      | RFCMapTop =>
          match value with
          | VAddr address =>
              HeapRead address (fun object =>
                match object with
                | OMap _ => HeapDone (Some true)
                | _ => HeapDone (Some false)
                end)
          | _ => HeapDone (Some false)
          end
      | RFCMap key_constraint value_constraint =>
          match value with
          | VAddr address =>
              HeapRead address (fun object =>
                match object with
                | OMap entries =>
                    (fix check_entries
                       (rest : list (val * val))
                       : heap_query (option bool) :=
                       match rest with
                       | nil => HeapDone (Some true)
                       | (key, item) :: tail =>
                           heap_query_decision_and
                             (heap_query_decision_and
                               (record_constraint_query
                                 fuel' key_constraint key)
                               (record_constraint_query
                                 fuel' value_constraint item))
                             (check_entries tail)
                       end) entries
                | _ => HeapDone (Some false)
                end)
          | _ => HeapDone (Some false)
          end
      | RFCListTop =>
          match value with
          | VAddr address =>
              HeapRead address (fun object =>
                match object with
                | OList _ => HeapDone (Some true)
                | _ => HeapDone (Some false)
                end)
          | _ => HeapDone (Some false)
          end
      | RFCList element_constraint =>
          match value with
          | VAddr address =>
              HeapRead address (fun object =>
                match object with
                | OList values =>
                    (fix check_values
                       (rest : list val)
                       : heap_query (option bool) :=
                       match rest with
                       | nil => HeapDone (Some true)
                       | item :: tail =>
                           heap_query_decision_and
                             (record_constraint_query
                               fuel' element_constraint item)
                             (check_values tail)
                       end) values
                | _ => HeapDone (Some false)
                end)
          | _ => HeapDone (Some false)
          end
      | RFCAddr =>
          match value with
          | VAddr _ => HeapDone None
          | _ => HeapDone (Some false)
          end
      | _ => HeapDone (record_constraint_decide constraint value)
      end
  end.

Definition esvalue_constraint : record_field_constraint :=
  RFCUnion
    (RFCRecord
       ((("Object", nil) :
          string *
            list (string * (bool * record_field_constraint))) ::
        (("Symbol", nil) :
          string *
            list (string * (bool * record_field_constraint))) ::
        nil) ::
     RFCNumber :: RFCBigInt :: RFCStr :: RFCBool true true ::
     RFCUndef :: RFCNull :: nil).

Definition required_record_field_constraints
  (fields : list string)
  : list (string * (bool * record_field_constraint)) :=
  List.map (fun field => (field, (false, RFCAny))) fields.

(** The same query-plan interface covers explicit IR type expressions.
    This removes the old eager one-level pre-resolution assumption: nested
    [TListOf] and structural record refinements now share the recursive
    checker above. *)
Fixpoint ty_check_query
  (fuel : nat) (t : tyexp) (value : val) {struct fuel}
  : heap_query (option bool) :=
  match fuel with
  | O => HeapDone None
  | S fuel' =>
      match t with
      | TUnion types =>
          (fix check_any
             (rest : list tyexp) : heap_query (option bool) :=
             match rest with
             | nil => HeapDone (Some false)
             | head :: tail =>
                 heap_query_decision_or
                   (ty_check_query fuel' head value)
                   (check_any tail)
             end) types
      | TRecord target =>
          record_constraint_query fuel'
            (RFCRecord
              ((target, nil) :: nil))
            value
      | TRecordFields target required =>
          record_constraint_query fuel'
            (RFCRecord
              ((target, required_record_field_constraints required) :: nil))
            value
      | TList =>
          match value with
          | VAddr address =>
              HeapRead address (fun object =>
                match object with
                | OList _ => HeapDone (Some true)
                | _ => HeapDone (Some false)
                end)
          | _ => HeapDone (Some false)
          end
      | TMapTy =>
          match value with
          | VAddr address =>
              HeapRead address (fun object =>
                match object with
                | OMap _ => HeapDone (Some true)
                | _ => HeapDone (Some false)
                end)
          | _ => HeapDone (Some false)
          end
      | TListOf element_type =>
          match value with
          | VAddr address =>
              HeapRead address (fun object =>
                match object with
                | OList values =>
                    (fix check_values
                       (rest : list val)
                       : heap_query (option bool) :=
                       match rest with
                       | nil => HeapDone (Some true)
                       | item :: tail =>
                           heap_query_decision_and
                             (ty_check_query fuel' element_type item)
                             (check_values tail)
                       end) values
                | _ => HeapDone (Some false)
                end)
          | _ => HeapDone (Some false)
          end
      | TCompletion =>
          match value with
          | VAddr address =>
              HeapRead address (fun object =>
                match object with
                | ORecord stored _ =>
                    HeapDone
                      (Some
                        (record_subtype stored "CompletionRecord"))
                | _ => HeapDone (Some false)
                end)
          | _ => HeapDone (Some false)
          end
      | TNormal =>
          match value with
          | VAddr address =>
              HeapRead address (fun object =>
                match object with
                | ORecord stored fields =>
                    HeapDone
                      (Some
                        (if record_subtype stored "NormalCompletion"
                         then true
                         else if
                           record_subtype stored "CompletionRecord"
                         then normal_fields_ok fields
                         else false))
                | _ => HeapDone (Some false)
                end)
          | _ => HeapDone (Some false)
          end
      | TAbrupt =>
          match value with
          | VAddr address =>
              HeapRead address (fun object =>
                match object with
                | ORecord stored fields =>
                    if record_subtype stored "AbruptCompletion"
                    then HeapDone (Some true)
                    else if record_subtype stored "CompletionRecord"
                    then
                      let check_required :=
                        fun field constraint =>
                          match fields_lookup fields field with
                          | None => HeapDone (Some false)
                          | Some VUndef => HeapDone (Some true)
                          | Some field_value =>
                              record_constraint_query
                                fuel' constraint field_value
                          end in
                      heap_query_decision_and
                        (check_required "Type"
                          (RFCEnum
                            ("break" :: "continue" :: "return" ::
                             "throw" :: nil)))
                        (heap_query_decision_and
                          (check_required "Value"
                            (RFCUnion
                              (esvalue_constraint ::
                               RFCEnum ("empty" :: nil) :: nil)))
                          (check_required "Target"
                            (RFCUnion
                              (RFCEnum ("empty" :: nil) ::
                               RFCStr :: nil))))
                    else HeapDone (Some false)
                | _ => HeapDone (Some false)
                end)
          | _ => HeapDone (Some false)
          end
      | _ => HeapDone (Some (ty_check_prim t value))
      end
  end.

(** Greater than the generated record-field nesting bound (3) and the
    observed environment/prototype chains used by current Test262 inputs.
    Exhaustion is [None]/UB, so increasing this bound can only turn a
    previously unsupported deep check into a concrete verdict. *)
Definition type_check_fuel : nat := 128.


(** [ETypeOf] (Interpreter.scala:297-309).  The fragment has no Number or
    BigInt values, and Math/Enum/Clo are not contained in ObjectT or
    SymbolT, so they yield "SpecType" exactly as ESMeta does.  Addresses
    need ObjectT/SymbolT containment, which depends on field refinements
    we do not model: those are left to the caller as UB (see Semantics.v),
    NOT guessed. *)
Definition typeof_prim (v : val) : option string :=
  match v with
  | VNumber _ => Some "Number"
  | VBigInt _ => Some "BigInt"
  | VStr _ => Some "String"
  | VBool _ => Some "Boolean"
  | VUndef => Some "Undefined"
  | VNull => Some "Null"
  (* not contained in ObjectT or SymbolT, so ESMeta answers "SpecType" *)
  | VMath _ | VEnum _ | VClo _ _ | VCont _ _ _ | VAst _ _ _
  | VInfinity _ | VCodeUnit _
  | VGrammarSymbol _ _ => Some "SpecType"
  (* an address needs the heap; see [typeof_obj] *)
  | VAddr _ => None
  end.

(** [ETypeOf] on an address (Interpreter.scala:305-308).  [ObjectT] and
    [SymbolT] are [RecordT("Object")] / [RecordT("Symbol")]
    (ty/package.scala:62, 94).  Both names are ROOTS of the exported type
    hierarchy [VF: neither appears on the left of [record_parent]], so
    [RecordTy.contains]'s lca branch cannot fire for an unrelated record
    and the containment test is exactly the subtype test.  A list or a map
    is neither, so it falls through to "SpecType" like every other
    non-Object, non-Symbol value. *)
Definition typeof_obj (o : obj) : string :=
  match o with
  | ORecord tn _ =>
      if record_subtype tn "Object" then "Object"
      else if record_subtype tn "Symbol" then "Symbol"
      else "SpecType"
  | _ => "SpecType"
  end.

(** Keys of a record/map as a list of values (EKeys, state/Obj.scala:88-99).
    The [intSorted] variant filters and numerically sorts map keys; that
    path is not modelled (the caller raises UB). *)
Definition obj_keys (o : obj) : option (list val) :=
  match o with
  | ORecord _ fs => Some (List.map (fun p => VStr (cu (fst p))) fs)
  | OMap es => Some (List.map fst es)
  | OList _ => None
  end.

(** JVM-faithful integer-key classification for [EKeys(_, true)].

    Scala first parses and re-renders each string key, then accepts it only
    when the rendered spelling is unchanged and [d == d.toLong].  Parsing,
    formatting and the checked JVM conversion are typed host queries; the
    filtering and stable signed-rank sort remain in the Rocq semantics. *)
Definition classify_integer_key
  (hosts : list host_cache_entry) (key : val)
  : option (option (Z * val)) :=
  match key with
  | VStr spelling =>
      match typed_host_cache_lookup (HQStrToNumber spelling) hosts with
      | Some (VNumber number) =>
          match typed_host_cache_lookup (HQToStr (VNumber number) 10) hosts with
          | Some (VStr rendered) =>
              if cstr_eqb rendered spelling
              then
                match typed_host_cache_lookup
                        (HQDoubleToLongChecked number) hosts with
                | Some (VMath rank) => Some (Some (rank, key))
                | Some VUndef => Some None
                | _ => None
                end
              else Some None
          | _ => None
          end
      | _ => None
      end
  | _ => Some None
  end.

Fixpoint classify_integer_keys
  (hosts : list host_cache_entry) (entries : list (val * val))
  : option (list (Z * val)) :=
  match entries with
  | nil => Some nil
  | (key, _) :: rest =>
      match classify_integer_key hosts key,
            classify_integer_keys hosts rest with
      | Some None, Some classified => Some classified
      | Some (Some ranked), Some classified => Some (ranked :: classified)
      | _, _ => None
      end
  end.

Fixpoint insert_ranked_key
  (ranked : Z * val) (sorted : list (Z * val)) : list (Z * val) :=
  match sorted with
  | nil => ranked :: nil
  | current :: rest =>
      if (fst ranked <=? fst current)%Z
      then ranked :: sorted
      else current :: insert_ranked_key ranked rest
  end.

Fixpoint stable_sort_ranked_keys
  (ranked : list (Z * val)) : list (Z * val) :=
  match ranked with
  | nil => nil
  | current :: rest =>
      insert_ranked_key current (stable_sort_ranked_keys rest)
  end.

Definition obj_integer_sorted_keys
  (hosts : list host_cache_entry) (entries : list (val * val))
  : option (list val) :=
  option_map
    (fun ranked => List.map snd (stable_sort_ranked_keys ranked))
    (classify_integer_keys hosts entries).

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
  (* Fewer arguments than parameters: ESMeta leaves the remaining
     parameters UNBOUND and stops binding (Interpreter.scala:377-381) —
     for an optional parameter deliberately, and for a required one only
     because the `RemainingParams(ps)` on line 381 is constructed and never
     thrown.  Either way nothing further is bound, and reading one of them
     is then UnknownVar, i.e. UB here. *)
  | _ :: _, nil => Some nil
  (* More arguments than parameters: RemainingArgs is thrown for a
     non-continuation callee (Interpreter.scala:382-386). *)
  | nil, _ :: _ => None
  end.

(** Continuation calls are the one ESMeta call form that discards surplus
    arguments (Interpreter.scala:382-386).  Missing parameters remain
    unbound, as for ordinary calls. *)
Fixpoint init_cont_env (params : list string) (args : list val) : env :=
  match params, args with
  | p :: ps, a :: aas => (LName p, a) :: init_cont_env ps aas
  | _, _ => nil
  end.

Definition captured_env (cs : list (string * val)) : env :=
  List.map (fun '(x, v) => (LName x, v)) cs.

(** ESMeta enters a closure with [getLocals(...) ++ captured].  Scala Map
    [++] applies the right-hand map last, so captured bindings override
    parameters and duplicate captured names collapse with their last value. *)
Definition merge_captured_env (ρ : env) (cs : list (string * val)) : env :=
  List.fold_left
    (fun acc '(x, v) => env_update (LName x) v acc)
    cs ρ.
