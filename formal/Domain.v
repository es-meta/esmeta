(** * ESMetaFV.Domain — pure semantic domain of IR-Core

    Everything here is framework-agnostic (Coq stdlib only): completions,
    pure operator evaluation, local environments, heap objects, and the
    pure helpers shared by the ITree denotation ([Semantics.v]) and the
    executable reference interpreter ([Exec.v]).

    Fidelity notes are attached to each definition; the authoritative
    catalogue is the header of [Semantics.v] and the research log. *)

From Stdlib Require Import String ZArith List Bool Floats Uint63.
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
  | UBNot, VMath z => Some (VMath (Z.lnot z))
  | _, _ => None
  end.

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

(** Structural equality on parse trees (needed by [val_eqb]). *)
Fixpoint ast_eqb (a1 a2 : ast) {struct a1} : bool :=
  match a1, a2 with
  (* [src] is derived from the other fields, and ESMeta compares case-class
     fields only, so it takes no part in equality. *)
  | ALex n1 s1 _, ALex n2 s2 _ => andb (String.eqb n1 n2) (String.eqb s1 s2)
  | ASyn n1 g1 r1 b1 c1 _, ASyn n2 g2 r2 b2 c2 _ =>
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
  match a with ASyn n _ _ _ _ _ => n | ALex n _ _ => n end.

(** Printed source text (ESourceText, Interpreter.scala:227-230):
    exporter-precomputed, see Fragment.v. *)
Definition ast_src (a : ast) : cstr :=
  match a with ASyn _ _ _ _ _ s => s | ALex _ _ s => s end.

Definition ast_children (a : ast) : list (option ast) :=
  match a with ASyn _ _ _ _ cs _ => cs | ALex _ _ _ => nil end.

(** Production chains (Ast.scala:38-44): the node itself, then, while a
    node has exactly one present child, that child — the fall-through used
    by SDO lookup.  Fuel is the tree size, which bounds the chain length,
    because the single-present-child projection is not a structural
    subterm the guard checker can follow. *)

Fixpoint ast_size (a : ast) : nat :=
  match a with
  | ALex _ _ _ => 1
  | ASyn _ _ _ _ cs _ =>
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
  | ASyn n _ r b _ _ =>
      (n ++ "[" ++ nat_str r ++ "," ++ nat_str b ++ "]." ++ m)%string
  | ALex n _ _ => (n ++ "[0,0]." ++ m)%string
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
  | VStr c1, VStr c2 =>
      (fix zs (l1 l2 : cstr) : bool :=
         match l1, l2 with
         | nil, nil => true
         | a :: t1, b :: t2 => andb (Z.eqb a b) (zs t1 t2)
         | _, _ => false
         end) c1 c2
  (* case-class equality on Number is doubleEquals (see above); MapObj keys
     are compared with Scala == , so this is the right notion there. *)
  | VNumber f1, VNumber f2 => num_struct_eqb f1 f2
  | VBigInt z1, VBigInt z2 => Z.eqb z1 z2
  | VInfinity p1, VInfinity p2 => Bool.eqb p1 p2
  | VCodeUnit c1, VCodeUnit c2 => Z.eqb c1 c2
  | VGrammarSymbol n1 p1, VGrammarSymbol n2 p2 =>
      andb (String.eqb n1 n2)
        ((fix bs (l1 l2 : list bool) : bool :=
            match l1, l2 with
            | nil, nil => true
            | x :: t1, y :: t2 => andb (Bool.eqb x y) (bs t1 t2)
            | _, _ => false
            end) p1 p2)
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

(** ** Strict binary operators (Interpreter.scala:566-666)

    [BAnd]/[BOr] on booleans are handled (short-circuit) by the
    interpreters and do not reach here.  [None] means ESMeta throws
    [InvalidBinaryOp], i.e. UB in this model — including the cases ESMeta
    itself leaves out (e.g. INF * 0). *)

(** Exact [Z] to double, used by [CToNumber] on Math.  Restricted to the
    range where doubles are exact; beyond it ESMeta's [BigDecimal.toDouble]
    rounds, and we do not model that rounding (limitation, not a guess). *)
Definition float_of_Z (z : Z) : option float :=
  if (Z.abs z <=? 9007199254740992)%Z    (* 2^53 *)
  then
    let m := PrimFloat.of_uint63 (Uint63.of_Z (Z.abs z)) in
    Some (if (z <? 0)%Z then PrimFloat.opp m else m)
  else None.

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
      if (0 <=? z2)%Z then Some (VMath (Z.pow z1 z2)) else None
  | BBAnd, VMath z1, VMath z2 => Some (VMath (Z.land z1 z2))
  | BBOr, VMath z1, VMath z2 => Some (VMath (Z.lor z1 z2))
  | BBXOr, VMath z1, VMath z2 => Some (VMath (Z.lxor z1 z2))
  | BLShift, VMath z1, VMath z2 => Some (VMath (Z.shiftl z1 z2))
  | BRShift, VMath z1, VMath z2 => Some (VMath (Z.shiftr z1 z2))

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

  (* --- structural equality (Interpreter.scala:638-640) --- *)
  (* AstValue-vs-AstValue is REFERENCE equality (`l eq r`) in ESMeta; the
     model carries no node identity, so that pair is UB rather than
     silently structural (Open Question for the AST exporter, G4/G5).
     The test is written as a nested match on the SECOND operand so that
     `BEq v VNull` still reduces when `v` is abstract — matching on the
     first operand would block every proof that compares an unknown value
     against a literal. *)
  | BEq, _, _ =>
      match v2 with
      | VAst _ =>
          match v1 with
          | VAst _ => None
          | _ => Some (VBool (val_eqb v1 v2))
          end
      | _ => Some (VBool (val_eqb v1 v2))
      end

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
  | BLShift, VBigInt z1, VBigInt z2 => Some (VBigInt (Z.shiftl z1 z2))
  | BRShift, VBigInt z1, VBigInt z2 => Some (VBigInt (Z.shiftr z1 z2))
  | BPow, VBigInt z1, VBigInt z2 =>
      if (0 <=? z2)%Z then Some (VBigInt (Z.pow z1 z2)) else None

  | _, _, _ => None
  end.

(** ** Conversions ([EConvert], Interpreter.scala:263-289)

    The string<->number cases go through [ESValueParser.str2number] and
    [toStringHelper], which are Scala implementations, not IR; they are UB
    here.  [EToStr] is likewise UB (limitation L-11). *)
Definition eval_cop (op : cop) (v : val) : option val :=
  match op, v with
  | CToMath, VCodeUnit c => Some (VMath c)
  | CToCodeUnit, VMath z => Some (VCodeUnit (Z.modulo z 65536))  (* n.toChar *)
  | CToNumber, VInfinity true => Some (VNumber PrimFloat.infinity)
  | CToNumber, VInfinity false => Some (VNumber PrimFloat.neg_infinity)
  | CToApproxNumber, VMath z => option_map VNumber (float_of_Z z)
  | CToNumber, VMath z => option_map VNumber (float_of_Z z)
  | CToBigInt, VMath z => Some (VBigInt z)
  | CToMath, VMath z => Some (VMath z)
  | CToNumber, VNumber f => Some (VNumber f)
  | CToMath, VBigInt z => Some (VMath z)
  | _, _ => None
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
  | VAst a, VGrammarSymbol nm _ =>
      match a with
      | ASyn _ _ _ _ _ _ => if String.eqb nm "" then VBool true
                            else VBool (String.eqb (ast_name a) nm)
      | ALex _ _ _ => VBool (String.eqb (ast_name a) nm)
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
  | VNumber _, TNumberTy => true
  | VBigInt _, TBigIntTy => true
  | VCodeUnit _, TCodeUnitTy => true
  | VInfinity _, TInfinityTy => true
  | VAst _, TAstTy => true
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
  | VNumber _ => Some "Number"
  | VBigInt _ => Some "BigInt"
  | VStr _ => Some "String"
  | VBool _ => Some "Boolean"
  | VUndef => Some "Undefined"
  | VNull => Some "Null"
  (* not contained in ObjectT or SymbolT, so ESMeta answers "SpecType" *)
  | VMath _ | VEnum _ | VClo _ _ | VAst _ | VInfinity _ | VCodeUnit _
  | VGrammarSymbol _ _ => Some "SpecType"
  | VAddr _ => None
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
