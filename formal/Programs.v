(** * ESMetaFV.Programs — IR-Core mirrors of ESMeta test programs

    Hand-mirrored from the standalone IR corpus (repository facts):
    - [sum_prog]  ← tests/ir/sum.ir
    - [gcd_prog]  ← tests/ir/gcd.ir
    - [fibo_prog] ← tests/ir/fibo.ir
    - [print2_prog] is a new two-print program exercising the observable
      effect (no tests/ir counterpart uses IPrint).

    Stdlib-only (no CRIS): shared by the ITree packaging ([Examples.v])
    and the executable validation ([Validation.v]).  Milestone 3's Scala
    exporter generates the same shape mechanically; these hand mirrors
    stay as the reviewed reference copies. *)

From Stdlib Require Import String ZArith List Floats.
From ESMetaFV Require Import Fragment.

Local Open Scope string_scope.
Local Open Scope Z_scope.

(** Readability helpers *)
Definition lref (x : string) : expr := ERef (RVar (VLocal (LName x))).
Definition tref (n : nat) : expr := ERef (RVar (VLocal (LTemp n))).
Definition gref (x : string) : expr := ERef (RVar (VGlobal x)).
Definition lassign (x : string) (e : expr) : inst :=
  IAssign (RVar (VLocal (LName x))) e.
Definition tassign (n : nat) (e : expr) : inst :=
  IAssign (RVar (VLocal (LTemp n))) e.
Definition gassign (x : string) (e : expr) : inst :=
  IAssign (RVar (VGlobal x)) e.

(** ** tests/ir/sum.ir

<<
@main def main() = {
  let sum = 0
  let i = 0
  while (< i 11) { sum = (+ sum i)  i = (+ i 1) }
  assert (= i 11)
  assert (= sum 55)
}
>> *)

Definition sum_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "sum" (EMath 0) ::
     ILet "i" (EMath 0) ::
     IWhile (EBinary BLt (lref "i") (EMath 11))
       (ISeq (lassign "sum" (EBinary BAdd (lref "sum") (lref "i")) ::
              lassign "i" (EBinary BAdd (lref "i") (EMath 1)) :: nil)) ::
     IAssert (EBinary BEq (lref "i") (EMath 11)) ::
     IAssert (EBinary BEq (lref "sum") (EMath 55)) :: nil)).

Definition sum_prog : prog := mkProg (sum_main :: nil).

(** The top-level return value is ESMeta's observable [RESULT].  This locks
    the distinction between an explicitly returned value and a main
    function that falls through with [VUndef]. *)
Definition main_return_main : func :=
  mkFunc true "main" nil (IReturn (EMath 7)).

Definition main_return_prog : prog :=
  mkProg (main_return_main :: nil).

(** ESMeta appends a closure's captured bindings after parameter binding in
    its mutable map, so a captured name overrides a same-named parameter. *)
Definition captured_param_fun : func :=
  mkFunc false "captured_param" ("x" :: nil)
    (IReturn (lref "x")).

Definition captured_param_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "x" (EMath 99) ::
     ICall (LName "answer") (EClo "captured_param" ("x" :: nil))
       (EMath 1 :: nil) ::
     IAssert (EBinary BEq (lref "answer") (EMath 99)) :: nil)).

Definition captured_param_prog : prog :=
  mkProg (captured_param_fun :: captured_param_main :: nil).

(** ** tests/ir/gcd.ir

<<
def gcd(a: Math, b: Math) = {
  if (= a 0) %0 = b
  else if (= b 0) %0 = a
  else if (< b a) call %0 = clo<"gcd">((- a b), b)
  else call %0 = clo<"gcd">(a, (- b a))
  return %0
}

@main def main() = {
  call g = clo<"gcd">(42, 224)
  assert (= g 14)
}
>> *)

Definition gcd_fun : func :=
  mkFunc false "gcd" ("a" :: "b" :: nil) (ISeq
    (IIf (EBinary BEq (lref "a") (EMath 0))
         (tassign 0 (lref "b"))
         (IIf (EBinary BEq (lref "b") (EMath 0))
              (tassign 0 (lref "a"))
              (IIf (EBinary BLt (lref "b") (lref "a"))
                   (ICall (LTemp 0) (EClo "gcd" nil)
                      (EBinary BSub (lref "a") (lref "b") :: lref "b" :: nil))
                   (ICall (LTemp 0) (EClo "gcd" nil)
                      (lref "a" :: EBinary BSub (lref "b") (lref "a") :: nil)))) ::
     IReturn (tref 0) :: nil)).

Definition gcd_main : func :=
  mkFunc true "main" nil (ISeq
    (ICall (LName "g") (EClo "gcd" nil) (EMath 42 :: EMath 224 :: nil) ::
     IAssert (EBinary BEq (lref "g") (EMath 14)) :: nil)).

Definition gcd_prog : prog := mkProg (gcd_fun :: gcd_main :: nil).

(** ** tests/ir/fibo.ir

<<
def fibo(n: Math) = {
  if (< n 2) %0 = n
  else {
    let n1 = (- n 1)
    let n2 = (- n 2)
    call f1 = clo<"fibo">(n1)
    call f2 = clo<"fibo">(n2)
    %0 = (+ f1 f2)
  }
  return %0
}

@main def main() = {
  call f9 = clo<"fibo">(9)
  assert (= f9 34)
}
>> *)

Definition fibo_fun : func :=
  mkFunc false "fibo" ("n" :: nil) (ISeq
    (IIf (EBinary BLt (lref "n") (EMath 2))
         (tassign 0 (lref "n"))
         (ISeq
           (ILet "n1" (EBinary BSub (lref "n") (EMath 1)) ::
            ILet "n2" (EBinary BSub (lref "n") (EMath 2)) ::
            ICall (LName "f1") (EClo "fibo" nil) (lref "n1" :: nil) ::
            ICall (LName "f2") (EClo "fibo" nil) (lref "n2" :: nil) ::
            tassign 0 (EBinary BAdd (lref "f1") (lref "f2")) :: nil)) ::
     IReturn (tref 0) :: nil)).

Definition fibo_main : func :=
  mkFunc true "main" nil (ISeq
    (ICall (LName "f9") (EClo "fibo" nil) (EMath 9 :: nil) ::
     IAssert (EBinary BEq (lref "f9") (EMath 34)) :: nil)).

Definition fibo_prog : prog := mkProg (fibo_fun :: fibo_main :: nil).

(** ** A program with observable effects *)

Definition print2_main : func :=
  mkFunc true "main" nil (ISeq
    (IPrint (EMath 1) :: IPrint (EMath 2) :: nil)).

Definition print2_prog : prog := mkProg (print2_main :: nil).

(** ** T-3: spec-shaped optional access (ADR-10)

    The ECMAScript specification defines optional chaining as *already
    guarded* code (ecma262 @ 84b38ad8, sec-optional-chaining-evaluation):

      OptionalExpression : MemberExpression OptionalChain
        1. Let baseReference be ? Evaluation of MemberExpression.
        2. Let baseValue be ? GetValue(baseReference).
        3. If baseValue is either undefined or null, then
           a. Return undefined.
        4. Return ? ChainEvaluation of OptionalChain with arguments
           baseValue and baseReference.
      OptionalChain : `?.` IdentifierName
        1. Return EvaluatePropertyAccessWithIdentifierKey(baseValue, …).

    ESMeta's *compiled* IR for that production has the same shape — the
    guard is real code, not just prose (see the quote in ADR-10, from
    `logs/dump/debugger/funcs.json`, `OptionalExpression[0,0].Evaluation`):
    one receiver evaluation, `if (|| (= baseValue undefined)
    (= baseValue null)) return NormalCompletion(undefined)`, then
    `ChainEvaluation`.

    The program below models that CONTROL SHAPE in mirrored IR-Core, with
    the receiver an effectful call to a context-supplied function — the
    case in which "evaluated exactly once" is genuinely OBSERVABLE (each
    call is an event at the linking boundary).

    No synthetic construct is involved: every constructor here mirrors
    real ESMeta IR, so both this program and its transformation are
    executable by ESMeta and exportable by the differential harness.

    BOUNDARY: this is a model of the control shape only — no Reference
    Records/GetValue (hence no getters), no prototype chain, no ToObject
    coercion (so `(42)?.foo` is UB here but `undefined` in JS), no abrupt
    completions.  It is NOT proven to be ECMAScript `?.`; see the "WHAT
    IS NOT ESTABLISHED" section of attic/T3Proof.v and limitation L-8. *)

(* Test order matches the compiled IR quoted above: undefined first,
   then null.  Both operands are pure equality tests on an
   already-computed value, so the order is unobservable; we match it
   anyway to keep the correspondence syntactically visible. *)
Definition t3_guard (v : expr) : expr :=
  EBinary BOr (EBinary BEq v EUndef) (EBinary BEq v ENull).

(** `x = f()?.prop`, spec-shaped; "f" is supplied by the linking context. *)
Definition t3_optaccess_main : func :=
  mkFunc true "main" nil (ISeq
    (ICall (LName "t") (EClo "f" nil) nil ::
     IIf (t3_guard (lref "t"))
         (ILet "x" EUndef)
         (ILet "x" (ERef (RField (RVar (VLocal (LName "t"))) (EStr (cu "prop"))))) ::
     IPrint (lref "x") :: nil)).

Definition t3ex_src : prog := mkProg (t3_optaccess_main :: nil).

(** *** Closed variants for executable validation

    A local receiver function that PRINTS before returning a record, so
    that re-evaluating the receiver is observable. *)

Definition t3v_f : func :=
  mkFunc false "f" nil (ISeq
    (IPrint (EMath 7) ::
     ILet "o" (ERecord "R" (("prop", EMath 42) :: nil)) ::
     IReturn (lref "o") :: nil)).

Definition t3v_src : prog := mkProg (t3v_f :: t3_optaccess_main :: nil).

(** A nullish-returning receiver: the guard must skip the property access. *)
Definition t3v_fnull : func :=
  mkFunc false "f" nil (ISeq
    (IPrint (EMath 7) :: IReturn ENull :: nil)).

Definition t3v_null : prog := mkProg (t3v_fnull :: t3_optaccess_main :: nil).

(** WRONG transformation: the receiver is re-evaluated inside the
    non-nullish branch, so the context call happens twice.  IR-Core has
    no call expressions, so re-evaluation is a second [ICall]. *)
Definition t3_reeval_main : func :=
  mkFunc true "main" nil (ISeq
    (ICall (LName "t") (EClo "f" nil) nil ::
     IIf (t3_guard (lref "t"))
         (ILet "x" EUndef)
         (ISeq (ICall (LName "t2") (EClo "f" nil) nil ::
                ILet "x" (ERef (RField (RVar (VLocal (LName "t2")))
                                  (EStr (cu "prop")))) :: nil)) ::
     IPrint (lref "x") :: nil)).

Definition t3v_reeval : prog := mkProg (t3v_f :: t3_reeval_main :: nil).

(** ** First-class continuation control smoke test

    This program exercises the properties that cannot be validated by the
    direct recursive interpreter:

    - two continuations captured from the same locals/stack compare equal;
    - a later capture sees newly bound named locals and compares unequal;
    - invoking a continuation discards its immediate caller;
    - surplus continuation arguments are ignored;
    - the saved caller frame is restored, so the target's return becomes
      the result of the original ordinary call.

    [cont_maker] captures the caller stack created by main's ordinary call,
    then jumps to [cont_target].  The instructions after that jump are
    intentionally unreachable. *)

Definition cont_target : func :=
  mkFunc false "cont_target" ("p" :: nil) (ISeq
    (IAssert (EBinary BEq (lref "p") (EMath 99)) ::
     IAssert (EBinary BEq (lref "x") (EMath 0)) ::
     IReturn (EMath 42) :: nil)).

Definition cont_maker : func :=
  mkFunc false "cont_maker" nil (ISeq
    (ILet "x" (EMath 0) ::
     ILet "k" (ECont "cont_target") ::
     lassign "x" (EMath 1) ::
     ICall (LName "unreachable") (lref "k")
       (EMath 99 :: EMath 100 :: nil) ::
     IReturn (EMath 0) :: nil)).

Definition cont_main : func :=
  mkFunc true "main" nil (ISeq
    (IAssert
       (EBinary BEq (ECont "cont_target") (ECont "cont_target")) ::
     ILet "k1" (ECont "cont_target") ::
     ILet "k2" (ECont "cont_target") ::
     IAssert
       (EUnary UNot (EBinary BEq (lref "k1") (lref "k2"))) ::
     ICall (LName "answer") (EClo "cont_maker" nil) nil ::
     IAssert (EBinary BEq (lref "answer") (EMath 42)) ::
     IPrint (lref "answer") :: nil)).

Definition cont_prog : prog :=
  mkProg (cont_target :: cont_maker :: cont_main :: nil).

(** A saved continuation whose caller has already returned reaches an
    ESMeta [CallContext] that may have advanced mutable locals/cursor state.
    The structured ITree executor cannot represent that invocation-time
    mutation, so it preserves the frame as poison and rejects an actual
    normal return into it rather than replaying its capture-time closure. *)
Definition stale_cont_target : func :=
  mkFunc false "stale_cont_target" ("p" :: nil)
    (IReturn (lref "p")).

Definition stale_cont_capture : func :=
  mkFunc false "stale_cont_capture" nil
    (IReturn (ECont "stale_cont_target")).

Definition stale_cont_main : func :=
  mkFunc true "main" nil (ISeq
    (ICall (LName "saved") (EClo "stale_cont_capture" nil) nil ::
     ICall (LName "answer") (lref "saved") (EMath 7 :: nil) ::
     IPrint (lref "answer") :: nil)).

Definition stale_cont_prog : prog :=
  mkProg
    (stale_cont_target :: stale_cont_capture :: stale_cont_main :: nil).

(** A stale caller is harmless when the resumed continuation never returns
    into it.  This is the control shape used by generators: the body jumps
    to a separately captured return continuation, discarding the poisoned
    saved chain.  The stale frame must therefore be rejected lazily at
    restoration, not eagerly at continuation invocation. *)
Definition poison_escape_target : func :=
  mkFunc false "poison_escape_target" ("p" :: nil)
    (IReturn (lref "p")).

Definition poison_identity : func :=
  mkFunc false "poison_identity" ("p" :: nil)
    (IReturn (lref "p")).

Definition poison_stale_target : func :=
  mkFunc false "poison_stale_target" ("p" :: nil) (ISeq
    (ICall (LName "q") (EClo "poison_identity" nil)
       (lref "p" :: nil) ::
     ICall (LName "unreachable") (gref "POISON_ESCAPE")
       (lref "q" :: nil) ::
     IReturn (EMath 0) :: nil)).

Definition poison_cont_capture : func :=
  mkFunc false "poison_cont_capture" nil
    (IReturn (ECont "poison_stale_target")).

Definition poison_cont_maker : func :=
  mkFunc false "poison_cont_maker" nil (ISeq
    (gassign "POISON_ESCAPE" (ECont "poison_escape_target") ::
     ICall (LName "saved") (EClo "poison_cont_capture" nil) nil ::
     gassign "POISON_STALE" (lref "saved") ::
     ICall (LName "unreachable") (lref "saved") (EMath 1 :: nil) ::
     IReturn (EMath 0) :: nil)).

Definition poison_cont_main : func :=
  mkFunc true "main" nil (ISeq
    (ICall (LName "answer") (EClo "poison_cont_maker" nil) nil ::
     IIf (gref "POISON_REPEATED")
       (IPrint (lref "answer"))
       (ISeq
         (gassign "POISON_REPEATED" (EBool true) ::
          ICall (LName "unreachable")
            (gref "POISON_STALE") (EMath 2 :: nil) ::
          IReturn (EMath 0) :: nil)) :: nil)).

Definition poison_cont_prog : prog :=
  mkProgFull
    (poison_escape_target :: poison_identity :: poison_stale_target ::
     poison_cont_capture :: poison_cont_maker :: poison_cont_main :: nil)
    None None nil
    (("POISON_ESCAPE", VUndef) ::
     ("POISON_STALE", VUndef) ::
     ("POISON_REPEATED", VBool false) :: nil)
    nil.

(** Invoking a still-live continuation clones its frame chain.  The first
    jump abandons [repeat_cont_maker], returns [1] to the original main
    call, and the second jump uses the same captured stack again.  A global
    guard makes the restarted caller print [2] and terminate. *)
Definition repeat_cont_target : func :=
  mkFunc false "repeat_cont_target" ("p" :: nil)
    (IReturn (lref "p")).

Definition repeat_cont_maker : func :=
  mkFunc false "repeat_cont_maker" nil (ISeq
    (ILet "k" (ECont "repeat_cont_target") ::
     gassign "SAVED_CONT" (lref "k") ::
     ICall (LName "unreachable") (lref "k") (EMath 1 :: nil) ::
     IReturn (EMath 0) :: nil)).

Definition repeat_cont_main : func :=
  mkFunc true "main" nil (ISeq
    (ICall (LName "answer") (EClo "repeat_cont_maker" nil) nil ::
     IIf (gref "CONT_REPEATED")
       (IPrint (lref "answer"))
       (ISeq
         (gassign "CONT_REPEATED" (EBool true) ::
          ICall (LName "unreachable")
            (gref "SAVED_CONT") (EMath 2 :: nil) ::
          IReturn (EMath 0) :: nil)) :: nil)).

Definition repeat_cont_prog : prog :=
  mkProgFull
    (repeat_cont_target :: repeat_cont_maker :: repeat_cont_main :: nil)
    None None nil
    (("CONT_REPEATED", VBool false) :: nil)
    nil.

(** Exporter-provided grammar field names drive named AST access.  This
    tiny closed program locks the data boundary independently of the
    generated ECMAScript grammar/specification. *)
Definition named_ast_child : ast :=
  ALex "IdentifierName" "x" (cu "x") (cu "x") nil.

Definition named_ast_root : ast :=
  ASyn "CallMemberExpression" nil 0 0
    (Some named_ast_child :: nil)
    ("MemberExpression" :: nil)
    (cu "root") (cu "root").

Definition named_ast_main : func :=
  mkFunc true "main" nil (ISeq
    (IAssert
       (EExists
         (RField (RVar (VGlobal "AST"))
           (EStr (cu "MemberExpression")))) ::
     IAssert
       (EExists (RField (RVar (VGlobal "AST")) (EMath 0))) ::
     IAssert
       (EUnary UNot
         (EExists (RField (RVar (VGlobal "AST")) (EMath 1)))) ::
     ILet "child"
       (ERef
         (RField (RVar (VGlobal "AST"))
           (EStr (cu "MemberExpression")))) ::
     IAssert
       (EBinary BEq
         (ESourceText (lref "child"))
         (EStr (cu "x"))) :: nil)).

Definition named_ast_prog : prog :=
  mkProgFull
    (named_ast_main :: nil)
    None None nil
    (("AST", VAst (AstExported 1) named_ast_root nil) :: nil)
    nil.

Definition ast_parent_exists_main : func :=
  mkFunc true "main" nil (ISeq
    (IAssert
       (EUnary UNot
         (EExists
           (RField (RVar (VGlobal "ROOT")) (EStr (cu "parent"))))) ::
     IAssert
       (EExists
         (RField (RVar (VGlobal "AST")) (EStr (cu "parent")))) ::
     ILet "parent"
       (ERef (RField (RVar (VGlobal "AST")) (EStr (cu "parent")))) ::
     IAssert
       (EBinary BEq
         (ESourceText (lref "parent"))
         (EStr (cu "root"))) :: nil)).

Definition ast_parent_exists_prog : prog :=
  mkProgFull
    (ast_parent_exists_main :: nil)
    None None nil
    (("ROOT", VAst (AstExported 1) named_ast_root nil) ::
     ("AST", VAst (AstExported 1) named_ast_root (0%nat :: nil)) :: nil)
    nil.

(** SDO fall-through must keep the descendant's cursor path.  The selected
    lexical receiver can therefore still observe its parsed parent. *)
Definition cursor_sdo_probe : func :=
  mkFunc false "IdentifierName[0,0].Probe" ("node" :: nil) (ISeq
    (IAssert
       (EExists
         (RField (RVar (VLocal (LName "node"))) (EStr (cu "parent")))) ::
     ILet "parent"
       (ERef
         (RField (RVar (VLocal (LName "node"))) (EStr (cu "parent")))) ::
     IAssert
       (EBinary BEq
         (ESourceText (lref "parent"))
         (EStr (cu "root"))) ::
     IReturn (EMath 7) :: nil)).

Definition cursor_sdo_main : func :=
  mkFunc true "main" nil (ISeq
    (ISdoCall (LName "answer") (ERef (RVar (VGlobal "AST"))) "Probe" nil ::
     IAssert (EBinary BEq (lref "answer") (EMath 7)) :: nil)).

Definition cursor_sdo_prog : prog :=
  mkProgFull
    (cursor_sdo_probe :: cursor_sdo_main :: nil)
    None None nil
    (("AST", VAst (AstExported 1) named_ast_root nil) :: nil)
    nil.

(** ESMeta's AST equality is reference identity. *)
Definition ast_eq_main : func :=
  mkFunc true "main" nil
    (IAssert
      (EBinary BEq
        (ERef (RVar (VGlobal "AST")))
        (ERef (RVar (VGlobal "AST"))))).

Definition ast_eq_prog : prog :=
  mkProgFull
    (ast_eq_main :: nil)
    None None nil
    (("AST", VAst (AstExported 1) named_ast_root nil) :: nil)
    nil.

(** Structurally identical trees with distinct exported origins are not
    the same AST reference. *)
Definition ast_distinct_roots_main : func :=
  mkFunc true "main" nil
    (IAssert
      (EUnary UNot
        (EBinary BEq
          (ERef (RVar (VGlobal "LEFT")))
          (ERef (RVar (VGlobal "RIGHT")))))).

Definition ast_distinct_roots_prog : prog :=
  mkProgFull
    (ast_distinct_roots_main :: nil)
    None None nil
    (("LEFT", VAst (AstExported 1) named_ast_root nil) ::
     ("RIGHT", VAst (AstExported 2) named_ast_root nil) :: nil)
    nil.

(** Child-parent-child navigation reconstructs the same cursor reference. *)
Definition ast_cursor_roundtrip_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "child"
      (ERef
        (RField (RVar (VGlobal "ROOT"))
          (EStr (cu "MemberExpression")))) ::
     ILet "parent"
      (ERef
        (RField (RVar (VLocal (LName "child")))
          (EStr (cu "parent")))) ::
     ILet "roundtrip"
      (ERef
        (RField (RVar (VLocal (LName "parent")))
          (EStr (cu "MemberExpression")))) ::
     IAssert (EBinary BEq (lref "child") (lref "roundtrip")) :: nil)).

Definition ast_cursor_roundtrip_prog : prog :=
  mkProgFull
    (ast_cursor_roundtrip_main :: nil)
    None None nil
    (("ROOT", VAst (AstExported 1) named_ast_root nil) :: nil)
    nil.

(** Integer-valued Binary64 numbers cross exactly into the integer-only
    mathematical-value fragment; no rounding is permitted. *)
Definition number_to_math_main : func :=
  mkFunc true "main" nil (ISeq
    (IAssert
       (EBinary BEq
         (EConvert CToMath (ENumber (1.0000000000000000)%float))
         (EMath 1)) ::
     IAssert
       (EBinary BEq
         (EConvert CToMath (ENumber (-0.0000000000000000)%float))
         (EMath 0)) ::
     IAssert
       (EBinary BEq
         (EUnary UNeg (ENumber (1.0000000000000000)%float))
         (ENumber (-1.0000000000000000)%float)) ::
     IAssert
       (EBinary BEq
         (EUnary UNeg (ENumber (0.0000000000000000)%float))
         (ENumber (-0.0000000000000000)%float)) ::
     IAssert
       (EBinary BEq
         (EUnary UNeg (ENumber (-0.0000000000000000)%float))
         (ENumber (0.0000000000000000)%float)) ::
     IAssert
       (EBinary BEq
         (EUnary UNeg (EInfinity true))
         (EInfinity false)) ::
     IAssert
       (EBinary BEq
         (EUnary UNeg (EBigInt 7))
         (EBigInt (-7))) :: nil)).

Definition number_to_math_prog : prog :=
  mkProg (number_to_math_main :: nil).

(** Trusted host-operation boundary smoke test.  Existing strings must not
    evaluate the radix expression; numeric formatting, String-to-Number,
    String-to-BigInt, and JVM Number exponentiation succeed only through
    exact typed queries. *)
Definition host_tostr_main : func :=
  mkFunc true "main" nil (ISeq
    (IAssert
       (EBinary BEq
         (EToStr (EStr (cu "ready"))
           (Some (EYet "string radix must stay unevaluated")))
         (EStr (cu "ready"))) ::
     IAssert
       (EBinary BEq
         (EToStr (ENumber (255.00000000000000)%float)
           (Some (EMath 16)))
         (EStr (cu "ff"))) ::
     IAssert
       (EBinary BEq
         (EToStr (EBigInt (-42)) None)
         (EStr (cu "-42"))) ::
     IAssert
       (EBinary BEq
         (EConvert CToNumber (EStr (cu "42")))
         (ENumber (42.000000000000000)%float)) ::
     IAssert
       (EBinary BEq
         (EConvert CToNumber (EStr (cu "not-a-number")))
         (ENumber PrimFloat.nan)) ::
     IAssert
       (EBinary BEq
         (EConvert CToNumber (EStr (cu "-0")))
         (ENumber (-0.0000000000000000)%float)) ::
     IAssert
       (EBinary BEq
         (EConvert CToNumber (EStr (cu "Infinity")))
         (ENumber PrimFloat.infinity)) ::
     IAssert
       (EBinary BEq
         (EConvert CToBigInt (EStr (cu "42")))
         (EBigInt 42)) ::
     IAssert
       (EBinary BEq
         (EConvert CToBigInt (EStr (cu "not-a-bigint")))
         EUndef) ::
     IAssert
       (EBinary BEq
         (EBinary BPow
           (ENumber (2.0000000000000000)%float)
           (ENumber (32.000000000000000)%float))
         (ENumber (4294967296.0000000)%float)) :: nil)).

Definition host_tostr_entries : list host_cache_entry :=
  mkHostCacheEntry
    (HQToStr (VNumber (255.00000000000000)%float) 16)
    (VStr (cu "ff")) ::
  mkHostCacheEntry
    (HQToStr (VBigInt (-42)) 10)
    (VStr (cu "-42")) ::
  mkHostCacheEntry
    (HQStrToNumber (cu "42"))
    (VNumber (42.000000000000000)%float) ::
  mkHostCacheEntry
    (HQStrToNumber (cu "not-a-number"))
    (VNumber PrimFloat.nan) ::
  mkHostCacheEntry
    (HQStrToNumber (cu "-0"))
    (VNumber (-0.0000000000000000)%float) ::
  mkHostCacheEntry
    (HQStrToNumber (cu "Infinity"))
    (VNumber PrimFloat.infinity) ::
  mkHostCacheEntry
    (HQStrToBigInt (cu "42"))
    (VBigInt 42) ::
  mkHostCacheEntry
    (HQStrToBigInt (cu "not-a-bigint"))
    VUndef ::
  mkHostCacheEntry
    (HQNumberPow
      (2.0000000000000000)%float
      (32.000000000000000)%float)
    (VNumber (4294967296.0000000)%float) ::
  nil.

Definition host_tostr_prog : prog :=
  mkProgFull
    (host_tostr_main :: nil)
    None None host_tostr_entries nil nil.

(** Deterministic mathematical host-operation smoke test.  The host cache
    supplies the exact mathematical result; conversion to an approximate
    Number happens only after the typed [MathOp] lookup succeeds. *)
Definition math_host_main : func :=
  mkFunc true "main" nil
    (IReturn
      (EConvert CToApproxNumber
        (EMathOp MSqrt (EMath 4 :: nil)))).

Definition math_host_entries : list host_cache_entry :=
  mkHostCacheEntry
    (HQMathOp MSqrt (4 :: nil))
    (VMath 2) ::
  nil.

Definition math_host_prog : prog :=
  mkProgFull
    (math_host_main :: nil)
    None None math_host_entries nil nil.

(** [2^53 + 1] is the first positive integer whose Math-to-Number
    conversion must use ESMeta's rounding host primitive. *)
Definition math_to_number_host_main : func :=
  mkFunc true "main" nil
    (IReturn (EConvert CToNumber (EMath 9007199254740993))).

Definition math_to_number_host_entries : list host_cache_entry :=
  mkHostCacheEntry
    (HQMathToNumber 9007199254740993)
    (VNumber (9007199254740992.0000)%float) ::
  nil.

Definition math_to_number_host_prog : prog :=
  mkProgFull
    (math_to_number_host_main :: nil)
    None None math_to_number_host_entries nil nil.

(** Cache misses and ill-typed host answers are UB, never guessed or
    accepted merely because the query key exists. *)
Definition host_missing_main : func :=
  mkFunc true "main" nil
    (IExpr (EConvert CToNumber (EStr (cu "1")))).

Definition host_missing_prog : prog :=
  mkProg (host_missing_main :: nil).

Definition host_wrong_type_entries : list host_cache_entry :=
  mkHostCacheEntry
    (HQStrToNumber (cu "1"))
    (VStr (cu "not a Number")) ::
  nil.

Definition host_wrong_type_prog : prog :=
  mkProgFull
    (host_missing_main :: nil)
    None None host_wrong_type_entries nil nil.

(** Semantic parser keys use the raw parser text and effective grammar
    parameters, not structural AST equality.  The source below deliberately
    has trimmed [src] = ["raw"] and untrimmed [parse_src] = ["raw "]. *)
Definition host_parse_source_ast : ast :=
  ASyn "Source" (true :: nil) 0 0 nil nil
    (cu "raw") (cu "raw ").

Definition host_parse_result_ast : ast :=
  ALex "Target" "ok" (cu "ok") (cu "ok") nil.

Definition host_parse_entries : list host_cache_entry :=
  mkHostCacheEntry
    (HQParseText (cu "raw ") "Target" (true :: nil))
    (VAst (AstExported 2) host_parse_result_ast nil) ::
  nil.

Definition host_parse_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "parsed"
       (EParse
         (ERef (RVar (VGlobal "AST")))
         (EGrammarSymbol "Target" nil)) ::
     IAssert
       (EBinary BEq
         (ESourceText (lref "parsed"))
         (EStr (cu "ok"))) :: nil)).

Definition host_parse_prog : prog :=
  mkProgFull
    (host_parse_main :: nil)
    None None host_parse_entries
    (("AST", VAst (AstExported 1) host_parse_source_ast nil) :: nil)
    nil.

(** A host-cache entry is a parser oracle result, not a reusable AST
    object: each parser invocation receives a fresh runtime origin. *)
Definition host_parse_fresh_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "left"
       (EParse
         (ERef (RVar (VGlobal "AST")))
         (EGrammarSymbol "Target" nil)) ::
     ILet "right"
       (EParse
         (ERef (RVar (VGlobal "AST")))
         (EGrammarSymbol "Target" nil)) ::
     IAssert
       (EUnary UNot (EBinary BEq (lref "left") (lref "right"))) :: nil)).

Definition host_parse_fresh_prog : prog :=
  mkProgFull
    (host_parse_fresh_main :: nil)
    None None host_parse_entries
    (("AST", VAst (AstExported 1) host_parse_source_ast nil) :: nil)
    nil.

(** The initial Script parse is checked before the general host cache.
    Deliberately provide a conflicting host result to lock that priority. *)
Definition initial_parse_cached_ast : ast :=
  ALex "Script" "cached" (cu "cached") (cu "cached") nil.

Definition initial_parse_host_ast : ast :=
  ALex "Script" "host" (cu "host") (cu "host") nil.

Definition initial_parse_priority_entries : list host_cache_entry :=
  mkHostCacheEntry
    (HQParseText (cu "same") "Script" nil)
    (VAst (AstExported 2) initial_parse_host_ast nil) ::
  nil.

Definition initial_parse_priority_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "parsed"
       (EParse
         (EStr (cu "same"))
         (EGrammarSymbol "Script" nil)) ::
     IAssert
       (EBinary BEq
         (ESourceText (lref "parsed"))
         (EStr (cu "cached"))) :: nil)).

Definition initial_parse_priority_prog : prog :=
  mkProgFull
    (initial_parse_priority_main :: nil)
    (Some (cu "same"))
    (Some initial_parse_cached_ast)
    initial_parse_priority_entries nil nil.

Definition host_parse_wrong_params_entries : list host_cache_entry :=
  mkHostCacheEntry
    (HQParseText (cu "raw ") "Target" (false :: nil))
    (VAst (AstExported 2) host_parse_result_ast nil) ::
  nil.

Definition host_parse_wrong_params_prog : prog :=
  mkProgFull
    (host_parse_main :: nil)
    None None host_parse_wrong_params_entries
    (("AST", VAst (AstExported 1) host_parse_source_ast nil) :: nil)
    nil.

(** ESMeta catches parser rejection and returns a freshly allocated empty
    error list.  [VUndef] is only the typed-cache sentinel; it never escapes
    as the expression result. *)
Definition host_parse_failure_entries : list host_cache_entry :=
  mkHostCacheEntry
    (HQParseText (cu "raw ") "Broken" (true :: nil))
    VUndef ::
  nil.

Definition host_parse_failure_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "errors"
       (EParse
         (ERef (RVar (VGlobal "AST")))
         (EGrammarSymbol "Broken" nil)) ::
     IAssert
       (EBinary BEq (ESizeOf (lref "errors")) (EMath 0)) :: nil)).

Definition host_parse_failure_prog : prog :=
  mkProgFull
    (host_parse_failure_main :: nil)
    None None host_parse_failure_entries
    (("AST", VAst (AstExported 1) host_parse_source_ast nil) :: nil)
    nil.

(** Integer-sorted [EKeys] retains filtering and stable sorting in Rocq;
    only the Scala/JVM primitive conversions are supplied by typed queries. *)
Definition integer_keys_entries : list host_cache_entry :=
  mkHostCacheEntry
    (HQStrToNumber (cu "10"))
    (VNumber (10.000000000000000)%float) ::
  mkHostCacheEntry
    (HQToStr (VNumber (10.000000000000000)%float) 10)
    (VStr (cu "10")) ::
  mkHostCacheEntry
    (HQDoubleToLongChecked (10.000000000000000)%float)
    (VMath 10) ::
  mkHostCacheEntry
    (HQStrToNumber (cu "2"))
    (VNumber (2.0000000000000000)%float) ::
  mkHostCacheEntry
    (HQToStr (VNumber (2.0000000000000000)%float) 10)
    (VStr (cu "2")) ::
  mkHostCacheEntry
    (HQDoubleToLongChecked (2.0000000000000000)%float)
    (VMath 2) ::
  mkHostCacheEntry
    (HQStrToNumber (cu "01"))
    (VNumber (1.0000000000000000)%float) ::
  mkHostCacheEntry
    (HQToStr (VNumber (1.0000000000000000)%float) 10)
    (VStr (cu "1")) ::
  mkHostCacheEntry
    (HQStrToNumber (cu "-1"))
    (VNumber (-1.0000000000000000)%float) ::
  mkHostCacheEntry
    (HQToStr (VNumber (-1.0000000000000000)%float) 10)
    (VStr (cu "-1")) ::
  mkHostCacheEntry
    (HQDoubleToLongChecked (-1.0000000000000000)%float)
    (VMath (-1)) ::
  mkHostCacheEntry
    (HQStrToNumber (cu "1.5"))
    (VNumber (1.5000000000000000)%float) ::
  mkHostCacheEntry
    (HQToStr (VNumber (1.5000000000000000)%float) 10)
    (VStr (cu "1.5")) ::
  mkHostCacheEntry
    (HQDoubleToLongChecked (1.5000000000000000)%float)
    VUndef ::
  mkHostCacheEntry
    (HQStrToNumber (cu "0"))
    (VNumber (0.0000000000000000)%float) ::
  mkHostCacheEntry
    (HQToStr (VNumber (0.0000000000000000)%float) 10)
    (VStr (cu "0")) ::
  mkHostCacheEntry
    (HQDoubleToLongChecked (0.0000000000000000)%float)
    (VMath 0) ::
  mkHostCacheEntry
    (HQStrToNumber (cu "-0"))
    (VNumber (-0.0000000000000000)%float) ::
  mkHostCacheEntry
    (HQToStr (VNumber (-0.0000000000000000)%float) 10)
    (VStr (cu "0")) ::
  mkHostCacheEntry
    (HQStrToNumber (cu "NaN"))
    (VNumber PrimFloat.nan) ::
  mkHostCacheEntry
    (HQToStr (VNumber PrimFloat.nan) 10)
    (VStr (cu "NaN")) ::
  mkHostCacheEntry
    (HQDoubleToLongChecked PrimFloat.nan)
    VUndef ::
  mkHostCacheEntry
    (HQStrToNumber (cu "Infinity"))
    (VNumber PrimFloat.infinity) ::
  mkHostCacheEntry
    (HQToStr (VNumber PrimFloat.infinity) 10)
    (VStr (cu "Infinity")) ::
  mkHostCacheEntry
    (HQDoubleToLongChecked PrimFloat.infinity)
    VUndef ::
  nil.

Definition integer_keys_index (index : Z) : expr :=
  ERef
    (RField
      (RVar (VLocal (LName "keys")))
      (EMath index)).

Definition integer_keys_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "map"
       (EMap
         ((EStr (cu "10"), EUndef) ::
          (EStr (cu "2"), EUndef) ::
          (EStr (cu "01"), EUndef) ::
          (EStr (cu "-1"), EUndef) ::
          (EStr (cu "1.5"), EUndef) ::
          (EStr (cu "0"), EUndef) ::
          (EStr (cu "-0"), EUndef) ::
          (EStr (cu "NaN"), EUndef) ::
          (EStr (cu "Infinity"), EUndef) ::
          (EMath 99, EUndef) :: nil)) ::
     ILet "keys" (EKeys (lref "map") true) ::
     IAssert (EBinary BEq (ESizeOf (lref "keys")) (EMath 4)) ::
     IAssert
       (EBinary BEq (integer_keys_index 0) (EStr (cu "-1"))) ::
     IAssert
       (EBinary BEq (integer_keys_index 1) (EStr (cu "0"))) ::
     IAssert
       (EBinary BEq (integer_keys_index 2) (EStr (cu "2"))) ::
     IAssert
       (EBinary BEq (integer_keys_index 3) (EStr (cu "10"))) :: nil)).

(** Literal construction uses the same insertion helpers as later writes:
    entries are evaluated left-to-right, and a duplicate keeps its original
    position while the last value wins. *)
Definition duplicate_literals_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "record"
       (ERecord "R"
         (("x", EMath 1) ::
          ("x", EMath 2) :: nil)) ::
     IAssert
       (EBinary BEq
         (ERef
           (RField
             (RVar (VLocal (LName "record")))
             (EStr (cu "x"))))
         (EMath 2)) ::
     ILet "map"
       (EMap
         ((EStr (cu "x"), EMath 1) ::
          (EStr (cu "x"), EMath 2) :: nil)) ::
     IAssert
       (EBinary BEq
         (ERef
           (RField
             (RVar (VLocal (LName "map")))
             (EStr (cu "x"))))
         (EMath 2)) ::
     IAssert
       (EBinary BEq
         (ESizeOf (EKeys (lref "map") false))
         (EMath 1)) :: nil)).

Definition duplicate_literals_prog : prog :=
  mkProg (duplicate_literals_main :: nil).

Definition integer_keys_prog : prog :=
  mkProgFull
    (integer_keys_main :: nil)
    None None integer_keys_entries nil nil.

(** Record key order is observable but Scala's generic mutable HashMap
    does not specify insertion order.  Initial records are serialized in
    the exact ESMeta iteration order and remain trusted across updates to
    existing fields. *)
Definition record_keys_index (index : Z) : expr :=
  ERef
    (RField
      (RVar (VLocal (LName "keys")))
      (EMath index)).

Definition initial_record_keys_main : func :=
  mkFunc true "main" nil (ISeq
    (IAssign
       (RField
         (RVar (VGlobal "ORDERED_RECORD"))
         (EStr (cu "b")))
       (EMath 9) ::
     ILet "keys" (EKeys (gref "ORDERED_RECORD") false) ::
     IAssert (EBinary BEq (ESizeOf (lref "keys")) (EMath 2)) ::
     IAssert
       (EBinary BEq (record_keys_index 0) (EStr (cu "b"))) ::
     IAssert
       (EBinary BEq (record_keys_index 1) (EStr (cu "a"))) :: nil)).

Definition initial_record_keys_prog : prog :=
  mkProgFull
    (initial_record_keys_main :: nil)
    None None nil
    (("ORDERED_RECORD", VAddr 0) :: nil)
    (Some
       (ORecord "R"
         (("b", VMath 2) :: ("a", VMath 1) :: nil)) :: nil).

(** Runtime record construction, copying, and insertion of a new field can
    all choose a HashMap order different from the model's field list. *)
Definition runtime_record_keys_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "record"
       (ERecord "R"
         (("b", EMath 2) :: ("a", EMath 1) :: nil)) ::
     ILet "keys" (EKeys (lref "record") false) ::
     IPrint (ESizeOf (lref "keys")) :: nil)).

Definition runtime_record_keys_prog : prog :=
  mkProg (runtime_record_keys_main :: nil).

Definition runtime_empty_record_keys_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "record" (ERecord "R" nil) ::
     ILet "keys" (EKeys (lref "record") false) ::
     IAssert (EBinary BEq (ESizeOf (lref "keys")) (EMath 0)) :: nil)).

Definition runtime_empty_record_keys_prog : prog :=
  mkProg (runtime_empty_record_keys_main :: nil).

Definition runtime_singleton_record_keys_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "record"
       (ERecord "R" (("value", EMath 1) :: nil)) ::
     ILet "keys" (EKeys (lref "record") false) ::
     IAssert (EBinary BEq (ESizeOf (lref "keys")) (EMath 1)) ::
     IAssert
       (EBinary BEq (record_keys_index 0) (EStr (cu "value"))) :: nil)).

Definition runtime_singleton_record_keys_prog : prog :=
  mkProg (runtime_singleton_record_keys_main :: nil).

Definition copied_singleton_record_keys_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "source"
       (ERecord "R" (("value", EMath 1) :: nil)) ::
     ILet "record" (ECopy (lref "source")) ::
     ILet "keys" (EKeys (lref "record") false) ::
     IAssert (EBinary BEq (ESizeOf (lref "keys")) (EMath 1)) ::
     IAssert
       (EBinary BEq (record_keys_index 0) (EStr (cu "value"))) :: nil)).

Definition copied_singleton_record_keys_prog : prog :=
  mkProg (copied_singleton_record_keys_main :: nil).

Definition expanded_record_keys_main : func :=
  mkFunc true "main" nil (ISeq
    (IExpand
       (RVar (VGlobal "ORDERED_RECORD"))
       (EStr (cu "new")) ::
     ILet "keys" (EKeys (gref "ORDERED_RECORD") false) ::
     IPrint (ESizeOf (lref "keys")) :: nil)).

Definition expanded_record_keys_prog : prog :=
  mkProgFull
    (expanded_record_keys_main :: nil)
    None None nil
    (("ORDERED_RECORD", VAddr 0) :: nil)
    (Some
       (ORecord "R"
         (("b", VMath 2) :: ("a", VMath 1) :: nil)) :: nil).

Definition assigned_record_keys_main : func :=
  mkFunc true "main" nil (ISeq
    (IAssign
       (RField
         (RVar (VGlobal "ORDERED_RECORD"))
         (EStr (cu "new")))
       (EMath 3) ::
     ILet "keys" (EKeys (gref "ORDERED_RECORD") false) ::
     IPrint (ESizeOf (lref "keys")) :: nil)).

Definition assigned_record_keys_prog : prog :=
  mkProgFull
    (assigned_record_keys_main :: nil)
    None None nil
    (("ORDERED_RECORD", VAddr 0) :: nil)
    (Some
       (ORecord "R"
         (("b", VMath 2) :: ("a", VMath 1) :: nil)) :: nil).

Definition copied_record_keys_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "record" (ECopy (gref "ORDERED_RECORD")) ::
     ILet "keys" (EKeys (lref "record") false) ::
     IPrint (ESizeOf (lref "keys")) :: nil)).

Definition copied_record_keys_prog : prog :=
  mkProgFull
    (copied_record_keys_main :: nil)
    None None nil
    (("ORDERED_RECORD", VAddr 0) :: nil)
    (Some
       (ORecord "R"
         (("b", VMath 2) :: ("a", VMath 1) :: nil)) :: nil).

(** RecordTy's structural descendant branch: an ancestor-tagged record is
    accepted only after the descendant's required field is present. *)
Definition record_refinement_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "base" (ERecord "ExecutionContext" nil) ::
     IAssert
       (EUnary UNot
         (ETypeCheck (lref "base")
           (TRecord "GeneratorExecutionContext"))) ::
     ILet "generatorContext"
       (ERecord "ExecutionContext" (("Generator", EUndef) :: nil)) ::
     IAssert
       (ETypeCheck (lref "generatorContext")
         (TRecord "GeneratorExecutionContext")) :: nil)).

Definition record_refinement_prog : prog :=
  mkProg (record_refinement_main :: nil).

(** The generated type model retains only [RFCAddr] for nested heap
    refinements.  An actual address therefore yields UB until its recursive
    type is exported; accepting it merely because it is an address would be
    an unsound structural subtype result. *)
Definition record_refinement_addr_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "generator" (ERecord "Object" nil) ::
     ILet "generatorContext"
       (ERecord "ExecutionContext"
         (("Generator", lref "generator") :: nil)) ::
     IAssert
       (ETypeCheck (lref "generatorContext")
         (TRecord "GeneratorExecutionContext")) :: nil)).

Definition record_refinement_addr_prog : prog :=
  mkProg (record_refinement_addr_main :: nil).

(** Completion subtypes share the runtime tag [CompletionRecord] and are
    distinguished by their [Type] field. *)
Definition completion_discriminant_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "normal"
       (ERecord "CompletionRecord"
         (("Type", EEnum "normal") ::
          ("Value", EEnum "empty") ::
          ("Target", EEnum "empty") :: nil)) ::
     IAssert (ETypeCheck (lref "normal") TNormal) ::
     IAssert
       (EUnary UNot
         (ETypeCheck (lref "normal") (TRecord "ReturnCompletion"))) ::
     IAssert
       (EUnary UNot
         (ETypeCheck (lref "normal") (TRecord "ThrowCompletion"))) ::
     ILet "return"
       (ERecord "CompletionRecord"
         (("Type", EEnum "return") ::
          ("Value", EUndef) ::
          ("Target", EEnum "empty") :: nil)) ::
     IAssert
       (ETypeCheck (lref "return") (TRecord "ReturnCompletion")) ::
     ILet "throw"
       (ERecord "CompletionRecord"
         (("Type", EEnum "throw") ::
          ("Value", EUndef) ::
          ("Target", EEnum "empty") :: nil)) ::
     IAssert
       (ETypeCheck (lref "throw") (TRecord "ThrowCompletion")) :: nil)).

Definition completion_discriminant_prog : prog :=
  mkProg (completion_discriminant_main :: nil).

(** Each evaluated leaf syntactic literal denotes a newly allocated AST. *)
Definition runtime_leaf_expr : expr :=
  ESyntactic "IdentifierName" nil 0 0 nil nil
    (Some (cu "x") :: nil).

Definition runtime_leaf_fresh_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "left" runtime_leaf_expr ::
     ILet "right" runtime_leaf_expr ::
     IAssert
       (EUnary UNot (EBinary BEq (lref "left") (lref "right"))) :: nil)).

Definition runtime_leaf_fresh_prog : prog :=
  mkProg (runtime_leaf_fresh_main :: nil).

(** Runtime [ESyntactic] construction carries the grammar-derived metadata
    needed by equality, named-child lookup, source text, and SDO dispatch.
    The source layout below represents:

        IdentifierName "=" OptionalTail?

    with the optional tail absent. *)
Definition runtime_syn_expr : expr :=
  ESyntactic "RuntimeNode" (true :: false :: nil) 2 0
    (Some
       (ESyntactic "IdentifierName" nil 0 0 nil nil
         (Some (cu "x") :: nil)) ::
     None :: nil)
    ("IdentifierName" :: "OptionalTail" :: nil)
    (None :: Some (cu "=") :: None :: nil).

Definition runtime_syn_probe : func :=
  mkFunc false "RuntimeNode[2,0].Probe" ("node" :: nil) (ISeq
    (IAssert
       (EBinary BEq (ESourceText (lref "node")) (EStr (cu "x ="))) ::
     IReturn (EMath 7) :: nil)).

Definition runtime_syn_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "node" runtime_syn_expr ::
     IAssert
       (EExists
         (RField (RVar (VLocal (LName "node")))
           (EStr (cu "IdentifierName")))) ::
     IAssert
       (EUnary UNot
         (EExists
           (RField (RVar (VLocal (LName "node")))
             (EStr (cu "OptionalTail"))))) ::
     IAssert
       (EBinary BEq (ESourceText (lref "node")) (EStr (cu "x ="))) ::
     ISdoCall (LName "answer") (lref "node") "Probe" nil ::
     IAssert (EBinary BEq (lref "answer") (EMath 7)) :: nil)).

Definition runtime_syn_prog : prog :=
  mkProg (runtime_syn_probe :: runtime_syn_main :: nil).

(** Refined NumberInt tests use exact Binary64 integrality plus a sign
    mask; NaN and fractional values do not accidentally pass. *)
Definition number_int_refinement_main : func :=
  mkFunc true "main" nil (ISeq
    (IAssert
       (ETypeCheck (ENumber (-1.0000000000000000)%float)
         (TNumberInt true true true false)) ::
     IAssert
       (ETypeCheck (ENumber (-0.0000000000000000)%float)
         (TNumberInt true true true false)) ::
     IAssert
       (ETypeCheck (ENumber (1.0000000000000000)%float)
         (TNumberInt true true true false)) ::
     IAssert
       (EUnary UNot
         (ETypeCheck (ENumber (1.5000000000000000)%float)
           (TNumberInt true true true false))) ::
     IAssert
       (EUnary UNot
         (ETypeCheck (ENumber PrimFloat.nan)
           (TNumberInt true true true false))) :: nil)).

Definition number_int_refinement_prog : prog :=
  mkProg (number_int_refinement_main :: nil).

Definition number_nonneg_int_main : func :=
  mkFunc true "main" nil (ISeq
    (IAssert
       (ETypeCheck (ENumber (0.0000000000000000)%float)
         (TNumberInt false true true false)) ::
     IAssert
       (ETypeCheck (ENumber (1.0000000000000000)%float)
         (TNumberInt false true true false)) ::
     IAssert
       (EUnary UNot
         (ETypeCheck (ENumber (-1.0000000000000000)%float)
           (TNumberInt false true true false))) ::
     IAssert
       (EUnary UNot
         (ETypeCheck (ENumber (0.50000000000000000)%float)
           (TNumberInt false true true false))) :: nil)).

Definition number_nonneg_int_prog : prog :=
  mkProg (number_nonneg_int_main :: nil).

(** `FunctionObject` is the structural refinement `Object + Call`. *)
Definition function_object_refinement_main : func :=
  mkFunc true "main" nil (ISeq
    (ILet "plain" (ERecord "Object" nil) ::
     IAssert
       (EUnary UNot
         (ETypeCheck (lref "plain") (TRecord "FunctionObject"))) ::
     ILet "callable"
       (ERecord "Object" (("Call", EMath 0) :: nil)) ::
     IAssert
       (ETypeCheck (lref "callable") (TRecord "FunctionObject")) :: nil)).

Definition function_object_refinement_prog : prog :=
  mkProg (function_object_refinement_main :: nil).
