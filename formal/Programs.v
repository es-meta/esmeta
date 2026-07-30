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

From Stdlib Require Import String ZArith List.
From ESMetaFV Require Import Fragment.

Local Open Scope string_scope.
Local Open Scope Z_scope.

(** Readability helpers *)
Definition lref (x : string) : expr := ERef (RVar (VLocal (LName x))).
Definition tref (n : nat) : expr := ERef (RVar (VLocal (LTemp n))).
Definition lassign (x : string) (e : expr) : inst :=
  IAssign (RVar (VLocal (LName x))) e.
Definition tassign (n : nat) (e : expr) : inst :=
  IAssign (RVar (VLocal (LTemp n))) e.

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
    IS NOT ESTABLISHED" section of T3Proof.v and limitation L-8. *)

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
