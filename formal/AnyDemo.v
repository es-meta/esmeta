(* CRIS의 Any가 vm_compute를 막는 이유 — 30줄 재현 *)
From Stdlib Require Import ClassicalEpsilon Eqdep.

(* ---- 1. "타입 지우는 상자": CRIS Any.t의 축소판 ---- *)
Record box := Box { ty : Type; content : ty }.

Definition upcast {T : Type} (v : T) : box := Box T v.

(* 꺼낼 때: 상자 속 타입이 T와 같은가? 를 공리로 판정 *)
Definition downcast (T : Type) (b : box) : option T :=
  match excluded_middle_informative (ty b = T) with
  | left e => Some (eq_rect _ (fun X => X) (content b) _ e)
  | right _ => None
  end.

(* 넣었다 바로 꺼내기: "당연히 Some 3"이어야 하지만... *)
Definition roundtrip_any : option nat := downcast nat (upcast 3).

(* 타입을 안 지우면 애초에 꺼낼 것도 없음 *)
Definition roundtrip_typed : option nat := Some 3.

(* ---- 2. 계산해보기 ---- *)
Eval vm_compute in roundtrip_typed.   (* = Some 3 : 즉시 *)
Eval vm_compute in roundtrip_any.     (* 뭐가 나올까? *)

(* ---- 3. 계산은 막혀도 '증명'은 된다 ---- *)
Lemma up_down : forall (T : Type) (v : T), downcast T (upcast v) = Some v.
Proof.
  intros T v. unfold downcast, upcast. simpl.
  destruct (excluded_middle_informative (T = T)) as [e | n].
  - rewrite <- Eqdep.EqdepTheory.eq_rect_eq. reflexivity.
  - exfalso. apply n. reflexivity.
Qed.

(* 증명 세계에서는 등식으로 통과: *)
Lemma roundtrip_any_is_some3 : roundtrip_any = Some 3.
Proof. apply up_down. Qed.
