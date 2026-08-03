(** * ESMetaFV.JSClosedEquiv — closed JavaScript ITree equivalence

    This module packages an exported JavaScript source, its parsed AST, and
    the host answers needed by the closed semantics.  The resulting tree is
    exactly [exec_itree] applied to the generated specification's
    [script_prog]; no separate evaluator or approximation is introduced.

    [quiet_result] is deliberately small: it witnesses a finite sequence of
    silent steps ending in a return.  Thus it excludes visible [coreE] events
    (including prints and undefined behaviour). *)

From Stdlib Require Import String List.
From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment ITreeExec Spec.
From ITree Require Import ITree.

Set Implicit Arguments.

Record prepared_js : Type := mkPreparedJS {
  prepared_source : cstr;
  prepared_ast : ast;
  prepared_hosts : list host_cache_entry;
}.

Definition prepare_js
  (src : cstr) (a : ast) (hosts : list host_cache_entry) : prepared_js :=
  mkPreparedJS src a hosts.

Definition prepared_program (input : prepared_js) : prog :=
  script_prog
    (prepared_source input)
    (prepared_ast input)
    (prepared_hosts input).

Definition closed_js_itree
  (module_name : String.string) (input : prepared_js)
  : ITreeS.ITreeDefinition.itree coreE val :=
  exec_itree module_name (prepared_program input).

(** Direct form for callers that already have the three exported inputs. *)
Definition closed_js_source_itree
  (module_name : String.string)
  (src : cstr) (a : ast) (hosts : list host_cache_entry)
  : ITreeS.ITreeDefinition.itree coreE val :=
  closed_js_itree module_name (prepare_js src a hosts).

Inductive quiet_result {E : Type -> Type} {R : Type}
  : ITreeS.ITreeDefinition.itree E R -> R -> Prop :=
| quiet_result_ret (t : ITreeS.ITreeDefinition.itree E R) (r : R) :
    ITreeS.ITreeDefinition.observe t = ITreeS.ITreeDefinition.RetF r ->
    quiet_result t r
| quiet_result_tau
    (t next : ITreeS.ITreeDefinition.itree E R) (r : R) :
    ITreeS.ITreeDefinition.observe t = ITreeS.ITreeDefinition.TauF next ->
    quiet_result next r ->
    quiet_result t r.

Fixpoint quiet_result_fuel {E : Type -> Type} {R : Type}
  (fuel : nat) (t : ITreeS.ITreeDefinition.itree E R) : option R :=
  match fuel with
  | O => None
  | S fuel' =>
      match ITreeS.ITreeDefinition.observe t with
      | ITreeS.ITreeDefinition.RetF r => Some r
      | ITreeS.ITreeDefinition.TauF next => quiet_result_fuel fuel' next
      | ITreeS.ITreeDefinition.VisF _ _ => None
      end
  end.

Theorem quiet_result_fuel_sound
  {E : Type -> Type} {R : Type}
  (fuel : nat) (t : ITreeS.ITreeDefinition.itree E R) (r : R) :
  quiet_result_fuel fuel t = Some r -> quiet_result t r.
Proof.
  revert t r.
  induction fuel as [| fuel IH]; intros t r Hresult.
  - discriminate.
  - cbn in Hresult.
    destruct (ITreeS.ITreeDefinition.observe t) eqn:Hobserve.
    + inversion Hresult; subst. apply quiet_result_ret. exact Hobserve.
    + eapply quiet_result_tau.
      * exact Hobserve.
      * eapply IH. exact Hresult.
    + discriminate.
Qed.

(** CRIS uses the [ITreeS] presentation of interaction trees, while the
    installed upstream ITree library supplies weak equivalence [eutt].  This
    structural translation changes no return or visible event and lets the
    closed execution theorem use that standard relation directly. *)
CoFixpoint to_standard_itree {E : Type -> Type} {R : Type}
  (t : ITreeS.ITreeDefinition.itree E R)
  : ITree.Core.ITreeDefinition.itree E R :=
  ITree.Core.ITreeDefinition.go
    (match ITreeS.ITreeDefinition.observe t with
     | ITreeS.ITreeDefinition.RetF r =>
         ITree.Core.ITreeDefinition.RetF r
     | ITreeS.ITreeDefinition.TauF t' =>
         ITree.Core.ITreeDefinition.TauF (to_standard_itree t')
     | ITreeS.ITreeDefinition.VisF e k =>
         ITree.Core.ITreeDefinition.VisF e
           (fun x => to_standard_itree (k x))
     end).

Theorem quiet_result_sound
  {E : Type -> Type} {R : Type}
  (t : ITreeS.ITreeDefinition.itree E R) (r : R) :
  quiet_result t r -> Eqit.eutt eq (to_standard_itree t) (Ret r).
Proof.
  intro Hquiet.
  induction Hquiet.
  - pstep. red. cbn. rewrite H. constructor. reflexivity.
  - transitivity (Tau (to_standard_itree next)).
    + pstep. red. cbn. rewrite H. constructor. left.
      apply Eqit.Reflexive_eqit. red. intro x. exact eq_refl.
    + etransitivity.
      * apply Eqit.tau_eutt.
      * exact IHHquiet.
Qed.

Theorem quiet_same_result_eutt
  {E : Type -> Type} {R : Type}
  (left right : ITreeS.ITreeDefinition.itree E R) (r : R) :
  quiet_result left r -> quiet_result right r ->
  Eqit.eutt eq (to_standard_itree left) (to_standard_itree right).
Proof.
  intros Hleft Hright.
  etransitivity.
  - apply quiet_result_sound. exact Hleft.
  - symmetry. apply quiet_result_sound. exact Hright.
Qed.

Definition closed_js_quiet_result
  (module_name : String.string) (input : prepared_js) (r : val) : Prop :=
  quiet_result (closed_js_itree module_name input) r.

Theorem closed_js_quiet_result_sound
  (module_name : String.string) (input : prepared_js) (r : val) :
  closed_js_quiet_result module_name input r ->
  Eqit.eutt eq
    (to_standard_itree (closed_js_itree module_name input))
    (Ret r).
Proof.
  apply quiet_result_sound.
Qed.

Theorem closed_js_same_result_eutt
  (left_module right_module : String.string)
  (left right : prepared_js) (r : val) :
  closed_js_quiet_result left_module left r ->
  closed_js_quiet_result right_module right r ->
  Eqit.eutt eq
    (to_standard_itree (closed_js_itree left_module left))
    (to_standard_itree (closed_js_itree right_module right)).
Proof.
  apply quiet_same_result_eutt.
Qed.

(** Computation-facing form: generated proofs need only establish the two
    finite evaluations with a kernel-checked computation certificate. *)
Theorem closed_js_same_result_fuel_eutt
  (left_module right_module : String.string)
  (left right : prepared_js) (r : val)
  (left_fuel right_fuel : nat) :
  quiet_result_fuel left_fuel (closed_js_itree left_module left) = Some r ->
  quiet_result_fuel right_fuel (closed_js_itree right_module right) = Some r ->
  Eqit.eutt eq
    (to_standard_itree (closed_js_itree left_module left))
    (to_standard_itree (closed_js_itree right_module right)).
Proof.
  intros Hleft Hright.
  apply quiet_same_result_eutt with (r := r).
  - eapply quiet_result_fuel_sound. exact Hleft.
  - eapply quiet_result_fuel_sound. exact Hright.
Qed.
