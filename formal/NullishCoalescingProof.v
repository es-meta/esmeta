(** * Nullish-coalescing: eliminating pure temporary bookkeeping

    This file isolates the denotational core of the usual transformation

      lhs ?? rhs

    into a target that evaluates [lhs] once, stores its value in a local
    temporary, reads the temporary back, and then performs the nullish test.

    The theorem is deliberately parametric in [lhs] and [rhs].  Consequently
    every event emitted by either operand, including calls and store events,
    is preserved without inspecting it.  The only side condition says that
    [rhs] cannot observe the fresh temporary.  A syntax-level transformation
    should discharge that condition from its freshness theorem. *)

From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment Domain Events Semantics.

Set Implicit Arguments.

Section NULLISH_CORE.
  Context `{!crisG Gamma Sigma alpha beta tau _S _I}.

  Variable mn : string.

  (** The abstract vertical slice discussed in the proof sketch.  [lhs] and
      [rhs] may contain arbitrary CRIS events.  Since [rhs] is already a
      computation rather than an expression evaluated from an environment,
      the private temporary cannot affect it. *)
  Definition nullish_source_tree
    (lhs rhs : itree crisE val) : itree crisE val :=
    value <- lhs;;
    match value with
    | VNull | VUndef => rhs
    | _ => Ret value
    end.

  Definition nullish_target_tree
    (temporary : local) (rho : env)
    (lhs rhs : itree crisE val) : itree crisE val :=
    value <- lhs;;
    let rho' := env_update temporary value rho in
    match env_lookup rho' temporary with
    | None => triggerUB
    | Some saved =>
        match saved with
        | VNull | VUndef => rhs
        | _ => Ret saved
        end
    end.

  Lemma nullish_target_tree_eq_source
    (temporary : local) (rho : env)
    (lhs rhs : itree crisE val) :
    nullish_target_tree temporary rho lhs rhs =
    nullish_source_tree lhs rhs.
  Proof.
    unfold nullish_target_tree, nullish_source_tree.
    apply bind_extk. intro value.
    rewrite env_lookup_update_same.
    reflexivity.
  Qed.

  (** The source evaluates [lhs] exactly once.  Only [null] and [undefined]
      select [rhs]; every other ECMAScript value is returned unchanged. *)
  Definition denote_nullish_source
    (lhs rhs : expr) (rho : env) : itree crisE val :=
    value <- denote_expr mn lhs rho;;
    match value with
    | VNull | VUndef => denote_expr mn rhs rho
    | _ => Ret value
    end.

  (** The target makes the compiler temporary explicit.  Local updates and
      reads are pure environment operations, so this introduces no [Vis]
      node.  The [None] branch models the semantics' impossible failed read
      as undefined behavior. *)
  Definition denote_nullish_target
    (temporary : local) (lhs rhs : expr) (rho : env) : itree crisE val :=
    value <- denote_expr mn lhs rho;;
    let rho' := env_update temporary value rho in
    match env_lookup rho' temporary with
    | None => triggerUB
    | Some saved =>
        match saved with
        | VNull | VUndef => denote_expr mn rhs rho'
        | _ => Ret saved
        end
    end.

  (** Semantic form of freshness: evaluating [rhs] is unchanged by adding
      or replacing the private temporary. *)
  Definition temp_irrelevant
    (temporary : local) (rhs : expr) (rho : env) : Prop :=
    forall value,
      denote_expr mn rhs (env_update temporary value rho) =
      denote_expr mn rhs rho.

  (** The key result is stronger than weak bisimulation: the two CRIS trees
      are equal.  [env_lookup_update_same] removes both the temporary read
      and its unreachable failure branch; [Hfresh] removes the private
      environment difference on the RHS branch. *)
  Lemma denote_nullish_target_eq_source
    (temporary : local) (lhs rhs : expr) (rho : env)
    (Hfresh : temp_irrelevant temporary rhs rho) :
    denote_nullish_target temporary lhs rhs rho =
    denote_nullish_source lhs rhs rho.
  Proof.
    unfold denote_nullish_target, denote_nullish_source, temp_irrelevant in *.
    apply bind_extk. intro value.
    rewrite env_lookup_update_same.
    destruct value; simpl; try reflexivity; apply Hfresh.
  Qed.

End NULLISH_CORE.

(** CRIS currently uses [ITreeS], while the upstream interaction-tree
    library defines [Eqit.eutt] over its standard representation.  This is
    the same structure-preserving bridge used by [JSClosedEquiv.v], kept
    local here so the core lemma does not depend on generated [Spec.v]. *)
From ITree Require Import ITree.

CoFixpoint nullish_to_standard_itree {E : Type -> Type} {R : Type}
  (tree : ITreeS.ITreeDefinition.itree E R)
  : ITree.Core.ITreeDefinition.itree E R :=
  ITree.Core.ITreeDefinition.go
    (match ITreeS.ITreeDefinition.observe tree with
     | ITreeS.ITreeDefinition.RetF result =>
         ITree.Core.ITreeDefinition.RetF result
     | ITreeS.ITreeDefinition.TauF next =>
         ITree.Core.ITreeDefinition.TauF
           (nullish_to_standard_itree next)
     | ITreeS.ITreeDefinition.VisF event continue_with =>
         ITree.Core.ITreeDefinition.VisF event
           (fun reply => nullish_to_standard_itree (continue_with reply))
     end).

Section NULLISH_EUTT.
  Context `{!crisG Gamma Sigma alpha beta tau _S _I}.

  Variable mn : string.

  Theorem denote_nullish_eutt
    (temporary : local) (lhs rhs : expr) (rho : env)
    (Hfresh : temp_irrelevant mn temporary rhs rho) :
    Eqit.eutt eq
      (nullish_to_standard_itree
        (denote_nullish_source mn lhs rhs rho))
      (nullish_to_standard_itree
        (denote_nullish_target mn temporary lhs rhs rho)).
  Proof.
    assert (Heq :
      denote_nullish_target mn temporary lhs rhs rho =
      denote_nullish_source mn lhs rhs rho).
    { apply denote_nullish_target_eq_source. exact Hfresh. }
    rewrite Heq.
    apply Eqit.Reflexive_eqit.
    red. intros. reflexivity.
  Qed.

  (** Unconditional [eutt] theorem for the abstract computations.  This is
      the exact formal counterpart of the source/target skeleton: [lhs] is
      shared once, the temporary round-trip is pure, and [rhs] is selected
      on precisely the same two values. *)
  Theorem nullish_temporary_eutt
    (temporary : local) (rho : env)
    (lhs rhs : ITreeS.ITreeDefinition.itree crisE val) :
    Eqit.eutt eq
      (nullish_to_standard_itree (nullish_source_tree lhs rhs))
      (nullish_to_standard_itree
        (nullish_target_tree temporary rho lhs rhs)).
  Proof.
    assert (Heq :
      nullish_target_tree temporary rho lhs rhs =
      nullish_source_tree lhs rhs).
    { apply nullish_target_tree_eq_source. }
    rewrite Heq.
    apply Eqit.Reflexive_eqit.
    red. intros. reflexivity.
  Qed.

End NULLISH_EUTT.

Print Assumptions denote_nullish_eutt.
Print Assumptions nullish_temporary_eutt.
