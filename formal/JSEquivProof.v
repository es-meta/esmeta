(** * JavaScript-source equivalence through the generated ITree semantics

    The witnesses in [tests/fv/js-equiv] cross ESMeta's real Script parser,
    initializer, interpreter, and Rocq exporter.  In particular, the two ASI
    witnesses below have different input bytes: one omits a statement
    terminator before [}], while the other writes it explicitly.  Both omit a
    final terminator so ESMeta's automatic-semicolon-insertion pass rewrites
    both inputs and checks that their effective source, AST, and captured host
    answers are exactly identical before emitting the aliases imported here.

    The theorem therefore concerns actual JavaScript inputs and the exact
    [exec_itree] semantics of their generated programs.  It does not claim
    correctness of the trusted Scala parser/export boundary, nor the stronger
    optional-chain-versus-hand-written-guard equivalence that would require a
    separate execution/bisimulation argument. *)

From Stdlib Require Import String List.
From ESMetaFV Require Import Fragment JSClosedEquiv.
From ESMetaFV.validation Require Import JSEquivArtifacts.

Import ListNotations.
Local Open Scope string_scope.

Definition js_equiv_module : String.string := "ESMetaFV-JS-Equiv".

(** The four earlier witnesses remain useful frontend-preservation checks.
    They are intentionally not presented as an ITree equivalence proof: their
    closed executions are too large to reify with [native_compute]. *)
Example constant_condition_left_source_preserved :
  constant_condition_left_input_source = constant_condition_left_source.
Proof. vm_compute. reflexivity. Qed.

Example constant_condition_right_source_preserved :
  constant_condition_right_input_source = constant_condition_right_source.
Proof. vm_compute. reflexivity. Qed.

Example optional_chain_left_source_preserved :
  optional_chain_left_input_source = optional_chain_left_source.
Proof. vm_compute. reflexivity. Qed.

Example optional_chain_right_source_preserved :
  optional_chain_right_input_source = optional_chain_right_source.
Proof. vm_compute. reflexivity. Qed.

Definition asi_optional_chain_implicit_program : prepared_js :=
  prepare_js
    asi_optional_chain_implicit_source
    asi_optional_chain_implicit_ast
    asi_optional_chain_implicit_hosts.

Definition asi_optional_chain_explicit_program : prepared_js :=
  prepare_js
    asi_optional_chain_explicit_source
    asi_optional_chain_explicit_ast
    asi_optional_chain_explicit_hosts.

(** This is a check on the two files' raw bytes, before ESMeta performs ASI. *)
Lemma asi_optional_chain_inputs_are_distinct :
  asi_optional_chain_implicit_input_source <>
  asi_optional_chain_explicit_input_source.
Proof. vm_compute. discriminate. Qed.

(** Both raw inputs were changed by ESMeta's ASI pass.  The generator then
    compared the complete rendered effective source, AST, and host cache before
    it was allowed to emit the aliases used by the following equalities. *)
Lemma asi_optional_chain_implicit_uses_asi :
  asi_optional_chain_implicit_input_source <>
  asi_optional_chain_implicit_source.
Proof. vm_compute. discriminate. Qed.

Lemma asi_optional_chain_explicit_uses_asi :
  asi_optional_chain_explicit_input_source <>
  asi_optional_chain_explicit_source.
Proof. vm_compute. discriminate. Qed.

Lemma asi_optional_chain_effective_sources_equal :
  asi_optional_chain_implicit_source =
  asi_optional_chain_explicit_source.
Proof. reflexivity. Qed.

Lemma asi_optional_chain_asts_equal :
  asi_optional_chain_implicit_ast =
  asi_optional_chain_explicit_ast.
Proof. reflexivity. Qed.

Lemma asi_optional_chain_host_answers_equal :
  asi_optional_chain_implicit_hosts =
  asi_optional_chain_explicit_hosts.
Proof. reflexivity. Qed.

Lemma asi_optional_chain_prepared_programs_equal :
  asi_optional_chain_implicit_program =
  asi_optional_chain_explicit_program.
Proof. reflexivity. Qed.

(** Exact equality of the generated inputs gives exact equality of their CRIS
    trees; reflexivity of standard ITree weak bisimulation then supplies the
    user-facing [eutt] theorem without executing or approximating either tree. *)
Theorem asi_optional_chain_closed_js_equiv :
  Eqit.eutt eq
    (to_standard_itree
      (closed_js_itree js_equiv_module
        asi_optional_chain_implicit_program))
    (to_standard_itree
      (closed_js_itree js_equiv_module
        asi_optional_chain_explicit_program)).
Proof.
  rewrite asi_optional_chain_prepared_programs_equal.
  apply Eqit.Reflexive_eqit.
  red. intros. reflexivity.
Qed.

Print Assumptions asi_optional_chain_closed_js_equiv.
