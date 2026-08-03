(** Complete closed Tier-A generic/direct differential fixture. *)

From CRIS Require Import CRIS.
From ESMetaFV Require Import
  Fragment Domain Events Semantics DirectSemantics ITreeExec DirectITreeExec.

Import ListNotations.
Set Implicit Arguments.
Local Open Scope string_scope.
Local Open Scope Z_scope.

Definition tier_a_mn : string := "ESMetaFV-Direct-Tier-A".
Definition tier_a_fnames : list string := ["main"].

(** The fixture allocates a result-reachable record, writes two globals,
    prints the result, and returns it.  Thus one run covers the result, IO
    trace, global store, reachable heap cell, record-order provenance, and
    both allocation counters. *)
Definition tier_a_main : func :=
  mkFunc true "main" nil
    (ISeq
      [ ILet "box" (ERecord "Box" [("value", EMath 7)])
      ; IAssign (RVar (VGlobal "box"))
          (ERef (RVar (VLocal (LName "box"))))
      ; IAssign (RVar (VGlobal "result")) (EMath 7)
      ; IPrint (ERef (RVar (VGlobal "result")))
      ; IReturn (ERef (RVar (VGlobal "result")))
      ]).

Definition tier_a_prog : prog :=
  mkProgFull [tier_a_main] None None nil
    [("box", VUndef); ("result", VMath 0)] nil.

Section DIRECT_FIXTURE.
  Context `{!crisG Γ Σ α β τ _S _I}.

  Definition tier_a_direct_inst (mn : string) : direct_inst_body :=
    direct_seq
      [ direct_let "box"
          (fun _ => direct_record_values mn "Box" [("value", VMath 7)])
      ; direct_assign mn (direct_ref_var (VGlobal "box"))
          (direct_read mn (direct_ref_var (VLocal (LName "box"))))
      ; direct_assign mn (direct_ref_var (VGlobal "result"))
          (fun _ => Ret (VMath 7))
      ; fun _ ρ =>
          value <- direct_read mn (direct_ref_var (VGlobal "result")) ρ;;
          log_val value;;;
          Ret (ρ, CNormal VUndef)
      ; direct_return
          (direct_read mn (direct_ref_var (VGlobal "result")))
      ].

  Definition tier_a_direct_body (mn : string) : ir_arg -> itree crisE val :=
    direct_fbody tier_a_fnames nil true (tier_a_direct_inst mn).

  Definition tier_a_direct_cont_body
    (mn : string) : ir_arg -> itree crisE val :=
    direct_cont_fbody tier_a_fnames nil true (tier_a_direct_inst mn).

  Definition tier_a_ordinary_entry (mn : string) :=
    direct_fnsem mn "main" (tier_a_direct_body mn).

  Definition tier_a_continuation_entry (mn : string) :=
    direct_cont_fnsem mn "main" (tier_a_direct_cont_body mn).

  Definition tier_a_main_entry (mn : string) :=
    direct_entry mn (tier_a_direct_body mn).
End DIRECT_FIXTURE.

Definition tier_a_ordinary_map : direct_fnsemmap :=
  list_to_map [@tier_a_ordinary_entry execΣ tier_a_mn].

Definition tier_a_continuation_map : direct_fnsemmap :=
  list_to_map [@tier_a_continuation_entry execΣ tier_a_mn].

Record tier_a_observation : Type := mkTierAObservation {
  tier_result : val;
  tier_result_global : val;
  tier_box_global : val;
  tier_box_address : nat;
  tier_box_type : string;
  tier_box_field : val;
  tier_box_record_order : bool;
  tier_alloc_counter : nat;
  tier_ast_alloc_counter : nat;
}.

Section DIAGNOSTIC_ENTRY.
  Context `{!crisG Γ Σ α β τ _S _I}.

  (** Collect diagnostics while the typed keyed-store interpreter is still
      active.  This makes every failed store cast UB rather than allowing two
      failed [option] casts to compare equal at the OCaml boundary. *)
  Definition tier_a_diagnostic_body
    (mn : string) (body : ir_arg -> itree crisE val)
    : unit -> itree crisE tier_a_observation :=
    fun _ =>
      result <- body (nil, nil);;
      result_global <- (cgetU (glb_key mn "result") : itree crisE val);;
      box_global <- (cgetU (glb_key mn "box") : itree crisE val);;
      match box_global with
      | VAddr address =>
          object <- get_obj mn address;;
          match object with
          | ORecord type_name fields =>
              field <- (fields_lookup fields "value")?;;
              order <-
                (cgetU (record_order_key mn address) : itree crisE bool);;
              alloc <- (cgetU (alloc_key mn) : itree crisE nat);;
              ast_alloc <- (cgetU (ast_alloc_key mn) : itree crisE nat);;
              Ret (mkTierAObservation result result_global box_global
                address type_name field order alloc ast_alloc)
          | _ => triggerUB
          end
      | _ => triggerUB
      end.

  Definition tier_a_diagnostic_entry
    (mn : string) (body : ir_arg -> itree crisE val)
    : fname * option (emask * (option fspec_rel * fbody)) :=
    (entry,
      Some (ir_mask mn,
        (fsp_none,
          cfunU (fntyp unit tier_a_observation)
            (tier_a_diagnostic_body mn body)))).
End DIAGNOSTIC_ENTRY.

Definition tier_a_direct_entry_body : option direct_packed_body :=
  snd (@tier_a_diagnostic_entry execΣ tier_a_mn
    (@tier_a_direct_body execΣ tier_a_mn)).

Definition tier_a_generic_entry_body : option direct_packed_body :=
  snd (@tier_a_diagnostic_entry execΣ tier_a_mn
    (@denote_fbody execΣ tier_a_mn tier_a_fnames tier_a_main)).

Definition tier_a_generic_ordinary_map : direct_fnsemmap :=
  delete entry (@ir_fnsems execΣ tier_a_mn tier_a_prog).

Definition tier_a_generic_tree : itree coreE tier_a_observation :=
  sr <- direct_compile_full false tier_a_mn tier_a_prog
    tier_a_generic_ordinary_map tier_a_generic_entry_body
    (@ir_cont_fnsems execΣ tier_a_mn tier_a_prog);;
  observation <- (@Any.downcast tier_a_observation (snd sr))?;;
  Ret observation.

Definition tier_a_direct_tree : itree coreE tier_a_observation :=
  sr <- direct_compile_full false tier_a_mn tier_a_prog
    tier_a_ordinary_map tier_a_direct_entry_body tier_a_continuation_map;;
  observation <- (@Any.downcast tier_a_observation (snd sr))?;;
  Ret observation.

(** The fixture pins the complete direct map shape independently of runtime
    equality: two [funid] maps with the same sole domain, and one separately
    installed [entry]. *)
Example tier_a_ordinary_domain_complete :
  dom tier_a_ordinary_map = {[funid "main"]}.
Proof. vm_compute. reflexivity. Qed.

Example tier_a_continuation_domain_complete :
  dom tier_a_continuation_map = {[funid "main"]}.
Proof. vm_compute. reflexivity. Qed.

Example tier_a_entry_installed :
  is_Some
    (complete_direct_ordinary_map tier_a_ordinary_map
      tier_a_direct_entry_body !! entry).
Proof.
  unfold complete_direct_ordinary_map, tier_a_direct_entry_body.
  cbn [tier_a_diagnostic_entry].
  rewrite lookup_insert. eauto.
Qed.
