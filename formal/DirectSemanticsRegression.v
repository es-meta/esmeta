(** Differential compile fixture for the phase-1 direct ITree ABI. *)

From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment Domain Semantics DirectSemantics Programs.

Set Implicit Arguments.
Local Open Scope string_scope.
Local Open Scope Z_scope.

Section REGRESSION.
  Context `{!crisG Γ Σ α β τ _S _I}.
  Variable mn : string.

  Definition fixture_func : func :=
    mkFunc false "direct-fixture" ("x" :: nil)
      (ISeq
        (ILet "and-result" (EBinary BAnd (EBool false) (EYet "skipped")) ::
         ILet "or-result" (EBinary BOr (EBool true) (EYet "skipped")) ::
         ILet "converted" (EConvert CToMath (EMath 3)) ::
         IAssert (EYet "bare assertion") ::
         IReturn (ERef (RVar (VLocal (LName "x")))) ::
         IReturn (EYet "unreachable") :: nil)).

  Definition fixture_direct_inst : direct_inst_body :=
    direct_seq
      (direct_let "and-result"
        (direct_and (fun _ => Ret (VBool false)) (fun _ => triggerUB)) ::
       direct_let "or-result"
        (direct_or (fun _ => Ret (VBool true)) (fun _ => triggerUB)) ::
       direct_let "converted"
        (direct_convert mn CToMath (fun _ => Ret (VMath 3))) ::
       direct_assert_yet ::
       direct_return
        (direct_read mn (direct_ref_var (VLocal (LName "x")))) ::
       direct_return (fun _ => triggerUB) :: nil).

  (** This equality simultaneously locks short-circuiting, the ordered
      [EConvert] clause, bare-[EYet] assertion handling, and early return. *)
  Example fixture_inst_matches_generic fnames ρ :
    fixture_direct_inst fnames ρ =
    denote_inst mn fnames (f_body fixture_func) ρ.
  Proof. reflexivity. Qed.

  Example fixture_body_matches_generic fnames arg :
    direct_fbody fnames (f_params fixture_func) (f_main fixture_func)
      fixture_direct_inst arg =
    denote_fbody mn fnames fixture_func arg.
  Proof. reflexivity. Qed.

  Definition assignment_order_inst : inst :=
    IAssign
      (RField (RVar (VGlobal "base")) (EStr (cu "field")))
      (EList (EMath 1 :: nil)).

  Definition assignment_order_direct : direct_inst_body :=
    direct_assign mn
      (direct_ref_field mn (direct_ref_var (VGlobal "base"))
        (fun _ => Ret (VStr (cu "field"))))
      (fun _ =>
        values <-
          (value <- Ret (VMath 1);;
           rest <- Ret nil;;
           Ret (value :: rest));;
        address <- alloc_obj mn (OList values);;
        Ret (VAddr address)).

  (** Resolving and reading [base] is bound before the RHS allocation. *)
  Example assignment_reference_precedes_rhs fnames ρ :
    assignment_order_direct fnames ρ =
    denote_inst mn fnames assignment_order_inst ρ.
  Proof.
    unfold assignment_order_direct, assignment_order_inst.
    unfold direct_assign, direct_ref_field, direct_ref_var, direct_list_values.
    cbn [denote_inst denote_ref denote_expr].
    reflexivity.
  Qed.

  Example false_assert_matches_generic fnames ρ :
    direct_assert (fun _ => Ret (VBool false)) fnames ρ =
    denote_inst mn fnames (IAssert (EBool false)) ρ.
  Proof. reflexivity. Qed.

  (** Catchable parse-operand failure is not collapsed into model UB. *)
  Example parse_operand_failure_matches_generic ρ :
    direct_parse_outcomes mn EvalThrow
      (EvalValue (VGrammarSymbol "Script" nil)) =
    denote_expr mn
      (EParse (EYet "caught") (EGrammarSymbol "Script" nil)) ρ.
  Proof.
    cbn [direct_parse_outcomes denote_expr denote_parse_operand eval_bind
      eval_throw].
    rewrite bind_ret_l. reflexivity.
  Qed.

  Definition main_fallthrough_direct : direct_inst_body := direct_nop.

  Definition main_fallthrough_func : func :=
    mkFunc true "main-fallthrough" nil INop.

  Definition nonmain_fallthrough_func : func :=
    mkFunc false "nonmain-fallthrough" nil INop.

  Example main_fallthrough_is_undefined fnames :
    direct_fbody fnames nil true main_fallthrough_direct (nil, nil) =
    denote_fbody mn fnames main_fallthrough_func (nil, nil).
  Proof. reflexivity. Qed.

  Example nonmain_fallthrough_is_ub fnames :
    direct_fbody fnames nil false main_fallthrough_direct (nil, nil) =
    denote_fbody mn fnames nonmain_fallthrough_func (nil, nil).
  Proof. reflexivity. Qed.

  Definition captured_return_func : func :=
    mkFunc false "captured-return" ("x" :: nil)
      (IReturn (ERef (RVar (VLocal (LName "x"))))).

  Example captured_binding_overrides_parameter fnames :
    direct_fbody fnames ("x" :: nil) false
      (direct_return
        (direct_read mn (direct_ref_var (VLocal (LName "x")))))
      (("x", VMath 9) :: nil, VMath 1 :: nil) =
    denote_fbody mn fnames captured_return_func
      (("x", VMath 9) :: nil, VMath 1 :: nil).
  Proof. reflexivity. Qed.

  Example continuation_ignores_surplus_arguments fnames :
    direct_cont_fbody fnames ("x" :: nil) false
      (direct_return
        (direct_read mn (direct_ref_var (VLocal (LName "x")))))
      (nil, VMath 1 :: VMath 2 :: nil) =
    denote_cont_fbody mn fnames captured_return_func
      (nil, VMath 1 :: VMath 2 :: nil).
  Proof. reflexivity. Qed.

  Example ordinary_entry_rejects_surplus_arguments fnames :
    direct_fbody fnames ("x" :: nil) false
      (direct_return
        (direct_read mn (direct_ref_var (VLocal (LName "x")))))
      (nil, VMath 1 :: VMath 2 :: nil) =
    denote_fbody mn fnames captured_return_func
      (nil, VMath 1 :: VMath 2 :: nil).
  Proof. reflexivity. Qed.

  Definition packaged_fixture :=
    direct_fnsem mn "direct-fixture"
      (direct_fbody nil (f_params fixture_func) (f_main fixture_func)
        fixture_direct_inst).

  Definition packaged_fixture_cont :=
    direct_cont_fnsem mn "direct-fixture"
      (direct_cont_fbody nil
        (f_params fixture_func) (f_main fixture_func)
        fixture_direct_inst).
End REGRESSION.
