(** * The assertions of tests/ir/branch.ir hold

    branch.ir is

      @main def main() = {
        if (< 1 2) assert true
        else assert false
        return 0
      }

    and the statement proven here is [ctx_refines Target Source], where both
    sides are generated from that one file: [Target] checks its assertions and
    [Source] passes them.  The two bodies are identical apart from the assertion
    sites, so the refinement carries exactly one claim -- no assertion in this
    program is ever violated -- and nothing about what else the program does.

    Why that direction.  [refines Mt Ms] unfolds to [∀ t, Beh Mt t -∗ Beh Ms t]
    (CRIS, simulations/ctxrefine/CtxRefine.v), so [Mt] is the side whose
    behaviours must be *contained*.  A violated assertion is [triggerUB], which
    admits every behaviour; on the left that is a contradiction, on the right it
    would make the statement vacuous.  The program therefore has to be [Target].

    This is the static counterpart of what EvalTinyTest checks dynamically by
    running the file, and it is the smallest instance of a statement ESMeta can
    generate for any `.ir` program without being told what the program computes.

    Build with `make proofs` in the directory `esmeta rocq-ir tests/ir/branch.ir`
    writes. *)

Require Import type manual_type op itree_state Signatures.
Require Import AbsOp_main.
From Stdlib Require Import ZArith String Ascii.
From CRIS Require Import CRIS.

(** CRIS provides [cStartFunSim]; this wrapper additionally exposes the typed
    body and discharges the ill-typed call case.  Copied from
    ~/code/verify/day1/exercises/Optimizations.v, which defines it locally. *)
Ltac cStartTypedFunSim x :=
  cStartFunSim;
  cStepsS; cStepsT;
  lazymatch goal with
  | arg : Any.t |- _ =>
      destruct (Any.downcast arg) as [x|];
        cStepsS; [cStepsT|]; ss
  end.

(** The single exported function id both modules register under. *)
Module AssertHdr.
  Definition mn := "ESMetaAssert".

  Definition run : fnsig_t IRFunctionInput IRFunctionOutput :=
    fnsig "ESMetaAssert.branch.main" ir_function_type.
End AssertHdr.

(** ** Source: the specification -- a violated assertion passes *)

Module AssertSource. Section AssertSource.
  Context `{!crisG Γ Σ α β τ _S _I}.

  Definition scopes : list string := cons AssertHdr.mn nil.

  Definition run : IRFunctionInput -> itree crisE IRFunctionOutput :=
    fun input =>
      let '(arguments, state) := input in
      match arguments with
      | nil => ir_AbsOp_main_assert_free state
      | _ => Ret FAIL
      end.

  Definition fnsems : fnsemmap :=
    {[fid AssertHdr.run #
        (msk_scp scopes msk_true,
         (fsp_none, cfunU AssertHdr.run run))]}.

  Program Definition smod : SMod.t := {|
    SMod.scopes := scopes;
    SMod.fnsems := fnsems;
    SMod.initial_st := ∅;
  |}.
  Solve All Obligations with mod_tac.

  Definition t : Mod.t := SMod.to_mod ∅ smod.
End AssertSource. End AssertSource.

(** ** Target: the program as extracted -- a violated assertion is UB *)

Module AssertTarget. Section AssertTarget.
  Context `{!crisG Γ Σ α β τ _S _I}.

  Definition scopes : list string := cons AssertHdr.mn nil.

  Definition run : IRFunctionInput -> itree crisE IRFunctionOutput :=
    fun input =>
      let '(arguments, state) := input in
      match arguments with
      | nil => ir_AbsOp_main state
      | _ => Ret FAIL
      end.

  Definition fnsems : fnsemmap :=
    {[fid AssertHdr.run #
        (msk_scp scopes msk_true,
         (fsp_none, cfunU AssertHdr.run run))]}.

  Program Definition smod : SMod.t := {|
    SMod.scopes := scopes;
    SMod.fnsems := fnsems;
    SMod.initial_st := ∅;
  |}.
  Solve All Obligations with mod_tac.

  Definition t : Mod.t := SMod.to_mod ∅ smod.
End AssertTarget. End AssertTarget.

Module AssertProof. Section AssertProof.
  Context `{!crisG Γ Σ α β τ _S _I}.

  Local Definition Source := AssertSource.t.
  Local Definition Target := AssertTarget.t.

  (** Neither module owns local state -- ESMeta threads the whole [State]
      through [IRFunctionInput]/[Exec_Result] as a value -- so every state pair
      is related. *)
  Definition Ist : ist_type Σ := fun _ _ => True%I.

  (** [main] takes no arguments, so any non-empty list returns [FAIL] on both
      sides. *)
  Local Ltac solve_arity_mismatch := cStep; iSplit; done.

  (** Unfold both bodies down to the generated monadic scaffolding.  The
      assertion combinators go too: that is the whole difference between the two
      sides, and it has to be exposed for the branch to close it. *)
  Local Ltac expose_bodies :=
    unfold AssertSource.run, AssertTarget.run;
    unfold ir_AbsOp_main, ir_AbsOp_main_assert_free;
    unfold itree_block_body, itree_block_seq, itree_block_return,
      itree_block_if, itree_block_fallthrough;
    unfold itree_state_assert, itree_state_assert_skip;
    unfold itree_state_bind, itree_state_return, itree_state_lift,
      state_return.

  Lemma simF_run :
    ISim.sim_fun open Source Target Ist (fid AssertHdr.run).
  Proof using.
    cStartTypedFunSim input.
    destruct input as [arguments state].
    destruct arguments as [| junk rest]; [| solve_arity_mismatch].
    expose_bodies.
    (** [op_lt] on two mathematical values reduces to [op_true], which selects
        the [then] arm on both sides.  The [else] arm -- the one holding
        [assert false], hence the UB -- is therefore unreachable, and this
        computation is the entire content of the theorem. *)
    cbn.
    ired.
    cStep. iSplit; done.
  Qed.

  Lemma sim : ISim.t open Source Target emp%I Ist.
  Proof using.
    cStartModSim.
    all: try solve [mod_tac].
    - apply simF_run.
  Qed.

  Lemma ctxr : ⊢ ctx_refines Target Source.
  Proof using. eapply main_adequacy, sim. Qed.
End AssertProof. End AssertProof.

(** Vacuity check.  Both sides are generated from the same file, so this proof
    would be worth little if it went through for a program whose assertion does
    fire.  It was checked against a negative control:

      # branchneg.ir -- the false assertion is now the reachable arm
      @main def main() = {
        if (< 2 1) assert true
        else assert false
        return 0
      }

      esmeta rocq-ir branchneg.ir
      cp Assert_branch.v $ESMETA_HOME/logs/rocq-ir/branchneg/func/
      cd $ESMETA_HOME/logs/rocq-ir/branchneg/func && make proofs

    That version fails in [simF_run] at the final [cStep], where the two sides
    have to agree: the target reaches [triggerUB] and the source returns.  It is
    not checked in because it cannot compile by design.

    What this check does *not* establish is the direction argument above.
    Swapping [Source] and [Target] should make the negative control provable --
    UB on the right is vacuous, and CRIS proves that with
    [isim_triggerUB_src_trigger] -- but this proof script reduces both sides to
    a syntactic match instead of invoking that rule, so the swapped version
    merely fails too.  The direction rests on the definition of [refines], not
    on an experiment. *)
