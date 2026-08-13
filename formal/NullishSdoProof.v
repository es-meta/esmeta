(** * The generated CoalesceExpression Evaluation SDO

    This file connects the abstract nullish proof to the generated ECMA-262
    specification at the first mechanical boundary.  It builds an actual
    [CoalesceExpression] AST, enters through [ISdoCall "Evaluation"], and
    closes that call against the generated [spec_funcs] table.

    The two operand nodes are small synthetic SDOs.  They make this first
    witness closed and deterministic: the left operand evaluates to [null]
    and the right operand to [1].  Replacing those leaves by abstract,
    effectful operand modules is the next non-mechanical proof obligation. *)

From Stdlib Require Import String ZArith List.
From CRIS Require Import CRIS ExtrOcamlCRIS.

From ESMetaFV Require Import
  Fragment Domain Semantics ITreeExec NullishCoalescingProof.
From ESMetaFV.validation Require Import Spec.

Import ListNotations.

Set Implicit Arguments.
Local Open Scope string_scope.
Local Open Scope Z_scope.

(** ** Stable names *)

Definition coalesce_evaluation_name : string :=
  "CoalesceExpression[0,0].Evaluation".

Definition nullish_left_node_name : string :=
  "$ESMetaFV.NullishLeft".

Definition nullish_right_node_name : string :=
  "$ESMetaFV.NullishRight".

Definition nullish_left_evaluation_name : string :=
  "$ESMetaFV.NullishLeft[0,0].Evaluation".

Definition nullish_right_evaluation_name : string :=
  "$ESMetaFV.NullishRight[0,0].Evaluation".

Definition nullish_sdo_entry_name : string :=
  "$ESMetaFV.NullishSdoEntry".

Definition NULLISH_SDO_MODULE : string :=
  "ESMetaFV-Nullish-SDO".

(** ** The smallest AST that selects the real coalescing production

    ESMeta's parser represents the token [??] by the production metadata on
    this node; the token itself is not stored as an [ast] constructor.  The
    two children occupy the same indices used by the generated SDO body. *)

Definition nullish_left_ast : ast :=
  ASyn nullish_left_node_name nil 0 0 nil nil nil nil.

Definition nullish_right_ast : ast :=
  ASyn nullish_right_node_name nil 0 0 nil nil nil nil.

Definition coalesce_ast : ast :=
  ASyn "CoalesceExpression" nil 0 0
    [Some nullish_left_ast; Some nullish_right_ast]
    ["CoalesceExpressionHead"; "BitwiseORExpression"]
    nil nil.

Example coalesce_candidate_is_generated_evaluation :
  sdo_candidate coalesce_ast "Evaluation" = coalesce_evaluation_name.
Proof. reflexivity. Qed.

(** ** Closed operand SDOs and an SDO-dispatch wrapper *)

Definition temp_ref (index : nat) : expr :=
  ERef (RVar (VLocal (LTemp index))).

Definition normal_completion_leaf
  (name : string) (value : expr) : func :=
  mkFunc false name ["this"]
    (ISeq
      [ICall (LTemp 0) (EClo "NormalCompletion" nil) [value];
       IReturn (temp_ref 0)]).

Definition nullish_left_evaluation : func :=
  normal_completion_leaf nullish_left_evaluation_name ENull.

Definition nullish_right_evaluation : func :=
  normal_completion_leaf nullish_right_evaluation_name (EMath 1).

Definition nullish_sdo_entry : func :=
  mkFunc false nullish_sdo_entry_name ["this"]
    (ISeq
      [ISdoCall (LTemp 0)
         (ERef (RVar (VLocal (LName "this")))) "Evaluation" nil;
       IReturn (temp_ref 0)]).

Definition nullish_sdo_program : prog :=
  mkProgFull
    (nullish_sdo_entry ::
     nullish_left_evaluation ::
     nullish_right_evaluation ::
     spec_funcs)
    None None nil base_globals init_heap.

(** The actual function-name table resolves the root cursor to the generated
    CoalesceExpression SDO.  This calculation also guards against generated
    spec drift: changing or omitting that SDO makes the file fail to build. *)
Example coalesce_sdo_resolves_in_program :
  sdo_resolve_cursor
    (prog_fnames nullish_sdo_program)
    coalesce_ast nil "Evaluation"
  = Some (nil, coalesce_evaluation_name).
Proof. vm_compute. reflexivity. Qed.

Definition find_func_by_name
  (name : string) (functions : list func) : option func :=
  List.find (fun function => String.eqb name (f_name function)) functions.

Example generated_coalesce_function_is_found_by_stable_name :
  option_map f_name
    (find_func_by_name coalesce_evaluation_name spec_funcs)
  = Some coalesce_evaluation_name.
Proof. vm_compute. reflexivity. Qed.

Example left_operand_sdo_resolves_in_program :
  sdo_resolve_cursor
    (prog_fnames nullish_sdo_program)
    coalesce_ast [0%nat] "Evaluation"
  = Some ([0%nat], nullish_left_evaluation_name).
Proof. vm_compute. reflexivity. Qed.

Example right_operand_sdo_resolves_in_program :
  sdo_resolve_cursor
    (prog_fnames nullish_sdo_program)
    coalesce_ast [1%nat] "Evaluation"
  = Some ([1%nat], nullish_right_evaluation_name).
Proof. vm_compute. reflexivity. Qed.

(** Invoke the wrapper as an ordinary function, then let its [ISdoCall]
    perform the production lookup above.  Unlike [SpecAlgorithmITree.v]'s
    diagnostic tree this definition adds no tracing events. *)
Definition coalesce_source_tree : itree coreE val :=
  let program := nullish_sdo_program in
  let module := exec_lmod NULLISH_SDO_MODULE program in
  let cont_fnsems := exec_ir_cont_fnsems NULLISH_SDO_MODULE program in
  body <- ((LMod.fnsems module) !! (funid nullish_sdo_entry_name))?;;
  state_and_result <-
    exec_trans
      (LMod.prog module)
      (fun name => cont_fnsems !! (funid name))
      (body
        (((@nil (string * val)),
          [VAst (AstExported 0) coalesce_ast nil])↑))
      (LMod.initial_st module);;
  result <- (entry_result (snd state_and_result))?;;
  Ret result.

(** ** Checked shape of the generated SDO

    [sf_1387] is used only for these generated-artifact regression checks.
    The executable source tree above resolves by the stable function name. *)

Definition is_evaluation_of_child (index : Z) (instruction : inst) : bool :=
  match instruction with
  | ISdoCall _
      (ERef (RField (RVar (VLocal (LName "this"))) (EMath actual)))
      method args =>
      andb (Z.eqb index actual)
        (andb (String.eqb method "Evaluation")
          (Nat.eqb (List.length args) 0))
  | _ => false
  end.

Fixpoint inst_contains_child_evaluation
  (index : Z) (instruction : inst) : bool :=
  if is_evaluation_of_child index instruction then true else
  match instruction with
  | ISeq instructions =>
      existsb (inst_contains_child_evaluation index) instructions
  | IIf _ then_branch else_branch =>
      orb (inst_contains_child_evaluation index then_branch)
          (inst_contains_child_evaluation index else_branch)
  | IWhile _ body => inst_contains_child_evaluation index body
  | _ => false
  end.

Definition is_generated_nullish_test (condition : expr) : bool :=
  match condition with
  | EBinary BOr
      (EBinary BEq (ERef (RVar (VLocal (LName "lVal")))) EUndef)
      (EBinary BEq (ERef (RVar (VLocal (LName "lVal")))) ENull) => true
  | _ => false
  end.

Fixpoint inst_contains_nullish_test (instruction : inst) : bool :=
  match instruction with
  | ISeq instructions => existsb inst_contains_nullish_test instructions
  | IIf condition then_branch else_branch =>
      orb (is_generated_nullish_test condition)
        (orb (inst_contains_nullish_test then_branch)
             (inst_contains_nullish_test else_branch))
  | IWhile _ body => inst_contains_nullish_test body
  | _ => false
  end.

(** A stronger branch-sensitive check: the right child is evaluated in the
    nullish branch and is absent from the non-nullish branch. *)
Fixpoint inst_contains_guarded_right_evaluation
  (instruction : inst) : bool :=
  match instruction with
  | ISeq instructions =>
      existsb inst_contains_guarded_right_evaluation instructions
  | IIf condition then_branch else_branch =>
      if is_generated_nullish_test condition
      then
        andb (inst_contains_child_evaluation 1 then_branch)
          (negb (inst_contains_child_evaluation 1 else_branch))
      else
        orb (inst_contains_guarded_right_evaluation then_branch)
            (inst_contains_guarded_right_evaluation else_branch)
  | IWhile _ body => inst_contains_guarded_right_evaluation body
  | _ => false
  end.

Example generated_coalesce_function_has_stable_name :
  f_name sf_1387 = coalesce_evaluation_name.
Proof. reflexivity. Qed.

Example generated_coalesce_evaluates_left_child :
  inst_contains_child_evaluation 0 (f_body sf_1387) = true.
Proof. vm_compute. reflexivity. Qed.

Example generated_coalesce_contains_nullish_test :
  inst_contains_nullish_test (f_body sf_1387) = true.
Proof. vm_compute. reflexivity. Qed.

Example generated_coalesce_contains_right_evaluation :
  inst_contains_child_evaluation 1 (f_body sf_1387) = true.
Proof. vm_compute. reflexivity. Qed.

Example generated_coalesce_guards_right_evaluation :
  inst_contains_guarded_right_evaluation (f_body sf_1387) = true.
Proof. vm_compute. reflexivity. Qed.

(** The next theorem is intentionally not asserted here.  It must relate
    the completion-producing generated SDO to the value-level
    [nullish_source_tree], while abstracting the two operand SDOs and proving
    that the right operand is called only on the nullish branch. *)
