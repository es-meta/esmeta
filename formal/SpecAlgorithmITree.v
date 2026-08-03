(** * A standalone ITree for one generated ECMA-262 algorithm

    This is deliberately separate from the Test262 execution path.  It takes
    the actual [IsCallable] function exported to [validation/Spec.v], supplies
    one concrete argument, and closes that function against the exported
    specification function table and initial state.

    Instruction markers are enabled only for [IsCallable], so the extracted
    logger shows the IR path taken by this one algorithm without first running
    a JavaScript program. *)

From Stdlib Require Import String List.
From CRIS Require Import CRIS ExtrOcamlCRIS.

From ESMetaFV Require Import Fragment Domain ITreeExec.
From ESMetaFV Require Import Spec.

Import ListNotations.

Set Implicit Arguments.
Local Open Scope string_scope.

Definition SPEC_ALGORITHM_MODULE : string :=
  "ESMetaFV-ECMA262-Algorithm".

(** The specification-only program has no Test262 source, parsed AST, host
    query cache, or expected result.  Its functions and initial store are the
    generated ECMA-262 artifacts from [Spec.v]. *)
Definition spec_algorithm_program : prog :=
  mkProgFull spec_funcs None None nil base_globals init_heap.

(** Close one named generated IR function directly, rather than entering the
    specification's [RunJobs] main. *)
Definition exec_spec_algorithm
  (name : string) (args : list val) : itree coreE val :=
  let p := trace_prog_func name spec_algorithm_program in
  let ms := exec_lmod_with_trace true SPEC_ALGORITHM_MODULE p in
  let cont_fnsems :=
    exec_ir_cont_fnsems_with_trace true SPEC_ALGORITHM_MODULE p in
  body <- ((LMod.fnsems ms) !! (funid name))?;;
  sr <-
    exec_trans
      (LMod.prog ms)
      (fun fn => cont_fnsems !! (funid fn))
      (body (((@nil (string * val)), args)↑))
      (LMod.initial_st ms);;
  v <- (entry_result (snd sr))?;;
  Ret v.

Definition is_callable_name : string := "IsCallable".
Definition is_callable_argument : val := VUndef.

(** Concrete sample: ECMA-262 [IsCallable(undefined)].  The function is
    selected by its stable algorithm name from the generated [spec_funcs]
    table, so regenerating [Spec.v] may reorder [sf_*] definitions without
    changing this entrypoint. *)
Definition is_callable_itree : itree coreE val :=
  exec_spec_algorithm is_callable_name (is_callable_argument :: nil).
