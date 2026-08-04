(** * ESMetaFV.DirectITreeCore — Test262 runner core over the direct maps

    The generic runner core ([ITreeCore.make_test_tree]) executes a [prog]
    whose functions are IR data, so every step goes through [denote_inst].
    This module builds the same [test_tree] from the generated direct
    function maps instead: the program is still needed, but only for the
    initial state (globals, heap, source, AST, host answers) — its
    [p_funcs] field is never consulted, because [DirectITreeExec] installs
    the supplied maps as the executable maps with no [ir_fnsems] fallback.

    [INTRINSICS.Math.random] is absent from the maps, exactly as it is
    absent from the generic export: [ERandom] has no deterministic formal
    semantics, so calling it is UB rather than a wrong answer. *)

From Stdlib Require Import String List.
From CRIS Require Import ExtrOcamlCRIS.

From ESMetaFV Require Import
  Fragment Domain ITreeExec DirectITreeExec TestEncoding.
From ESMetaFV Require Import Spec ITreeCore.
From ESMetaFV.validation.spec_direct Require Import DirectFuncs.

Import ListNotations.

Set Implicit Arguments.
Local Open Scope string_scope.

(** The generated facade is generic in the resource algebra; executing it
    means instantiating it at the one the extracted runner uses. *)
Definition direct_ordinary_map : direct_fnsemmap :=
  @direct_ir_funid_fnsems execΣ TEST262_MODULE.

Definition direct_continuation_map : direct_fnsemmap :=
  @direct_ir_cont_fnsems execΣ TEST262_MODULE.

Definition direct_entry_body : option direct_packed_body :=
  snd (@direct_ir_entry execΣ TEST262_MODULE).

(** The program is consulted only through [ir_initial_st], which reads
    [p_source], [p_cached], [p_hosts], [p_heap], and the globals — never
    [p_funcs] (Semantics.v:1234).  The direct maps are supplied separately
    and [DirectITreeExec] installs them without an [ir_fnsems] lookup, so
    the IR function list is dead weight here and is left out: extraction
    then never reaches the generated [spec_funcs] at all. *)
Definition direct_script_prog
  (src : cstr) (a : ast) (hosts : list host_cache_entry) : prog :=
  mkProgFull nil (Some src) (Some a) hosts
    (("SOURCE_TEXT", VStr src) :: base_globals) init_heap.

Definition direct_production_exec (p : prog) : itree coreE val :=
  direct_exec_itree TEST262_MODULE p
    direct_ordinary_map direct_entry_body direct_continuation_map.

(** Tracing mirrors [ITreeCore]: it is built lazily and never consumes
    production fuel.  [trace_prog_func] is not applied, because it rewrites
    [p_funcs], which the direct maps do not read. *)
Definition direct_trace_exec (p : prog) : itree coreE val :=
  direct_exec_itree_traced TEST262_MODULE p
    direct_ordinary_map direct_entry_body direct_continuation_map.

Definition direct_make_test_tree
  (t : String.string * cstr * ast * list host_cache_entry *
       (val * list val)) : test_tree :=
  let '(name, src, a, hosts, expected) := t in
  let '(expected_result, expected_prints) := expected in
  let program := direct_script_prog src a hosts in
  mkTestTree
    name
    (direct_production_exec program)
    expected_result
    expected_prints
    (fun _ => direct_trace_exec program).
