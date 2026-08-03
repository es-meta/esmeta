(** * ESMetaFV.DirectITreeExec — execute complete direct function maps

    This module is the only bridge from generated direct Gallina functions to
    the existing explicit-call ITree machine.  Callers provide the complete
    ordinary and continuation maps.  The ordinary executable map is exactly
    the supplied funid map plus [entry] when a main body is supplied; the
    continuation executable map is exactly the supplied continuation map.

    In particular, there is no lookup in [ir_fnsems] or
    [ir_cont_fnsems], and therefore no generic-denoter fallback. *)

From CRIS Require Import CRIS.
From ESMetaFV Require Import Fragment Domain Semantics ITreeExec.

Set Implicit Arguments.

Definition direct_packed_body : Type :=
  @emask execΣ * (option (@fspec_rel execΣ) * @fbody execΣ).

Definition direct_fnsemmap : Type :=
  gmap fname (option direct_packed_body).

(** [ordinary] has only [funid] keys.  [main_entry = None] means that the
    closed program has no main; [Some body] installs the sole [entry] key. *)
Definition complete_direct_ordinary_map
  (ordinary : direct_fnsemmap) (main_entry : option direct_packed_body)
  : direct_fnsemmap :=
  match main_entry with
  | Some body => <[entry := Some body]> ordinary
  | None => ordinary
  end.

Definition direct_exec_lmod_with_trace
  (traced : bool) (mn : string) (p : prog)
  (ordinary : direct_fnsemmap) (main_entry : option direct_packed_body)
  : LMod.t := {|
  LMod.fnsems := map_imap (project_fnsem traced)
    (complete_direct_ordinary_map ordinary main_entry);
  LMod.initial_st := (ir_initial_st mn p, tt↑)
|}.

Definition direct_exec_cont_fnsems_with_trace
  (traced : bool) (continuations : direct_fnsemmap)
  : gmap fname (Any.t -> itree lmodE Any.t) :=
  map_imap (project_fnsem traced) continuations.

(** The state-preserving form is used by the Tier-A diagnostic projection. *)
Definition direct_compile_full
  (traced : bool) (mn : string) (p : prog)
  (ordinary : direct_fnsemmap) (main_entry : option direct_packed_body)
  (continuations : direct_fnsemmap)
  : itree coreE (lstateT * Any.t) :=
  compile_full
    (direct_exec_lmod_with_trace traced mn p ordinary main_entry)
    (direct_exec_cont_fnsems_with_trace traced continuations)
    tt↑.

Definition direct_exec_itree
  (mn : string) (p : prog)
  (ordinary : direct_fnsemmap) (main_entry : option direct_packed_body)
  (continuations : direct_fnsemmap) : itree coreE val :=
  sr <- direct_compile_full false mn p ordinary main_entry continuations;;
  v <- (entry_result (snd sr))?;;
  Ret v.

Definition direct_exec_itree_traced
  (mn : string) (p : prog)
  (ordinary : direct_fnsemmap) (main_entry : option direct_packed_body)
  (continuations : direct_fnsemmap) : itree coreE val :=
  sr <- direct_compile_full true mn p ordinary main_entry continuations;;
  v <- (entry_result (snd sr))?;;
  Ret v.
