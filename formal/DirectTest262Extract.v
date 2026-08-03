(** Extraction of the Test262 payload set under both backends.

    [Separate Extraction] is required, not stylistic: a monolithic extraction
    of both spec backends collides same-named types across modules (Rocq
    renames one to [..0]) and the result does not typecheck.  The
    configuration comes from [ExtractionConfig] for the same reason
    [ExtractCore.v] uses it — [utf16_hex] and [nat_decimal] have to be the
    OCaml overrides that read a [Pstring.t]. *)

From Stdlib Require Import Extraction.
From ESMetaFV Require Import ExtractionConfig DirectTest262 ITreeCore.

Set Extraction Output Directory "build/direct-t262".
Extraction Blacklist String List.
Separate Extraction
  generic_t262_trees direct_t262_trees observable_outcome_eqb.
