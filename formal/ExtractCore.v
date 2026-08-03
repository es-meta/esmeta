(** One-time extraction of the Test262-independent executable semantics.

    [Separate Extraction] is intentional here.  Rocq 9's whole-library
    extraction traverses CRIS proof-facing declarations and rejects a
    sort-polymorphic [prod] instance in [Prop].  Starting from the two
    executable entrypoints produces the same module-oriented dependency
    closure while erasing those proof-only declarations. *)

From Stdlib Require Import Extraction.
From ESMetaFV Require Import
  ExtractionConfig ITreeCore SpecAlgorithmITree TestEncoding.

Set Extraction Output Directory "build/itree/core".
Extraction Blacklist String List.
Separate Extraction
  make_test_tree vals_eqb observable_outcome_eqb utf16_hex nat_decimal
  is_callable_itree.
