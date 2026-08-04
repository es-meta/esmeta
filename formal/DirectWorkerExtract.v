(** Direct-only extraction for the Test262 runner.

    [direct_make_test_tree] is extracted as a function, not applied to a
    baked-in test list.  Payloads are decoded at run time by
    `payload_codec.ml`, exactly as the generic worker does, which means the
    number of tests is a run-time argument: no rebuild to run more of them.

    That also removes the two things that blocked `ocamlopt`:
      - the per-test `T*.ml` modules (~3 MB of AST data each; `T004.ml` is
        what overflowed the compiler's stack), and
      - the generated `spec_funcs`, which [direct_script_prog] leaves out of
        the program because [ir_initial_st] never reads [p_funcs].

    Verdicts are still checked: each payload carries the observable ESMeta
    produced, and [observable_outcome_eqb] compares against it.  Agreement
    with the generic denoter is established separately by [DirectTest262]. *)

From Stdlib Require Import Extraction.
From ESMetaFV Require Import
  ExtractionConfig TestEncoding DirectITreeCore ITreeCore.

Set Extraction Output Directory "build/direct-worker".
Extraction Blacklist String List.
Separate Extraction
  direct_make_test_tree observable_outcome_eqb vals_eqb utf16_hex nat_decimal.
