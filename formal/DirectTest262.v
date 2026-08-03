(** * ESMetaFV.DirectTest262 — the Test262 payload set under both backends

    [tests] is pure data: a name, the source, its AST, the host answers, and
    the observable ESMeta produced.  Nothing here is our expectation.  The
    two tree lists differ only in which semantics executes that data, so a
    disagreement between them is a direct-backend defect, and a disagreement
    with the payload's observable is a model defect. *)

From Stdlib Require Import String List.

From ESMetaFV Require Import Fragment Domain ITreeCore DirectITreeCore.
From ESMetaFV.validation Require Import Tests.

Definition generic_t262_trees : list test_tree :=
  List.map make_test_tree tests.

Definition direct_t262_trees : list test_tree :=
  List.map direct_make_test_tree tests.
