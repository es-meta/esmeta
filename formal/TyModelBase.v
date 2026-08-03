(** * ESMetaFV.TyModelBase — shared record-refinement syntax
  *
  * Regenerate with:
  *   sbt "run dump-debugger" && sbt "runMain esmeta.fv.FVTyModel"
  *
  * Exported from ESMeta's own type model ([parentOf] and [ownFieldsOf]),
  * rather than guessing either the hierarchy or descendant discriminants.
  * Pinned to ESMeta 0.7.3 with ecma262 @ 84b38ad8.
  *)

From Stdlib Require Import String ZArith List.
Import ListNotations.
From ESMetaFV Require Import Fragment.
Local Open Scope string_scope.

(** Runtime-checkable field constraints used by the structural descendant
    branch of [RecordTy.contains].  Heap containers retain the recursive
    [ValueTy] shape that ESMeta checks.  A target record stores both its name
    and its inline [FieldMap] refinements; the latter matter for anonymous
    records and explicitly refined named records. *)
Inductive record_field_constraint : Type :=
| RFCAny
| RFCNever
| RFCUnion (cs : list record_field_constraint)
| RFCEnum (names : list string)
| RFCEnumAny
| RFCStr
| RFCStrSet (values : list cstr)
| RFCBool (allow_false allow_true : bool)
| RFCMath
| RFCMathSign (allow_neg allow_zero allow_pos : bool)
| RFCMathIntSign (allow_neg allow_zero allow_pos : bool)
| RFCMathSet (values : list Z)
| RFCMathIntSet (values : list Z)
| RFCInfinity (allow_neg allow_pos : bool)
| RFCNumber
| RFCBigInt
| RFCUndef
| RFCNull
| RFCCodeUnit
| RFCClo
| RFCCloNames (names : list string)
| RFCCont
| RFCAst
| RFCAstNames (names : list string)
| RFCAstDetail (name : string) (idx : nat)
| RFCGrammarSymbol
| RFCRecordTop
| RFCRecord
    (targets :
      list
        (string *
          list (string * (bool * record_field_constraint))))
| RFCMapTop
| RFCMap
    (key_constraint value_constraint : record_field_constraint)
| RFCListTop
| RFCList (element_constraint : record_field_constraint)
(** A precise leaf constraint that this exporter cannot yet encode.
    Its executable decision is [None]/UB, never a permissive kind test. *)
| RFCUnsupported
(** Kept as an explicit conservative boundary for hand-written regression
    terms and stale generated models.  [FVTyModel] no longer emits it. *)
| RFCAddr.

Record record_field_binding : Type := mkRecordFieldBinding {
  rfb_name : string;
  rfb_absent : bool;
  rfb_constraint : record_field_constraint;
}.
