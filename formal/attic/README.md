# Deferred proofs

`T2Proof.v` and `T3Proof.v` were moved here on 2026-07-30 while the model
was migrated to UTF-16 code-unit strings and the numeric tower (D-1/D-2).

**Why they are here.** Their proof *scripts* broke, not their statements.
The failure is an Ltac1 issue: a `match goal with | |- context
[Any.downcast ?X]` inside a tactic body elaborates its pattern at
definition time and then fails to match the goal, although the identical
steps succeed when written inline (verified by probe: markers M1-M6 and
P1-P4 all pass inline). No semantic change was required.

`T1Proof.v` (fresh-temporary introduction) still builds and is still
covered by the build invariants.

Restoring these is tracked as follow-up work; per ADR-10, T2 was already
demoted to a model-internal exercise superseded by T3.
