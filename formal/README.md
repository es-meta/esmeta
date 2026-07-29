# ESMetaFV — Formal verification of IR-level transpilation

A Rocq (Coq) development proving semantic preservation of program
transformations over a small fragment ("IR-Core") of the ESMeta IR,
using interaction trees and the CRIS refinement framework.

Architecture, design rationale (ADRs), proof-obligation ledger, and the
research log live in [`../docs/formal-verification/`](../docs/formal-verification/).

## What is (and is not) claimed

This development covers a **small fragment** of ESMeta IR (see
`Fragment.v` and the limitations section of the architecture note). It
does **not** formalize ECMAScript, full ESMeta IR, or JavaScript-level
transpilation. Theorems are stated relative to the ITree model defined
here; the connection to ESMeta's executable semantics is established by
differential testing (Milestone 3) and is *testing, not proof*, unless a
faithfulness theorem is explicitly stated.

## One-time setup

Requires [opam](https://opam.ocaml.org/) ≥ 2.1. The toolchain is the same
pinned snapshot used by the CRIS workshop:

```sh
opam switch create cris-workshop ocaml-base-compiler.4.14.1
eval "$(opam env --switch=cris-workshop --set-switch)"
opam repo add rocq-released https://rocq-prover.org/opam/released
opam pin add -y rocq-cris \
  'git+https://github.com/snu-sf/CRIS.git#c0bcd04e7ddfed32f1d7b8e5e2e328e3b5957bdd'
```

This installs Rocq 9.0.0, coq-itree 5.2.1, coq-paco 4.2.3, coq-iris 4.4.0,
coq-stdpp 1.12.0, coq-ext-lib 0.13.0, coq-ordinal 0.5.6, and the pinned
CRIS framework. Verify with `coqc --version` (expect 9.0.0).

## Build

```sh
eval "$(opam env --switch=cris-workshop --set-switch)"   # each new shell
cd formal
make          # builds all .v files
make clean
```

Editors (VsRocq / Coqtail / Proof General) pick up `_CoqProject` when the
editor is opened from this directory with the opam switch active.

## Layout

| File | Content |
|---|---|
| `Fragment.v` | IR-Core syntax, mirrored from `src/main/scala/esmeta/ir/` (framework-agnostic; stdlib only) |
| `Domain.v` | Pure semantic domain: completions, operator evaluation, environments, heap objects (stdlib only) |
| `Events.v` | Observable-event interface: how fragment effects map onto CRIS events |
| `Semantics.v` | ITree denotation of IR-Core + CRIS module packaging (`ir_mod`) + first completion/effect lemmas |
| `Programs.v` | Hand-mirrored corpus programs (`sum`, `gcd`, `fibo`) + an effectful print program (stdlib only) |
| `Examples.v` | The corpus programs packaged as CRIS modules; denotation-level effect-order lemma |
| `Exec.v` | Fuel-based executable reference interpreter mirroring the denotation clause by clause (stdlib only; validation role — see its header) |
| `Transform.v` | T-1, fresh-temporary introduction: the transformation function, decidable freshness, and `fresh_temp_is_fresh` (stdlib only) |
| `Validation.v` | `vm_compute` runs of the corpus + `t1_prog` trace preservation + effect-sensitivity and negative-transformation tests (testing, not proof) |
| `T1Proof.v` | **The Milestone 4 theorem**: mutual contextual refinement of `ir_mod mn p` and `ir_mod mn (t1_prog p)` for the effectful exemplar, over all linking contexts; ends with a build-time `Print Assumptions` axiom audit |
| `T2Proof.v` | **The T-2 theorem**: mutual contextual refinement of the optional-field desugaring (`t2_prog`, ADR-9) for the effectful exemplar — full receiver case analysis incl. the nullish guard and abstract-store field reads; build-time axiom audit |

## Differential validation (Milestone 3)

```sh
# from the repo root, with ESMETA_HOME set:
sbt "runMain esmeta.fv.FVExport"   # exports fragment-compatible tests/ir
                                   # programs + ESMeta-observed expectations
cd formal && make validate         # re-runs them under Exec.v via vm_compute
```

`FVExport` (in `src/main/scala/esmeta/fv/`, isolated from the rest of
ESMeta) translates each fragment-compatible `tests/ir/*.ir` program to a
Rocq term, executes it with ESMeta's interpreter capturing `IPrint`
values and the final `RESULT` global, and writes
`formal/validation/Generated.v` (git-ignored). Compiling that file checks
that `Exec.v` reproduces the same observables; any mismatch fails the
build. Out-of-fragment programs are skipped with per-construct reasons
printed by the exporter.

Milestone 4 adds the transformation and its equivalence proof.

## Conventions

- No `Admitted`/`admit`/`Axiom` in completed milestones. Work-in-progress
  admits are allowed only in files named `*_wip.v`, which must be removed
  before a milestone is declared complete.
- Every non-obvious design choice references an ADR number from the
  architecture note.
- New IR constructs: extend `Fragment.v`, mirror the exact constructor
  semantics from the Scala sources cited in that file's header, and record
  any deviation as a new ADR + limitation entry.
