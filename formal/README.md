# ESMetaFV — Formal verification of IR-level transpilation

A Rocq (Coq) development proving semantic preservation of program
transformations over an executable model of ESMeta IR, using interaction
trees and the CRIS refinement framework. The same denotation is extracted to
OCaml for large generated-specification and Test262 runs.

Architecture, design rationale (ADRs), proof-obligation ledger, and the
research log live in [`../docs/formal-verification/`](../docs/formal-verification/).

## What is (and is not) claimed

The transformation theorems are stated relative to the ITree model defined
here. The executable mirror now covers most generated-spec IR and can run the
eligible Test262 pool, but this is not itself a proof that the model is
faithful to ESMeta or ECMAScript. That connection is differential testing
unless a faithfulness theorem is explicitly stated.

`JSEquivProof.v` goes one step beyond a hand-mirrored IR example. ESMeta's real
Script frontend processes six JavaScript files. Four are retained as frontend
preservation witnesses. The remaining two have distinct raw bytes but become
the same effective source, AST, and typed host-answer cache after ESMeta's
automatic-semicolon-insertion pass. The generator checks those equalities
before emitting aliases, and Rocq proves the resulting closed `exec_itree`
trees `eutt` by exact prepared-program equality. The theorem
`asi_optional_chain_closed_js_equiv` is compiled by `make js-equiv`.

The claim is deliberately narrow: equivalence of this ASI-canonicalized pair,
not arbitrary JavaScript contexts, optional-chain/guard equivalence, or
correctness of the parser/exporter. Parsing, host-answer capture, and artifact
generation remain a trusted frontend boundary.

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
CRIS framework. Verify with `coqc --version` (expect 9.0.0). This switch is
sufficient for the core, runner, and JavaScript-witness builds.

## Build

```sh
eval "$(opam env --switch=cris-workshop --set-switch)"   # each new shell
cd formal
make          # builds the core IR model and active transformation proof
make check    # also compiles the complete active regression/proof surface
make clean    # build/generated artifacts only; preserves campaign logs
# make purge-runs CONFIRM=YES   # explicit destruction of archived run evidence
```

`make check` includes the compiled `JSEquivProof.v` theorem through the
regression target. `make` alone builds the smaller core surface.

Editors (VsRocq / Coqtail / Proof General) pick up `_CoqProject` when the
editor is opened from this directory with the opam switch active.
For MCP-driven interactive sessions (stepping proofs, executing fragment
programs in the model) and the Test262 execution boundary, see
[INTERACTIVE.md](INTERACTIVE.md).

### Inspect one generated ECMA-262 algorithm ITree

The standalone target below does not run Test262 or enter `RunJobs`.  It
directly closes the generated `Spec.v::spec_funcs` IR function for
`IsCallable(undefined)` against the exported specification state and writes
its instruction-marked ITree shape. The lookup uses the stable algorithm name
inside `spec_funcs`, so it does not depend on the generated `sf_*` index:

```sh
make itree-spec-algorithm
less logs/itree-spec-IsCallable.log
```

Set `ITREE_ALGORITHM_LINES=N` to change the shape-line limit.

## Layout

| File | Content |
|---|---|
| `Fragment.v` | IR-Core syntax, mirrored from `src/main/scala/esmeta/ir/` (framework-agnostic; stdlib only) |
| `Domain.v` | Pure semantic domain: completions, operator evaluation, environments, heap objects (stdlib only) |
| `Events.v` | Observable-event interface: how fragment effects map onto CRIS events |
| `Semantics.v` | ITree denotation of IR-Core + CRIS module packaging (`ir_mod`) + first completion/effect lemmas |
| `ITreeExec.v` | executable ITree call machine used by extraction and proofs |
| `ITreeCore.v` | Test262 wrapper and observable comparison; production tree is untraced |
| `JSClosedEquiv.v` | Packages exported JavaScript source/AST/host answers as `script_prog`, runs `exec_itree`, and connects finite silent completion to upstream ITree `eutt` |
| `JSEquivProof.v` | Frontend-preservation checks for four real `.js` files plus the compiled ASI-canonicalized closed `eutt` theorem for two byte-distinct inputs |
| `validation/JSEquivArtifacts.v` | Generated effective source, parsed AST, and typed host answers for the JavaScript equivalence witnesses |
| `SpecAlgorithmITree.v` | Standalone closed ITree for the generated ECMA-262 `IsCallable` IR function; independent of Test262 |
| `Programs.v` | Hand-mirrored corpus programs (`sum`, `gcd`, `fibo`) + an effectful print program (stdlib only) |
| `Examples.v` | The corpus programs packaged as CRIS modules; denotation-level effect-order lemma |
| `Exec.v` | Fuel-based executable reference interpreter used only by validation/proof computations; it is not linked into the production Test262 worker |
| `Transform.v` | T-1, fresh-temporary introduction: the transformation function, decidable freshness, and `fresh_temp_is_fresh` (stdlib only) |
| `Validation.v` | `vm_compute` runs of the corpus + `t1_prog` trace preservation + effect-sensitivity and negative-transformation tests (testing, not proof) |
| `T1Proof.v` | **The Milestone 4 theorem**: mutual contextual refinement of `ir_mod mn p` and `ir_mod mn (t1_prog p)` for the effectful exemplar, over all linking contexts; ends with a build-time `Print Assumptions` axiom audit |
| `attic/T2Proof.v`, `attic/T3Proof.v` | Archived proofs for superseded optional-field experiments; not part of the active build |

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

## Closed JavaScript ITree witnesses

From the repository root, regenerate the artifacts with ESMeta's Script parser
and ordinary interpreter, then compile the Rocq proof:

```sh
cd formal
make js-equiv
```

The six inputs are under `tests/fv/js-equiv/`. The generator preserves both
the file contents and ESMeta's effective source in the artifact, rejects a
non-normal ESMeta exit or unexpected print, and records the parsed AST plus
typed host answers. For the ASI pair it additionally requires distinct raw
bytes and exact equality of effective source, AST, and host answers before it
may emit the aliases used by the proof. The proof builds each closed program
as:

```text
script_prog effective_source parsed_ast host_answers
  → exec_itree
  → prepared-program equality
  → eutt
```

The compiled theorem is `asi_optional_chain_closed_js_equiv`. Its fixture
still makes duplicate receiver evaluation observable—the helper throws if
called twice—but this theorem obtains equality at the frontend canonicalization
boundary; it does not separately execute the optional chain and a handwritten
guard. The earlier constant-condition and optional-chain/guard pairs remain
artifact-preservation checks only. Attempting to reify their full executions
with `vm_compute`/`native_compute` was not practical, so the stronger closed
computational equivalences remain open.

This proof path does not import `Exec.v`. Keep `Exec.v` as the independent
fuel-based validation oracle and proof-computation regression layer until
PO-013, or an equivalent direct ITree-validation theorem, discharges that
role.

## Test262 production execution

`run-test262-full.py` is the production runner. It starts one persistent ESMeta
exporter JVM and persistent native OCaml workers, then executes compact
per-test payloads with the extracted `Semantics.v`/`ITreeExec.v` denotation.
`Exec.v` remains an independent validation oracle; it is not the production
Test262 engine. The modular audit driver and persistent worker share
`itree_test_runtime.ml` for execution and verdict formatting. See
[TEST262_FULL_RUNNER.md](TEST262_FULL_RUNNER.md).

## Conventions

- No `Admitted`/`admit`/`Axiom` in completed milestones. Work-in-progress
  admits are allowed only in files named `*_wip.v`, which must be removed
  before a milestone is declared complete.
- Every non-obvious design choice references an ADR number from the
  architecture note.
- New IR constructs: extend `Fragment.v`, mirror the exact constructor
  semantics from the Scala sources cited in that file's header, and record
  any deviation as a new ADR + limitation entry.
