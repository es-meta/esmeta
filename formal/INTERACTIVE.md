# Driving the ITree model interactively (Coqtail-MCP)

The [Coqtail-MCP](https://github.com/park-sunho/Coqtail-mcp) server lets an
AI session (or you, via any MCP client) drive Rocq interactively against
this development: step through the proofs, inspect goals, and **execute
IR-Core programs inside the formal model**.

## Setup (done once; recorded here for reproducibility)

```sh
cd /path/to/LAB && git clone https://github.com/park-sunho/Coqtail-mcp
cd Coqtail-mcp && python3 -m venv .venv
.venv/bin/pip install -e . 'mcp>=1.2,<2'   # NOTE: mcp 2.x removed
                                            # mcp.server.fastmcp — pin <2
claude mcp add coqtail \
  --env "PATH=$HOME/.opam/cris-workshop/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin" \
  -- /path/to/LAB/Coqtail-mcp/.venv/bin/coqtail-mcp
```

The `PATH` override makes the server spawn `coqidetop` from the pinned
`cris-workshop` opam switch. Sessions started on files in this directory
pick up `_CoqProject` automatically (`-Q . ESMetaFV` + the CRIS flags).
Build the project first (`make`) so the `.vo` dependencies exist.

## Executing programs in the model

Open a session on any file here (`rocq_start`), then use `rocq_query`:

```coq
(* run a corpus program under the executable model semantics *)
Eval vm_compute in (run 1000 gcd_prog).
  (* = Ok (VUndef, nil) — normal termination, no prints *)

(* run the OUTPUT OF A VERIFIED TRANSFORMATION *)
Eval vm_compute in (run 1000 (t1_prog gcd_prog)).
Eval vm_compute in (run 1000 (t2_prog t2v_rec)).   (* from Validation.v *)

(* inspect a theorem or its assumptions *)
Check t1_contextual_equivalence.
Print Assumptions t2_contextual_equivalence.
```

`run fuel p : out (val * list val)` returns the termination value and the
print trace (the observable image of a `Tr.done` behavior); `Stuck` = UB,
`OOF` = fuel exhausted (raise the bound). Equivalent batch form, no MCP
needed:

```sh
printf 'From ESMetaFV Require Import Fragment Domain Exec Programs Transform.
Eval vm_compute in (run 1000 (t1_prog gcd_prog)).\n' \
  | coqtop -q -Q . ESMetaFV -require-import ExtLib.Structures.Monad
```

For proof work, `rocq_step_to` + `rocq_goals` replace the batch
goal-printing probes used during development (see the research log,
"Failed Attempts", M4).

## What can and cannot run in the model — the Test262 boundary

**Test262 does NOT run on the ITree/CRIS model, by design.**
The honest execution matrix:

| Program class | ESMeta interpreter | ITree model (this dir) |
|---|---|---|
| Test262 / arbitrary JavaScript | ✔ (`sbt test262EvalTest`, `esmeta test262-test` — full spec-derived IR) | ✘ — out of fragment |
| Fragment-compatible standalone IR (`tests/ir`, 19 programs) | ✔ | ✔ (`run` via vm_compute; differential harness `make validate`) |
| Outputs of the verified transformations (`t1_prog`, `t2_prog`) | ✔ (post-desugar IR-Core, exportable) | ✔ |
| Synthetic source forms (`EOptField`) | ✘ (not ESMeta syntax; ADR-9) | ✔ |

Why: a Test262 test is JavaScript. ESMeta executes it by running the
*entire spec-compiled IR* (thousands of functions) over the parsed AST as
data — that machinery (AST values, `ISdoCall`/`EParse`, completion
records at spec level, IEEE-754 Numbers, strings, …) is exactly what the
IR-Core fragment excludes (architecture note, limitations L-1/L-4/L-5).
Making Test262 run on the model would mean mechanizing the spec-derived
IR itself — the [FW] JavaScript-level route (architecture note §1), not a
configuration task. Claiming otherwise would violate the project's
claim-classification rules, so we don't.

What the model-side execution IS for: exercising the proven fragment,
running transformation outputs before/while proving them equivalent, and
regression-checking the semantics against ESMeta on the shared corpus
(`sbt "runMain esmeta.fv.FVExport"` + `make validate`).
