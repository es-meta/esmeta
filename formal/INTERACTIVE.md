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
Print Assumptions t1_contextual_equivalence.
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

## Interactive proofs and full Test262 execution

There are two deliberately separate execution lanes:

| Program class | Interactive/reference lane | Production ITree lane |
|---|---|---|
| Fragment-compatible standalone IR | `Exec.run` via `vm_compute`; `make validate` | extracted `ITreeExec.exec_itree` |
| Generated ECMA-262 algorithms | inspect/prove against `validation/Spec.v` | extracted `Semantics.v`/`ITreeExec.v` |
| Test262 JavaScript | impractical to reduce interactively | `run-test262-full.py` using compact payloads and persistent native workers |

The full runner parses each JavaScript test with ESMeta, executes ESMeta once
to obtain the expected observable, exports the parsed AST plus host answers,
and executes the generated ECMA-262 IR through the extracted ITree denotation.
See [TEST262_FULL_RUNNER.md](TEST262_FULL_RUNNER.md) for the exact assertion,
checkpoint, and verdict contract.

`Exec.v` remains a small fuel-based validation oracle and is useful in proofs
and differential regression tests. It is not the production Test262 engine.
Likewise, Coqtail is the right tool for goals and bounded examples, but not for
reducing the multi-megabyte generated specification test by test.
