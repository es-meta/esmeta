# Full Test262 ITree runner

`run-test262-full.py` drives the extracted ITree semantics over the complete
target inventory:

1. build/extract the source-independent Rocq semantics and `Spec.v` once;
2. one persistent `FVInitState --test262-server --payload-only` JVM keeps
   ESMeta's CFG and Test262 corpus loaded and writes compact test tuples;
3. persistent `fvitree-worker` processes decode each tuple, call the
   Rocq-extracted `ITreeCore.make_test_tree`, and execute the resulting ITree.

The persistent worker and the modular audit driver both use
`itree_test_runtime.ml` for tree execution, verdict category, reason, and line
formatting. The modular driver no longer refers to the nonexistent
`exec_diagnostic`; diagnostics stay on the ITree call-path and instruction
marker boundary.

The production path has no per-test or per-shard Rocq typecheck, extraction,
OCaml compilation, link, JVM launch, or ESMeta CFG initialization. The old
Rocq payload path remains as an independent audit oracle:
`make payload-roundtrip` compares its extracted tuples with the decoded
compact payloads field by field.

The inventory currently records 32,207 target tests and 18,618 tests filtered
before export.

## Current campaign evidence

The preserved full-campaign result under
`logs/test262-full-20260801-v2/summary.txt` predates the latest semantic and
payload fixes:

| Result | Count |
|---|---:|
| PASS | 27,238 |
| ESMETA_FAILED | 4,359 |
| NOT_REPRESENTABLE | 436 |
| UNSUPPORTED | 174 |
| MISMATCH / BUILD_ERROR / CRASH / TIMEOUT | 0 |

The recorded v7 worker revision was subsequently run against the union of those 436
`NOT_REPRESENTABLE` and 174 `UNSUPPORTED` cases. The targeted 610-test result
under `logs/test262-archived-failures-final-v2-20260802/summary.txt` is 609
PASS and one `UNSUPPORTED`, with every other outcome zero. The final 16-case
residual run under `logs/test262-residual-final-v2-20260802/summary.txt` is 15
PASS and the same one `UNSUPPORTED`.

The remaining case is `built-ins/Math/random/S15.8.2.14_A1.js`; it reaches the
unhandled proof-oriented `Take` effect and fails closed as UB. Supporting it
requires an explicit nondeterministic `Take`/`Choose` policy. Replaying one
sampled random value is not a sound implementation.

No full 32,207-test campaign has been rerun after these fixes. Do not combine
the targeted 609/610 result with the old full-campaign totals to report a new
full-suite PASS count.

## Safe first run

From `formal/`:

```sh
make t262-full-dry
./run-test262-full.py --smoke \
  --state-dir logs/test262-smoke
```

The dry run first validates the reusable base. If `Spec.v`, the inventory, and
`validation/test262-base.json` match the current source fingerprint plus
inventory digest, no exporter process is started. Otherwise it runs one empty
payload-only shard, then regenerates and restamps the base.
It does not build or execute the model, create campaign metadata, or write
result checkpoints. Generated paths are protected by the same repository-wide
lock as a real run.

The smoke run processes at most two target tests. On the 16-core, 64-GB
development machine, the measured full-run configuration is:

```sh
./run-test262-full.py \
  --jobs 16 \
  --export-jobs 16 \
  --state-dir logs/test262-full
```

`--jobs` is both the number of persistent OCaml workers and the maximum number
of extracted ITree executions in flight. `--export-jobs` is the number of
parallel ESMeta oracle/export tasks inside the one persistent JVM. Each OCaml
worker loads the source-independent specification once and then handles many
payloads. Use smaller values on a lower-core or lower-memory machine.
`--shard-size` controls checkpoint and temporary-payload granularity; its
default is 1,280 and it no longer causes JVM restarts. A measured 320-test
shard produced 426 MiB of payloads, so this default is intended for the 64-GB
development machine and trades finer recovery granularity for lower fixed
overhead.

On a 160-test deterministic benchmark, 16-way fused export reduced exporter
time from 116.983 s to 13.694 s (8.54x) while preserving the manifest and every
payload byte. A separate 320-test non-Array end-to-end sample completed in
about 92 s including startup, with 297 PASS, one ESMETA_FAILED, 18
NOT_REPRESENTABLE, four UNSUPPORTED, and no MISMATCH, BUILD_ERROR, CRASH, or
TIMEOUT. Those measurements predict roughly two hours for the full target
inventory; allow two to three hours for corpus-dependent runtime variance.

Timeouts and fuel are independently configurable:

```sh
./run-test262-full.py \
  --export-timeout 900 \
  --build-timeout 1800 \
  --run-timeout 900 \
  --fuel 100000000
```

Use `--prefix language/`, `--start OFFSET`, and `--count COUNT` for a bounded
subset. `STATE/campaign.json` locks the selected global indices, paths, and
execution policy (shard size, worker count, fuel, and export/build/run
timeouts) so a state directory cannot silently mix results from another
campaign.
The campaign also records a deterministic fingerprint of the Scala exporter
and semantics sources, every file under `src/main/resources` (including
`.ir`, `.algo`, and extensionless semantic data), the explicit production
Rocq/OCaml dependency closure, every generated TyModel shard named by the
managed `_CoqProject` block, every generated Spec facade/shard named by
`SpecSources.mk`, and the `client`, `ecma262`, and `tests/test262` revisions
plus dirty tracked or untracked source content. Missing, duplicate, or
non-contiguous generated shards and facade/manifest drift fail before a
campaign can be reused.
Validation-only `Exec.v`, `modular_driver.ml`, and `gen-extract-shard.sh` are
deliberately outside the production closure and do not invalidate a campaign.
The generated semantic TyModel/Spec shards listed above are included;
per-test payload modules, modular-audit extraction shards, build products, and
logs are excluded. Any
production source, resource, or Test262 change rejects reuse with an
actionable error; an identical dirty worktree produces the same fingerprint
and resumes normally. Use a separate state directory for a logically separate
campaign.
The runner rechecks this fingerprint before and after every shard. If it
changes during a run, `STATE/source-drift.json` permanently quarantines that
state directory so already-written checkpoints cannot later be trusted after
the worktree is restored.

Campaign metadata created before the current source and execution-policy
identity was added cannot safely prove which semantics and timeout policy
produced its checkpoints. Such state directories, and legacy checkpoint
directories without `campaign.json`, are rejected. Start a new state
directory; retain the old directory only as an archived report.

## Checkpoint and output contract

Every terminal test result is atomically written to
`STATE/results/GLOBAL_INDEX.tsv`. On restart, those global indices are
removed before pending work is divided into contiguous shards. Thus a
partially completed old shard does not re-export or rerun its completed
indices.

Every checkpoint is validated against both the selected global index and its
Test262 relative path before it is counted as completed. Likewise, each
exported shard manifest must contain exactly the selected index/path pairs.
Foreign, stale, duplicate, or renamed rows become an export integrity error
instead of being reused. Each payload also carries its global Test262 index
and relative path; the OCaml worker checks both against the requested test
before constructing an ITree.

At startup, after every shard, and at clean shutdown, two deterministic,
atomically replaced aggregates are generated:

- `STATE/results.tsv`, sorted by global target index
- `STATE/summary.txt`, with stable alphabetical status rows

Exporter outcomes retain `ESMETA_FAILED` and `NOT_REPRESENTABLE`. Emitted
tests receive one of `PASS`, `MISMATCH`, `UNSUPPORTED`, `TIMEOUT`, `CRASH`,
or `BUILD_ERROR`. Result, print, and AST-alias differences are all
`MISMATCH`. Per-test command transcripts are retained only for non-PASS
results; exporter/build transcripts and per-worker stderr stay under
`STATE/logs/`. `exporter-session.log` records the single JVM session and every
`READY`, `DONE`, `ERROR`, and `BYE` protocol marker. A transient shard-level
export timeout or process failure aborts without writing terminal checkpoints,
so the next invocation can retry it. A deterministic per-test build failure is
`BUILD_ERROR`; a caught exception inside the extracted driver is `CRASH`. An
external runtime timeout and the driver's out-of-fuel verdict are both
`TIMEOUT`.

The runner does not clean the repository, archived campaign logs, or the
extracted core cache. `make clean` also preserves `formal/logs`; deleting run
evidence requires the explicit `make purge-runs CONFIRM=YES` target.
Worker construction uses the same advisory build lock as modular audit builds,
so a manual extraction cannot concurrently rewrite the shared core. Test262
control options also use the canonical default source when generating
`Spec.v`; content-preserving regeneration leaves its timestamp unchanged and
therefore does not trigger a native-core rebuild.
`build/itree/core/.compiled` remains available to later shards.
The initial inventory bootstrap also writes the source-independent `Spec.v`.
The production exporter is then launched directly with `java -cp`, not through
one `sbt runMain` per shard. Its compile classpath is queried once and cached in
`build/test262-exporter-classpath.json`, keyed by the same source fingerprint.
The server reuses the specification, inventory, CFG, and Test262 corpus while
still rerunning ESMeta and regenerating each selected test's AST, host inputs,
and expected observable.

Compact payloads use a strict, versioned decoder with bounded lengths and
constructor tags. ECMAScript strings retain exact UTF-16 code units; integers
use exact decimal encoding; finite doubles retain their IEEE-754 bits,
including negative zero, while NaN follows Rocq's canonical `PrimFloat.nan`.
The current payload magic is `ESFVIT07`; v7 includes typed Number/Math host
queries and `LVUndef` for the Test262 `TV = undefined` value. Files are
atomically replaced. Malformed, truncated, trailing, invalid UTF-8, or
out-of-range data is rejected before execution.

## Test262 assertions

Assertions are not translated to the Rocq `IAssert` constructor. ESMeta's
`Test262.loadTest` prepends `harness/assert.js`, `harness/sta.js`, and each
test's requested harness files to the test source. That combined JavaScript is
parsed to the same AST/IR as the test and executed by the same extracted ITree
semantics. A successful assertion simply returns. A failed assertion throws a
JavaScript `Test262Error`, so the script's final `RESULT` is not `undefined`.

The exporter accepts an ESMeta oracle only when its final `RESULT` is exactly
`undefined`, matching the normal ESMeta Test262 runner. The extracted drivers
also reject any legacy/generated test tuple whose expected result is not
`VUndef`; it is reported as `CRASH invalid-oracle-*` and can never become a
false `PASS`.

## Inspecting one generated ITree

The shape dumper intentionally uses the dual-output audit path. Export
without `--payload-only`, build that diagnostic shard, then dump one tree:

```sh
sbt "runMain esmeta.fv.FVInitState --test262-shard 0 10 --reuse-test262-base"
make itree-shard
./fvitree-modular 100000000 logs/itree-shape.log \
  --only 0 \
  --trace-func RunJobs \
  --dump-itree-shape 200
```

The log compresses consecutive internal `Tau` nodes and shows visible I/O,
function entry/exit markers, selected IR-instruction markers, and the final
`Ret`. `Take` and `Choose` remain visible as unsupported proof-oriented
effects; the Test262 execution handler does not assign them operational
meaning.

## Concurrency

`--jobs N` starts N core-only OCaml workers and runs up to N emitted tests at
once. Workers stay alive across every shard, eliminating repeated
specification loading and process startup. Each request has its own timeout;
a timed-out worker is terminated and replaced without stopping other workers.

`--export-jobs N` runs the per-test Initialize, ESMeta oracle, assertion gate,
and compact-payload encoding pipeline on N JVM workers. Only the known
non-thread-safe parser/load boundary and dynamic `EParse` calls use a narrow
shared lock. Results are committed in inventory order, so parallel and serial
export produce byte-identical manifests and payloads. All shard requests use
one persistent JVM. An exporter timeout terminates that JVM; a later shard may
start a replacement without changing completed checkpoints.
A repository-wide atomic lock at `build/test262-full.lock` prevents two
runner processes (including processes using different state directories)
from racing on those paths.

The dual-output audit mode still typechecks and extracts each Test262 tuple
from its own Rocq module, but that work is not part of a production campaign.
Its purpose is to prove that compact transport did not change the tuple
supplied to the extracted semantics.

## Runner audit evidence

The final runner audit passed all 40 runner tests and all 50 tests in the
complete `formal/tests` Python suite. These include fingerprint regressions
showing that `.ir`, `.algo`, extensionless resource changes, and changes to
generated TyModel/Spec shards invalidate a campaign, while `.vo`, logs, and
the validation-only files above do not.

A fresh source build and link of the modular lane executed representative
T000 with the shared runtime policy and produced PASS after 32,440 steps. The
shape dumper independently displayed the same closed computation as:

```text
Tau x 32440
Ret VUndef
```

This checks build/link freshness, shared verdict formatting, and the absence
of a visible effect on that representative path. It is a runner regression,
not a replacement for the Test262 campaign evidence above.
