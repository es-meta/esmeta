# ITree-Based Verification of IR-Level Transpilation in ESMeta — Architecture Note

Status: Milestone 1 (architecture + skeleton). Last updated 2026-07-29.

Version pins this document assumes:

| Component | Version |
|---|---|
| ESMeta | 0.7.3 (`build.sbt`, `src/main/scala/esmeta/package.scala`) |
| ecma262 submodule | `84b38ad8` (tag es2025) |
| Rocq | 9.0.0 |
| CRIS | opam `rocq-cris` pinned to `snu-sf/CRIS@c0bcd04e7ddfed32f1d7b8e5e2e328e3b5957bdd` |
| coq-itree / coq-paco / coq-iris / coq-stdpp / coq-ext-lib / coq-ordinal | 5.2.1 / 4.2.3 / 4.4.0 / 1.12.0 / 0.13.0 / 0.5.6 |

Claim classification used throughout (mandatory project rule):
**[RF]** repository fact (verified in source) · **[VF]** verified by running a
command · **[PF]** paper-derived fact · **[EA]** engineering assumption ·
**[DH]** design hypothesis · **[FW]** future work. Unmarked statements in ADR
"Decision" fields are decisions, not facts.

Companion documents: [PROOF_OBLIGATIONS.md](PROOF_OBLIGATIONS.md) (Rule 3
ledger), [RESEARCH_LOG.md](RESEARCH_LOG.md) (Rule 5 log).

---

## 1. Objective and theorem restatement

The project goal, as originally posed, is a mechanized theorem of shape

```
∀ p, admissible p → denote(lower p) ≈ctx denote(lower (transpile p))
```

with `≈ctx` mutual contextual refinement, and side effects (including their
order) preserved.

**[RF]** ESMeta contains no `lower : JS → IR`. The `esmeta eval` pipeline
(`src/main/scala/esmeta/Command.scala:130`) compiles the *ECMAScript
specification* to IR (one `ir.Func` per spec algorithm) and executes the
parsed JS program as a runtime **value** (`AstValue`,
`src/main/scala/esmeta/state/Value.scala:83`) flowing through that
spec-derived IR.

**Restated first-iteration theorem** (per ADR-1, user decision 2026-07-29):
programs are IR-Core programs (a fragment of standalone ESMeta IR, which is
parseable and executable in ESMeta today via `Program.fromFile` +
`CFGBuilder` + `Interpreter` **[RF]**), and the transpiler is an IR→IR pass:

```
∀ p : prog, admissible p →
  ctx_equiv (denote p) (denote (transpile p))
```

where `ctx_equiv P Q := ctx_refines P Q ∧ ctx_refines Q P` and
`ctx_refines` quantifies over linking contexts (§5, ADR-2). JS-level
transpilation **[FW]** requires an additional faithfulness argument
connecting spec-derived IR execution to this model and is out of scope for
Milestones 1–5.

**What a proved theorem will and will not mean.** Three distinct claims,
never to be conflated (project integrity rule):

1. equivalence relative to **the ITree model in `formal/`** — what M4 proves;
2. equivalence relative to **ESMeta's executable semantics** — supported by
   differential testing (M3, Strategy B), upgraded to proof only if the
   fragment-level faithfulness theorem (PO-012) is completed;
3. equivalence relative to **ECMAScript** — never claimed in this project
   [FW]; would additionally require JISET/ESMeta's spec-extraction guarantees.

---

## 2. Repository analysis (Milestone 0 summary)

### 2.1 ESMeta [RF]

- **IR syntax**: `src/main/scala/esmeta/ir/` — 16 `Inst`, 39 `Expr`, 4 `Ref`
  constructors, 52 operator cases. Full concrete-syntax parser
  (`ir/util/Parser.scala`), stringifier, rewriting walker.
- **Interpreter**: `src/main/scala/esmeta/interpreter/Interpreter.scala`
  (742 lines). Executes the **CFG** (`cfgbuilder/CFGBuilder.scala` eliminates
  `IIf`/`IWhile`/`ISeq`), small-step over CFG nodes (one basic block per
  step), big-step recursive expression evaluation, explicit call stack
  (`state/CallContext.scala`). `IReturn` sets `context.retVal`; control
  transfers at `ExitCursor` (`Interpreter.scala:87–102`).
- **Completions**: heap records `RecordObj("CompletionRecord", …)`; ECMAScript
  `?`/ReturnIfAbrupt compiles to explicit type-test + branch + `IReturn`
  (`compiler/Compiler.scala:1222–1236`). No interpreter-level exception
  mechanism for the object language; Scala exceptions = stuck states only.
- **Result convention**: termination writes main's return value to global
  `RESULT` (`Interpreter.scala:92–96`). Under the RunJobs convention `Undef`
  = normal, a list address = uncaught thrown values; consumers:
  `injector/ExitTag.scala`, `test262/Test262.scala:211`.
- **Output channel**: `IPrint` → stdout, suppressed under `TEST_MODE`
  (`Interpreter.scala:152–154`).
- **Executable IR corpus**: `tests/ir/` — 44 standalone programs run by
  `src/test/scala/esmeta/interpreter/InterpreterTest.scala:15`
  (`interp(State(CFGBuilder(Program.fromFile(f))))`); differential-testing
  oracle for M3. No CLI command runs `.ir` files today.
- **Existing transformations**: none (no transpiler/optimizer in the repo;
  fuzzer mutators are ES-AST-level randomizers).
- **Build/CI**: single sbt root project; new top-level directories are
  invisible to sbt and CI. CI = `sbt basicTest` + `sbt formatCheck`
  (`.github/workflows/ci.yml`). `ESMETA_HOME` required for tests.

### 2.2 CRIS [RF]

Framework source installed at
`~/.opam/cris-workshop/lib/coq/user-contrib/CRIS/` (127 files, ~42k lines);
practice repo (read-only) at `…/LAB/Study/2026-verification-workshop`.

- **ITrees**: vendored fork `ITreeS/`, datatype identical to upstream
  coq-itree (`Ret`/`Tau`/`Vis`); errors and nondeterminism are *events*.
- **Events** (`CRIS/common/Events.v`): `coreE = Choose | Take | IO`; only
  `IO` is observable; `Take False` = undefined behavior (refines everything),
  `Choose False` = no behavior. Module-level algebra
  `crisE = agE +' callE +' pgE +' coreE` (`agE` is Iris-valued;
  `pgE` = keyed store `SPut`/`SGet`; `callE` = named calls).
- **Behavior** (`CRIS/common/Behavior.v`, Iris-free):
  `Tr.t = done retv | abort | spin | hang e | interact e tl` (coinductive);
  `Beh.of_itree`; refinement = trace inclusion; `abort` is a behavior of
  every tree (bottom element); productivity requires an observable event.
- **Simulations**: stack `wsim → isim → msim → lsim → gsim`. `gsim`/`lsim`
  Iris-free; `msim`/`isim` carry a resource. Asymmetric one-sided stuttering
  with progress flags; `msim_call` re-establishes a state invariant across
  arbitrary callee effects (the source of contextuality); I/O must match
  exactly on both sides.
- **Adequacy**: `main_adequacy : ISim.t open Ms Mt IC Ist → IC ⊢
  ctx_refines Mt Ms` (`ctxrefine/MainAdequacy.v:337`); trace-level
  `refines_adequacy` (`CtxRefineFacts.v:176`).
- **Contexts**: linking — `ctx_refines Mt Ms := ∀ Ctx, refines (Mt ★ Ctx)
  (Ms ★ Ctx)` (`ctxrefine/CtxRefine.v:34`); composition algebra
  `ctxr_trans`, `ctxr_frameL/R`.
- **Trivial-resource use is demonstrated**: workshop
  `day1/answers/Optimizations.v` proves refinements with
  `Ist := fun _ _ => True%I`, `init_cond = emp` — full `isim` + tactics with
  zero ghost state.
- **Tactics**: ~1.9k lines (`cStartFunSim`, `cStepsS/T`, `cCall`, `cCoind`,
  `unfoldIterS/T`, …) written against `isim`.
- **Optional feature modules not needed here**: APC, helping, prophecy,
  scheduler, cancellation (~14.5k lines).

---

## 3. Observable behavior specification (Rule 4)

**Review gate: Milestone 2 (semantics implementation) must not begin until
this section has been reviewed.**

The observation universe is CRIS's trace type `Tr.t` over the *closed,
linked* program (program ★ context, compiled). All theorem statements
quantify over these observations, never over syntax.

**Observable:**

- **O-1. External-effect events**, in order. One primitive effect in v1:
  `IO "esmeta.print" v` — emitted by the denotation of `IPrint` (see
  `formal/Events.v`). The trace records the event name, its payload, and
  the (unit) reply. Additional host-call effects [FW] extend this list.
- **O-2. Termination with a result value**: `Tr.done retv` — the value
  returned by the linked program's main entry.
- **O-3. Divergence**: `Tr.spin` (infinitely many internal steps without an
  observable event).
- **O-4. Blocking**: `Tr.hang` (an `IO` event that never receives a reply)
  — inherited from CRIS's behavior definition; not distinguished further.
- **O-5. Abort**: `Tr.abort` — per CRIS, a behavior of every program
  (bottom of the refinement order).

**Internal (not observable):**

- state events on globals/heap (`pgE`), and local environments (no events
  at all — ADR-6);
- calls between functions *of the program*; calls to *context* functions are
  internal in the linked closed program but constrained by the ∀-context
  quantification, which is what forces effect ordering across unknown code
  to be preserved;
- `Tau` steps: finite stuttering is invisible (weak simulation);
  infinite stuttering is exactly O-3;
- `Choose`/`Take` resolution (nondeterminism/UB bookkeeping).

**Trace equality and effect ordering.** Traces are compared by the
coinductive equality of `Tr.t`: same events, same payloads, same replies,
same order, same termination status. Behavioral refinement is trace-set
inclusion (`Beh.of_itree P ⊆ Beh.of_itree Q`); there is no reordering,
deletion, or duplication of `interact` nodes — effect order preservation is
therefore built into the definition, not an added condition.

**Uncaught abrupt completion.** At IR-Core level an "uncaught throw" is a
*value*: main returning a completion record with `Type ≠ ~normal~`
(repository convention, §2.1). It is observable through O-2 as the returned
value. No separate trace constructor is introduced. [DH: adequate for the
fragment; revisit when modeling the RunJobs job-queue loop.]

**Stuck states** (assert failure, unknown variable, field access on
non-address, call of non-callable, arity mismatch): provisionally undefined
behavior — `Take False`, whose behavior set is all traces (ADR-7,
**open**; the M4 theorem will carry an explicit "the source program does
not get stuck" admissibility premise, making the UB-vs-abort choice
non-load-bearing for the PoC — see ADR-7 for why).

**Observable state: none.** No part of the store is directly observable;
state influences observations only through printed/returned values.

**Address opacity (OQ-8, open).** `Tr.done`/`IO` payloads containing heap
addresses (`VAddr`) would leak allocation order, which transformations may
legitimately change. Provisional restriction: **admissible programs may
print and return only address-free values**; v1 states this as a syntactic
premise on `IPrint`/main-return sites. Alternatives (observation up to
address renaming — a partial-bijection relation in `Ist` and a
value-relation in `RR`) are recorded in OQ-8 and deferred unless the PoC
transformation needs them.

---

## 4. Semantic design (layered)

Following the required conceptual separation:

| Layer | Mechanism | Visible? |
|---|---|---|
| 1. Pure computation (operator evaluation, value coercions) | plain Rocq functions returning `option val`; `Ret` | no |
| 2. Internal deterministic steps | `Tau` only where recursion requires guardedness (`ITree.iter` for `IWhile`, recursive calls); weak simulation ignores finite `Tau` | no |
| 3. State | locals: pure environment threading inside the denotation (no events); globals + heap: CRIS keyed store (`pgE` `SGet`/`SPut`) via the module's `initial_st` — ADR-6 | no |
| 4. Control effects | an explicit `completion` result type threaded by the denotation: `CNormal v \| CReturn v` in v1 (Break/Continue excluded — not IR-Core constructs; ECMAScript-level throw is data, §2.1) | no |
| 5. Observable effects | `IO "esmeta.print"` (O-1); context calls via `callE` at the linking boundary; termination/divergence per §3 | yes |

**Completion handling.** IR-Core's only abrupt control at the *instruction*
level is `IReturn` [RF: ESMeta models spec-level throw as completion-record
values, not control]. The denotation of an instruction returns
`itree crisE (env * completion)`, where `CReturn v` short-circuits the rest
of the enclosing body — mirroring `retVal`-then-exit in the Scala
interpreter. ECMAScript-style `Throw` completions need no instruction-level
support: they are `RecordObj` values built and tested by ordinary IR code.

**Recursion/iteration.** `IWhile` denotes via `ITree.iter` (following the
workshop's `countdown`/`KVSortedList` patterns [RF]); function calls between
program functions denote via `callE`-level calls resolved by CRIS linking
(`msim_inline_*` handles known-local calls in proofs).

---

## 5. Contextual refinement (ADR-2)

Definitions (all reused from CRIS, §2.2):

- **plug** = CRIS linking: `plug C P := P ★ C` (`Mod.add`).
- **hole/program** = a CRIS module: a finite map of named functions with an
  initial keyed store. The transformed function set is "the program";
  everything else — callers, callees, harness — is "the context".
- **behavioral refinement** = `Beh.of_itree (compile tgt) ⊆
  Beh.of_itree (compile src)` (trace inclusion).
- **contextual refinement** = `ctx_refines Mt Ms := ∀ Ctx, refines
  (Mt ★ Ctx) (Ms ★ Ctx)`.
- **contextual equivalence** = mutual contextual refinement.

**Admissible context (v1):** any well-formed CRIS module whose function
bodies are denotations of IR-Core programs (same fragment), not capturing
the program's private scope names (`Mod.scopes` enforces namespace
disjointness [RF]), and satisfying the same address-opacity restriction
(§3). Contexts may call program functions with arbitrary argument values of
the fragment, may perform their own `IO`, and may diverge. Contexts cannot:
introduce direct `eval`/`with`/Proxy/async behavior (not expressible in the
fragment — excluded by construction), or inspect program-private store keys.

**Alpha-renaming/freshness:** function names are global strings; linking
requires disjoint function tables [RF: `Mod.add` well-formedness]. The PoC
transformation introduces only *local* temporaries (`LTemp`), which are
invisible to contexts by construction — no cross-module capture is
possible; the freshness precondition is purely local to the transformed
function body.

**Route to ≈ctx.** CRIS-style: prove one `ISim.t` (with trivial resource)
between source and target modules, apply `main_adequacy` to get
`ctx_refines` in each direction. This is the "open-program simulation"
route; the alternative eutt+congruence route was considered and rejected
for v1 — see ADR-3 rationale. **Syntactic fragment contexts** (`C[·]` as an
IR-Core term with a hole) and their relation to linking contexts are
deferred to PO-010 [user decision: "linking now, syntactic later"].

---

## 6. Architecture Decision Records

### ADR-1 — Theorem stated over IR-level programs
- **Problem.** The target theorem mentions `lower : JS → IR`, which does not
  exist in ESMeta [RF, §1].
- **Alternatives.** (a) IR-level programs, transpile = IR→IR;
  (b) JS-level via denoting the spec-derived IR (hundreds of spec functions
  to mechanize before any theorem); (c) IR-level plus a maintained prose
  bridge document.
- **Decision.** (a). **User decision, 2026-07-29** (M0 report Q1).
- **Consequences.** M4 theorem quantifies over IR-Core programs. JS-level
  meaning is [FW] and requires a faithfulness argument (§1). Risk: results
  may be misread as JS-level claims — mitigated by the three-claim
  distinction in §1 and README warnings.

### ADR-2 — Linking contexts now; syntactic contexts later
- **Problem.** "Contextual" equivalence needs a context definition; classical
  syntactic contexts require congruence over all syntax forms.
- **Alternatives.** (a) CRIS linking contexts only; (b) syntactic hole
  contexts with a hand-proved congruence; (c) linking now + a bridging
  theorem later.
- **Decision.** (c). **User decision, 2026-07-29** (M0 report Q2).
- **Consequences.** M4 delivers linking-based ≈ctx via `main_adequacy`;
  PO-010 records the bridge obligation. Expected impact: the bridge needs a
  "syntactic context ⇒ linkable module" compilation lemma; its difficulty
  depends on how much of the fragment a hole can appear in (to be scoped
  when PO-010 is activated).

### ADR-3 — CRIS `isim` with a trivial resource; keep Iris as a dependency
- **Problem.** The proof framework choice: reuse CRIS's Iris-valued `isim`
  (with its ~1.9k-line tactic library and complete adequacy chain) versus a
  smaller Iris-free development (raw `gsim`/`lsim`, or fresh eutt-based).
- **Alternatives.** (a) `isim` with `Ist` a pure relation
  (`⌜state_rel s t⌝`), no ghost state; (b) Iris-free `gsim`/`lsim` core —
  hand-rolled paco coinduction for every proof; (c) fresh eutt +
  compositionality route — must rebuild behavior/adequacy/linking from
  scratch and eutt's symmetry is a poor fit for one-sided stuttering.
- **Decision.** (a). **User decision, 2026-07-29** (M0 report Q3).
  The workshop's `Optimizations.v` demonstrates exactly this mode [RF].
- **Consequences.** `coq-iris` remains a build dependency (cost accepted);
  no resource algebras appear in our proofs; if a future transformation
  needs ownership reasoning (e.g., allocation-site reshuffling), the
  machinery is already present. Disadvantage: proofs are stated in iProp
  even when trivially pure; mitigated by the tactic library hiding this.
- **eutt-vs-simulation comparison (required by the brief).** The eutt route
  (`denote P ≈ denote Q` + compositional handlers ⇒ ≈ctx) is *simpler in
  principle* for whole-program, context-free equivalences, but for IR-Core
  the two sides interact with unknown context code through calls, where
  CRIS's `msim_call` rule (re-establish invariant across arbitrary callee
  behavior) does precisely the needed work; with eutt we would have to prove
  a congruence-under-interp theorem for our specific handler stack ourselves.
  For this fragment the open-simulation route is strictly less new
  infrastructure. [DH — revisit if a transformation is ever purely local
  and call-free.]

### ADR-4 — Denote the tree IR; connect to ESMeta's CFG interpreter by testing first
- **Problem.** ESMeta executes the CFG; the tree IR is what `.ir` files and
  the transformation manipulate [RF, §2.1]. ITree denotations are natural
  over trees; a CFG denotation would need explicit cursor/program-counter
  encoding.
- **Alternatives.** (a) denote tree IR; validate against ESMeta (which runs
  `CFGBuilder` internally) by differential testing (Strategy B), fragment
  faithfulness theorem later (PO-012); (b) denote the CFG — faithful to the
  implementation but awkward ITrees and awkward transformation statements;
  (c) denote both + verified tree→CFG translation — most work.
- **Decision.** (a). [DH: my recommendation, adopted in the M0 report
  without objection; flagged here for explicit review.]
- **Consequences.** The M3 correspondence to ESMeta execution is initially
  *testing, not proof* (explicitly so labeled, per the brief's Strategy B).
  The trusted gap "tree denotation ≙ CFG execution" is listed in the TCB
  (§9) until PO-012 closes it. `CFGBuilder` is only ~60 lines [RF], so
  PO-012 is plausibly attainable later.

### ADR-5 — Fragment `Math` restricted to integers (`Z`)
- **Problem.** ESMeta `Math` is unbounded-precision `BigDecimal`; division
  silently rounds to DECIMAL128 (`Interpreter.scala:584–587`) [RF] — an
  implementation artifact that would poison the model with rounding.
- **Alternatives.** (a) `Z`, exclude `Div`/non-integer literals from the
  fragment; (b) rationals `Q` + a rounding model; (c) mirror BigDecimal.
- **Decision.** (a). On the included operators (+, −, ×, <, =) integer
  arithmetic agrees exactly with BigDecimal arithmetic on integer inputs
  [EA — checked against operator implementations `Interpreter.scala:566–668`
  for the included cases; to be re-verified by the M3 differential tests].
- **Consequences.** Admissibility includes "all Math literals are integers";
  `gcd.ir`/`fibo.ir`/`sum.ir`/`branch.ir` satisfy it [RF — inspected].
  Non-integer arithmetic is [FW].

### ADR-6 — State: locals threaded purely; globals+heap in the CRIS keyed store
- **Problem.** Where does ESMeta state live in the denotation? (required
  justification of state-events-vs-state-transformer).
- **Alternatives.** (a) everything via `pgE` store events; (b) everything
  via a state-transformer over ITrees (`stateT env (itree E)`);
  (c) split: locals threaded as a pure parameter (they are per-call,
  unshared, die at return [RF: flat `MMap[Local, Value]` per `Context`]);
  globals + heap in the module's keyed store (`pgE`), which CRIS's
  `Ist`/`msim_call` machinery is designed around.
- **Decision.** (c), provisional pending M2 validation on the first
  nontrivial denotation. Rationale: locals-as-events would force the state
  invariant to speak about call-local data that no context can observe,
  bloating every `msim_call` obligation; a full state transformer would put
  the heap outside the store that `Ist` relates, forfeiting CRIS's
  call-boundary treatment. The split matches the semantic scoping exactly.
- **Consequences.** The denotation type is
  `env → itree crisE (env * completion)`. Heap keys must encode addresses
  (`nat`) into CRIS store keys — a fixed injection defined in M2.
  Disadvantage: two state mechanisms to explain; mitigated by this ADR and
  layer table §4.

### ADR-7 — Stuck states are undefined behavior (provisional; open OQ-4)
- **Problem.** ESMeta's interpreter aborts (Scala exception) on assert
  failure, unknown variables, bad coercions, non-callable calls [RF]. The
  model must give these a semantics.
- **Alternatives.** (a) UB — `Take False`: behavior set = all traces; the
  CompCert convention "source stuck ⇒ anything refines it";
  (b) observable abort — a distinguished `IO` event or `Choose False`
  (empty behavior); (c) total semantics via type-checked admissibility
  (rule out stuck states statically).
- **Decision.** (a) provisionally, because it matches CRIS's own `unwrapU`
  convention [RF] and gets `take_src` handling free in proofs. Crucially,
  the M4 theorem carries an admissibility premise strong enough that the
  *source* never sticks on the transformed sites, and the PoC transformation
  preserves stuck-sites exactly (same asserts, same variable references), so
  both refinement directions treat UB symmetrically — the choice is not
  load-bearing for the PoC. It becomes load-bearing for transformations that
  remove or add potential stuck-sites; OQ-4 stays open until such a
  transformation is attempted.
- **Consequences.** "Refines" statements are vacuous at source-stuck points;
  differential tests (M3) must therefore only use non-stuck admissible
  programs when validating the model against ESMeta (whose stuck states are
  hard aborts). The negative tests (deliberately wrong transformation) must
  be chosen to differ observably *before* any stuck point.

### ADR-11 — Restricted type expressions for `ETypeCheck`; subtyping exported, not guessed
- **Problem.** `(? x : T)` is 1030 functions' worth of the compiled spec —
  the single biggest blocker. ESMeta evaluates it as `T.contains(v, st)`
  over the full `esmeta.ty` language (unions, record field maps).
- **Alternatives.** (a) mirror the whole `ty` language + `contains`
  (large, and its own faithfulness problem); (b) a **restricted `tyexp`
  grammar** covering what the spec actually tests, with the record
  hierarchy **exported from ESMeta** and anything outside the grammar
  rejected by the exporter; (c) treat all type tests as unknown calls
  (loses all reasoning about completions).
- **Decision.** (b). `formal/TyModel.v` is generated by
  `esmeta.fv.FVTyModel` from `esmeta.ty.TyModel.parentOf` (112 decls, 74
  edges), so `record_subtype` agrees with ESMeta by construction
  (`ThrowCompletion <: AbruptCompletion <: CompletionRecord` verified).
  Abrupt/Normal are decided by the completion's `Type` field, matching the
  declared refinements, because at runtime completions are stored under the
  base tname (State.scala:169-175).
- **Consequences.** Field refinements other than Completion's `Type` are
  not modelled; the exporter must reject those `Ty`s rather than
  mis-modelling them. `TyModel.v` is a checked-in generated file (small,
  pinned) so `make` works without running the generator; regenerate with
  the command in its header.

### ADR-12 — AST values and SDO dispatch; grammar-derived data precomputed
- **Problem.** `ISdoCall` blocks 1033 functions — everything JS-level.
  ESMeta resolves it at runtime (Ast.scala:102-113): walk the production
  *chain*, try `Name[rhsIdx,subIdx].Method`, else `DEFAULT:Method`.
- **Alternatives.** (a) carry the grammar in the model and recompute
  `subIdx`/named-field indices; (b) **precompute grammar-derived data in
  the exporter** and carry only the tree; (c) generate a dispatcher IR
  function per SDO method.
- **Decision.** (b). `ast` carries `subIdx` as a field because ESMeta
  derives it from the grammar (Ast.scala:116-128), which the model does not
  carry; `sdo_resolve` reproduces the chain walk and DEFAULT fallback
  against the program's own function names, so dispatch is faithful without
  a grammar. Chain length is bounded by `ast_size` (fuel) since the
  single-present-child projection is not a structural subterm.
- **Consequences.** Named AST field access (`ast.Foo`) and `parent` need
  grammar/parent data we do not carry: **UB, not guessed** (limitation
  L-9). Lexical SDOs (`StringValue`, `NumericValue`, `MV`, `SV`, `TV`,
  `TRV`) are implemented in *Scala* (`ESValueParser`, 462 lines), not IR,
  so a lexical receiver is UB until that is reimplemented — the main
  remaining blocker for running real JS (L-10).

### ADR-13 — A targeted differential corpus alongside `tests/ir`
- **Problem.** `FVExport` derives its expectations by *running ESMeta's own
  interpreter*, so `tests/ir` is the ground truth — but it is ESMeta's test
  suite, not ours, and it does not cover every construct we model at a
  granularity we can export. Concretely: after implementing `EVariadic`,
  `VoMin`/`VoMax` had **zero** occurrences in `Generated.v`, because the
  only min/max test (`tests/ir/expr/variadic.ir`) is skipped over its `3.2`
  literal (ADR-5). Validation was green and the code was untested.
- **Alternatives.** (a) add cases to `tests/ir` — pollutes ESMeta's suite
  and changes what `EvalTinyTest` runs; (b) hand-write expectations in
  `Validation.v` — the expectation would then be *our* belief, not ESMeta's
  behaviour, which defeats the point of a differential test;
  (c) **a second input directory that `FVExport` also walks.**
- **Decision.** (c). `FVExport` walks `tests/ir` *and*
  `formal/validation/extra`; ids are namespaced by the parent directory
  (`g_extra_*` vs `g_ir_*`), so nothing collides. Expectations are still
  produced by running ESMeta, so these remain true differential tests.
- **Consequences.** Coverage of the model can be driven independently of
  ESMeta's suite, and a construct we implement can always be given a test
  that actually reaches it. The obligation this creates: **after adding any
  construct, grep `Generated.v` for its constructor**; a green
  `make validate` proves nothing about code the corpus never executes.
  First instance: `formal/validation/extra/variadic-int.ir`, the
  integer-and-infinity lines of the upstream file copied verbatim.

### ADR-10 — CORRECTION to ADR-9: the specification *is* the desugaring; T-3 supersedes T-2
- **Problem.** ADR-9 introduced a synthetic `EOptField` to have an
  optional-chaining *source* form. User challenge (2026-07-29): ESMeta
  already parses optional chaining, so is the synthetic construct needed?
- **Empirical findings.**
  1. **[VF]** ESMeta (v0.7.3 source; note the checked-in `bin/esmeta` is a
     stale v0.6.4 that fails on the current spec) parses `var x = a?.prop`
     into `|OptionalExpression|[FF]<0>( |MemberExpression|…(a),
     |OptionalChain|[FF]<2>( |IdentifierName|(prop) ) )`. The user was
     right.
  2. **[PF]** `ecma262@84b38ad8` `sec-optional-chaining-evaluation`
     defines `OptionalExpression : MemberExpression OptionalChain` as:
     *(1)* `baseReference ← Evaluation of MemberExpression`;
     *(2)* `baseValue ← GetValue(baseReference)` — **receiver evaluated
     once**; *(3)* if `baseValue` is undefined or null, **return
     undefined**; *(4)* otherwise `ChainEvaluation` →
     `EvaluatePropertyAccessWithIdentifierKey`.
  3. **[RF — strongest available]** ESMeta's *compiled IR* for that
     production confirms the guard is real code, not a reading of prose.
     From `logs/dump/debugger/funcs.json` (`sbt "run dump-debugger"`),
     function `OptionalExpression[0,0].Evaluation`, location annotations
     stripped:

     ```
     sdo-call %0 = this[0]->Evaluation()
     if (? %0: Abrupt) return %0 else %0 = %0.Value
     let baseReference = %0
     call %1 = clo<"GetValue">(baseReference)
     if (? %1: Abrupt) return %1 else %1 = %1.Value
     let baseValue = %1
     if (|| (= baseValue undefined) (= baseValue null)) {
       call %2 = clo<"NormalCompletion">(undefined)
       return %2 }
     sdo-call %3 = this[1]->ChainEvaluation(baseValue, baseReference)
     ```

     One receiver evaluation, the literal nullish disjunction, early
     `undefined` return, property access only afterwards. (All three
     `OptionalExpression` productions compile to the same shape.)
- **Consequence — ADR-9's factual claim stands, its design choice was
  wrong.** There is still no IR-level `?.` primitive, and now we know
  *why*: the guard is introduced by the specification itself, hence by
  spec→IR compilation. At IR level there is nothing left to desugar, so
  inventing a source form was unnecessary. Every obligation the project
  brief asks of this transformation class — receiver evaluated exactly
  once, no property access on the nullish branch, unchanged effect order
  — is expressible **between two mirrored-IR programs**, by taking the
  source to be the spec-shaped guarded code.
- **What T-2 actually proves** (recorded plainly): "our desugaring
  implements *our* definition of `EOptField`". Sound, and its case
  analysis and guard obligation were real, but the source semantics is
  model-defined, so the theorem cannot be right or wrong *relative to
  ECMAScript*, and ESMeta cannot execute its source side — the
  differential harness never covered it.
- **Decision.** Add **T-3** (`formal/T3Proof.v`, `Programs.v`
  `t3_optaccess_main`): the spec-shaped optional access
  `x = f()?.prop` in mirrored IR only, receiver an **effectful context
  call**, transformed by the already-verified `t1_prog`. T-3 becomes the
  project's optional-chaining result; T-2 is retained (proved, and its
  proof machinery — abstract-store `SGet` pairing, receiver case
  analysis — is reused by T-3) but **demoted to a model-internal
  exercise** and must not be cited as an ECMAScript-level claim.
- **Why the receiver must be a call.** ADR-9 already noted that IR-Core
  has no getters, so re-evaluating a *pure* receiver is unobservable.
  Making the receiver a context call fixes exactly that gap: each call
  is an event at the linking boundary, so "exactly once" becomes a
  genuinely observable obligation — the wrong, re-evaluating
  transformation calls it twice and is detected
  (`t3v_reeval_detected`: traces `[7;42]` vs `[7;7;42]`).
- **What is still NOT proven (asked directly, 2026-07-29).** *"Is it
  proven that JavaScript `?.` equals the guard form?"* — **No.** Nothing
  in `formal/` mentions JavaScript or `?.`; T-3's theorem is about two
  IR-Core programs. The chain from JS to that theorem has one
  unmechanized link:

  | Step | Status |
  |---|---|
  | `a?.b` parses in ESMeta | **[VF]** verified by running the parser |
  | spec text defines `?.` as the guarded form | **[PF]** reading of normative prose |
  | ESMeta's compiled IR has the guard shape | **[RF]** inspected above — machine-generated, so not a reading |
  | `t3ex_src` models that compiled IR | **[EA] unverified modelling step — the weak link** |
  | `t1_prog` preserves `t3ex_src`'s behaviour in all contexts | **proved (Qed)** |

  The modelling step abstracts, and in one case **diverges from**, real
  JavaScript: no Reference Records/`GetValue` (hence no getter calls,
  which in real JS can run arbitrary user code at steps 2 and 4), no
  prototype chain or accessor properties, no `ToObject` coercion, no
  abrupt-completion propagation. The divergence: `(42)?.foo` is
  `undefined` in JS (a Number is not nullish, so access proceeds through
  `ToObject`) whereas the model makes a non-address receiver **UB**
  (limitation L-8). Closing the link means mechanizing the compiled IR
  functions above (`GetValue`, `ChainEvaluation`,
  `EvaluatePropertyAccessWithIdentifierKey`, …) and proving them
  equivalent to the guarded program — the PO-012-style spec-level
  faithfulness route, [FW].
- **Consequences.** Both sides of T-3 are real IR: ESMeta can execute
  them and `FVExport` can emit them, so differential validation applies
  to source *and* target — an honesty gap T-2 could not close. What
  remains open for T-3 is the modelling step above, which must be stated
  whenever the result is described. Full JS-level optional chaining
  (References, getters, calls, private names, nested chains) remains
  [FW] — it needs the spec-derived IR.

### ADR-9 — T-2 optional-field desugaring over a synthetic source construct
> **Superseded in part by ADR-10** (2026-07-29): the synthetic construct
> was unnecessary; see ADR-10 for the spec evidence and for T-3, the
> mirrored-IR replacement. Retained because T-2 is proved and its proof
> machinery is reused.
- **Problem.** Prove an optional-chaining-style desugaring (the brief's
  candidate #3). **[RF]** ESMeta IR has no `?.` construct — real optional
  chaining is compiled from the spec into AST dispatch + completion-record
  plumbing; there is no IR-level primitive to desugar.
- **Alternatives.** (a) JS-level statement — requires the spec model, out
  of first-iteration scope [FW]; (b) extend IR-Core with a **synthetic**
  expression `EOptField recv fld` ("recv?.fld") whose semantics we define
  (receiver once; `Null`/`Undef` ⇒ `Undef` with **no heap access**; else
  record-field read), and prove the desugaring into guarded IR-Core;
  (c) a pure-receiver duplication equivalence — weaker, misses the guard
  obligation.
- **Decision.** (b), 2026-07-29, plus adding the *mirrored* `ERecord`
  constructor (a real ESMeta construct, `ir/Expr.scala:56`; field order
  and allocation semantics verified against `Interpreter.scala:337-338`,
  `Heap.scala:50-53`, `Obj.scala:113-121`) so field access is exercisable.
- **Honesty boundary.** `EOptField` is marked SYNTHETIC in `Fragment.v`:
  ESMeta cannot parse or execute it, the exporter never emits it, and the
  differential harness therefore validates only (i) post-desugar target
  programs and (ii) closed exemplars of the desugaring inside the model.
  The source-form semantics is model-defined by construction. Fragment.v
  is thus IR-Core⁺ = mirror ∪ {one synthetic constructor}.
- **Consequences.** The T-2 theorem's obligations are: the **guard**
  property (no heap access on the nullish branch — an unguarded desugar
  is UB where the source yields `Undef`, so the equivalence proof fails;
  demonstrated executable: `t2v_bad_detected`), effect interleaving
  around unknown calls, and temp non-interference. Within IR-Core,
  receiver re-evaluation is not independently observable (no getters in
  records **[RF]**), so "receiver once" is enforced syntactically but
  carries no separate observable obligation — in full JS it does, which
  the [FW] spec-level extension would add. Restriction: `EOptField` only
  in binding position (`t2_ok_inst`); nested occurrences out of scope.

### ADR-8 — Depend on the pinned `rocq-cris` opam package; do not vendor
- **Problem.** Reuse mechanism for ~12k lines of framework core.
- **Alternatives.** (a) opam dependency on the pinned SHA; (b) vendoring the
  framework into `formal/`; (c) reimplementing a minimal core.
- **Decision.** (a). Vendoring 42k lines into ESMeta violates the "no large
  blind copies" rule and bloats the repo; reimplementation forfeits the
  adequacy chain and tactics (see ADR-3).
- **Consequences.** Build reproducibility depends on the git SHA staying
  reachable [EA — risk recorded; mitigation: document the SHA (done, header
  table) and, before M5/CI, mirror the commit or cache the archive].
  License: CRIS is used as an external library, not copied; adapted *ideas*
  (event mapping, module encoding, tactic usage patterns) are credited in
  §10 and per-file headers.

---

## 7. IR interchange format (Scala ↔ Rocq boundary)

**[RF]** ESMeta's JSON codecs for IR serialize instruction bodies as
concrete-syntax strings (`ir/util/JsonProtocol.scala`); there is no
structural JSON for `Inst`/`Expr` today.

**Implemented (M3, amending the original JSON plan).** The exporter
(`src/main/scala/esmeta/fv/FVExport.scala`, isolated package) walks
fragment-compatible `ir.Program`s and emits **Rocq terms of the
`Fragment.v` datatypes directly** (constructor-for-constructor), plus a
per-construct rejection report. Amendment rationale (vs. the structural
JSON originally planned): the consumer is Rocq itself, so generating `.v`
text removes the need for any JSON parser on the proof-assistant side and
makes the generated expectations reviewable; the interchange artifact is
`formal/validation/Generated.v` (git-ignored, regenerated by
`sbt "runMain esmeta.fv.FVExport"`). JSON can be added later if a
non-Rocq consumer appears. `formal/Fragment.v` remains a
**hand-mirrored datatype**; this duplication is acceptable because (i)
the fragment is ≤30 constructors, (ii) both sides are pinned (header
table), and (iii) the differential tests over `tests/ir/` catch drift
mechanically [validated: 18 programs exported and matching, 26 skipped
with reasons, 2026-07-29]. Synchronization procedure is documented in
`formal/README.md` (Conventions).

---

## 8. Directory layout and dependency boundary

```
esmeta/
├── formal/                      # Rocq development (this project)
│   ├── _CoqProject  Makefile  README.md
│   ├── Fragment.v               # IR-Core syntax (stdlib-only)
│   └── Events.v                 # observable-event interface (imports CRIS)
├── docs/formal-verification/
│   ├── itree-transpiler-plan.md # this document (incl. ADRs)
│   ├── PROOF_OBLIGATIONS.md     # Rule 3 ledger
│   └── RESEARCH_LOG.md          # Rule 5 log
└── src/…                        # untouched until M3's exporter/ir-eval phase
```

Dependency direction: `formal/` depends on installed opam packages only —
never on Scala build outputs; the Scala side never depends on `formal/`.
The only crossing point is the M3 interchange JSON + test harness, which is
data, not code. `formal/` is invisible to sbt and existing CI [RF]; a
dedicated CI workflow is added only at M5 after local builds are stable.

Build commands: see `formal/README.md` (verified: `make` compiles cleanly
on Rocq 9.0.0 with the pinned switch [VF 2026-07-29]).

---

## 9. Trusted computing base

When M4's theorem is `Qed`-complete, trusting its *stated meaning* requires
trusting:

1. **Rocq 9.0.0** kernel + the pinned libraries (paco, ITreeS fork, Iris,
   stdpp, ext-lib) — standard.
2. **CRIS framework definitions** (`Beh`, `Tr.t`, simulations, adequacy,
   `Mod`/linking): we treat its adequacy theorems as *proved facts* [RF —
   they are Qed in the installed package]; its *definitions* (what "behavior"
   and "linking" mean) are part of our theorem's meaning and must be read to
   understand the claim.
3. **`formal/Fragment.v` + the denotation (M2)** as a faithful model of
   ESMeta IR-Core — until PO-012, this is validated only by differential
   testing [explicitly testing, not proof].
4. **The observable-behavior spec (§3)** as the right notion of observation.
5. NOT trusted / not claimed: ESMeta's spec extraction, the ECMAScript
   connection, anything outside the fragment.

---

## 10. Initial proof-of-concept transformation

**T-1: fresh-temporary introduction** (M4 target):

```
use(f(args...))            ⇒        %t := f(args...) ; use(%t)
```

concretely on IR-Core: an `ICall` whose result feeds one use-site is split
so the call lands in a fresh `LTemp` first. Chosen because [DH]:

- expressible entirely in IR-Core (calls, locals, sequencing) [RF];
- not syntactically identity — the proof must track that the *single* call
  happens exactly once, at the same program point, with unchanged
  argument-evaluation order, before the use;
- has a real effect-ordering obligation when `f` is a context function that
  prints (the M3 negative test duplicates the call or reorders it past
  another print — both must be observably detected);
- does not touch the object model beyond what IR-Core has.

**Preconditions (to appear verbatim in the theorem):** the introduced
`LTemp n` does not occur in the enclosing function body (freshness — local,
checkable syntactically); the program is admissible (fragment-only
constructs, integer Math literals, address-free observables §3, closed
well-scoped function references). Alternatives considered: conditional-
expression desugaring (needs expression-level laziness the fragment's `IIf`
already gives statement-level — weaker ordering content); nullish-access
desugaring (drags in property access on possibly-null receivers — object
model). Recorded per Rule 1.

---

## 11. Milestone plan

- **M1 (this)**: architecture + ADRs + PO ledger + research log + buildable
  skeleton (`Fragment.v`, `Events.v`). Gate: review of §3.
- **M2**: state model + denotation (`env → itree crisE (env * completion)`),
  module packaging (`SMod`), small executable examples mirroring 3–5
  `tests/ir` programs, `Fragment_wip.v` rule honored. POs: 001–004.
- **M3**: Scala exporter + `ir-eval` phase (optional), differential harness
  over `tests/ir/` incl. effectful and negative tests. Explicitly testing.
  POs: 011.
- **M4**: T-1 transformation in Rocq + `ISim.t` both directions +
  `main_adequacy` application ⇒ `ctx_equiv`. No `Admitted`. POs: 005–009.
- **M5**: CI workflow (build `formal/` + run differential suite), developer
  docs, gap analysis toward more IR constructs / JS-level [FW].

Each milestone ends with: changed-file list, exact build/test commands,
proved-statement summary, claim classification, and a research-log entry.

---

## 12. Relation to the cited papers

- **JISET (ASE 2020, 10.1145/3324884.3416632)** — provenance of ESMeta's
  spec-to-IR extraction; explains why "lowering JS" is not an ESMeta
  concept (§1) and why ECMAScript-level claims need extraction faithfulness
  [PF].
- **CRIS (10.1145/3808317)** — supplies behaviors, open simulation,
  adequacy, linking-based contextual refinement (§2.2, §5, ADR-3). Per the
  project brief and the paper's own layering, its imaginary-specification /
  Assume-Guarantee / ownership machinery is deliberately **not** used: both
  compared objects are ordinary programs, so the trivial-resource
  instantiation suffices [PF + RF].
- **Interaction Trees (POPL 2020, 10.1145/3371119)** — the representation
  (`Ret`/`Tau`/`Vis`), `ITree.iter` for loops, weak bisimulation ignoring
  finite `Tau` (§4) [PF].

---

## 13. Limitations of the initial fragment

- **L-1** *(rewritten 2026-07-30 — the original list is stale; what follows
  is the measured state.)* The constructs still outside the fragment, as
  reported by `FVSpecScan` over the 2951 compiled spec functions, with the
  number of functions each one blocks:
  `EGrammarSymbol` 49, `ESourceText` 37, `EParse` 33, `ESubstring` 22,
  `EMathOp` 21, `ECont` 11, `EInstanceOf` 4, `ESyntactic` 4,
  `EConvert(COp.ToStr)` 2, non-integer `EMath` 2, `ETrim` 1, `ERandom` 1;
  plus `ELexical` and `EDebug`, and the `^^` (`BOp.Xor`) operator. Each is
  UB in the model, never approximated. 2804/2951 (95%) of spec functions
  contain none of them. Constructs the original L-1 listed as absent that
  are now modelled: `Number`, `BigInt`, `Infinity`, `CodeUnit`, maps
  (`EMap`/`EKeys`), `IPush`/`IPop`, `IExpand`/`IDelete`, `ECopy`,
  `ETypeOf`/`ETypeCheck`, `EYet` (as UB by construction), `ISdoCall`
  (ADR-12), `EVariadic`, `EContains`, and string operations including
  UTF-16 code-unit indexing (`State.scala:57-59`).
- **L-2** `Math` is integer-only (`Z`), whereas ESMeta's `Math` is a
  `BigDecimal` (ADR-5). `Div`/`Mod`/`Pow` *are* modelled, over `Z`;
  the exporter rejects any non-integer `Math` literal rather than rounding
  it, so the restriction is visible as a skip, never as a wrong answer.
  Consequence for testing: constructs whose only corpus test carries a
  non-integer literal get no coverage from `tests/ir` — see ADR-13.
- **L-3** Untyped: parameter/return type annotations and `FuncKind` dropped;
  optional parameters unsupported.
- **L-4** Promise jobs, async/await, generators, agents/shared memory,
  Proxy, eval, host/DOM behavior: excluded (and mostly rely on continuations
  [RF], which are excluded).
- **L-5** The connection to ESMeta execution is by testing until PO-012;
  the connection to ECMAScript is not part of this project.
- **L-6** Observable payloads restricted to address-free values (OQ-8).
- **L-9** AST access is limited to numeric child indexing (`this[0]`);
  named-field access and `parent` are UB (ADR-12).
- **L-10** Lexical SDOs are UB: ESMeta implements them in Scala
  (`ESValueParser`), so no numeric/string literal value can be computed
  yet. This, plus the numeric tower (`ENumber`/`EConvert`) and the initial
  intrinsics heap, is what still stands between the model and running real
  JavaScript — see the JS-level roadmap in the research log.
- **L-8** The T-3 optional-access model (ADR-10) captures only the
  *control shape* of `?.`: receiver evaluated once, nullish guard, access
  only on the non-nullish branch. It has no Reference Records/`GetValue`
  (so no getter calls), no prototype chain or accessor properties, no
  `ToObject`, and no abrupt-completion propagation. It **diverges** from
  JavaScript on primitive receivers: `(42)?.foo` is `undefined` in JS but
  UB in the model. No claim is made that the model *is* ECMAScript `?.`.
- **L-7** Short-circuit `And`/`Or` semantics pinned to the interpreter's
  behavior in M2 (OQ-7): **[RF]** `Interpreter.scala:358–365` short-circuits;
  the fragment's denotation must do the same, and `tests/ir/expr/` fixtures
  will be included in the differential suite to check it.

---

## 14. Open questions ledger

| ID | Question | State | Blocking |
|---|---|---|---|
| OQ-4 | Stuck = UB vs observable abort | provisional UB (ADR-7) | not for PoC; yes for stuck-site-changing transformations |
| OQ-7 | Strict vs short-circuit `And`/`Or` in denotation | resolved: short-circuit, mirror interpreter (L-7) | M2 |
| OQ-8 | Address-bearing observables | provisional: excluded by admissibility (§3) | M4 statement wording |
| OQ-9 | Does `SMod`'s keyed store cleanly encode `nat` addresses + globals in one keyspace? | **resolved 2026-07-29** (M2): one module scope, prefixed key families `g$x` / `h$n` / `alloc$` (`formal/Semantics.v`); all `SMod` well-formedness obligations proved generically | — |
| OQ-10 | Exact `RR` (return relation) for `main` at the `Any.t` boundary | **resolved 2026-07-29** (M2): every denoted function crosses `Any.t` uniformly as `ir_arg = captured × args ↦ val` (`ir_sig`); `entry` is `unit ↦ val` | — |
| OQ-11 | Does ESMeta's `RecordObj.update` require the field to exist (vs. create it)? | **resolved 2026-07-29** (M3): `Obj.scala:29-30` — unconditional insert-or-update; model fixed (`fields_insert`, `Domain.v`) | — |
