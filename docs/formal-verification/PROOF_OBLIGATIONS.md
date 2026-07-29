# Proof Obligation Ledger — ESMetaFV

Rule-3 ledger. Implementation never begins with the final theorem; POs are
discharged bottom-up. Status values: `not-started` · `in-progress` ·
`proved (Qed)` · `testing-only` (explicitly not a proof) · `deferred`.
Difficulty: ★ (routine) to ★★★★★ (research-level).

Definitions referenced below live in `formal/` (Fragment.v, Events.v; M2
adds Denote.v, Modules.v) and the CRIS framework (pinned; see architecture
note header). "AN §n" = architecture-note section n.

---

### PO-001 — Fragment induction principles
- **Statement.** Mutual/nested induction principles for `expr`/`ref`,
  `inst` (nested `list inst`), and `val` (nested `list (string * val)`)
  strong enough to prove structural lemmas over closures and sequences.
- **Motivation.** Coq's auto-generated schemes are too weak for the nested
  lists; every later structural proof depends on these.
- **Dependencies.** Fragment.v only.
- **Technique.** Hand-written nested `Fixpoint` schemes (standard).
- **Status.** deferred to first use (M4 schematic proofs). M2 update:
  no M2 proof needed them — `denote_seq_*` used plain list reasoning and
  `print2_body_trace` is equational. Writing schemes with no consumer
  risks proving the wrong strengthening. **Difficulty.** ★

### PO-002 — Denotation totality and shape
- **Statement.** The denotation `denote_inst : inst → env → itree crisE
  (env * completion)` (and `denote_expr`, `denote_func`, module packaging)
  is well-defined for every fragment term; guardedness obligations for
  `ITree.iter`/recursion discharged.
- **Motivation.** Executable-semantics deliverable; everything downstream
  mentions these definitions.
- **Dependencies.** PO-001; ADR-6 state split; §3 observable spec (review
  gate).
- **Technique.** Definition + `Program`/structural recursion; no
  coinductive proofs expected beyond what `ITree.iter` encapsulates.
- **Status.** **proved (definitional), 2026-07-29** — `formal/Semantics.v`:
  full IR-Core denotation accepted by the guard checker (mutual
  `denote_expr`/`denote_ref`, nested fixes for lists, `ITree.iter` for
  `IWhile`); `ir_smod`'s four `SMod.t` well-formedness obligations are
  `Qed` for *every* program; four example modules typecheck
  (`formal/Examples.v`). **Difficulty.** ★★

### PO-003 — State-relation preservation (Ist adequacy for IR-Core)
- **Statement.** The pure state relation used as `Ist` (source/target keyed
  stores related — for T-1, equality on all keys) is preserved by every
  internal step of the denotation and re-establishable at call boundaries.
- **Motivation.** Feeds every `msim_call`/`isim` step in PO-006.
- **Dependencies.** PO-002; OQ-9 resolved (2026-07-29: single-scope keyed
  store — globals `g$x`, heap `h$n`, counter `alloc$`).
- **Technique.** Per-instruction lemmas via CRIS step tactics
  (`cStepsS/T`); for T-1, `Ist := IstEq`-style equality should make most
  cases reflexive.
- **Status.** **discharged within PO-006's proofs, 2026-07-29** — the
  relation is store equality (`Ist := ⌜s = t⌝`); preservation is
  established step-by-step inside the four simulation lemmas
  (`formal/T1Proof.v`) and re-established across the unknown call by
  `cCall`. No standalone lemma was needed for the exemplar (its main
  performs no store operations). **Difficulty.** ★★

### PO-004 — Completion preservation
- **Statement.** The denotation propagates `CReturn` past residual
  instructions exactly as ESMeta's `retVal`-then-`ExitCursor` discipline:
  denoting `ISeq (i :: rest)` where `i` yields `CReturn v` never executes
  `rest`'s effects.
- **Motivation.** Effect-ordering claims are false if return short-circuiting
  is wrong; also documents the (unused-by-compiler) "code after return in a
  block" corner [RF, M0 report].
- **Dependencies.** PO-002.
- **Technique.** Simple structural lemmas + itree rewriting (`my_red_both`
  / `red_bind` family).
- **Status.** **proved (Qed), 2026-07-29** — `formal/Semantics.v`:
  `denote_seq_cons` (one-step unfolding, definitional) and
  `denote_seq_return_shortcircuit` (a returning instruction's
  continuation is never executed). Related equational sanity fact:
  `print2_body_trace` (`formal/Examples.v`) — the denotation emits the
  two log events in program order, nothing else. **Difficulty.** ★

### PO-005 — Observable-effect preservation lemmas for T-1
- **Statement.** Local lemmas: denoting `ICall lhs f args` then using `lhs`
  emits the same `callE`/`IO` events, in the same order, as the split form
  through a fresh temp; freshness premise stated syntactically.
- **Motivation.** The heart of T-1's simulation argument, isolated from the
  simulation plumbing.
- **Dependencies.** PO-002, PO-004; freshness definition.
- **Technique.** Denotation unfolding + bind associativity; no coinduction.
- **Status.** **proved (Qed), 2026-07-29** — materialized as
  `env_lookup_update_same` (`formal/Domain.v`: reading back the variable
  just written in an arbitrary environment — the fact that makes the
  fresh-temp copy transparent) plus the freshness infrastructure
  `fresh_temp_is_fresh` (`formal/Transform.v`, general theorem over all
  fragment syntax, by mutual nested induction). **Difficulty.** ★★

### PO-006 — Simulation soundness for T-1 (both directions)
- **Statement.** `ISim.t open Src Tgt emp Ist` and `ISim.t open Tgt Src emp
  Ist` for the module pair (p, transpile p), for every admissible p —
  i.e., a *schematic* simulation over the syntax of the fragment, or (fallback,
  narrower) for the concrete PoC program family if full schematicity proves
  too hard for M4.
- **Motivation.** Direct precursor of contextual refinement via adequacy.
- **Dependencies.** PO-003–PO-005; ADR-3 (trivial resource); ADR-7 premise
  (source non-stuck admissibility).
- **Technique.** CRIS `isim` tactics (`cStartFunSim`, `cStepsS/T`, `cCall`,
  `cCoind`, `unfoldIterS/T`); induction over fragment syntax for the
  schematic version. **Risk:** the schematic-vs-concrete scope decision is
  the main M4 unknown; decide after PO-005.
- **Status.** **proved for the exemplar family (Qed), 2026-07-29** —
  `formal/T1Proof.v`: `sim_st : ISim.t open T1Src T1Tgt emp Ist` and
  `sim_ts : ISim.t open T1Tgt T1Src emp Ist`, where `T1Tgt` is the
  literal output of `t1_prog` and main calls an unknown context function
  and prints — per-function lemmas cover arbitrary call arguments
  (arity-mismatch UB branches included) and arbitrary contexts.
  The **schematic ∀-programs version remains open** (this PO's recorded
  fallback was exercised; reopening it is the main M5+ item).
  **Difficulty.** ★★★★

### PO-007 — Adequacy application
- **Statement.** From PO-006 via CRIS `main_adequacy` (+
  `refines_adequacy` to land in plain trace inclusion):
  `ctx_refines Tgt Src` and `ctx_refines Src Tgt`.
- **Motivation.** Converts simulation into the §5 refinement notion.
- **Dependencies.** PO-006. CRIS's adequacy theorems are reused as proved
  facts [RF], not re-proved.
- **Technique.** Direct application (`eapply main_adequacy, sim` per the
  workshop pattern).
- **Status.** **proved (Qed), 2026-07-29** — `ctxr_tgt_src`,
  `ctxr_src_tgt` (`formal/T1Proof.v`). **Difficulty.** ★

### PO-008 — Mutual contextual refinement ⇒ contextual equivalence
- **Statement.** `ctx_equiv (denote p) (denote (transpile p))` assembled
  from PO-007's two directions; behavior-level corollary: for every
  admissible context C, the linked closed programs have equal behavior sets.
- **Motivation.** The project's stated equivalence shape (mutual refinement).
- **Dependencies.** PO-007.
- **Technique.** Conjunction + set-inclusion antisymmetry at trace level.
- **Status.** **proved (Qed), 2026-07-29** — the conjunction is
  `t1_contextual_equivalence` (`formal/T1Proof.v`); trace-level corollary
  via `refines_adequacy` not yet unfolded (future work). **Difficulty.** ★

### PO-009 — Transpiler correctness (final theorem, M4 deliverable)
- **Statement.** `∀ p, admissible p → fresh_ok p → ctx_equiv (denote p)
  (denote (transpile p))` with every premise explicit in the statement; no
  `Admitted`, no axioms.
- **Motivation.** End-to-end deliverable #7 of the project brief.
- **Dependencies.** PO-008 (+ admissibility/freshness definitions from
  PO-005).
- **Technique.** Composition of the above.
- **Status.** **proved for the exemplar family (Qed), 2026-07-29** —
  `t1_contextual_equivalence : (⊢ ctx_refines (ir_mod mn (t1_prog
  t1ex_src)) (ir_mod mn t1ex_src)) ∧ (⊢ ctx_refines (ir_mod mn t1ex_src)
  (ir_mod mn (t1_prog t1ex_src)))`. Freshness premise is discharged by
  the general theorem `fresh_temp_is_fresh`; admissibility of the
  exemplar is by construction (fragment syntax only, address-free
  observables). Axiom audit: `Print Assumptions` reports only the
  framework's standard axiom base (proof irrelevance, functional
  extensionality, UIP, constructive definite description, classic,
  ITreeS `bisim_is_eq`) — no project axioms, no `Admitted`.
  The ∀-programs schematic statement remains open under PO-006.
  **Difficulty.** ★★ (given PO-006).

### PO-010 — Syntactic-context bridge (deferred by user decision)
- **Statement.** Define IR-Core syntactic contexts C[·] and `plug`; prove
  that for admissible C, `plug C ·` is expressible as linking against a
  derived module, hence linking-based ≈ctx implies syntactic ≈ctx for the
  fragment.
- **Motivation.** Recovers the classical statement; user decision
  2026-07-29: "linking now, syntactic later".
- **Dependencies.** PO-008; a hole-position taxonomy for the fragment.
- **Status.** deferred (post-M5 candidate). **Difficulty.** ★★★

### PO-011 — Differential-validation harness (testing, not proof)
- **Statement.** (a) Executable comparison: for each fragment-compatible
  `tests/ir` program, ESMeta's interpreter result (final `RESULT`, print
  sequence) matches the model's execution; (b) effect-sensitivity: harness
  detects call duplication, reordering across prints, and skipped
  evaluation; (c) ≥1 negative test with an intentionally wrong
  transformation is rejected.
- **Motivation.** Deliverable #8; guards Fragment.v against drift (AN §7);
  validates ADR-5's exactness assumption [EA→tested].
- **Dependencies.** PO-002; Scala exporter or `ir-eval`; extraction or
  simulation-by-hand of the model's interpreter.
- **Technique (as implemented).** Scala-side exporter/runner
  (`esmeta.fv.FVExport`: translates fragment programs to Rocq terms,
  captures ESMeta observables) + a fuel-based executable reference
  interpreter *inside* Rocq (`formal/Exec.v`) checked by `vm_compute` —
  no extraction needed.
- **Status.** **passing (testing), 2026-07-29** —
  (a) 18/18 exported `tests/ir` programs match ESMeta observables
  (8 substantive: sum, gcd, fibo, branch, parity, inst/{let,return,assert};
  10 near-empty placeholder fixtures), 26 skipped with per-construct
  reasons (`make validate`);
  (b) effect-sensitivity: duplicate-call, reorder, and skipped-call
  corruptions all detected (`formal/Validation.v`, `vm_compute` +
  `discriminate`);
  (c) negative transformation tests: 3 intentionally wrong transforms
  rejected; the correct fresh-temp introduction preserves the trace
  (`eff_temp_intro_preserves`).
  Caveat: validates `Exec.v`; its agreement with the ITree denotation is
  a documented engineering assumption until PO-013. **Difficulty.** ★★.
- **Classification.** testing-only — never cited as proof.

### PO-012 — Fragment-level faithfulness to ESMeta execution (stretch)
- **Statement.** For fragment programs, ESMeta's CFG-interpreter execution
  relates to the ITree denotation: same termination status, same print
  trace, related final result (Strategy A of the brief, restricted to
  IR-Core).
- **Motivation.** Upgrades claim (2) of AN §1 from testing to proof;
  discharges ADR-4's trusted gap and TCB item 3.
- **Dependencies.** PO-002; a mechanized model of `CFGBuilder` (~60 lines
  [RF]) and the cursor small-step; substantial new infrastructure.
- **Status.** deferred (post-M4; explicitly not required for M4).
  **Difficulty.** ★★★★★

### PO-013 — Executable interpreter agrees with the denotation (deferred)
- **Statement.** For every fragment program `p` and fuel `n`:
  if `run n p = Ok (v, outs)` then the closed ITree denotation of `p` has
  the behavior `Tr.done`-terminating with (an encoding of) `v` after
  exactly the `IO "esmeta.print"` events `outs` in order; and
  corresponding statements for `Stuck` (UB) and non-termination
  (for all fuels, `OOF`).
- **Motivation.** Upgrades the differential evidence of PO-011 from
  "validates `Exec.v`" to "validates the denotation"; closes the
  clause-parallelism engineering assumption documented in `Exec.v`.
- **Dependencies.** PO-002; behavior-level reasoning over `LMod.compile`.
- **Technique.** Fuel-indexed simulation between `exec_inst` and
  `denote_inst` composed with the CRIS compile pipeline; substantial.
- **Status.** deferred (post-M4; optional strengthening).
  **Difficulty.** ★★★★

### PO-014 — T-2 optional-field desugaring equivalence
> **Demoted by ADR-10** (2026-07-29): proved, but over a *synthetic*
> source construct, so it is a model-internal exercise. Superseded as the
> project's optional-chaining result by PO-015 (T-3). Do not cite as an
> ECMAScript-level claim.
- **Statement.** For the T-2 exemplar (main receives a value from an
  unknown context call, applies `EOptField`, prints):
  `ctx_refines (ir_mod mn (t2_prog p)) (ir_mod mn p)` and the converse,
  where `t2_prog` is the real desugaring (`Transform.v`) and the source
  uses the synthetic construct of ADR-9.
- **Motivation.** Second transformation theorem; exercises the guard
  obligation (nullish branch must not touch the heap) and abstract-value
  case analysis at the simulation level, which T-1 did not need.
- **Dependencies.** ADR-9; PO-005 (`env_lookup_update_same`); PO-006's
  proof idiom; new: store-event (`SGet`) matching under an abstract
  post-call store.
- **Technique.** Same isim skeleton as T-1 + case analysis on the
  receiver value (`val_eqb` equations + `destruct rv`), symmetric UB
  closers for ill-typed receivers, paired `SGet` stepping on equal
  stores for the address case.
- **Status.** **proved for the exemplar family (Qed), 2026-07-29** —
  `formal/T2Proof.v`: `t2_contextual_equivalence`, both `ctx_refines`
  directions via `main_adequacy`, with complete receiver case analysis
  (nullish ×2, four ill-typed shapes as symmetric UB, address case with
  paired `SGet` on equal stores via `wsim_sget_src/tgt` + downcast /
  record-shape / field-presence case splits). Executable validation:
  `t2v_rec_preserved`, `t2v_null_preserved`, negative `t2v_bad_detected`
  (all vm_compute). Axiom audit: framework base only. Schematic
  ∀-programs version open, as with T-1. **Difficulty.** ★★★★

### PO-015 — T-3 spec-shaped optional access: receiver-once (supersedes PO-014)
- **Statement.** For `t3ex_src` (`x = f()?.prop` modelled in **mirrored
  IR only**: receiver = call to a context-supplied function, nullish
  guard, property access only on the non-nullish branch):
  `ctx_refines (ir_mod mn (t1_prog t3ex_src)) (ir_mod mn t3ex_src)` and
  the converse.
- **Motivation.** ADR-10: the specification itself *is* the guarded form,
  so no synthetic construct is needed; and with an effectful receiver,
  "evaluated exactly once" becomes an *observable* obligation (T-2's
  version was only syntactic, since IR-Core has no getters).
- **Dependencies.** ADR-10; `env_lookup_update_same`; T-2's proof
  machinery (abstract-store `SGet` pairing, receiver case analysis);
  `fresh_temp_is_fresh` for the freshness premise.
- **Technique.** T-1/T-2 isim skeleton + full receiver case analysis
  (undefined / null / four non-address shapes as symmetric UB / address
  with paired `SGet`, downcast, record-shape and field-presence splits).
- **Status.** **proved (Qed), 2026-07-29** — `formal/T3Proof.v`:
  `t3_contextual_equivalence`. Executable validation in `Validation.v`:
  `t3v_src_trace` = `[7;42]`, `t3v_null_trace` = `[7;undefined]`,
  `t3v_preserved`/`t3v_null_preserved`, and the receiver-once negative
  test `t3v_reeval_detected` (`[7;42]` vs `[7;7;42]`). Axiom audit:
  framework base only.
- **Explicitly NOT proved.** That JavaScript `?.` *is* this guarded form.
  Evidence chain and the one unmechanized modelling step are tabulated in
  ADR-10; the model diverges from JS on primitive receivers (L-8).
  Closing it is the PO-012-style spec-level route. **Difficulty.** ★★★★

---

## Dependency graph (summary)

```
PO-001 → PO-002 → {PO-003, PO-004} → PO-005 → PO-006 → PO-007 → PO-008 → PO-009
                    PO-002 → PO-011 (testing)         PO-008 → PO-010 (deferred)
                    PO-002 → PO-012 (deferred)        PO-002 → PO-013 (deferred)
```
