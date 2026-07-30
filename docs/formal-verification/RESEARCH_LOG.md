# Research Log — ESMetaFV

Living log (Rule 5). Newest entry first. Every entry: Date, Objective,
Current Status, Observations, Design Decisions, Proof Progress, Failed
Attempts, Research Debt, Open Questions, Next Steps, Relevant Commits,
Relevant Papers.

---

## 2026-07-30 (later) — G2 complete: 2909/2951 (99%), 39/39 match, and ESMeta silently skips assertions

**Objective.** Stage G2: `EGrammarSymbol`, `EInstanceOf`, `ESubstring`,
`ESourceText`, `EParse` (cached-AST case only). Gate: >= 2890/2951 with
every remaining blocker documented as UB.

**Current Status.** Gate met.

```
[fv] translatable as-is: 2909 / 2951 (99%)
[fv] exported 39 program(s), skipped 8 (each with a reason)
make: rc=0, 0 errors, 11 modules;  make validate: rc=0, 0 errors
```

Remaining blockers, all UB and all documented: EMathOp 21, ECont 11,
ESyntactic 4, EConvert(ToStr) 2, non-integer EMath 2, ETrim 1, ERandom 1
(plus ELexical and EDebug, which no spec function uses).

**Observations.**

1. *ESMeta's `IAssert` silently swallows evaluation errors — so a corpus
   program can "pass" without its assertions ever running
   [repository fact, Interpreter.scala:147-151].*

   ```scala
   case IAssert(expr) =>
     optional(eval(expr)) match
       case None             => /* skip not yet compiled assertions */
       case Some(Bool(true)) =>
       case v                => throw AssertionFail(expr)
   ```

   `optional` is `try Some(f) catch { case _: Throwable => None }`
   (BaseUtils.scala:76-78). This surfaced as a second differential
   mismatch (model `Stuck` vs ESMeta `Ok (VUndef, [])`) on
   `tests/ir/expr/substring.ir`, which contains

   ```
   assert (= (substring str 0f 1f) "0")
   ```

   `0f` parses to `ENumber(0.0)`, and `ESubstring` calls `.asInt`, which
   accepts only `Math` (Value.scala:23-25). So the call throws, `optional`
   swallows it, and **three of that file's assertions are never checked**
   — while our model, which does not swallow UB, gets stuck.

   *Neither side is wrong about ECMAScript; the file is not a valid
   differential test.* Making `IAssert` swallow `Stuck` to match would
   have been the wrong fix: our `Stuck` also means "the model does not
   implement this", so catching it would silently mask exactly the
   modelling gaps this harness exists to find. Instead the exporter now
   *counts* skipped assertions and refuses such programs with a reason
   (ADR-14). It reports `substring.ir: ESMeta silently skipped 3
   assertion(s)` and the Math-index cases moved to
   `formal/validation/extra/substring-int.ir`, which also covers the two
   clauses upstream never reaches: an omitted upper bound, and an upper
   bound strictly greater than the length (ESMeta degrades that to
   `substring(from)` rather than raising — Interpreter.scala:240-242).

2. *ADR-13's grep obligation paid off twice more [verified by command].*
   Constructor counts in `Generated.v` after wiring each construct:
   `EGrammarSymbol` 0 and `EInstanceOf` 0 — no file in `tests/ir/` mentions
   either. Closed with `formal/validation/extra/grammar-symbol.ir`
   (5 and 4 occurrences now). Still at **zero**: `ESourceText` and
   `EParse`. Both need an AST value, and the model has no way to build one
   — `EParse`'s fast path needs the cached AST an initial state would
   supply, and `ESyntactic` is UB because `subIdx` is grammar-derived
   (Ast.scala:116-128) and cannot be precomputed for a node built at
   runtime. So they are *implemented and unexercised* until G4. Said
   plainly rather than left to be inferred from a green run.

3. *Run parameters belong in the state, not in a new parameter
   [repository fact].* `sourceText` and `cachedAst` are `val` fields of
   `State` (State.scala:17-18), so the model puts them where ESMeta does:
   two new store keys `src$`/`cached$` in the CRIS keyed store, and two
   fields on `xstate`. `denote_expr` needed no new argument. `prog` gained
   `p_source`/`p_cached` with `mkProg` kept as a smart constructor, so
   every existing one-argument `mkProg` call still typechecks.

4. *`ESubstring` is exact under D-1 [repository fact].* A `cstr` *is*
   Java's UTF-16 code-unit sequence, so `substring` is the same index
   arithmetic; `java.lang.String.substring` throws unless
   `0 <= from <= to <= length`, which is UB here.

5. *`EInstanceOf` is total.* `(instanceof 42 g)` is `false`, not an error
   (Interpreter.scala:310-314) — the wildcard `GrammarSymbol("")` matches
   any *syntactic* node, while a lexical node still matches by name.

**Design Decisions.** ADR-14 (refuse programs whose assertions ESMeta
silently skipped). `ELexical` deliberately NOT added: it blocks zero spec
functions, and modelling it would force the lexeme through the model's
ASCII-only `ALex.str`, adding a hidden restriction for no coverage gain.

**Proof Progress.** `T1Proof.v` still compiles. Extending the initial
store from one key to three broke two `ir_smod` obligations; both are
re-proved, no new assumptions.

**Failed Attempts.**
- `rewrite !dom_insert_L dom_singleton_L` fails: with Iris loaded
  `rewrite` is ssreflect's (so no commas between rules), and `!` is greedy
  enough to unfold the singleton itself, leaving `dom_singleton_L` nothing
  to match. Replaced with an `assert (k = _ \/ _ \/ _) by set_solver`.
- `destruct Hor as [->|[->|->]]` is rejected under ssreflect; used
  explicit equation names and `rewrite`.
- Two `Non exhaustive pattern-matching` rounds in `Transform.v` again —
  five new `expr` constructors, three traversals, one bullet structure.

**Research Debt.**
- `ESourceText`/`EParse` implemented but unexercised (see 2 above).
- `EParse` models only the cached-AST branch; a real parse is UB.
- The `translatable as-is` metric is **syntactic**. It counts a function
  as translatable when the exporter can emit it, not when it will run:
  `EYet`, `EParse`'s non-cached branch, named AST field access and
  `ETypeOf` on addresses are all translatable-but-UB. 2909/2951 is a
  bound on executable coverage, not a measurement of it.

**Open Questions.** None new.

**Next Steps.** G3 (lexical values via the exporter), then G4 (initial-state
exporter + extraction harness), which is also what finally makes
`ESourceText`/`EParse` reachable.

**Relevant Commits.** ESMeta `dev` @ `de537ba9` (baseline).

---

## 2026-07-30 — G1 complete: 2804/2951 spec functions, 37/37 corpus programs match

**Objective.** Close stage G1 of the Test262 goal: D-1 (UTF-16 code-unit
strings) plus the numeric tower, gated at FVSpecScan >= 2800/2951 and
FVExport > 28 programs all matching ESMeta.

**Current Status.** G1 gate met. `formal/` builds clean (12 modules);
`make validate` is green over 37 differential programs; the falsification
check fails and recovers as required.

```
[fv] translatable as-is: 2804 / 2951 (95%)
[fv] exported 37 program(s), skipped 8 (each with a reason)
formal/: rc=0, 0 errors, 12 .vo
make validate: rc=0, 0 errors
```

**Observations.**

1. *A genuine differential mismatch, found and diagnosed before any fix
   [verified by command].* After the D-1 migration `make validate` reported

   ```
   File "./validation/Generated.v", line 103:
   Error: Unable to unify "Ok (VUndef, [])" with "Stuck".
   ```

   on `tests/ir/expr/ref.ir`, whose body asserts `(= str[0] 97cu)`.
   **The model was wrong, ESMeta was right.** ESMeta indexes a *string*
   value and returns a code unit — `State.scala:57-59`,
   `def apply(str: String, field: Value) = field match { case Math(k) =>
   CodeUnit(str(k.toInt)); case _ => throw WrongStringRef(...) }` — while
   `read_target`/`read_target_x` handled only `VAddr` and `VAst` bases and
   fell through to UB. Added the missing case verbatim in both
   `Semantics.v` and `Exec.v`; out-of-range and non-`Math` fields stay UB
   because Scala throws there. This case only became *expressible* under
   D-1: a byte-string model could not have returned a code unit.

2. *The cumulative-unlock table chose the work, not intuition
   [verified by command].* The numeric tower alone reached 2704/2951. The
   scanner's cumulative table showed `EVariadic` then `EContains` are
   exactly what crosses the gate (2761, then 2804), so both were pulled
   forward from G2. Remaining blockers, all still UB: EGrammarSymbol 49,
   ESourceText 37, EParse 33, ESubstring 22, EMathOp 21, ECont 11,
   EInstanceOf 4, ESyntactic 4, EConvert(ToStr) 2, EMath(non-integer) 2,
   ETrim 1, ERandom 1.

3. *Scala `==` on `Number` is `doubleEquals`, so `EContains` is exact
   [repository fact].* `case class Number(double: Double) extends Numeric
   with DoubleEquals` (`state/Value.scala:143`) overrides `equals` with
   `doubleEquals` (`util/DoubleEquals.scala:7-10`). `EContains` is
   `Bool(l.values.contains(e))` (`Interpreter.scala:233-236`), i.e. Scala
   `==`, which is precisely what `val_eqb` already models via
   `num_struct_eqb`. No new equality notion was needed — the existing
   comment in `Domain.v` is now verified rather than assumed.

4. *`EVariadic` is order-sensitive in a way worth writing down
   [repository fact].* `Interpreter.scala:669-693`: `Min` short-circuits on
   `-inf`, *then* drops every `+inf`, and returns `+inf` if that leaves
   nothing; only then does `asMath` demand every survivor be a `Math`.
   `Concat` maps `Str(s) |-> s` and `CodeUnit(c) |-> c.toString` — under
   D-1 the latter is a one-code-unit string, so `Concat` is exact.
   An empty argument list is `InvalidVariadicOp` (UB).

5. *A silent coverage hole, found and closed [verified by command].*
   After wiring `EVariadic`, `grep -o 'VoMin|VoMax|VoConcat' Generated.v`
   showed `VoConcat` 4 and `VoMin`/`VoMax` **0**: the only corpus test for
   min/max, `tests/ir/expr/variadic.ir`, is skipped for its `3.2` literal
   (ADR-5). Green validation would have implied coverage that did not
   exist. Closed by ADR-13 (below) with
   `formal/validation/extra/variadic-int.ir`, the integer-and-infinity
   lines of the upstream file copied verbatim; expectations still come
   from running ESMeta, so it is a real differential test. Now VoMin 4,
   VoMax 4.

6. *`Print Assumptions` for `T1Proof.v` now also lists Coq's
   `PrimFloat`/`PrimInt63` primitives [verified by command].* This is a
   consequence of D-2 putting `PrimFloat` in the value domain. They are
   kernel-realised primitive operators, not admitted lemmas; the framework
   axioms are unchanged (proof_irrelevance, functional_extensionality_dep,
   eq_rect_eq, constructive_definite_description, classic, bisim_is_eq).
   The goal's invariant 5 was amended to say so.

**Design Decisions.** ADR-13 (targeted differential corpus in
`formal/validation/extra/`). Scope change recorded in the goal artifacts:
`T2Proof.v`/`T3Proof.v` moved to `formal/attic/` per the user's
instruction to focus on Test262, so invariant 5 covers `T1Proof.v` only.
Their *statements* are untouched; only the Ltac1 scripts broke (see
`formal/attic/README.md`).

**Proof Progress.** None this stage — G1 is modelling and differential
testing. `T1Proof.v` still compiles unchanged.

**Failed Attempts.**
- `Generated.v` newly exported `tests/ir/map-unzip.ir` (unlocked by
  `EContains`), which contains a `Number` map key, and the generated header
  did not `Require Floats`: `Error: Unknown scope delimiting key float.`
  Fixed in the exporter header rather than by hand-editing the generated
  file.
- Adding two `expr` constructors broke four exhaustive matches in
  `Transform.v` (`temp_fresh_expr`, `temp_bound_expr`, `opt_free_expr`) and
  the bullet structure of `temp_fresh_expr_bound`. Mechanical, but it is
  the recurring cost of every fragment extension.

**Research Debt.**
- `VoMin`/`VoMax` are exercised only over `Z` and `±inf`; ESMeta's `Math`
  is `BigDecimal` (ADR-5 limitation L-2), so non-integer min/max is
  untested by construction.
- `EContains` is tested over `Math`, `BigInt` and `Number` elements via
  `map-unzip.ir`; equality over `VAst` elements is still untested and, per
  the `Eq`-on-AST finding, is reference equality in ESMeta.

**Open Questions.** No new ones. The string-indexing gap was a missing
faithful case, not a semantic ambiguity, so it did not need an OQ.

**Next Steps.** G2: `EGrammarSymbol`, `ESourceText`, `EParse` (cached-AST
case only), `ESubstring`, `ETrim`, `EInstanceOf`, `ESyntactic`, `ELexical`
— target >= 2890/2951.

**Relevant Commits.** ESMeta `dev` @ `de537ba9` (baseline). Pins: ecma262
`84b38ad8`; rocq-cris `c0bcd04e`.

---

## 2026-07-29 (late night) — Scaling toward JS-level execution: 8 -> 2417 of 2951 spec functions

**Objective.** User asked to model enough that Test262 can actually run.
Measure the real requirement, then build toward it.

**Current Status.** Model constructor coverage over the compiled spec:
**2417 / 2951 functions (82%)**, up from 8 at the start of the session.
13 modules build clean, differential validation passes, no `Admitted`, and
T-1/T-2/T-3 still hold under the extended model.

**Measurements (new tooling, `esmeta.fv`).**
- `FVSpecScan` reparses the compiled spec IR with ESMeta's own parser and
  reports blocking constructors with a cumulative-unlock ordering.
- Call-graph closure from `OptionalExpression[0,0].Evaluation`:
  1628/2951 with calls expanded, **1513** with user-code calls treated as
  boundaries, **290** once the receiver's own `Evaluation` is a black box,
  **21** for property access alone. So the JS-level route needs the
  reachable closure — NOT all of Test262 — and open simulation lets the
  shared parts stay unmodelled as matched calls.
- `FVTyModel` exports ESMeta's record subtyping (112 decls / 74 edges) to
  the generated `formal/TyModel.v`.

**Implemented this session.** Operators Abs/Floor/Div/Mod/Equal;
`EExists`, `ETypeOf`, `ETypeCheck` (ADR-11), `EYet`, `EMap`, `EKeys`,
`ECopy`; `IPush`, `IPop`, `IExpand`, `IDelete`; insertion-ordered `OMap`;
AST values with `ISdoCall` dispatch (ADR-12) including the production-chain
walk and `DEFAULT:` fallback; AST child indexing and `ESizeOf` on ASTs.
Each clause cites its `Interpreter.scala`/`Obj.scala`/`Ast.scala`
counterpart; where faithfulness needs machinery we lack, the model raises
**UB instead of guessing** (ETypeOf on addresses, UTF-16 string length,
int-sorted `EKeys`, named AST fields, lexical SDOs).

**Roadmap to actually running Test262 (honest accounting).**
1. Numeric tower — `ENumber` (162), `EConvert` (245), `EInfinity` (33),
   `ECodeUnit` (30), `EBigInt` (12): ~480 functions. Rocq `PrimFloat`
   gives vm_compute-executable IEEE-754 doubles. `EMathOp` (21,
   transcendentals) has no PrimFloat support -> UB.
2. Cheap remainder — `EVariadic` (72), `EContains` (49), `bop:**` (49),
   `EGrammarSymbol` (49), `ESourceText` (37), `EParse` (33, only the
   cached-AST case is needed since ESMeta short-circuits the main script),
   `ESubstring` (22).
3. `ESValueParser` in Rocq (462 Scala lines): lexical SDOs
   (StringValue/NumericValue/MV/SV/TV/TRV). Required for any literal.
4. Initial-state export: ESMeta's intrinsics heap, global object, realm
   (hundreds-to-thousands of objects) as exported data.
5. Efficient heap: the current `list obj` heap is O(n) per access; real
   execution needs a map/trie or vm_compute will crawl.
6. `ECont` (11) for generators/async -> UB for most tests.
Items 1, 2 and 4 are mechanical; 3 and 5 are real chunks. This is several
more sessions at the present pace, not one.

**Failed Attempts.** `ast_chain` rejected by the guard checker (the
single-present-child projection is not a structural subterm) -> fuel by
`ast_size`. `.(` parsed as record projection in `| UFloor.(* … *)` ->
needed a space. `++` re-parsed in `string_scope` inside list code ->
`%list`. A blanket `try (apply andb_true_intro; …)` in the freshness proof
intercepted `IIf` -> reverted to explicit bullets. `ty_check` placed after
`val_eqb` although the latter now calls `ast_eqb` -> reordered.

**Research Debt.** `Semantics.denote_inst` and `Exec.exec_inst` now take
the program's function-name list for SDO resolution; `ir_mod`'s signature
was preserved deliberately so the T-1/T-2/T-3 proofs did not have to
change. `TyModel.v` is generated but checked in — keep it in sync when the
spec pin moves.

**Open Questions.** Whether `EMathOp`'s transcendentals can be given any
executable meaning in Rocq (probably not without a float library) — they
stay UB. Whether the intrinsics heap export is small enough for
vm_compute at usable speed (item 5 depends on it).

**Next Steps.** Numeric tower (biggest unlock), then the cheap remainder,
then `ESValueParser`.

**Relevant Commits.** `b6f0d4e1` and the follow-up on `dev-cris`.

**Relevant Papers.** Unchanged.

---

## 2026-07-29 (night, correction) — ADR-10: the spec IS the guard; T-3 replaces T-2

**Objective.** Answer a user challenge: optional chaining already parses
in ESMeta, so was T-2's synthetic `EOptField` necessary?

**Current Status.** Challenge upheld; correction implemented and proved.
`t3_contextual_equivalence` (`formal/T3Proof.v`) — mutual contextual
refinement of the spec-shaped optional-access program and its `t1_prog`
transform, **mirrored IR only, no synthetic construct**. All builds green
(11 → 12 modules), validation extended, axiom base unchanged.

**Observations (evidence gathered, strongest last).**
- **[VF]** ESMeta v0.7.3 source parses `var x = a?.prop` →
  `|OptionalExpression|[FF]<0>( … |OptionalChain|[FF]<2>(
  |IdentifierName|(prop) ) )`. The user was right. (Aside: the
  checked-in `bin/esmeta` is a stale v0.6.4 that dies on the current
  spec — must use `sbt run`.)
- **[PF]** `sec-optional-chaining-evaluation` defines `?.` as: receiver
  evaluated once → `GetValue` → if undefined/null return undefined →
  else `ChainEvaluation`.
- **[RF]** `sbt "run dump-debugger"` →
  `logs/dump/debugger/funcs.json`: the *compiled* IR of
  `OptionalExpression[0,0].Evaluation` literally contains
  `if (|| (= baseValue undefined) (= baseValue null)) { call %2 =
  clo<"NormalCompletion">(undefined); return %2 }` after one
  `sdo-call`+`GetValue`, then `ChainEvaluation`. All three
  `OptionalExpression` productions share the shape.
- Therefore **the specification itself is the desugaring**: the guard is
  introduced by spec→IR compilation, so at IR level there is nothing to
  desugar and inventing a source form was unnecessary. ADR-9's factual
  claim (no IR `?.` primitive) stands; its design decision was wrong.

**Design Decisions.** ADR-10 written as an explicit correction record
(ADR-9 marked superseded-in-part, retained since T-2 is proved and its
machinery is reused). T-3 = spec-shaped source + `t1_prog`, receiver is
an effectful **context call** so "exactly once" is observable (T-2's
version was only syntactic — IR-Core has no getters). Guard test order
flipped to `undefined`-then-`null` to match the compiled IR verbatim
(unobservable, but keeps the correspondence visible). T-2 demoted to a
model-internal exercise; PO-015 added, PO-014 marked demoted.

**Honesty correction made to my own prose.** The first draft of
T3Proof.v's header said the program was "written exactly as the
ECMAScript specification defines it" — an overclaim. Rewritten: it models
the *control shape*; a "WHAT IS NOT ESTABLISHED" section now tabulates the
evidence chain and names the single unmechanized link (model ↔ compiled
IR), the abstractions (References/GetValue → no getters, no prototype
chain, no ToObject, no abrupt completions) and the outright **divergence**
`(42)?.foo` = `undefined` in JS vs UB in the model (new limitation L-8).
Asked directly whether "JS `?.` = guard form" is proven, the answer
recorded everywhere is **no**.

**Proof Progress.** PO-015 proved (exemplar family, both directions).
Reused unchanged: T-2's `SGet`-pairing and receiver case analysis,
`env_lookup_update_same`, `fresh_temp_is_fresh`, `main_adequacy`.
Validation added: `t3v_src_trace` `[7;42]`, `t3v_null_trace`
`[7;undefined]`, `t3v_preserved`, `t3v_null_preserved`, and the
receiver-once negative test `t3v_reeval_detected` (`[7;42]` vs
`[7;7;42]`).

**Failed Attempts.** Factored the proof prefix into an `Ltac t3prefix`
referencing `st_tgt`: Ltac1 resolves free term identifiers at *definition*
time, so it failed with "reference st_tgt was not found". Inlined at each
use site, as T1/T2 already did.

**Research Debt.** The `?.`-to-model link is the honest gap (tabulated in
ADR-10); nested chains (`a?.b?.c`), call chains (`f?.()`), and bracket
form remain unmodelled. `logs/dump/debugger/` is now populated — useful
for future spec-IR inspection, and gitignored.

**Open Questions.** OQ-4/OQ-8 unchanged. New: whether a *fragment-level*
faithfulness proof against the compiled `OptionalExpression` IR is
feasible without the full object model (probably not — `GetValue` reaches
getters), which is why it stays [FW].

**Next Steps.** Unchanged candidates: schematic ∀p generalization,
PO-013, fragment growth.

**Relevant Commits.** `bac2a188`, `8c163fc9` on `dev-cris`; this work
pending commit.

**Relevant Papers.** ecma262 @ 84b38ad8 §13.3.9 (optional chains); CRIS;
Interaction Trees.

---

## 2026-07-29 (night, tooling) — Coqtail-MCP + interactive model execution

Work committed on branch `dev-cris` (bac2a188; local only, per user).
Installed [Coqtail-MCP](https://github.com/park-sunho/Coqtail-mcp) in an
isolated venv (gotcha: requires `mcp<2` — SDK 2.0 removed
`mcp.server.fastmcp`) registered against the `cris-workshop` coqidetop;
future sessions can step proofs (`rocq_step_to`/`rocq_goals`) instead of
the batch goal-printing probes, and EXECUTE fragment programs in the
model via `rocq_query` + `Eval vm_compute in (run …)` (path verified in
batch: gcd and t1_prog gcd both `Ok (VUndef, nil)`).

**Boundary restated (user asked whether Test262 can now run "via CRIS
itree"):** No — Test262 is JavaScript and requires the full spec-derived
IR; the model runs IR-Core only. The execution matrix and rationale are
documented in `formal/INTERACTIVE.md`; making Test262 model-runnable is
the [FW] JS-level route (spec-IR mechanization), not configuration.

---

## 2026-07-29 (night) — T-2: optional-field desugaring proved

**Objective.** Answer "can an optional-chaining-style transformation be
proved?" by proving one: the ADR-9 desugaring (PO-014).

**Current Status.** **Done, all Qed.** `t2_contextual_equivalence`
(`formal/T2Proof.v`): mutual contextual refinement of
`ir_mod mn t2ex_src` and `ir_mod mn (t2_prog t2ex_src)` over all linking
contexts, where main receives an arbitrary value from an unknown context
call, applies `EOptField`, prints. Fragment extended: `ERecord`
(mirrored; semantics verified against Interpreter.scala:337-338,
Obj.scala:113-121 before implementing) and `EOptField` (SYNTHETIC,
ADR-9). Differential corpus grew to 19/19 passing (`inst/assign.ir`
now exports thanks to `ERecord`). Executable T-2 validation:
record/nullish branches preserved by the real `t2_prog`; unguarded
desugaring detected (`t2v_bad_detected` — Stuck vs printed undefined).

**Observations (new proof machinery).**
- Store reads under an ABSTRACT post-call store: `cStepsS/T` do not fire
  on `SGet` when (a) wrapped in our definitions (`unfold get_obj, cgetU`
  needed) and (b) the store is abstract (`state_lookup_simpl` fails) —
  apply `wsim_sget_src`/`wsim_sget_tgt` manually; the rule returns
  `default tt↑ (mjoin (st !! k))`, so absent keys yield `tt↑` whose
  obj-downcast fails into UB — exactly "reading unallocated memory is
  UB", and identical on both sides since the stores are equal.
- Ill-typed receiver branches leave `_q : False` in context after the
  spec-side `Take False` steps: `contradiction` closes them.
- Unbounded `repeat first [...]` normalization loops can diverge/crawl
  with these tactic sets; bounded `do 6 (try …)` rounds are reliable.
- OQ-12 (new, resolved by reading WSim.v:179-187): SGet on an absent
  key is NOT stuck at the event level — it returns the `tt↑` default;
  UB arises from the typed downcast. Recorded because Exec.v models
  heap_get absence as immediate Stuck — same observable outcome (UB)
  through a different mechanism; PO-013 must map this correctly.

**Design Decisions.** ADR-9 (synthetic source construct + honesty
boundary + mirrored ERecord); T-2 restriction to binding positions
(`t2_ok_inst`); receiver-once clause is syntactic-only within the
fragment (no getters) — stated in ADR-9, not claimed as an observable
obligation.

**Proof Progress.** PO-014 proved (exemplar family). Reused unchanged:
`env_lookup_update_same`, the T-1 skeleton, `main_adequacy`. New shared
tactic `t2branches` (receiver case analysis).

**Failed Attempts.** The convergence-loop tactic (`repeat first [...]`)
ran >5 minutes without terminating visibly — replaced by bounded rounds.
`cStep as reply` again failed pre-normalization (same `log_val` unfold
issue as T-1; now folded into the branch tactic).

**Research Debt.** `t2branches`' hypothesis-pattern matching
(`fs : list (string * val)`) relies on most-recent-first matching and
would pick `captured` if the field list were absent — acceptable in the
validated scripts, but brittle for reuse; name bindings explicitly if
the tactic is generalized. Exec.v vs denotation absent-key mechanism
divergence (OQ-12 note above) for PO-013.

**Open Questions.** OQ-4/OQ-8 unchanged. OQ-12 resolved (above).

**Next Steps.** Candidates: schematic ∀-programs theorems (PO-006/
PO-009/PO-014 remainder — the fundamental-lemma route), PO-013, or
fragment growth (lists/strings ops, `IPush`/`IPop`) to widen the
differential corpus.

**Relevant Commits.** Working tree on `dev` @ `de537ba9`, not committed.

**Relevant Papers.** Unchanged.

---

## 2026-07-29 (late) — Decision: Milestone 5 deferred

User decision: the project is not being productionized now, so M5 (CI
workflow, final packaging) is deferred indefinitely. Local build/validate
commands in `formal/README.md` remain the verification entry points.
Next exploration target under discussion: an optional-chaining-style
desugaring proof (assessment in session notes; becomes a log entry when
work starts).

---

## 2026-07-29 (evening) — Milestone 4: the transpilation equivalence theorem

**Objective.** Define T-1 (fresh-temporary introduction) as a real Rocq
function; prove contextual equivalence of source and transformed modules
with no `Admitted` (PO-003…PO-009).

**Current Status.** **Done.** New files `formal/Transform.v` (the
transformation + decidable freshness + `fresh_temp_is_fresh`, a general
theorem over all fragment syntax) and `formal/T1Proof.v` (the equivalence
proof). Final theorem:

    t1_contextual_equivalence :
      (⊢ ctx_refines (ir_mod mn (t1_prog t1ex_src)) (ir_mod mn t1ex_src))
      ∧ (⊢ ctx_refines (ir_mod mn t1ex_src) (ir_mod mn (t1_prog t1ex_src)))

where `t1ex_src`'s main calls an UNKNOWN context-supplied function and
prints the result, and the target module is the literal output of
`t1_prog`. Both directions of `ISim.t` are proved (`sim_st`, `sim_ts`),
covering arbitrary call arguments (arity-UB branches) and, via the
∀-context quantification of `ctx_refines`, every callee behavior —
printing, state, re-entrancy, divergence. Effect ordering is preserved
by the trace-inclusion definition of refinement itself.

**Scope honesty.** This is PO-006's *recorded fallback* (concrete program
family, real transformation output), NOT the schematic ∀-programs
theorem — that remains open in the ledger and is the main next-phase
item. Claim classification: proved facts relative to the ITree model;
connection to ESMeta execution remains testing (PO-011/PO-013); no
ECMAScript-level claim.

**Axiom audit.** `Print Assumptions t1_contextual_equivalence` (emitted
at every build, end of T1Proof.v): only the framework's standard base —
`proof_irrelevance`, `functional_extensionality_dep`, `eq_rect_eq`,
`constructive_definite_description`, `classic`, ITreeS `bisim_is_eq`.
No project axioms; zero `Admitted`/`admit`/`Axiom` in `formal/` (swept).

**Observations.**
- CRIS's `cStartFunSim`/`cStartModSim` work directly against our
  `list_to_map`-packaged `ir_mod` modules — no hand-literal module
  workaround needed. Module-sim goals surface function names as
  `(ir_fnsem …).1` projections; order-robust `all: try solve […]`
  bullets handle it.
- Executable validation added first: `t1_prog` preserves `run` on eff
  (prints), gcd, fibo (`Validation.v`) — caught nothing, but bounded the
  risk before proof work began.

**Design Decisions.**
- `Ist := ⌜s = t⌝` (store equality) suffices; the exemplar performs no
  store operations, and `cCall` re-establishes equality across unknown
  calls.
- One designated fresh temp per function (`fresh_temp` = 1 + max index)
  — the temp is dead after each copy, so one index serves all sites;
  freshness is a proved theorem, not an assumption.

**Proof Progress.** PO-003 (discharged in-proof), PO-004 (M2), PO-005
(`env_lookup_update_same`, `fresh_temp_is_fresh`), PO-006 (exemplar
family), PO-007, PO-008, PO-009 (exemplar) — see ledger for exact
statements and the open schematic remainder.

**Failed Attempts** (diagnosed via a batch-mode goal-printing probe file,
since no interactive session):
- `cStep as reply` initially found no IO event: (a) the target was stuck
  on `env_lookup (env_update (LTemp 0) rv (captured_env captured))
  (LTemp 0)` — abstract `captured` blocks computation; fixed by the new
  `env_lookup_update_same` rewrite; (b) `log_val` is a wrapper
  definition hiding `trigger (IO …)` from the tactic's syntactic match;
  fixed by `unfold log_val` before the event step.
- Assumed goal order in `cStartModSim` bullets — wrong; made
  order-robust.

**Research Debt.** The schematic ∀-programs theorem (PO-006/PO-009
remainder) needs: PO-001 induction schemes, an environment-agreement
fundamental lemma, and loop coinduction (`cCoind`) — none needed for the
exemplar. Trace-level (`refines_lmod`) corollary via `refines_adequacy`
not yet unfolded.

**Open Questions.** OQ-4/OQ-8 unchanged; ADR-7's premise materialized as
proof branches (UB on the spec side discharged the arity/downcast
mismatches — symmetric in both directions, as predicted).

**Next Steps.** M5: CI workflow for `formal/` (build + validate),
developer docs on adding constructs/proofs, gap analysis (schematic
theorem, PO-012/PO-013, fragment growth), final report.

**Relevant Commits.** Working tree on `dev` @ `de537ba9`, not committed.

**Relevant Papers.** CRIS (simulation/adequacy/linking used as-is;
imaginary-spec layer never needed — trivial resource throughout, as
planned); Interaction Trees; JISET.

---

## 2026-07-29 (later still) — Milestone 3: executable differential validation

**Objective.** PO-011: run fragment programs under both ESMeta's
interpreter and an executable counterpart of the Rocq semantics; detect
effect reordering/duplication/skipping; include negative transformation
tests. Resolve OQ-11.

**Current Status.** Done; all validation passing.
- New Rocq files: `Domain.v` (pure domain, refactored out of
  `Semantics.v`), `Programs.v` (corpus terms, refactored out of
  `Examples.v`), `Exec.v` (fuel-based executable reference interpreter,
  stdlib-only, clause-by-clause mirror of the denotation),
  `Validation.v` (vm_compute corpus runs + effect-sensitivity +
  3 negative tests + T-1-shape positive test).
- New Scala: `src/main/scala/esmeta/fv/FVExport.scala` (isolated package;
  scalafmt-clean; no existing files touched) — translates
  fragment-compatible IR to Rocq terms, runs ESMeta capturing `IPrint`
  values (via an `Interpreter` subclass) and the final `RESULT` global,
  emits `formal/validation/Generated.v` (git-ignored).
- Differential result: **18/44 `tests/ir` programs exported, 18/18 match
  ESMeta observables** under `make validate` (8 substantive — sum, gcd,
  fibo, branch, parity, inst/{let,return,assert}; 10 near-empty
  placeholder fixtures); 26 skipped with per-construct reasons (ENumber,
  EBigInt, ERecord/EMap literals, IPush/IPop, ECopy/EKeys, shifts, …) —
  exactly the declared fragment boundary, no silent coverage claims.

**Observations.**
- Several `tests/ir/expr/*` fixtures are empty placeholders in ESMeta
  itself (`@main def main() = {}` + commented-out bodies, "TODO
  implementation is missing") — they validate trivially and are counted
  separately above.
- OQ-11 resolved from `state/Obj.scala:29-30`: record-field write is
  insert-or-update; the M2 model (which made absent-field update UB) was
  WRONG and is fixed (`fields_insert` in `Domain.v`). This is precisely
  the class of error the Rule-2 discipline + differential testing exist
  to catch; caught by source reading before any test could.

**Design Decisions.**
- Interchange format amendment (architecture note §7): export Rocq terms
  directly instead of structural JSON — no parser needed on the Rocq
  side; generated expectations are human-reviewable. JSON deferred until
  a non-Rocq consumer exists.
- Validation executes inside Rocq via `vm_compute` (no OCaml extraction);
  fuel-based `Exec.v` with `Ok/Stuck/OOF` outcomes; `OOF` is
  inconclusive-by-construction, never a pass.
- PO-013 (Exec ↔ denotation correspondence) added to the ledger as the
  explicitly-documented residual gap; until then PO-011 validates
  `Exec.v` and the denotation only via clause parallelism [engineering
  assumption].

**Proof Progress.** No new theorem-level proofs (M3 is testing by
design). `Validation.v` contains 9 vm_compute-checked `Example`s; the
generated file adds 18 more.

**Failed Attempts.** `++` parsed in `string_scope` inside `Exec.v`
(fixed with `%list`); `esmeta.state.*` wildcard import shadowed
`scala.math.BigInt` in the exporter (fixed by qualifying).

**Research Debt.** The 10 placeholder fixtures inflate the raw exported
count — always report the substantive-8 figure alongside. Cosmetic:
`Generated.v` triggers Rocq's large-nat-literal warning for the fuel
constant (harmless; generated file is not committed).

**Open Questions.** OQ-4/OQ-8 unchanged (both non-blocking for M4's
planned theorem); OQ-11 resolved.

**Next Steps.** M4: define T-1 (fresh-temporary introduction) as a Rocq
function on IR-Core, state its preconditions (freshness, admissibility),
prove `ISim.t` both directions (PO-005, PO-006, PO-003 as needed), apply
`main_adequacy` (PO-007), assemble `ctx_equiv` (PO-008, PO-009).

**Relevant Commits.** Working tree on `dev` @ `de537ba9`, not yet
committed.

**Relevant Papers.** Unchanged.

---

## 2026-07-29 (later) — Milestone 2: executable ITree semantics

**Objective.** Implement the IR-Core state model and ITree denotation per
ADR-6/ADR-7, package programs as CRIS modules, mirror 3–5 `tests/ir`
programs, discharge PO-002/PO-004, settle OQ-9/OQ-10.

**Current Status.** Done and building cleanly (`make` in `formal/`, zero
warnings). New files: `formal/Semantics.v` (completion type, pure operator
evaluation incl. structural `val_eqb`, environments, heap objects, store
layout, `denote_expr`/`denote_ref`/`denote_inst`/`denote_fbody`,
`ir_smod`/`ir_mod` packaging with all four `SMod.t` obligations `Qed`,
PO-004 lemmas) and `formal/Examples.v` (mirrors of `sum.ir`, `gcd.ir`,
`fibo.ir` + a two-print program; four packaged modules; `print2_body_trace`).
The observable-behavior spec (architecture §3) was user-approved before
implementation began (Rule 4 gate passed).

**Observations.**
- The CRIS pattern-bind notation requires a type ascription
  (`' p : T <- t ;; k`); plain `'(a,b) <- t;;` does not parse.
- Our `fname := string` alias clashed with CRIS `Fn.fname`; renamed to
  `irname` (Fragment.v).
- `mod_tac` only discharges module obligations for *concrete* insert-built
  maps; for the program-parameterized `ir_fnsems` the four obligations
  needed ~45 lines of hand proof (map_Forall over `list_to_map` via
  `elem_of_list_to_map_2` + stdlib `in_map_iff`).
- `msk_scp` masks store events by a bare `bool_decide (k.1 ∈ scp)`.
- `ired` (CRIS itree-rewriting tactic) proves equational trace facts
  directly; `print2_body_trace` needed only `cbn. by ired.`

**Design Decisions.**
- OQ-9 resolved: one scope per program module; key families `g$x`
  (globals), `h$n` (heap, `pretty`-encoded counter addresses), `alloc$`.
- OQ-10 resolved: uniform cross-`Any.t` signature
  `ir_arg = captured-env × args ↦ val`; `entry` runs main nullary.
- Main falling through without `IReturn` returns `VUndef` (ESMeta leaves
  RESULT unset — modeling choice, documented in Semantics.v header);
  non-main fallthrough is UB mirroring `NoReturnValue`.
- Strict call arity (UB on mismatch): deliberate deviation from ESMeta's
  latent silent-underflow bug; excluded by admissibility.
- New OQ-11: record-field update currently requires field existence;
  verify against `state/Obj.scala` in M3.

**Proof Progress.** PO-002 proved (definitional + obligations `Qed`);
PO-004 proved (`denote_seq_cons`, `denote_seq_return_shortcircuit`);
first effect-order fact `print2_body_trace` `Qed`. PO-001 deferred to
first consumer (M4); PO-003 repositioned to M4 alongside PO-006 (ledger
updated with rationale). No `Admitted` anywhere.

**Failed Attempts.**
- `'(ρ1, k) <- …;;` without type ascription — parse error; fixed with
  `: env * completion` ascriptions.
- First obligation proof used `injection … as <-` inside a `[…|…]`
  bracket (Ltac parse error) and assumed `msk_scp` produced a
  conjunction (it does not); rewritten per the actual definition.

**Research Debt.** Unchanged from M1 (mirroring drift until PO-011;
ADR-5 exactness assumption untested until M3; rocq-cris SHA mirroring
before M5). Added: `print2` has no `tests/ir` counterpart — the M3
harness must also run model-vs-ESMeta on print-bearing programs it
generates itself.

**Open Questions.** OQ-4, OQ-8 unchanged; OQ-9/OQ-10 resolved; OQ-11 new.

**Next Steps.** M3: Scala-side structural-JSON exporter for
fragment-compatible IR + differential harness over `tests/ir`
(PO-011), including effect-sensitive and negative tests; resolve OQ-11
by reading `state/Obj.scala`; then M4 (T-1 transformation + PO-005/006).

**Relevant Commits.** Working tree on `dev` @ `de537ba9`, not yet
committed.

**Relevant Papers.** Unchanged (JISET / CRIS / Interaction Trees).

---

## 2026-07-29 — Milestones 0 (inspection) and 1 (architecture + skeleton)

**Objective.** M0: map ESMeta and the CRIS practice environment without
modifying code; surface every ambiguity blocking a sound design. M1:
architecture note with ADRs, PO ledger, this log, and a buildable minimal
Rocq project defining the fragment syntax and event interface.

**Current Status.** M0 complete (report delivered in-session, 2026-07-29).
M1 artifacts written: `docs/formal-verification/{itree-transpiler-plan.md,
PROOF_OBLIGATIONS.md, RESEARCH_LOG.md}`, `formal/{_CoqProject, Makefile,
README.md, Fragment.v, Events.v}`; `formal` builds cleanly (`make` on
Rocq 9.0.0, cris-workshop switch). Awaiting the Rule-4 review gate on the
observable-behavior specification (architecture note §3) before starting
M2 semantics.

**Observations** (details + file/line citations in the architecture note §2):
- ESMeta has no `lower : JS → IR`; the spec compiles to IR and JS ASTs are
  runtime values. [Repository fact]
- The interpreter executes the CFG, not the instruction tree; `CFGBuilder`
  (~60 lines) eliminates `IIf`/`IWhile`/`ISeq`. [Repository fact]
- Completion records are heap records; abrupt propagation is data + explicit
  branches; no interpreter exception mechanism for the object language.
  [Repository fact]
- Termination result = global `RESULT`; `IPrint`→stdout is the only other
  output channel (suppressed under TEST_MODE). [Repository fact]
- 44 standalone `.ir` programs under `tests/ir/` run via test-only
  `interpFile`; ready-made differential oracle. [Repository fact]
- The CRIS framework proper lives in the pinned opam package (source at
  `~/.opam/cris-workshop/lib/coq/user-contrib/CRIS/`, ~42k lines), not in
  the workshop repo. Trivial-resource `isim` use is demonstrated by the
  workshop (`day1/answers/Optimizations.v`). Iris-free core exists
  (`Behavior.v`, `gsim`, `lsim`) but carries no tactics. [Repository facts]
- Interpreter quirks catalogued for admissibility fencing: `Cont` replaces
  the call stack; `MapObj` insertion order; `Math` division rounds to
  DECIMAL128; `-0.0` special case; silent skipped asserts when the
  condition itself fails to evaluate; latent unthrown `RemainingParams`
  (arity underflow silently ignored — possible upstream ESMeta bug, worth
  reporting independently). [Repository facts]

**Design Decisions** (full ADRs in the architecture note §6):
- ADR-1 IR-level theorem restatement — **user decision** (options presented
  in M0 report).
- ADR-2 linking contexts now, syntactic bridge deferred (PO-010) — **user
  decision**.
- ADR-3 CRIS `isim` + trivial resource; keep Iris dependency — **user
  decision**; eutt-vs-simulation comparison recorded in the ADR.
- ADR-4 denote tree IR, connect to CFG execution by testing first —
  [design hypothesis, flagged for review].
- ADR-5 `Math` = ℤ; `Div` excluded. ADR-6 locals threaded purely,
  globals+heap in the CRIS keyed store [provisional]. ADR-7 stuck = UB
  [provisional, OQ-4 open]. ADR-8 depend on pinned opam package, no
  vendoring.

**Proof Progress.** None yet (by design — Rule 3: PO ledger written first;
PO-001 is the M2 entry task). One trivial lemma `local_eqb_eq` is Qed in
`Fragment.v` as a build sanity check.

**Failed Attempts.**
- First `formal/` Makefile passed `coqc` flags to `coqdep`, which rejects
  `-require-import`; fixed by a separate `COQDEPFLAGS`.
- `From Coq Require` triggers a Rocq-9 deprecation; switched to
  `From Stdlib`.
- (Design-level) Initially drafted `Scheme Equality for local`; replaced
  with a hand-written `local_eqb` because derived equality over
  string-containing types is fragile across Rocq versions.

**Research Debt.**
- Hand-mirrored `Fragment.v` can drift from `ir/*.scala` until PO-011's
  differential harness lands (M3). Accepted per AN §7.
- ADR-5's "integer arithmetic agrees with BigDecimal on included operators"
  is an engineering assumption until PO-011 tests it.
- The `rocq-cris` git-SHA pin has no release artifact; mirror before M5 CI.

**Open Questions.** OQ-4 (stuck: UB vs abort — provisional UB, becomes
load-bearing only for stuck-site-changing transformations), OQ-8
(address-bearing observables — provisionally excluded by admissibility),
OQ-9 (keyed-store encoding of addresses+globals — M2 experiment), OQ-10
(`RR` at the `Any.t` main boundary — M2/M4). OQ-7 (And/Or short-circuit)
resolved: mirror the interpreter (short-circuit), L-7.

**Next Steps.** (1) User review of the observable-behavior spec (AN §3) —
the Rule-4 gate for M2. (2) M2: PO-001 induction principles; state model +
denotation per ADR-6; settle OQ-9/OQ-10 with a spike; 3–5 executable
examples mirroring `tests/ir` programs.

**Relevant Commits.** ESMeta `dev` @ `de537ba9` (baseline; M1 files not yet
committed at the time of writing). Pins: ecma262 `84b38ad8`; rocq-cris
`c0bcd04e`.

**Relevant Papers.** JISET (10.1145/3324884.3416632) — provenance of the
spec-derived IR; CRIS (10.1145/3808317) — behaviors/simulation/adequacy/
linking, imaginary-spec layer deliberately unused; Interaction Trees
(10.1145/3371119) — representation, iter, weak bisimulation.
