# Polyfill end-to-end test

Runs the [test262](https://github.com/tc39/test262) conformance suite against
the polyfills ESMeta generates from ECMA-262, so a change to the generator,
the DSL rules, or the runtime is caught by the specification's own tests.

## Running

Generate the polyfill library first, then run the suite:

```sh
sbt "run gen-poly -gen-poly:out=logs/polyfill/basic"
sbt "run gen-poly -gen-poly:opt -gen-poly:out=logs/polyfill/opt"
cd tests/polyfill
npm ci
npm test
```

`POLYFILL_LIB_BASIC` and `POLYFILL_LIB_OPT` override the library locations.
`--target=<name>` runs a single target and `--config=basic|opt` a single
configuration, both much faster while iterating:

```sh
node run.js --target=Set.prototype.union --config=opt
```

## The two configurations

`basic` is the plain translation of the specification. `opt` additionally
applies the rule-based optimizer, which rewrites specification steps into
internal operations backed by an optimized runtime.

Both are checked against their own baseline, and against each other: the
optimizer may change the code that is emitted, never what that code does. A
test that passes under one configuration and fails under the other fails the
run, reported separately from an ordinary regression so the cause is not
mistaken for a translation bug.

That cross-check is the reason both are run. Conformance alone cannot see the
optimizer: with it switched off entirely the suite still scores 8033/8049,
because unoptimized output is simply plainer, not wrong. Only the comparison
tells you the optimizer preserved behaviour.

An intentional difference goes in `allowedDivergences` in the baseline, keyed
by test id, with the reason -- the same discipline as a known failure.

## How a target is run

Each entry in `targets.yaml` names a built-in, the module the packager emits
for it, a statement that removes the host's own implementation, and the
test262 tests that exercise it. For each target the runner builds an entry
that deletes the built-in and requires the generated module, bundles it, and
passes the bundle to `test262-harness` as a prelude.

Two details are easy to get wrong:

- **The built-in must be deleted.** The packager guards each installation with
  `if (!Base.member)`, so on a modern engine the generated code would never
  install and the suite would quietly test the host's built-in instead of
  ESMeta's. `deleteStmt` is what makes the polyfill the thing under test.

- **The prelude must be bundled.** test262-harness runs each test in its own
  realm. A prelude that merely calls `require` installs the polyfill into the
  host realm while the test observes its own — the require succeeds and the
  built-in still looks absent. Only inlined code reaches the test.

## The baseline

`baseline.json` records the tests known to fail, each with the reason it is
expected to. The run fails on any test that is not already listed, and reports
tests that now pass so the baseline can be tightened.

The 16 currently recorded fall into three groups, none of them defects in the
generator, and core-js fails every one of them as well:

- **cross-realm** (`proto-from-ctor-realm`, 5 built-ins) — the test constructs
  through a constructor from another realm and expects that realm's prototype.
  No implementation written in JavaScript can reach it.
- **`[[Construct]]` pre-creation** (`get-prototype-abrupt-executor-not-callable`)
  — constructing an ordinary function creates `this` via
  `OrdinaryCreateFromConstructor` before the body runs, so a throwing
  `prototype` getter fires ahead of the executor's callable check. A built-in
  constructor has no such step.
- **Lists as arrays** (`does-not-invoke-array-setters`, `Promise.all` and
  `Promise.allSettled`) — a deliberate performance choice: specification Lists
  are represented as JS arrays, so writing a list index can reach a setter
  inherited from `Array.prototype`.

A re-record keeps the reason already written against a failure that persists,
and marks anything new `UNREVIEWED` — which is the signal to investigate and
write a real reason, not to leave it. After deliberately changing behaviour:

```sh
npm run baseline
```

Review that diff — a shrinking baseline is an improvement, a growing one needs
justifying.
