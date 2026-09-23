#!/usr/bin/env node
"use strict";

/**
 * End-to-end test: run test262 against the polyfills ESMeta generates.
 *
 * For each target in `targets.yaml`:
 *   1. build an entry that removes the host built-in and requires the
 *      generated module,
 *   2. bundle it into a single self-contained file, and
 *   3. run the target's test262 tests with that bundle as a prelude.
 *
 * The bundling step is not an optimization. test262-harness runs each test in
 * its own realm, so a prelude that merely calls `require` installs the polyfill
 * into the host realm while the test observes its own -- the require succeeds
 * and the built-in still looks absent. Only inlined code reaches the test.
 *
 * Results are compared against `baseline.json`, which records the tests known
 * to fail. A newly failing test fails the run; a newly passing one is reported
 * so the baseline can be tightened.
 */

const fs = require("fs");
const path = require("path");
const os = require("os");
const { execFileSync } = require("child_process");
const yaml = require("js-yaml");

const HERE = __dirname;
const REPO = path.resolve(HERE, "..", "..");
const TEST262 = path.join(REPO, "tests", "test262");
const BUNDLES = path.join(HERE, "bundles");
const BASELINE = path.join(HERE, "baseline.json");

/** the two generated libraries under test
 *
 * `basic` is the plain translation of the specification; `opt` additionally
 * applies the rule-based optimizer of Section 5. Both must be conformant, and
 * they must agree with each other -- the optimizer may change the code it
 * emits, never what that code does.
 */
const CONFIGS = [
  { name: "basic", lib: process.env.POLYFILL_LIB_BASIC ||
      path.join(REPO, "logs", "polyfill", "basic") },
  { name: "opt", lib: process.env.POLYFILL_LIB_OPT ||
      path.join(REPO, "logs", "polyfill", "opt") },
];

const bin = (name) => path.join(HERE, "node_modules", ".bin", name);

/** build a single-file bundle that installs one target's polyfill */
function bundle(cfgName, lib, target) {
  const entry = path.join(BUNDLES, `entry.${cfgName}.${target.name}.js`);
  const outName = `${cfgName}.${target.name}.bundle.js`;
  fs.writeFileSync(
    entry,
    [target.deleteStmt, `require(${JSON.stringify(path.join(lib, target.module))});`].join("\n"),
  );
  execFileSync(
    bin("webpack"),
    ["--mode", "production", "--target", "node",
     "--entry", entry, "--output-path", BUNDLES, "--output-filename", outName],
    { cwd: HERE, stdio: "pipe" },
  );
  fs.unlinkSync(entry);
  return path.join(BUNDLES, outName);
}

/** run the target's test262 tests against the bundle */
function runTests(target, bundlePath) {
  const out = execFileSync(
    bin("test262-harness"),
    ["--host-type=node", `--host-path=${process.execPath}`,
     `--test262-dir=${TEST262}`, `--threads=${os.availableParallelism()}`,
     `--prelude=${bundlePath}`, "--reporter=json",
     path.join(TEST262, target.testPattern)],
    { cwd: HERE, stdio: ["ignore", "pipe", "ignore"], maxBuffer: 1 << 28 },
  );
  // one result per scenario (a test may run both strict and non-strict)
  return JSON.parse(out.toString()).map((r) => ({
    id: `${path.relative(TEST262, r.file)} [${r.scenario}]`,
    pass: r.result.pass,
    message: (r.result.message || "").split("\n")[0],
  }));
}

/** run every target of one configuration, returning id -> pass */
function runConfig(cfg, targets, baseline, report) {
  const results = new Map();
  let total = 0, passed = 0;

  for (const target of targets) {
    if (!fs.existsSync(path.join(cfg.lib, target.module))) {
      report.skipped.push(`${cfg.name}/${target.name} (no ${target.module})`);
      continue;
    }
    let rs;
    try {
      rs = runTests(target, bundle(cfg.name, cfg.lib, target));
    } catch (e) {
      report.regressions.push(
        `${cfg.name}/${target.name}: harness failed -- ${e.message.split("\n")[0]}`,
      );
      continue;
    }

    const known = new Set(Object.keys(baseline[target.name] || {}));
    const nowFailing = rs.filter((r) => !r.pass).map((r) => r.id).sort();
    total += rs.length;
    passed += rs.filter((r) => r.pass).length;
    for (const r of rs) results.set(r.id, r.pass);

    if (nowFailing.length) {
      const prev = baseline[target.name] || {};
      report.failures[target.name] = Object.fromEntries(
        nowFailing.map((id) => [id, prev[id] || "UNREVIEWED"]),
      );
    }
    for (const r of rs.filter((r) => !r.pass))
      if (!known.has(r.id))
        report.regressions.push(`${cfg.name}/${target.name}: ${r.id} -- ${r.message}`);
    for (const id of known)
      if (!nowFailing.includes(id)) report.fixed.push(`${cfg.name}/${target.name}: ${id}`);

    const p = rs.filter((r) => r.pass).length;
    console.log(
      `${p === rs.length ? "ok  " : "FAIL"} ${cfg.name.padEnd(6)} ${target.name.padEnd(38)} ${p}/${rs.length}`,
    );
  }
  console.log(`\n${cfg.name}: ${passed}/${total} passed\n`);
  return results;
}

function main() {
  const update = process.argv.includes("--update-baseline");
  const only = (process.argv.find((a) => a.startsWith("--target=")) || "").slice(9);
  const onlyCfg = (process.argv.find((a) => a.startsWith("--config=")) || "").slice(9);

  const configs = onlyCfg ? CONFIGS.filter((c) => c.name === onlyCfg) : CONFIGS;
  for (const cfg of configs)
    if (!fs.existsSync(cfg.lib)) {
      console.error(`no generated library for "${cfg.name}" at ${cfg.lib}`);
      console.error(`generate both first:`);
      console.error(`  sbt "run gen-poly -gen-poly:out=logs/polyfill/basic"`);
      console.error(`  sbt "run gen-poly -gen-poly:opt -gen-poly:out=logs/polyfill/opt"`);
      process.exit(2);
    }

  fs.rmSync(BUNDLES, { recursive: true, force: true });
  fs.mkdirSync(BUNDLES, { recursive: true });

  let targets = yaml.load(fs.readFileSync(path.join(HERE, "targets.yaml"), "utf8"));
  if (only) targets = targets.filter((t) => t.name === only);

  const stored = fs.existsSync(BASELINE)
    ? JSON.parse(fs.readFileSync(BASELINE, "utf8"))
    : {};
  const baseline = stored.knownFailures || {};
  const allowed = new Set(Object.keys(stored.allowedDivergences || {}));

  const report = { failures: {}, regressions: [], fixed: [], skipped: [] };
  const byConfig = {};
  const recorded = {};

  for (const cfg of configs) {
    report.failures = {};
    byConfig[cfg.name] = runConfig(cfg, targets, baseline[cfg.name] || {}, report);
    recorded[cfg.name] = report.failures;
  }

  // the optimizer may change the emitted code, never its behaviour
  const divergences = [];
  if (byConfig.basic && byConfig.opt)
    for (const [id, basicPass] of byConfig.basic) {
      const optPass = byConfig.opt.get(id);
      if (optPass !== undefined && optPass !== basicPass && !allowed.has(id))
        divergences.push(
          `${id}: basic ${basicPass ? "passes" : "fails"}, opt ${optPass ? "passes" : "fails"}`,
        );
    }

  if (report.skipped.length) console.log(`skipped: ${report.skipped.join(", ")}`);

  if (update) {
    fs.writeFileSync(
      BASELINE,
      JSON.stringify(
        { knownFailures: recorded, allowedDivergences: stored.allowedDivergences || {} },
        null,
        2,
      ) + "\n",
    );
    console.log(`baseline updated for: ${Object.keys(recorded).join(", ")}`);
    return;
  }

  if (report.fixed.length) {
    console.log(`\n${report.fixed.length} test(s) now pass that the baseline expects to fail:`);
    for (const f of report.fixed.slice(0, 20)) console.log(`  ${f}`);
    console.log(`run \`npm run baseline\` to record the improvement`);
  }
  if (divergences.length) {
    console.error(`\n${divergences.length} test(s) where the optimizer changed behaviour:`);
    for (const d of divergences.slice(0, 20)) console.error(`  ${d}`);
  }
  if (report.regressions.length) {
    console.error(`\n${report.regressions.length} newly failing test(s):`);
    for (const r of report.regressions.slice(0, 40)) console.error(`  ${r}`);
  }
  if (divergences.length || report.regressions.length) process.exit(1);
  console.log("\nno regressions; basic and opt agree");
}

main();
