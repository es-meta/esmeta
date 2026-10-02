#!/usr/bin/env python3
"""RQ1: generation time, branch coverage, programs, and defects."""
import argparse
import sys
from pathlib import Path

import json
import statistics

from results import (DATA, INTERACTION, PLAIN, Defects, Tex, cell, conform_time, covered, failure_split, failures, generation_time,
                     fuzz_at, mann_whitney, mean, oracle_runs, programs, runs, solver_summary, table,
                     test262_counts, tests)


def main() -> int:
    parser = argparse.ArgumentParser(description="the table of RQ1")
    parser.add_argument("--draft", action="store_true", help="add untriaged failures to the ledger")
    parser.add_argument("--tex", type=Path, help="write the paper's number macros here")
    parser.add_argument("--coverage-only", action="store_true", help="update generation metrics only")
    args = parser.parse_args()

    solve, fuzz = runs("solve"), runs("fuzz")
    s = [solver_summary(r) for r in solve]
    timing = [generation_time(r) for r in solve]
    test_counts = test262_counts()
    if not args.coverage_only:
        injection = mean([conform_time(r, "injectionMs") for r in solve])
        total = None if injection is None else mean([x["real"] for x in timing]) + injection
        defects = Defects()
        plain, p1 = defects.group([r / PLAIN["solve"] for r in solve])
        aware, p2 = defects.group([r / INTERACTION for r in solve])
        fplain, p3 = defects.group([r / PLAIN["fuzz"] for r in fuzz])
        faware, p4 = defects.group([r / INTERACTION for r in fuzz])
    test262 = covered(DATA / "test262" / "branch-coverage.json")

    rows = [["Synth262", cell(mean([x["real"] for x in timing]), time=True),
             cell(mean([x["coverage"] for x in s])),
             cell(mean([programs(r / "programs") for r in solve]))]]
    if not args.coverage_only:
        rows[0][0] = "  w/o interaction-based"
        rows[0].append(cell(mean(plain), p1))
        rows.insert(0, ["Synth262", cell(total, time=True), cell(mean([x["coverage"] for x in s])),
                        cell(mean([tests(r) for r in solve])), cell(mean(aware), p2)])
    for hours in (1, 10, 50):
        at = [fuzz_at(r, hours) for r in fuzz]
        row = [f"JEST {hours} h", cell(hours * 3600, time=True),
               cell(mean([x["coverage"] for x in at])), cell(mean([x["programs"] for x in at]))]
        if not args.coverage_only:
            row.append(cell(mean(fplain), p3) if hours == 50 else "--")
        rows.append(row)
    if not args.coverage_only:
        rows.append(["  w/ interaction-based", cell(50 * 3600, time=True),
                     cell(mean([fuzz_at(r, 50)["coverage"] for r in fuzz])),
                     cell(mean([tests(r) for r in fuzz])), cell(mean(faware), p4)])
    rows.append(["Test262", "--", cell(len(test262)), cell(test_counts["executed"])] + ([] if args.coverage_only else ["--"]))
    table("RQ1: run means", ["", "time", "coverage", "programs"] +
          ([] if args.coverage_only else ["defects"]), rows)
    if not args.coverage_only:
        available = sum(x is not None for x in aware + faware)
        if available < len(aware) + len(faware):
            print(f"  interaction-based logs present in {available} of {len(aware) + len(faware)} runs")

    tex = Tex("rq1_table.py")
    tex.count("NumSynthCoverageRuns", len(solve))
    tex.mean("NumSynthCoveredAll", mean([x["coverage"] for x in s]))
    tex.mean("NumSynthPrograms", mean([programs(r / "programs") for r in solve]))
    # every run of each generator against every run of the other (Figure 6a)
    fuzz_all = [fuzz_at(r, 50)["coverage"] for r in fuzz]
    tex.pct("PctSynthOverFuzz", mean([x["coverage"] for x in s]) - mean(fuzz_all), mean(fuzz_all))
    if not args.coverage_only:
        tex.seconds("OracleTime", injection)
        tex.seconds("SynthTime", total)
        tex.put("SynthMinutes", None if total is None else f"{round(total / 60)}")
        tex.seconds("FuzzOracleTime", mean([conform_time(r, "injectionMs") for r in fuzz]))
        tex.mean("NumDefectsPlainMean", mean(plain), p1)
        tex.mean("NumDefectsOracleMean", mean(aware), p2)
        # Synth262 as shipped: the originals and the instrumented programs
        shipped = [None if a is None else len(a | (defects.found(r / PLAIN["solve"]) or set()))
                   for a, r in zip((defects.found(r / INTERACTION) for r in solve), solve)]
        tex.put("SynthDefectsRange", None if None in shipped else f"{min(shipped)}--{max(shipped)}", p1 or p2)
        # conformance testing of a run on all engines, and the ESMeta runs of the injector
        tex.mean("NumSynthTests", mean([tests(r) for r in solve]))
        tex.seconds("ConformTime", mean([conform_time(r, "conformMs") for r in solve]))
        oracle = [oracle_runs(r) for r in solve]
        if None not in oracle:
            for key, name in (("guided_median", "NumOracleRunsMedian"), ("guided_p90", "NumOracleRunsPNinety")):
                values = {o[key] for o in oracle}
                tex.put(name, f"{values.pop()}" if len(values) == 1 else f"{statistics.mean(o[key] for o in oracle):.1f}")
            ratio = statistics.mean(o["plain_total"] for o in oracle) / statistics.mean(o["guided_total"] for o in oracle)
            tex.put("OracleRunsRatio", f"{ratio:.1f} times")
        # rows only the interaction-based oracle reproduces, per run
        only = [
            None if a is None or p is None else len(a - p)
            for a, p in zip((defects.found(r / INTERACTION) for r in solve), (defects.found(r / PLAIN["solve"]) for r in solve))
        ]
        tex.mean("NumInteractionOnlyDefects", mean(only), p1 or p2)
        # how the failing tests of a run split by triage (threats to validity)
        splits = [failure_split(defects, r / INTERACTION) for r in solve]
        if None not in splits:
            total = mean([sum(x[k] for k in ("defect", "candidate", "false", "pending")) for x in splits])
            tex.mean("NumFailingPairs", total)
            for key, name in (("defect", "Defect"), ("candidate", "Candidate"), ("false", "False")):
                tex.pct(f"PctFailing{name}", mean([x[key] for x in splits]), total)
            labels: set[str] = set()
            for r in solve:
                for engine, program in failures(json.loads((r / INTERACTION).read_text(encoding="utf-8"))):
                    v = defects.ledger.get(engine, {}).get(program, "?")
                    labels |= {x for x in ([v] if isinstance(v, str) else v) if x.startswith("new:")}
            tex.count("NumCandidateDefects", len(labels))
    for hours, name in ((1, "OneHour"), (10, "TenHours"), (50, "All")):
        at = [fuzz_at(r, hours) for r in fuzz]
        tex.mean(f"NumFuzzCovered{name}", mean([x["coverage"] for x in at]))
        tex.mean(f"NumFuzzPrograms{name}", mean([x["programs"] for x in at]))
    if not args.coverage_only:
        tex.mean("NumDefectsFuzzPlainMean", mean(fplain), p3)
        tex.mean("NumDefectsFuzzOracleMean", mean(faware), p4)
    tex.seconds("GenerationTime", mean([x["real"] for x in timing]))
    tex.count("NumTestPrograms", test_counts["executed"])
    tex.put("NumTestDefects", "--")
    tex.count("NumTestCoveredAll", len(test262))
    tex.write(args.tex)

    if not args.coverage_only:
        defects.report(args.draft)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (FileNotFoundError, ValueError) as err:
        print(f"error: {err}", file=sys.stderr)
        raise SystemExit(1)
