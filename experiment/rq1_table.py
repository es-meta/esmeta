#!/usr/bin/env python3
"""RQ1: generation time, branch coverage, programs, and defects."""
import argparse
import sys
from pathlib import Path

from results import (DATA, INTERACTION, PLAIN, Defects, Tex, cell, comparison, conform_time, covered, distribution, generation_time,
                     fuzz_at, mean, programs, runs, solver_summary, table, test262_counts, tests)


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
    tex.time("SolveSearchTime", mean([x["time"] for x in s]))
    tex.count("NumSynthCoveredAll", mean([x["coverage"] for x in s]))
    tex.count("NumSynthPrograms", mean([programs(r / "programs") for r in solve]))
    if not args.coverage_only:
        tex.time("OracleTime", injection)
        tex.time("SynthTime", total)
        tex.time("ConformOracleTime", mean([conform_time(r, "conformMs") for r in solve]))
        tex.time("FuzzOracleTime", mean([conform_time(r, "injectionMs") for r in fuzz]))
        tex.count("NumDefectsPlainMean", mean(plain), p1)
        tex.count("NumDefectsOracleMean", mean(aware), p2)
        # rows only the interaction-based oracle reproduces, per run
        only = [
            None if a is None or p is None else len(a - p)
            for a, p in zip((defects.found(r / INTERACTION) for r in solve), (defects.found(r / PLAIN["solve"]) for r in solve))
        ]
        tex.count("NumInteractionOnlyDefects", mean(only), p1 or p2)
    for hours, name in ((1, "OneHour"), (10, "TenHours"), (50, "All")):
        at = [fuzz_at(r, hours) for r in fuzz]
        tex.count(f"NumFuzzCovered{name}", mean([x["coverage"] for x in at]))
        tex.count(f"NumFuzzPrograms{name}", mean([x["programs"] for x in at]))
    if not args.coverage_only:
        tex.count("NumDefectsFuzzPlainMean", mean(fplain), p3)
        tex.count("NumDefectsFuzzOracleMean", mean(faware), p4)
    tex.time("GenerationTime", mean([x["real"] for x in timing]))
    tex.time("GenerationOverheadTime", mean([x["real"] - summary["time"] for x, summary in zip(timing, s)]))
    tex.put("GenerationUserCPUHours", f"{mean([x['user'] for x in timing]) / 3600:.2f}")
    tex.put("GenerationCPUHours", f"{mean([x['cpu'] for x in timing]) / 3600:.2f}")
    tex.put("SynthCoverageStats", distribution([x["coverage"] for x in s]))
    full = [covered(r / "branch-coverage.json") for r in solve]
    fuzz_cov = [covered(r / "branch-coverage.json") for r in fuzz]
    tex.put("FuzzCoverageStats", distribution([len(c) for c in fuzz_cov]))
    tex.put("SynthVsFuzzCoverageStats", comparison([len(c) for c in full], [len(c) for c in fuzz_cov]))
    tex.put("SynthTestMissedStats", distribution([len(c - test262) for c in full]))
    tex.put("FuzzTestMissedStats", distribution([len(c - test262) for c in fuzz_cov]))
    tex.count("NumSynthAlwaysCovered", len(set.intersection(*full)))
    tex.count("NumTestPrograms", test_counts["executed"])
    tex.count("NumTestSkipped", test_counts["skipped"])
    tex.count("NumTestBuiltinPrograms", test_counts["builtin_executed"])
    tex.count("NumTestBuiltinSkipped", test_counts["builtin_skipped"])
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
