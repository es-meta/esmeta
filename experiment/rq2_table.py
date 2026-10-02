#!/usr/bin/env python3

# python3 experiment/rq2_table.py [--draft] [--tex ../synth262-paper/fse27/numbers/rq2_table.tex]
#
# The numbers of RQ2: the full solver against the runs that drop one extension,
# structural types for properties (-solve:no-shape) and built-in call templates
# (-solve:no-template). Five-run means of solving time, branch coverage,
# programs, and defects with final-state assertions only and with the
# interaction-based oracle. Defects count as in rq1_table.py.

from __future__ import annotations

import argparse
import sys

from pathlib import Path

from results import INTERACTION, PLAIN, Defects, Tex, cell, conform_time, generation_time, mean, programs, runs, solver_summary, table, universe

SETTINGS = {"solve": "Synth262", "noshape": "w/o structural", "notemplate": "w/o template"}


def main() -> int:
    parser = argparse.ArgumentParser(description="the numbers of RQ2")
    parser.add_argument("--draft", action="store_true", help="add untriaged failures to the ledger")
    parser.add_argument("--tex", type=Path, help="write the paper's number macros here")
    parser.add_argument("--coverage-only", action="store_true", help="update generation metrics only")
    args = parser.parse_args()
    defects = None if args.coverage_only else Defects()

    counted = []
    for prefix, label in SETTINGS.items():
        rs = runs(prefix)
        s = [{**solver_summary(r), "wall_time": generation_time(r)["real"]} for r in rs]
        plain = defects.group([r / PLAIN["solve"] for r in rs]) if defects else None
        aware = defects.group([r / INTERACTION for r in rs]) if defects else None
        counted.append((label, s, [programs(r / "programs") for r in rs], plain, aware))
    rows = [
        [label, cell(mean([x["wall_time"] for x in s]), time=True), cell(mean([x["coverage"] for x in s])),
         cell(mean(ps))] + ([] if args.coverage_only else
         [cell(mean(plain[0]), plain[1]), cell(mean(aware[0]), aware[1])])
        for label, s, ps, plain, aware in counted
    ]
    table("RQ2: run means", ["", "time", "coverage", "programs"] +
          ([] if args.coverage_only else ["w/o interaction-based", "w/ interaction-based"]), rows)

    # Synth262's own row comes from rq1_table.py
    tex = Tex("rq2_table.py")
    for (label, s, ps, plain, aware), name in zip(counted[1:], ("NoShape", "NoTemplate")):
        tex.time(f"{name}Time", mean([x["wall_time"] for x in s]))
        tex.mean(f"Num{name}CoveredAll", mean([x["coverage"] for x in s]))
        tex.mean(f"Num{name}Programs", mean(ps))
        if not args.coverage_only:
            prefix = "noshape" if name == "NoShape" else "notemplate"
            tex.time(f"{name}OracleTime", mean([conform_time(r, "injectionMs") for r in runs(prefix)]))
            tex.mean(f"NumDefects{name}PlainMean", mean(plain[0]), plain[1])
            tex.mean(f"NumDefects{name}OracleMean", mean(aware[0]), aware[1])
    targets = universe(runs("solve")[0])
    if any(universe(r) != targets for prefix in SETTINGS for r in runs(prefix)):
        raise ValueError("ablation runs use different target universes")
    tex.count("NumSynthTemplates", mean([s["templates"] for s in counted[0][1]]))
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
