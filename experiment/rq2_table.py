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

from rq1_table import INTERACTION, PLAIN, Defects, Tex, cell, mean, programs, runs, solver_summary, table

SETTINGS = {"solve": "Synth262", "noshape": "w/o structural", "notemplate": "w/o template"}


def main() -> int:
    parser = argparse.ArgumentParser(description="the numbers of RQ2")
    parser.add_argument("--draft", action="store_true", help="add untriaged failures to the ledger")
    parser.add_argument("--tex", type=Path, help="write the paper's number macros here")
    args = parser.parse_args()
    defects = Defects()

    counted = []
    for prefix, label in SETTINGS.items():
        rs = runs(prefix)
        s = [solver_summary(r) for r in rs]
        plain = defects.group([r / PLAIN["solve"] for r in rs])
        aware = defects.group([r / INTERACTION for r in rs])
        counted.append((label, s, [programs(r / "programs") for r in rs], plain, aware))
    rows = [
        [label, cell(mean([x["time"] for x in s]), time=True), cell(mean([x["coverage"] for x in s])),
         cell(mean(ps)), cell(mean(plain[0]), plain[1]), cell(mean(aware[0]), aware[1])]
        for label, s, ps, plain, aware in counted
    ]
    table("RQ2: five-run means",
          ["", "time", "coverage", "programs", "w/o interaction-based", "w/ interaction-based"], rows)

    # Synth262's own row comes from rq1_table.py
    tex = Tex("rq2_table.py")
    for (label, s, ps, plain, aware), name in zip(counted[1:], ("NoShape", "NoTemplate")):
        tex.time(f"{name}Time", mean([x["time"] for x in s]))
        tex.count(f"Num{name}CoveredAll", mean([x["coverage"] for x in s]))
        tex.count(f"Num{name}Programs", mean(ps))
        tex.count(f"NumBugs{name}PlainMean", mean(plain[0]), plain[1])
        tex.count(f"NumBugs{name}OracleMean", mean(aware[0]), aware[1])
        tex.pct(f"Pct{name}CoveredMean", mean([x["targets"] for x in s]), s[0]["universe"])
    tex.write(args.tex)

    defects.report(args.draft)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (FileNotFoundError, ValueError) as err:
        print(f"error: {err}", file=sys.stderr)
        raise SystemExit(1)
