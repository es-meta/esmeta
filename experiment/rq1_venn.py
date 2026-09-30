#!/usr/bin/env python3

# python3 experiment/rq1_venn.py [--tex ../synth262-paper/fse27/numbers/rq1_venn.tex]
#
# The region counts of the two RQ1 Venn diagrams, which the paper draws by hand
# in fse27/fig/images.key: branch sides covered by the union of each tool's
# runs, and the defect rows the triage ledger finds in their conform logs.
# --tex also writes the coverage numbers the introduction gives within the
# solver's targets, and the defect counts by status.

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from rq1_table import BRANCH_RE, DATA, INTERACTION, PLAIN, Defects, Tex, bug_status, covered, mean, runs


def universe(run: Path) -> set[tuple[int, str]]:
    """the target sides a solver run lists in its summary"""
    text = (run / "summary").read_text(encoding="utf-8")
    return {(int(b), s) for b, s in BRANCH_RE.findall(text)}


def regions(sets: dict[str, set]) -> list[tuple[str, int]]:
    """every non-empty combination of the named sets, as disjoint counts"""
    names_ = list(sets)
    everything = set().union(*sets.values())
    out = []
    for mask in range(1, 2 ** len(names_)):
        inside = [n for i, n in enumerate(names_) if mask >> i & 1]
        part = set(everything)
        for n in names_:
            part = part & sets[n] if n in inside else part - sets[n]
        out.append((" & ".join(inside) + (" only" if len(inside) == 1 else ""), len(part)))
    return out


def show(title: str, sets: dict[str, set]) -> None:
    print(title)
    for label, n in regions(sets):
        print(f"  {label:<28}{n:>7,}")


def main() -> int:
    parser = argparse.ArgumentParser(description="the region counts of the RQ1 Venn diagrams")
    parser.add_argument("--tex", type=Path, help="write the paper's number macros here")
    args = parser.parse_args()
    solve, fuzz = runs("solve"), runs("fuzz")
    per_solve = [covered(r / "branch-coverage.json") for r in solve]
    per_fuzz = [covered(r / "branch-coverage.json") for r in fuzz]
    S, F = set().union(*per_solve), set().union(*per_fuzz)
    T = covered(DATA / "test262" / "branch-coverage.json")
    show("branch coverage (union of runs, including outside targets)", {"Synth262": S, "Test262": T, "JEST": F})
    status = bug_status()
    rows = set(status)
    defects = Defects()
    # rows either oracle reproduces in any run of the tool
    stored = {
        src: set().union(*(defects.found(r / log) or set() for r in rs for log in (PLAIN[src], INTERACTION)))
        for src, rs in (("solve", solve), ("fuzz", fuzz))
    }
    show(f"\ndefects ({len(rows)} rows; {len(rows - stored['solve'] - stored['fuzz'])} reproduced by neither)",
         {"Synth262": stored["solve"], "JEST": stored["fuzz"]})

    tex = Tex("rq1_venn.py")
    tex.count("NumOnlySynthAll", len(S - T - F))
    tex.count("NumOnlyFuzzAll", len(F - S - T))
    tex.count("NumSynthOfTestMissedAll", len(S - T))
    tex.count("NumFuzzOfTestMissedAll", len(F - T))
    # the introduction's numbers, within the solver's targets
    U = universe(solve[0])
    missed = U - T
    tex.count("NumBranchUniverse", len(U))
    tex.pct("PctSynthCoveredMean", mean([len(c & U) for c in per_solve]), len(U))
    tex.pct("PctFuzzZeroCoveredMean", mean([len(c & U) for c in per_fuzz]), len(U))
    tex.pct("PctTestCovered", len(T & U), len(U))
    tex.count("NumTestMissed", len(missed))
    tex.count("NumSynthOfTestMissed", len(S & missed))
    tex.count("NumFuzzOfTestMissed", len(F & missed))
    tex.count("NumOnlySynth", len((S & U) - T - F))
    # defects Synth262 found, by status
    found = stored["solve"]
    novel = {n for n in found if status[n] != "known-upstream"}
    tex.count("NumSolverBugs", len(found))
    tex.count("NumSolverNovelBugs", len(novel))
    tex.count("NumSolverNovelAcked", sum(status[n] in ("fixed", "confirmed") for n in novel))
    tex.count("NumSolverNovelPatched", sum(status[n] == "fixed" for n in novel))
    tex.write(args.tex)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (FileNotFoundError, ValueError) as err:
        print(f"error: {err}", file=sys.stderr)
        raise SystemExit(1)
