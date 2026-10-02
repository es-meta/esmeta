#!/usr/bin/env python3

# python3 experiment/rq1_venn.py [--tex ../synth262-paper/fse27/numbers/rq1_venn.tex]
#
# The numbers of the two RQ1 Venn diagrams (fse27/fig/rq1-venn.tex): each
# region is the mean over all pairs of a Synth262 run and a fuzzing run, against
# the single Test262 run, for branch sides and for the defect rows the triage
# ledger finds in their conform logs. Stdout also shows the unions of all runs.
# --tex also writes the defects Synth262 found by engine and status, and the
# defects either tool found by kind.

from __future__ import annotations

import argparse
import json
import re
import statistics
import sys
from itertools import product
from pathlib import Path

from results import DATA, INTERACTION, PLAIN, TABLE, Defects, Tex, bug_status, covered, runs, universe


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
    parser.add_argument("--coverage-only", action="store_true", help="update generation metrics only")
    args = parser.parse_args()
    solve, fuzz = runs("solve"), runs("fuzz")
    per_solve = [covered(r / "branch-coverage.json") for r in solve]
    per_fuzz = [covered(r / "branch-coverage.json") for r in fuzz]
    S, F = set().union(*per_solve), set().union(*per_fuzz)
    T = covered(DATA / "test262" / "branch-coverage.json")
    show("branch coverage (union of runs, including outside targets)", {"Synth262": S, "Test262": T, "JEST": F})
    tex = Tex("rq1_venn.py")
    # Figure 6(b) draws per-run means: each region averages all pairs of a
    # Synth262 run and a fuzzing run, against the single Test262 run
    tex.mean("NumSynthOfTestMissedMean", statistics.mean(len(x - T) for x in per_solve))
    tex.mean("NumFuzzOfTestMissedMean", statistics.mean(len(x - T) for x in per_fuzz))
    synth_missed, fuzz_missed = (statistics.mean(len(x - T) for x in xs) for xs in (per_solve, per_fuzz))
    tex.pct("PctSynthOverFuzzMissed", synth_missed - fuzz_missed, fuzz_missed)
    pairs = list(product(per_solve, per_fuzz))
    for name, region in (("S", lambda s, f: s - T - f), ("T", lambda s, f: T - s - f), ("F", lambda s, f: f - s - T),
                         ("ST", lambda s, f: (s & T) - f), ("SF", lambda s, f: (s & f) - T),
                         ("TF", lambda s, f: (T & f) - s), ("STF", lambda s, f: s & T & f)):
        tex.mean(f"NumVennCov{name}", statistics.mean(len(region(s, f)) for s, f in pairs))
    # the sides only Synth262 covers in any run, by the algorithm the solver names
    names = {}
    for r in solve:
        for branch, side, func in re.findall(r"Branch\[(\d+)\]:(T|F)\s+(\S+)", (r / "summary").read_text()):
            names[(int(branch), side)] = func
    base64 = re.compile(r"Base64|Hex|GetUint8ArrayBytes")
    tex.count("NumSynthOnlyBaseSixtyFour", sum(bool(base64.search(names.get(x, ""))) for x in S - T - F))
    U = universe(solve[0])
    tex.count("NumBranchUniverse", len(U))
    if any(universe(r) != U for r in solve):
        raise ValueError("solver runs use different target universes")
    if not args.coverage_only:
        status = bug_status()
        rows = set(status)
        defects = Defects()
        # rows each tool reproduces in any run, as shipped: Synth262 with both
        # oracles, the fuzzer with its final-state assertions only (RQ2 adds the
        # interaction-based oracle to the fuzzer)
        logs = {"solve": (PLAIN["solve"], INTERACTION), "fuzz": (PLAIN["fuzz"],)}
        stored = {
            src: set().union(*(defects.found(r / log) or set() for r in rs for log in logs[src]))
            for src, rs in (("solve", solve), ("fuzz", fuzz))
        }
        complete = all((r / INTERACTION).is_file() for r in solve + fuzz) and not defects.pending
        # the defect Venn of Figure 6(b), also per pair of runs
        if complete:
            ours = [set().union(*(defects.found(r / log) or set() for log in logs["solve"])) for r in solve]
            theirs = [defects.found(r / PLAIN["fuzz"]) or set() for r in fuzz]
            dpairs = list(product(ours, theirs))
            tex.mean("NumVennDefS", statistics.mean(len(a - b) for a, b in dpairs))
            tex.mean("NumVennDefSF", statistics.mean(len(a & b) for a, b in dpairs))
            tex.mean("NumVennDefF", statistics.mean(len(b - a) for a, b in dpairs))
        if complete:
            show(f"\ndefects ({len(rows)} rows; {len(rows - stored['solve'] - stored['fuzz'])} reproduced by neither)",
             {"Synth262": stored["solve"], "JEST": stored["fuzz"]})

        if defects.pending:
            print(f"Defect Venn pending: {len(defects.pending)} unclassified engine/program pairs")

        # defects Synth262 found, by status
        found = stored["solve"]
        novel = {n for n in found if status[n] != "known-upstream"}
        tex.count("NumSolverDefects", len(found) if complete else None)
        tex.count("NumSolverNovelDefects", len(novel) if complete else None)
        tex.count("NumSolverNovelAcked", sum(status[n] in ("fixed", "confirmed") for n in novel) if complete else None)
        tex.count("NumSolverNovelPatched", sum(status[n] == "fixed" for n in novel) if complete else None)
        # Synth262's defects by engine and status (NumEngineXsFixed, ..., NumEngineAllTotal)
        for engine, macro in (("v8", "VEight"), ("jsc", "Jsc"), ("sm", "Sm"), ("graal", "Graal"),
                              ("xs", "Xs"), ("qjs", "Qjs"), (None, "All")):
            mine = {n for n in found if engine in (None, n.split("-")[0])}
            for name, state in (("Fixed", "fixed"), ("Confirmed", "confirmed"),
                                ("Reported", "reported"), ("Known", "known-upstream")):
                tex.count(f"NumEngine{macro}{name}", sum(status[n] == state for n in mine) if complete else None)
            tex.count(f"NumEngine{macro}Total", len(mine) if complete else None)
            tex.count(f"NumEngine{macro}New", sum(status[n] != "known-upstream" for n in mine) if complete else None)
        # defects either tool found, by kind tag (wrong-check-order -> NumKindWrongCheckOrderAll)
        # Synth262's defects by kind (Table 5): those its final-state assertions
        # reproduce in any run, and those only its instrumented programs reveal
        plain = set().union(*(defects.found(r / PLAIN["solve"]) or set() for r in solve))
        for kind, names in json.loads(TABLE.read_text(encoding="utf-8"))["tags"].items():
            macro = "NumKind" + kind.title().replace("-", "")
            tex.count(macro + "All", sum(n in found for n in names) if complete else None)
            tex.count(macro + "Plain", sum(n in found & plain for n in names) if complete else None)
            tex.count(macro + "Oracle", sum(n in found - plain for n in names) if complete else None)
        tex.count("NumKindTotalAll", len(found) if complete else None)
        tex.count("NumKindTotalPlain", len(found & plain) if complete else None)
        tex.count("NumKindTotalOracle", len(found - plain) if complete else None)
        order = {n for k in ("missing-check", "wrong-check-order", "extra-check")
                 for n in json.loads(TABLE.read_text(encoding="utf-8"))["tags"][k]}
        tex.count("NumKindOrderOracle", len((found - plain) & order) if complete else None)

    tex.write(args.tex)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (FileNotFoundError, ValueError) as err:
        print(f"error: {err}", file=sys.stderr)
        raise SystemExit(1)
