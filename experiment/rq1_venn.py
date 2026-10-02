#!/usr/bin/env python3

# python3 experiment/rq1_venn.py [--tex ../synth262-paper/fse27/numbers/rq1_venn.tex]
#
# The region counts of the two RQ1 Venn diagrams, which the paper draws by hand
# in fse27/fig/images.key: branch sides covered by the union of each tool's
# runs, and the defect rows the triage ledger finds in their conform logs.
# --tex writes full-coverage comparisons, defect counts by status, and table rows by kind.

from __future__ import annotations

import argparse
import json
import sys
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
    tex.count("NumOnlySynthAll", len(S - T - F))
    tex.count("NumSynthOfTestMissedAll", len(S - T))
    tex.count("NumFuzzOfTestMissedAll", len(F - T))
    U = universe(solve[0])
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
        tex.count("NumOnlyFuzzDefects", len(stored["fuzz"] - found) if complete else None)
        # Synth262's defects by engine and status (NumEngineXsFixed, ..., NumEngineAllTotal)
        for engine, macro in (("v8", "VEight"), ("jsc", "Jsc"), ("sm", "Sm"), ("graal", "Graal"),
                              ("xs", "Xs"), ("qjs", "Qjs"), (None, "All")):
            mine = {n for n in found if engine in (None, n.split("-")[0])}
            for name, state in (("Fixed", "fixed"), ("Confirmed", "confirmed"),
                                ("Reported", "reported"), ("Known", "known-upstream")):
                tex.count(f"NumEngine{macro}{name}", sum(status[n] == state for n in mine) if complete else None)
            tex.count(f"NumEngine{macro}Total", len(mine) if complete else None)
        # defects either tool found, by kind tag (wrong-check-order -> NumKindWrongCheckOrderAll)
        either = found | stored["fuzz"]
        for kind, names in json.loads(TABLE.read_text(encoding="utf-8"))["tags"].items():
            tex.count("NumKind" + kind.title().replace("-", "") + "All",
                      sum(n in either for n in names) if complete else None)

    tex.write(args.tex)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (FileNotFoundError, ValueError) as err:
        print(f"error: {err}", file=sys.stderr)
        raise SystemExit(1)
