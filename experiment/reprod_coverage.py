#!/usr/bin/env python3

# python3 experiment/reprod_coverage.py
#
# Final branch-side coverage of each run, cut to the solver's universe as in
# coverage-venn.pdf. The reducer keeps coverage, so it gets no box here.

from __future__ import annotations

import argparse
import os
import sys
from pathlib import Path

import box
import venn_coverage


def parse_args() -> argparse.Namespace:
    home = Path(os.environ.get("ESMETA_HOME", Path(__file__).resolve().parents[1]))
    parser = argparse.ArgumentParser(description="Box plot of coverage per solver and fuzzer run.")
    parser.add_argument("-d", "--data", type=Path, default=home / "experiment" / "data")
    parser.add_argument(
        "-s",
        "--solver-log",
        type=Path,
        default=None,
        help="solver run whose summary gives the universe (default: solve-1)",
    )
    parser.add_argument("-o", "--out", type=Path, default=home / "experiment" / "reprod-coverage.pdf")
    parser.add_argument("--configs", default="solve,0fs,1fs", help="comma-separated run prefixes")
    parser.add_argument("--width", type=float, default=240.0, help="figure width in points")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    solver_log = args.solver_log or box.config_runs(args.data, "solve")[0]
    universe, _, _, _ = venn_coverage.load_solver_sets(solver_log, {"pass"})

    groups: dict[str, list[int]] = {}
    per_run: dict[str, dict[str, int]] = {}
    for config in args.configs.split(","):
        counts = []
        for run in box.config_runs(args.data, config):
            covered = venn_coverage.load_branch_coverage([run / "branch-coverage.json"])
            per_run[run.name] = {"universe": len(covered & universe), "all": len(covered)}
            counts.append(len(covered & universe))
        groups[box.label(config)] = counts

    box.render(
        args.out, groups, "Coverage (%)", args.width, integer=False, total=len(universe),
    )
    json_out = args.out.with_suffix(".json")
    box.write_json(
        json_out,
        {"counts": groups, "runs": per_run, "universe": len(universe), "solver_log": str(solver_log)},
    )
    print(f"wrote:      {args.out}")
    print(f"wrote JSON: {json_out}")
    for label, counts in groups.items():
        print(f"{label}: {counts}  ({', '.join(f'{100 * c / len(universe):.1f}%' for c in counts)})")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except venn_coverage.MissingLog as missing:
        print(f"error: {missing}", file=sys.stderr)
        raise SystemExit(1)
