#!/usr/bin/env python3

# python3 experiment/venn_coverage.py
#
# The default fuzzer set is the 1-FS runs only, never everything under
# experiment/data: 0-FS and 1-FS are separate configurations and one figure
# must not mix them. Name the other runs to draw that configuration instead.

from __future__ import annotations

import argparse
import html
import json
import os
import re
import sys
from pathlib import Path
from typing import Iterable

import venn

BranchSide = tuple[int, str]

SUMMARY_BRANCH_RE = re.compile(r"\bBranch\[(\d+)\]:(T|F)\b")
SUMMARY_STATUS_RE = re.compile(r"^\s*\[([A-Za-z0-9_.-]+)\]\s+\d+\s*$")
SIDE_ORDER = {"T": 0, "F": 1}

# what to run when a log the figure needs has not been produced yet
SOLVE_HINT = 'sbt \'run solve -solve:log\''
TEST262_HINT = (
    "sbt 'run test262-test -test262-test:progress -test262-test:coverage "
    "-test262-test:detail-log -test262-test:concurrent=0'"
)
FUZZ_HINT = "experiment/data.sh unpack"


class MissingLog(Exception):
    """a log is absent or incomplete, with the command that produces it"""

    def __init__(self, what: str, path: Path, hint: str):
        super().__init__(f"{what} not found: {path}\n  run: {hint}")


def format_int(n: int) -> str:
    return f"{n:,}"


def parse_side(value: object) -> str:
    if isinstance(value, bool):
        return "T" if value else "F"
    text = str(value).strip().lower()
    if text in {"t", "true", "then", "1"}:
        return "T"
    if text in {"f", "false", "else", "0"}:
        return "F"
    raise ValueError(f"unknown branch side: {value!r}")


def parse_branch_side_from_summary_line(line: str) -> BranchSide | None:
    match = SUMMARY_BRANCH_RE.search(line)
    if not match:
        return None
    return int(match.group(1)), match.group(2)


def load_solver_programs(solver_log: Path) -> dict[BranchSide, str]:
    """branch side -> the program the solver kept for it

    The run's branch-coverage.json is the only place that ties a branch side to
    the file covering it; `summary` names neither.
    """
    coverage = solver_log / "branch-coverage.json"
    if not coverage.exists():
        return {}
    programs: dict[BranchSide, str] = {}
    for entry in json.loads(coverage.read_text(encoding="utf-8")):
        script = entry.get("script")
        if script is None:
            continue
        path = solver_log / script
        if not path.is_file():
            continue
        cond = entry["condView"]["cond"]
        side = (int(cond["branch"]), parse_side(cond["cond"]))
        programs[side] = path.read_text(encoding="utf-8").strip()
    return programs


def load_solver_sets(
    solver_log: Path,
    solver_statuses: set[str],
) -> tuple[set[BranchSide], set[BranchSide], dict[str, int], dict[BranchSide, str]]:
    """(universe, solver set, per-status counts, program per solver branch side)"""
    summary = solver_log / "summary"
    if not summary.exists():
        raise MissingLog("solver summary", summary, SOLVE_HINT)

    universe: set[BranchSide] = set()
    solver: set[BranchSide] = set()
    status_counts: dict[str, int] = {}
    status: str | None = None
    with summary.open(encoding="utf-8") as f:
        for line in f:
            match = SUMMARY_STATUS_RE.match(line)
            if match:
                status = match.group(1)
                status_counts.setdefault(status, 0)
                continue
            side = parse_branch_side_from_summary_line(line)
            if side is None or status is None:
                continue
            universe.add(side)
            status_counts[status] += 1
            if status in solver_statuses:
                solver.add(side)

    if not universe:
        raise ValueError(f"no branch sides found in {summary}")
    unknown = solver_statuses - set(status_counts)
    if unknown:
        raise ValueError(f"no such solver status: {', '.join(sorted(unknown))}")
    return universe, solver, status_counts, load_solver_programs(solver_log)


def coverage_json_files(paths: list[Path], what: str, hint: str) -> list[Path]:
    """every branch-coverage.json under the given paths, merged into one set"""
    found: list[Path] = []
    for path in paths:
        if path.is_file():
            found.append(path)
        elif path.exists():
            found.extend(sorted(path.rglob("branch-coverage.json")))
    if not found:
        raise MissingLog(
            f"{what} branch coverage",
            Path(", ".join(str(p) for p in paths)),
            hint,
        )
    return sorted(set(found))


def load_branch_coverage(paths: Iterable[Path]) -> set[BranchSide]:
    covered: set[BranchSide] = set()
    for path in paths:
        with path.open(encoding="utf-8") as f:
            data = json.load(f)
        if not isinstance(data, list):
            raise ValueError(f"expected a JSON array in {path}")
        for entry in data:
            try:
                cond = entry["condView"]["cond"]
                branch = cond["branch"]
                side = parse_side(cond["cond"])
            except (KeyError, TypeError) as exc:
                raise ValueError(f"unexpected coverage entry in {path}: {entry!r}") from exc
            if isinstance(branch, dict):
                branch = branch.get("id")
            if branch is None:
                raise ValueError(f"branch without an id in {path}: {entry!r}")
            covered.add((int(branch), side))
    return covered


def region_sets(
    solver: set[BranchSide],
    test262: set[BranchSide],
    fuzz: set[BranchSide],
    universe: set[BranchSide],
) -> dict[str, set[BranchSide]]:
    """the eight disjoint parts, over sets already cut down to the universe"""
    return {
        "solver_only": solver - test262 - fuzz,
        "test262_only": test262 - solver - fuzz,
        "fuzz_only": fuzz - solver - test262,
        "solver_test262": (solver & test262) - fuzz,
        "solver_fuzz": (solver & fuzz) - test262,
        "test262_fuzz": (test262 & fuzz) - solver,
        "all_three": solver & test262 & fuzz,
        "none": universe - (solver | test262 | fuzz),
    }


def branch_side_key(side: BranchSide) -> tuple[int, int]:
    branch, side_name = side
    return branch, SIDE_ORDER[side_name]


def region_details(
    regions: dict[str, set[BranchSide]],
    solver_programs: dict[BranchSide, str],
) -> dict[str, list[dict[str, object]]]:
    solver_regions = {
        "solver_only",
        "solver_test262",
        "solver_fuzz",
        "all_three",
    }
    details: dict[str, list[dict[str, object]]] = {}
    for name, sides in regions.items():
        entries: list[dict[str, object]] = []
        for branch, side in sorted(sides, key=branch_side_key):
            entry: dict[str, object] = {"branch": branch, "side": side}
            if name in solver_regions:
                program = solver_programs.get((branch, side))
                if program is not None:
                    entry["program"] = program
            entries.append(entry)
        details[name] = entries
    return details


def figure(
    counts: dict[str, int],
    solver_label: str,
    test262_label: str,
    fuzz_label: str,
    width_pt: float,
) -> tuple[str, list[str]]:
    names = [solver_label, test262_label, fuzz_label]
    s, t, f = names
    regions = {
        frozenset({s}): counts["solver_only"],
        frozenset({t}): counts["test262_only"],
        frozenset({f}): counts["fuzz_only"],
        frozenset({s, t}): counts["solver_test262"],
        frozenset({s, f}): counts["solver_fuzz"],
        frozenset({t, f}): counts["test262_fuzz"],
        frozenset({s, t, f}): counts["all_three"],
    }
    circles = venn.place(names, regions, 1.0)
    svg = venn.render(
        names,
        regions,
        outside=counts["none"],
        outside_label="covered by none",
        width_pt=width_pt,
        title="Branch sides each source covers",
    )
    return svg, venn.fit_report(circles, regions, 1.0)


def parse_args() -> argparse.Namespace:
    default_home = Path(
        os.environ.get("ESMETA_HOME", Path(__file__).resolve().parents[1]),
    )
    parser = argparse.ArgumentParser(
        description=(
            "Generate a Venn diagram for Solver/Test262/Fuzz branch-side coverage "
            "restricted to builtin-entry reachable solver targets."
        ),
    )
    parser.add_argument(
        "fuzz_log",
        type=Path,
        nargs="*",
        default=sorted((default_home / "experiment" / "data").glob("1fs-*/")),
        help="fuzzer runs to merge (default: every 1-FS run under experiment/data)",
    )
    parser.add_argument(
        "-t",
        "--test262-log",
        type=Path,
        default=default_home / "logs" / "test262" / "recent",
        help="test262 log directory or branch-coverage.json",
    )
    parser.add_argument(
        "-s",
        "--solver-log",
        type=Path,
        default=default_home / "logs" / "solver" / "recent",
        help="solver run directory holding `summary` and branch-coverage.json",
    )
    parser.add_argument(
        "--solver-statuses",
        default="pass",
        help="comma-separated solver statuses counted as the Solver set",
    )
    parser.add_argument(
        "-o",
        "--out",
        type=Path,
        default=default_home / "experiment" / "coverage-venn.pdf",
        help="output PDF (rsvg-convert); an .svg path writes the SVG itself",
    )
    parser.add_argument(
        "--json-out",
        type=Path,
        default=None,
        help="optional JSON summary path; defaults to OUT with .json suffix",
    )
    parser.add_argument(
        "--width",
        type=float,
        default=240.0,
        help="figure width in points (240 fits one column)",
    )
    parser.add_argument("--solver-label", default="Synth262", help="left set label")
    parser.add_argument("--test262-label", default="Test262", help="right set label")
    parser.add_argument("--fuzz-label", default="ESMeta Fuzzer", help="bottom set label")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    solver_statuses = {
        status.strip()
        for status in args.solver_statuses.split(",")
        if status.strip()
    }
    if not solver_statuses:
        print("error: --solver-statuses must not be empty", file=sys.stderr)
        return 2

    universe, raw_solver, solver_status_counts, solver_programs = load_solver_sets(
        args.solver_log,
        solver_statuses,
    )
    fuzz_files = coverage_json_files(args.fuzz_log, "fuzzer", FUZZ_HINT)
    test262_files = coverage_json_files([args.test262_log], "test262", TEST262_HINT)
    raw_fuzz = load_branch_coverage(fuzz_files)
    raw_test262 = load_branch_coverage(test262_files)

    solver = raw_solver & universe
    fuzz = raw_fuzz & universe
    test262 = raw_test262 & universe
    regions = region_sets(solver, test262, fuzz, universe)
    counts = {name: len(sides) for name, sides in regions.items()} | {
        "universe": len(universe),
        "solver": len(solver),
        "test262": len(test262),
        "fuzz": len(fuzz),
    }
    outside_universe = {
        "solver": len(raw_solver - universe),
        "test262": len(raw_test262 - universe),
        "fuzz": len(raw_fuzz - universe),
    }

    args.out.parent.mkdir(parents=True, exist_ok=True)
    svg, off = figure(
        counts, args.solver_label, args.test262_label, args.fuzz_label, args.width,
    )
    venn.write(args.out, svg)

    json_out = args.json_out or args.out.with_suffix(".json")
    json_out.parent.mkdir(parents=True, exist_ok=True)
    json_out.write_text(
        json.dumps(
            {
                "counts": counts,
                "outside_universe": outside_universe,
                "regions": region_details(regions, solver_programs),
                "solver_status_counts": solver_status_counts,
                "sources": {
                    "solver_log": str(args.solver_log),
                    "solver_statuses": sorted(solver_statuses),
                    "fuzz_coverage": [str(p) for p in fuzz_files],
                    "test262_coverage": [str(p) for p in test262_files],
                },
            },
            indent=2,
            sort_keys=True,
        )
        + "\n",
        encoding="utf-8",
    )

    print(f"wrote:      {args.out}")
    print(f"wrote JSON: {json_out}")
    print(f"universe:   {format_int(counts['universe'])}")
    print(f"merged:     {len(fuzz_files)} fuzz, {len(test262_files)} test262 coverage files")
    print(
        "sets:       "
        f"{args.solver_label}={format_int(counts['solver'])}, "
        f"{args.test262_label}={format_int(counts['test262'])}, "
        f"{args.fuzz_label}={format_int(counts['fuzz'])}",
    )
    print(
        "regions:    "
        f"only({args.solver_label})={format_int(counts['solver_only'])}, "
        f"only({args.test262_label})={format_int(counts['test262_only'])}, "
        f"only({args.fuzz_label})={format_int(counts['fuzz_only'])}, "
        f"all={format_int(counts['all_three'])}, "
        f"none={format_int(counts['none'])}",
    )
    for line in off:
        print(f"  area off:  {line}", file=sys.stderr)
    if any(outside_universe.values()):
        print(
            "outside universe: "
            + ", ".join(
                f"{name}={format_int(count)}"
                for name, count in outside_universe.items()
                if count
            ),
        )
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except MissingLog as missing:
        print(f"error: {missing}", file=sys.stderr)
        raise SystemExit(1)
