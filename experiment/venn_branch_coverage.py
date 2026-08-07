#!/usr/bin/env python3

# python3 experiment/venn_branch_coverage.py
#   ../fuzz-data/0fs-50h logs/test262/recent \
#   -o experiment/result.svg

from __future__ import annotations

import argparse
import html
import json
import os
import re
import sys
from pathlib import Path
from typing import Iterable

BranchSide = tuple[int, str]

BRANCH_FILE_RE = re.compile(r"^branch-(\d+)-([TF])$")
INCIDENTAL_DIR = "incidental-programs"
INCIDENTAL_FILE_RE = re.compile(r"^(\d+)-([TF])\.js$")
INCIDENTAL_STATUS = "incidental"
SUMMARY_BRANCH_RE = re.compile(r"\bBranch\[(\d+)\]:(T|F)\b")
SUMMARY_STATUS_RE = re.compile(r"^\s*\[([A-Za-z0-9_.-]+)\]\s+\d+\s*$")
JS_LINE_RE = re.compile(r"^\s*js:\s*(.*\S)\s*$")
SIDE_ORDER = {"T": 0, "F": 1}


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


def parse_branch_side_from_name(path: Path) -> BranchSide | None:
    match = BRANCH_FILE_RE.match(path.name)
    if not match:
        return None
    return int(match.group(1)), match.group(2)


def parse_branch_side_from_summary_line(line: str) -> BranchSide | None:
    match = SUMMARY_BRANCH_RE.search(line)
    if not match:
        return None
    return int(match.group(1)), match.group(2)


def load_solver_program(path: Path) -> str | None:
    with path.open(encoding="utf-8") as f:
        for line in f:
            match = JS_LINE_RE.match(line)
            if match:
                return match.group(1)
    return None


def load_incidental(solver_log: Path) -> dict[BranchSide, str]:
    programs: dict[BranchSide, str] = {}
    directory = solver_log / INCIDENTAL_DIR
    if not directory.is_dir():
        return programs
    for path in sorted(directory.iterdir()):
        match = INCIDENTAL_FILE_RE.match(path.name)
        if match is None or not path.is_file():
            continue
        side = (int(match.group(1)), match.group(2))
        programs[side] = path.read_text(encoding="utf-8").strip()
    return programs


def load_solver_sets(
    solver_log: Path,
    solver_statuses: set[str],
) -> tuple[set[BranchSide], set[BranchSide], dict[str, int], dict[BranchSide, str]]:
    if not solver_log.exists():
        raise FileNotFoundError(f"solver log directory not found: {solver_log}")

    universe: set[BranchSide] = set()
    solver: set[BranchSide] = set()
    status_counts: dict[str, int] = {}
    solver_programs: dict[BranchSide, str] = {}

    for child in sorted(solver_log.iterdir()):
        if not child.is_dir():
            continue
        sides: set[BranchSide] = set()
        for path in child.iterdir():
            if not path.is_file():
                continue
            side = parse_branch_side_from_name(path)
            if side is None:
                continue
            sides.add(side)
            if child.name == "pass":
                program = load_solver_program(path)
                if program is not None:
                    solver_programs[side] = program
        if not sides:
            continue
        status_counts[child.name] = len(sides)
        universe |= sides
        if child.name in solver_statuses:
            solver |= sides

    incidental = load_incidental(solver_log)
    if incidental:
        status_counts[INCIDENTAL_STATUS] = len(incidental)
        universe |= set(incidental)
        if INCIDENTAL_STATUS in solver_statuses:
            solver |= set(incidental)
            for side, program in incidental.items():
                solver_programs.setdefault(side, program)

    if universe:
        return universe, solver, status_counts, solver_programs

    summary = solver_log / "summary"
    if not summary.exists():
        raise FileNotFoundError(
            "could not infer solver branch sides: expected status directories "
            f"or {summary}",
        )

    current_status: str | None = None
    with summary.open(encoding="utf-8") as f:
        for line in f:
            status_match = SUMMARY_STATUS_RE.match(line)
            if status_match:
                current_status = status_match.group(1)
                status_counts.setdefault(current_status, 0)
                continue
            side = parse_branch_side_from_summary_line(line)
            if side is None or current_status is None:
                continue
            universe.add(side)
            status_counts[current_status] = status_counts.get(current_status, 0) + 1
            if current_status in solver_statuses:
                solver.add(side)

    if not universe:
        raise ValueError(f"no branch sides found in solver log: {solver_log}")
    return universe, solver, status_counts, solver_programs


def coverage_json_files(path: Path, merge_nested: bool) -> list[Path]:
    if path.is_file():
        return [path]
    if not path.exists():
        raise FileNotFoundError(f"log path not found: {path}")

    direct = path / "branch-coverage.json"
    if direct.exists():
        return [direct]

    recent = path / "recent" / "branch-coverage.json"
    if recent.exists() and not merge_nested:
        return [recent]

    matches = sorted(path.rglob("branch-coverage.json"))
    if not matches:
        raise FileNotFoundError(f"no branch-coverage.json found under {path}")
    if merge_nested:
        return matches
    return [max(matches, key=lambda p: p.stat().st_mtime)]


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
            covered.add((int(branch), side))
    return covered


def region_counts(
    universe: set[BranchSide],
    solver: set[BranchSide],
    test262: set[BranchSide],
    fuzz: set[BranchSide],
) -> dict[str, int]:
    regions = region_sets(universe, solver, test262, fuzz)
    s = solver & universe
    t = test262 & universe
    f = fuzz & universe
    return {
        **{name: len(sides) for name, sides in regions.items()},
        "universe": len(universe),
        "solver": len(s),
        "test262": len(t),
        "fuzz": len(f),
    }


def region_sets(
    universe: set[BranchSide],
    solver: set[BranchSide],
    test262: set[BranchSide],
    fuzz: set[BranchSide],
) -> dict[str, set[BranchSide]]:
    s = solver & universe
    t = test262 & universe
    f = fuzz & universe
    return {
        "solver_only": s - t - f,
        "test262_only": t - s - f,
        "fuzz_only": f - s - t,
        "solver_test262": (s & t) - f,
        "solver_fuzz": (s & f) - t,
        "test262_fuzz": (t & f) - s,
        "all_three": s & t & f,
        "none": universe - (s | t | f),
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


def svg_text(
    counts: dict[str, int],
    solver_label: str,
    test262_label: str,
    fuzz_label: str,
    title: str,
) -> str:
    title_e = html.escape(title)
    solver_e = html.escape(solver_label)
    test262_e = html.escape(test262_label)
    fuzz_e = html.escape(fuzz_label)
    desc = (
        f"{title}: {solver_label}, {test262_label}, and {fuzz_label} "
        f"branch-side sets restricted to {format_int(counts['universe'])} "
        "builtin-entry reachable branch sides."
    )
    return f'''<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" width="920" height="700" viewBox="0 0 920 700" role="img" aria-labelledby="title desc">
  <title id="title">{title_e}</title>
  <desc id="desc">{html.escape(desc)}</desc>
  <style>
    .bg {{ fill: #ffffff; }}
    .title {{ font: 700 28px Arial, sans-serif; fill: #1f2933; }}
    .circle {{ stroke-width: 2.5; mix-blend-mode: multiply; }}
    .solver {{ fill: #f28b82; fill-opacity: 0.44; stroke: #d35f54; }}
    .test262 {{ fill: #80cbc4; fill-opacity: 0.48; stroke: #3f9f98; }}
    .fuzz {{ fill: #ffe082; fill-opacity: 0.58; stroke: #d3a72c; }}
    .set-label {{ font: 700 18px Arial, sans-serif; fill: #1f2933; text-anchor: middle; }}
    .set-size {{ font: 400 13px Arial, sans-serif; fill: #5b6570; text-anchor: middle; }}
    .count {{ font: 700 20px Arial, sans-serif; fill: #111827; text-anchor: middle; dominant-baseline: middle; }}
    .small-count {{ font: 700 16px Arial, sans-serif; fill: #111827; text-anchor: middle; dominant-baseline: middle; }}
    .universe {{ fill: none; stroke: #9aa5b1; stroke-width: 1.5; }}
    .universe-count {{ font: 700 18px Arial, sans-serif; fill: #5b6570; text-anchor: end; }}
    .universe-label {{ font: 400 11px Arial, sans-serif; fill: #9aa5b1; text-anchor: end; }}
  </style>

  <rect class="bg" x="0" y="0" width="920" height="700" />

  <rect class="universe" x="55" y="72" width="810" height="606" rx="6" />
  <text class="universe-count" x="850" y="100">{format_int(counts['none'])}</text>
  <text class="universe-label" x="850" y="118">not in any set</text>

  <text class="title" x="48" y="52">{title_e}</text>

  <g transform="translate(0,20)">
    <circle class="circle solver" cx="355" cy="275" r="178" />
    <circle class="circle test262" cx="525" cy="275" r="178" />
    <circle class="circle fuzz" cx="440" cy="420" r="178" />

    <text class="set-label" x="270" y="122">{solver_e}</text>
    <text class="set-size" x="270" y="144">{format_int(counts['solver'])}</text>
    <text class="set-label" x="610" y="122">{test262_e}</text>
    <text class="set-size" x="610" y="144">{format_int(counts['test262'])}</text>
    <text class="set-label" x="440" y="630">{fuzz_e}</text>
    <text class="set-size" x="440" y="652">{format_int(counts['fuzz'])}</text>

    <text class="count" x="272" y="290">{format_int(counts['solver_only'])}</text>
    <text class="count" x="608" y="290">{format_int(counts['test262_only'])}</text>
    <text class="count" x="440" y="535">{format_int(counts['fuzz_only'])}</text>
    <text class="small-count" x="440" y="218">{format_int(counts['solver_test262'])}</text>
    <text class="small-count" x="342" y="405">{format_int(counts['solver_fuzz'])}</text>
    <text class="small-count" x="538" y="405">{format_int(counts['test262_fuzz'])}</text>
    <text class="count" x="440" y="340">{format_int(counts['all_three'])}</text>
  </g>
</svg>
'''


def write_json_summary(
    path: Path,
    counts: dict[str, int],
    regions: dict[str, list[dict[str, object]]],
    sources: dict[str, object],
    outside_universe: dict[str, int],
    solver_status_counts: dict[str, int],
) -> None:
    data = {
        "counts": counts,
        "outside_universe": outside_universe,
        "regions": regions,
        "solver_status_counts": solver_status_counts,
        "sources": sources,
    }
    path.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def parse_args() -> argparse.Namespace:
    default_home = Path(
        os.environ.get("ESMETA_HOME", Path(__file__).resolve().parents[1]),
    )
    parser = argparse.ArgumentParser(
        description=(
            "Generate a Venn SVG for Solver/Test262/Fuzz branch-side coverage "
            "restricted to builtin-entry reachable solver targets."
        ),
    )
    parser.add_argument("fuzz_log", type=Path, help="fuzzer log directory or branch-coverage.json")
    parser.add_argument("test262_log", type=Path, help="test262 log directory or branch-coverage.json")
    parser.add_argument(
        "-s",
        "--solver-log",
        type=Path,
        default=default_home / "logs" / "solver",
        help="solver log directory used for universe and solver set",
    )
    parser.add_argument(
        "--solver-statuses",
        default=f"pass,{INCIDENTAL_STATUS}",
        help=(
            "comma-separated solver statuses counted as the Solver set; "
            f"'{INCIDENTAL_STATUS}' reads {INCIDENTAL_DIR}/"
        ),
    )
    parser.add_argument(
        "--merge-nested",
        action="store_true",
        help="merge every nested branch-coverage.json under fuzz/test262 paths",
    )
    parser.add_argument(
        "-o",
        "--out",
        type=Path,
        default=default_home / "experiment" / "venn.svg",
        help="output SVG path",
    )
    parser.add_argument(
        "--json-out",
        type=Path,
        default=None,
        help="optional JSON summary path; defaults to OUT with .json suffix",
    )
    parser.add_argument("--title", default="", help="SVG title")
    parser.add_argument("--solver-label", default="Solver", help="left set label")
    parser.add_argument("--test262-label", default="Test262", help="right set label")
    parser.add_argument("--fuzz-label", default="Fuzz", help="bottom set label")
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
    fuzz_files = coverage_json_files(args.fuzz_log, args.merge_nested)
    test262_files = coverage_json_files(args.test262_log, args.merge_nested)
    raw_fuzz = load_branch_coverage(fuzz_files)
    raw_test262 = load_branch_coverage(test262_files)

    solver = raw_solver & universe
    fuzz = raw_fuzz & universe
    test262 = raw_test262 & universe
    counts = region_counts(universe, solver, test262, fuzz)
    regions = region_details(
        region_sets(universe, solver, test262, fuzz),
        solver_programs,
    )
    outside_universe = {
        "solver": len(raw_solver - universe),
        "test262": len(raw_test262 - universe),
        "fuzz": len(raw_fuzz - universe),
    }

    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(
        svg_text(
            counts,
            args.solver_label,
            args.test262_label,
            args.fuzz_label,
            args.title,
        ),
        encoding="utf-8",
    )

    json_out = args.json_out or args.out.with_suffix(".json")
    json_out.parent.mkdir(parents=True, exist_ok=True)
    write_json_summary(
        json_out,
        counts,
        regions,
        sources={
            "solver_log": str(args.solver_log),
            "solver_statuses": sorted(solver_statuses),
            "fuzz_coverage": [str(p) for p in fuzz_files],
            "test262_coverage": [str(p) for p in test262_files],
        },
        outside_universe=outside_universe,
        solver_status_counts=solver_status_counts,
    )

    print(f"wrote SVG:  {args.out}")
    print(f"wrote JSON: {json_out}")
    print(f"universe:   {format_int(counts['universe'])}")
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
    raise SystemExit(main())
