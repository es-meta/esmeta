#!/usr/bin/env python3

# python3 experiment/venn_bug.py
#
# Two-set Venn over the reported bug rows: which ones Synth262 reproduces,
# which ones the fuzzer reproduces, and which ones neither does.
#
# experiment/bugs.json lists every row under its status, and separately the
# rows sharing a tag. Nothing else is stored there: the engine comes from the
# row name, and whether a tool reproduces a row is whether it left a file under
# bugs/solver or bugs/fuzzer, which is the only place a claimed reproduction
# can be run. Issue links stay out so the artifact can be anonymous.

from __future__ import annotations

import argparse
import html
import json
import os
import re
import sys

import venn
from pathlib import Path

# a row whose status says upstream does not consider it a defect
NOT_A_BUG = {"invalid"}
# a row name carries its engine, so the table does not repeat it
ENGINE_BY_PREFIX = {
    "jsc": "JavaScriptCore",
    "qjs": "QuickJS",
    "sm": "SpiderMonkey",
    "xs": "Moddable XS",
    "v8": "V8",
    "graal": "GraalJS",
}
PREFIX_RE = re.compile(r"^(jsc|qjs|sm|xs|v8|graal)-[\w.-]+$")


def format_int(n: int) -> str:
    return f"{n:,}"


def engine_of(name: str) -> str:
    match = PREFIX_RE.match(name)
    if match is None:
        raise ValueError(f"cannot tell the engine of {name!r}")
    return ENGINE_BY_PREFIX[match.group(1)]


def load_rows(path: Path) -> list[dict[str, str]]:
    """one row per bug, in the order the statuses list them"""
    if not path.is_file():
        raise FileNotFoundError(f"bug table not found: {path}")
    doc = json.loads(path.read_text(encoding="utf-8"))
    tag_of = {
        name: tag for tag, names in doc.get("tags", {}).items() for name in names
    }
    rows: list[dict[str, str]] = []
    seen: set[str] = set()
    for status, names in doc["status"].items():
        for name in names:
            if name in seen:
                raise ValueError(f"{name}: listed under more than one status")
            seen.add(name)
            rows.append(
                {
                    "bug-id": name,
                    "engine": engine_of(name),
                    "status": status,
                    "tag": tag_of.get(name, ""),
                },
            )
    stray = set(tag_of) - seen
    if stray:
        raise ValueError(f"tagged but under no status: {', '.join(sorted(stray))}")
    return rows


def reproductions(bugs_dir: Path, source: str) -> set[str]:
    """the rows one tool left a runnable program for"""
    directory = bugs_dir / source
    if not directory.is_dir():
        raise FileNotFoundError(f"reproduction directory not found: {directory}")
    return {p.stem for p in directory.glob("*.js")}


def check_reproductions(
    rows: list[dict[str, str]],
    stored: dict[str, set[str]],
) -> list[str]:
    """a reproduction of a row the table does not list means one of them is stale"""
    listed = {r["bug-id"] for r in rows}
    return [
        f"bugs/{source}/{name}.js has no row in the table"
        for source, names in sorted(stored.items())
        for name in sorted(names - listed)
    ]


def tally(
    rows: list[dict[str, str]],
    stored: dict[str, set[str]],
) -> tuple[dict[str, int], dict[str, list[str]]]:
    regions: dict[str, list[str]] = {
        "solver_only": [],
        "both": [],
        "fuzz_only": [],
        "none": [],
    }
    for r in rows:
        name = r["bug-id"]
        s = name in stored["solver"]
        f = name in stored["fuzzer"]
        key = "both" if s and f else "solver_only" if s else "fuzz_only" if f else "none"
        regions[key].append(name)
    counts = {k: len(v) for k, v in regions.items()}
    counts["rows"] = len(rows)
    counts["solver"] = counts["solver_only"] + counts["both"]
    counts["fuzz"] = counts["fuzz_only"] + counts["both"]
    return counts, regions


def breakdown(rows: list[dict[str, str]], column: str) -> dict[str, int]:
    out: dict[str, int] = {}
    for r in rows:
        key = r[column].strip() or "(blank)"
        out[key] = out.get(key, 0) + 1
    return dict(sorted(out.items()))


def figure(
    counts: dict[str, int],
    solver_label: str,
    fuzz_label: str,
    width_pt: float,
) -> tuple[str, list[str]]:
    names = [solver_label, fuzz_label]
    regions = {
        frozenset({solver_label}): counts["solver_only"],
        frozenset({solver_label, fuzz_label}): counts["both"],
        frozenset({fuzz_label}): counts["fuzz_only"],
    }
    circles = venn.place(names, regions, 1.0)
    svg = venn.render(
        names,
        regions,
        outside=counts["none"],
        outside_label="reproduced by neither",
        width_pt=width_pt,
        title="Reported bugs each tool reproduces",
    )
    return svg, venn.fit_report(circles, regions, 1.0)


def parse_args() -> argparse.Namespace:
    default_home = Path(
        os.environ.get("ESMETA_HOME", Path(__file__).resolve().parents[1]),
    )
    parser = argparse.ArgumentParser(
        description="Generate a Venn SVG for which reported bugs each tool reproduces.",
    )
    parser.add_argument(
        "table",
        type=Path,
        nargs="?",
        default=default_home / "experiment" / "bugs.json",
        help="the bug rows, grouped by status and by tag",
    )
    parser.add_argument(
        "-b",
        "--bugs-dir",
        type=Path,
        default=default_home / "bugs",
        help="directory holding solver/ and fuzzer/ reproductions",
    )
    parser.add_argument(
        "-o",
        "--out",
        type=Path,
        default=default_home / "experiment" / "bug-venn.svg",
        help="output SVG path",
    )
    parser.add_argument(
        "--json-out",
        type=Path,
        default=None,
        help="optional JSON summary path; defaults to OUT with .json suffix",
    )
    parser.add_argument(
        "--skip-check",
        action="store_true",
        help="write the figure even if bugs/ holds a row the table does not list",
    )
    parser.add_argument(
        "--width",
        type=float,
        default=240.0,
        help="figure width in points (240 fits one column)",
    )
    parser.add_argument("--solver-label", default="Synth262", help="left set label")
    parser.add_argument("--fuzz-label", default="Fuzzer", help="right set label")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    every = load_rows(args.table)
    rows = [r for r in every if r["status"].strip() not in NOT_A_BUG]
    dropped = len(every) - len(rows)

    stored = {s: reproductions(args.bugs_dir, s) for s in ("solver", "fuzzer")}
    # against every row, not just the bugs: a row excluded as not a bug still
    # keeps its reproduction for provenance
    complaints = check_reproductions(every, stored)
    if complaints:
        print(f"{len(complaints)} reproduction(s) with no row:", file=sys.stderr)
        for line in complaints:
            print(f"  {line}", file=sys.stderr)
        if not args.skip_check:
            print("  (pass --skip-check to write the figure anyway)", file=sys.stderr)
            return 1

    counts, regions = tally(rows, stored)

    args.out.parent.mkdir(parents=True, exist_ok=True)
    svg, off = figure(counts, args.solver_label, args.fuzz_label, args.width)
    args.out.write_text(svg, encoding="utf-8")
    json_out = args.json_out or args.out.with_suffix(".json")
    json_out.write_text(
        json.dumps(
            {
                "counts": counts,
                "regions": regions,
                "by_status": breakdown(rows, "status"),
                "by_engine": breakdown(rows, "engine"),
                "by_tag": breakdown(rows, "tag"),
                "excluded": {
                    "count": dropped,
                    "statuses": sorted(NOT_A_BUG),
                    "rows": sorted(
                        r["bug-id"] for r in every if r["status"].strip() in NOT_A_BUG
                    ),
                },
                "sources": {"table": str(args.table), "bugs_dir": str(args.bugs_dir)},
            },
            indent=2,
            sort_keys=True,
            ensure_ascii=False,
        )
        + "\n",
        encoding="utf-8",
    )

    print(f"wrote SVG:  {args.out}")
    print(f"wrote JSON: {json_out}")
    print(f"bug rows:   {format_int(counts['rows'])}  ({dropped} excluded as not a bug)")
    print(
        "regions:    "
        f"only({args.solver_label})={counts['solver_only']}, "
        f"both={counts['both']}, "
        f"only({args.fuzz_label})={counts['fuzz_only']}, "
        f"neither={counts['none']}",
    )
    print(f"sets:       {args.solver_label}={counts['solver']}, {args.fuzz_label}={counts['fuzz']}")
    for line in off:
        print(f"  area off:  {line}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (FileNotFoundError, ValueError) as err:
        print(f"error: {err}", file=sys.stderr)
        raise SystemExit(1)
