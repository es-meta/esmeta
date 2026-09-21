#!/usr/bin/env python3

# How many reported bugs does the order-aware oracle have to see?
#
# Runs conform-test twice over the stored reproductions, once instrumenting the
# program for evaluation order and once not, and splits the rows three ways:
# caught either way, caught only when instrumented, and caught by neither
# because the mechanized specification cannot run them at all.
#
# python3 experiment/order_oracle_bugs.py bugs \
#   --home ~/.frozen-engines --merge 'qjs-1738-*' \
#   -o experiment/order-oracle.json
#
# --merge names the reproductions that witness one row; a numeric suffix cannot
# decide it on its own, since xs-1669-1 .. -5 are five rows and qjs-1738-1, -2
# are one. Each arm's conform-test report is written beside the output.

from __future__ import annotations

import argparse
import fnmatch
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

# a bug row is (issue, engine); the directory a reproduction sits in names the
# engine, except for known-upstream, where the file name prefix does
ENGINE_BY_DIR = {
    "jsc": "javascriptcore",
    "qjs": "quickjs",
    "sm": "spidermonkey",
    "xs": "xs",
    "v8": "v8",
    "graal": "graaljs",
}
UPSTREAM_DIR = "known-upstream"
# a deviation upstream judged to be outside what the specification constrains;
# kept for provenance, counted as a row of no bug
INVALID_DIR = "invalid"
UPSTREAM_PREFIX_RE = re.compile(r"^(jsc|qjs|sm|xs|v8|graal)-")


class Row:
    """one row of the reported-bug table, with every program that witnesses it"""

    def __init__(self, engine: str) -> None:
        self.engine = engine
        self.programs: set[str] = set()

    def flagged(self, found: set[tuple[str, str]]) -> bool:
        return any((self.engine, p) in found for p in self.programs)


def row_of(path: Path) -> tuple[str, str]:
    """the row name and engine a reproduction belongs to"""
    directory = path.parent.name
    stem = path.stem
    if directory == UPSTREAM_DIR:
        match = UPSTREAM_PREFIX_RE.match(stem)
        if match is None:
            raise ValueError(f"cannot tell the engine of {path}")
        engine = ENGINE_BY_DIR[match.group(1)]
        name = stem
    else:
        if directory not in ENGINE_BY_DIR:
            raise ValueError(f"unknown engine directory: {path}")
        engine = ENGINE_BY_DIR[directory]
        name = f"{directory}-{stem}"
    return name, engine


def collect(bugs: Path, merges: list[str]) -> dict[str, Row]:
    rows: dict[str, Row] = {}
    for path in sorted(bugs.rglob("*.js")):
        if path.parent.name == INVALID_DIR:
            continue
        name, engine = row_of(path)
        # 2. the merged row is the name without its trailing index, taken from
        # the name itself rather than from the glob, which can spell anything
        if any(fnmatch.fnmatch(name, pattern) for pattern in merges):
            name = name.rsplit("-", 1)[0]
        row = rows.setdefault(name, Row(engine))
        if row.engine != engine:
            raise ValueError(f"{name} merges rows of different engines")
        row.programs.add(path.read_text(encoding="utf-8").strip())
    return rows


def flagged_pairs(report: Path) -> set[tuple[str, str]]:
    """(engine, program) pairs the report calls a bug"""
    data = json.loads(report.read_text(encoding="utf-8"))
    return {
        (engine, bug["program"].strip())
        for engine, result in data["engines"].items()
        for bug in result["bugs"]
    }


def stage(rows: dict[str, Row], work: Path) -> Path:
    """one flat directory of reproductions, since conform-test takes a dir"""
    scripts = work / "rows"
    scripts.mkdir(parents=True)
    for name, row in rows.items():
        for index, program in enumerate(sorted(row.programs)):
            suffix = "" if len(row.programs) == 1 else f"-{index}"
            (scripts / f"{name}{suffix}.js").write_text(program, encoding="utf-8")
    return scripts


def measure(args: argparse.Namespace, scripts: Path, work: Path) -> dict:
    """both arms of conform-test, which instruments unless told not to"""
    env = dict(os.environ)
    if args.home is not None:
        env["JAVA_OPTS"] = f"-Duser.home={args.home} {env.get('JAVA_OPTS', '')}".strip()
    reports = {}
    keep = args.out.parent if args.out else work
    for arm, instrumented in (("on", "true"), ("off", "false")):
        report = keep / f"arm-{arm}.json"
        # bin/esmeta is an sbt assembly launcher with no shebang, so exec it
        # through a shell the way an interactive caller does
        subprocess.run(
            [
                "sh",
                str(args.root / "bin" / "esmeta"),
                "conform-test",
                str(scripts),
                f"-conform-test:instrument={instrumented}",
                f"-conform-test:out={report}",
            ],
            check=True,
            env=env,
        )
        reports[arm] = flagged_pairs(report)
    return reports


def main() -> int:
    parser = argparse.ArgumentParser(
        description="count the bugs only the order-aware oracle reveals",
    )
    parser.add_argument("bugs", type=Path, help="directory of stored reproductions")
    parser.add_argument("--root", type=Path, default=Path.cwd(), help="esmeta checkout")
    parser.add_argument("--home", type=Path, help="fake home selecting frozen engines")
    parser.add_argument(
        "--on",
        type=Path,
        help="reuse an instrumented conform-test report instead of running one",
    )
    parser.add_argument(
        "--off",
        type=Path,
        help="reuse an uninstrumented conform-test report instead of running one",
    )
    parser.add_argument(
        "--merge",
        action="append",
        default=[],
        help="glob of names that are one row, e.g. 'qjs-1738-*' (repeatable)",
    )
    parser.add_argument("-o", "--out", type=Path, help="write the split as JSON")
    args = parser.parse_args()

    rows = collect(args.bugs, args.merge)
    if not rows:
        print(f"no reproductions under {args.bugs}", file=sys.stderr)
        return 1

    if args.on and args.off:
        reports = {"on": flagged_pairs(args.on), "off": flagged_pairs(args.off)}
    else:
        work = Path(tempfile.mkdtemp(prefix="order-oracle-"))
        try:
            reports = measure(args, stage(rows, work), work)
        finally:
            shutil.rmtree(work, ignore_errors=True)

    on = {n for n, r in rows.items() if r.flagged(reports["on"])}
    off = {n for n, r in rows.items() if r.flagged(reports["off"])}
    split = {
        "rows": len(rows),
        "instrumented": sorted(on),
        "uninstrumented": sorted(off),
        "order_only": sorted(on - off),
        "plain_only": sorted(off - on),
        "neither": sorted(set(rows) - on - off),
    }

    print(f"rows                  {split['rows']}")
    print(f"instrumented          {len(split['instrumented'])}")
    print(f"uninstrumented        {len(split['uninstrumented'])}")
    print(f"order-aware only      {len(split['order_only'])}")
    print(f"uninstrumented only   {len(split['plain_only'])}")
    print(f"neither (no oracle)   {len(split['neither'])}")
    for label in ("order_only", "plain_only", "neither"):
        if split[label]:
            print(f"\n{label}:")
            for name in split[label]:
                print(f"  {name}")
    if args.out:
        args.out.write_text(json.dumps(split, indent=2) + "\n", encoding="utf-8")
    return 0


if __name__ == "__main__":
    sys.exit(main())
