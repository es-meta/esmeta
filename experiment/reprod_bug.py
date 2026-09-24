#!/usr/bin/env python3

# python3 experiment/reprod_bug.py
#
# Bug rows each run reproduces, with and without the reducer. A failure on the
# frozen engines counts as the row experiment/conform/triage.json gives it;
# `fp:` and `new:` do not count. --draft adds unseen failures as `?` guesses,
# and the plot waits until none is left. data.sh unpack restores the conform
# logs; to redo them:
#
#   H=/private/tmp/frozen-home   # see ConformTest's EngineSpec.baseDir
#   for r in experiment/data/*fs-?; do
#     ./bin/esmeta reduce $r
#     JAVA_OPTS=-Duser.home=$H ./bin/esmeta conform-test $r/minimal -conform-test:out=$r/conform-minimal.json
#     JAVA_OPTS=-Duser.home=$H ./bin/esmeta conform-test $r/reduced/programs -conform-test:out=$r/conform-reduced.json
#   done
#   for r in experiment/data/solve-?; do
#     ./bin/esmeta reduce $r
#     JAVA_OPTS=-Duser.home=$H ./bin/esmeta conform-test $r/programs -conform-test:out=$r/conform-programs.json
#     JAVA_OPTS=-Duser.home=$H ./bin/esmeta conform-test $r/reduced/programs -conform-test:out=$r/conform-reduced.json
#   done

from __future__ import annotations

import argparse
import json
import os
import re
import sys
from pathlib import Path

import box
import venn_bug

# conform-test engine id -> row name prefix
PREFIX = {
    "v8": "v8",
    "javascriptcore": "jsc",
    "graaljs": "graal",
    "spidermonkey": "sm",
    "xs": "xs",
    "quickjs": "qjs",
}
WORD_RE = re.compile(r"[A-Za-z_$][\w$]*")
DECL_RE = re.compile(r"\b(?:var|let|const|function|class)\s+([A-Za-z_$][\w$]*)")
KEYWORDS = {"var", "let", "const", "function", "class", "new", "typeof", "undefined", "use", "strict"}
# box label -> (run prefix, conform log)
GROUPS = {
    box.label("solve"): ("solve", "conform-programs.json"),
    box.label("solve", True): ("solve", "conform-reduced.json"),
    box.label("0fs"): ("0fs", "conform-minimal.json"),
    box.label("0fs", True): ("0fs", "conform-reduced.json"),
    box.label("1fs"): ("1fs", "conform-minimal.json"),
    box.label("1fs", True): ("1fs", "conform-reduced.json"),
}


def failures(doc: dict) -> set[tuple[str, str]]:
    """(engine, program) per failure, divergences included"""
    out = set()
    for engine, result in doc["engines"].items():
        for bug in result["bugs"]:
            out.add((PREFIX[engine], bug["program"].strip()))
    for d in doc.get("divergences", []):
        for engine in d["odd"]:
            out.add((PREFIX[engine], d["program"].strip()))
    return out


def names(program: str) -> set[str]:
    """what a program refers to, less what it declares itself"""
    words = set(WORD_RE.findall(program))
    return words - set(DECL_RE.findall(program)) - KEYWORDS - {w for w in words if w.startswith("__res")}


def suggest(engine: str, program: str, stored: dict[str, str]) -> str:
    """the most specific row whose reproduction's names the program all uses"""
    # ponytail: word tokens, not a parse; a person confirms every guess
    used = names(program)
    fits = [
        (len(need), row)
        for row, text in stored.items()
        if row.startswith(engine + "-") and (need := names(text)) <= used
    ]
    return "?" + (max(fits)[1] if fits else "")


def parse_args() -> argparse.Namespace:
    home = Path(os.environ.get("ESMETA_HOME", Path(__file__).resolve().parents[1]))
    parser = argparse.ArgumentParser(description="Box plot of bugs reproduced per run.")
    parser.add_argument("-d", "--data", type=Path, default=home / "experiment" / "data")
    parser.add_argument("-t", "--table", type=Path, default=home / "experiment" / "bugs.json")
    parser.add_argument("-b", "--bugs-dir", type=Path, default=home / "bugs")
    parser.add_argument(
        "-l",
        "--ledger",
        type=Path,
        default=home / "experiment" / "conform" / "triage.json",
        help="engine -> failing program -> row name, fp:<family> or new:<name>",
    )
    parser.add_argument("--draft", action="store_true", help="add unseen failures as guesses and stop")
    parser.add_argument("-o", "--out", type=Path, default=home / "experiment" / "reprod-bug.pdf")
    parser.add_argument("--width", type=float, default=360.0, help="figure width in points")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    rows = {
        r["bug-id"]
        for r in venn_bug.load_rows(args.table)
        if r["status"].strip() not in venn_bug.NOT_A_BUG
    }
    stored = {
        p.stem: p.read_text(encoding="utf-8")
        for source in ("solver", "fuzzer")
        for p in (args.bugs_dir / source).glob("*.js")
        if p.stem in rows
    }
    ledger: dict[str, dict[str, str]] = (
        json.loads(args.ledger.read_text(encoding="utf-8")) if args.ledger.is_file() else {}
    )

    logs: dict[str, dict[str, set[tuple[str, str]]]] = {}
    for label, (config, log) in GROUPS.items():
        for run in box.config_runs(args.data, config):
            path = run / log
            if not path.is_file():
                raise FileNotFoundError(f"conform log not found: {path}; see the header")
            logs.setdefault(label, {})[f"{run.name}/{log}"] = failures(json.loads(path.read_text(encoding="utf-8")))

    seen = {f for by_run in logs.values() for fs in by_run.values() for f in fs}
    if args.draft:
        added = 0
        for engine, program in sorted(seen):
            if program not in ledger.setdefault(engine, {}):
                ledger[engine][program] = suggest(engine, program, stored)
                added += 1
        args.ledger.parent.mkdir(parents=True, exist_ok=True)
        args.ledger.write_text(json.dumps(ledger, indent=1, sort_keys=True, ensure_ascii=False) + "\n", encoding="utf-8")
        print(f"added {added} failure(s) to {args.ledger}; review every `?` entry")
        return 0
    verdict = {(e, p): v for e, by in ledger.items() for p, v in by.items()}
    pending = [f for f in seen if verdict.get(f, "?").startswith("?")]
    if pending:
        raise ValueError(f"{len(pending)} failure(s) not yet triaged in {args.ledger}; run with --draft and review")
    uncounted = ("fp:", "new:")
    stray = {v for v in verdict.values() if not v.startswith(uncounted)} - rows
    if stray:
        raise ValueError(f"ledger names rows the table does not list: {', '.join(sorted(stray))}")

    groups: dict[str, list[int]] = {}
    per_run: dict[str, list[str]] = {}
    for label, by_run in logs.items():
        counts = []
        for run, fs in by_run.items():
            found = sorted({verdict[f] for f in fs if not verdict[f].startswith(uncounted)})
            per_run[run] = found
            counts.append(len(found))
        groups[label] = counts

    box.render(args.out, groups, "# bugs reproduced", args.width, integer=True)
    json_out = args.out.with_suffix(".json")
    new = sorted({v for f, v in verdict.items() if f in seen and v.startswith("new:")})
    box.write_json(json_out, {"counts": groups, "runs": per_run, "rows": len(rows), "new": new})
    print(f"wrote:      {args.out}")
    print(f"wrote JSON: {json_out}")
    for label, counts in groups.items():
        print(f"{label}: {counts}")
    if new:
        print(f"not counted, not in the table: {', '.join(new)}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (FileNotFoundError, ValueError) as err:
        print(f"error: {err}", file=sys.stderr)
        raise SystemExit(1)
