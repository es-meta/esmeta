#!/usr/bin/env python3
"""Score one conform-test run of the interaction oracle on the fixed corpus.

Compares a conform JSON with the baseline of experiment/data/solve-1 and prints
one summary: known defects (strict, by the triage ledger, as check-oracle.sh),
defects matched per original program (robust to changed interaction texts),
test counts, per-engine failures, timings, and the interaction statistics of
the run log.

  python3 experiment/oracle-eval.py RUN.json [--log RUN.log] [--time TIME.txt]
      [--ref REF.json]          # another run to diff against (e.g. greedy)
      [--skip jsc]              # engines missing locally; their rows are left out
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from collections import defaultdict
from pathlib import Path

HOME = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(HOME / "experiment"))
from rq1_table import Defects, PREFIX, ledger_key, UNCOUNTED  # noqa: E402

CORPUS = HOME / "experiment" / "data" / "solve-1"


def stems() -> tuple[dict[str, str], list[tuple[str, str]]]:
    """original program text -> stem, and (injected text, test name) pairs"""
    originals = {
        p.read_text(encoding="utf-8").strip(): p.stem
        for p in (CORPUS / "programs").glob("*.js")
    }
    injected = [
        (p.read_text(encoding="utf-8"), p.name)
        for p in (CORPUS / "injected").glob("*.js")
    ]
    return originals, injected


def test_of(bug: dict, originals, injected) -> list[str]:
    """test names of one bug: recorded ones, or recovered from the texts"""
    if bug.get("names"):
        return bug["names"]
    program = bug["program"].strip()
    if program in originals:
        return [originals[program] + ".js"]
    body = bug["program"].strip()
    return [name for text, name in injected if body in text]


def kind_stem(name: str) -> tuple[str, str]:
    stem = name.split(".")[0]
    return stem, ("interaction" if ".interaction" in name else "original")


def per_program(doc: dict, defects: Defects, originals, injected):
    """(engine, stem, kind) -> rows, and failures the ledger does not know"""
    rows = defaultdict(set)
    unknown = set()
    for engine, result in doc["engines"].items():
        e = PREFIX[engine]
        for bug in result["bugs"]:
            verdicts = defects.ledger.get(e, {}).get(ledger_key(bug["program"]), "?")
            verdicts = [verdicts] if isinstance(verdicts, str) else verdicts
            for name in test_of(bug, originals, injected) or ["?"]:
                key = (e, *kind_stem(name))
                for v in verdicts:
                    if v.startswith("?"):
                        unknown.add(key)
                    elif not v.startswith(UNCOUNTED):
                        rows[key].add(v)
    return rows, unknown


def summary_lines(path: Path) -> dict[str, str]:
    out = {}
    if path and path.is_file():
        text = path.read_text(encoding="utf-8", errors="replace").replace("\r", "\n")
        for line in text.splitlines():
            m = re.match(r"(Interaction [^:]+|Injected .*|.*: \d+/\d+ bugs)(:\s*(.*))?$", line.strip())
            if m and line.strip().startswith(("Interaction", "Injected")):
                out[m.group(1)] = (m.group(3) or "").strip()
            m2 = re.match(r"^(\w+): (\d+)/(\d+) bugs$", line.strip())
            if m2:
                out[f"bugs {m2.group(1)}"] = f"{m2.group(2)}/{m2.group(3)}"
    return out


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("run", type=Path)
    ap.add_argument("--log", type=Path)
    ap.add_argument("--time", type=Path)
    ap.add_argument("--ref", type=Path, help="reference run (e.g. the greedy oracle)")
    ap.add_argument("--skip", default="jsc", help="comma-separated engine prefixes")
    args = ap.parse_args()
    skip = {s for s in args.skip.split(",") if s}

    baseline = json.loads((CORPUS / "baseline.json").read_text())
    previous = {r for r in baseline["known_rows"] if r.split("-")[0] not in skip}
    originals, injected = stems()

    defects = Defects()
    doc = json.loads(args.run.read_text())
    found = {r for r in defects.found(args.run) if r.split("-")[0] not in skip}
    rows, unknown = per_program(doc, defects, originals, injected)

    print("=" * 66)
    print(f"Run: {args.run}")
    if args.time and args.time.is_file():
        print("Time:", args.time.read_text().strip())
    info = summary_lines(args.log)
    for k, v in info.items():
        print(f"  {k}: {v}")
    print("-" * 66)
    print(f"Known defects (strict ledger, excluding {sorted(skip)}): "
          f"{len(found)} (baseline: {len(previous)})")
    print("  No longer matched:", ", ".join(sorted(previous - found)) or "(none)")
    print("  Newly matched:", ", ".join(sorted(found - previous)) or "(none)")
    pend = {p for p in defects.pending if p[0] not in skip}
    print(f"  Unclassified engine/program pairs: {len(pend)}")

    if args.ref:
        ref_doc = json.loads(args.ref.read_text())
        ref_rows, _ = per_program(ref_doc, Defects(), originals, injected)
        # rows the reference reaches through interaction tests of a stem
        lost = defaultdict(set)
        for (e, stem, kind), rs in ref_rows.items():
            if e in skip or kind != "interaction":
                continue
            if (e, stem, "interaction") in rows and rows[(e, stem, "interaction")] >= rs:
                continue
            if (e, stem, "interaction") in unknown:
                lost["fails, text unknown to ledger (triage)"] |= {f"{r}@{stem}" for r in rs}
            else:
                lost["no failing interaction test"] |= {f"{r}@{stem}" for r in rs}
        print("-" * 66)
        print("Interaction rows of the reference, per original program:")
        for why, items in lost.items():
            print(f"  {why}: {len(items)}")
            for it in sorted(items)[:30]:
                print("    ", it)
        if not lost:
            print("  all reproduced by the same programs")

    print("-" * 66)
    kinds = defaultdict(int)
    for e, stem, kind in unknown:
        if e not in skip:
            kinds[kind] += 1
    print("Failures unknown to the ledger, by test kind:", dict(kinds) or "{}")
    print("Engine failures (bugs):",
          {PREFIX[e]: len(r["bugs"]) for e, r in doc["engines"].items()})
    print("=" * 66)


if __name__ == "__main__":
    main()
