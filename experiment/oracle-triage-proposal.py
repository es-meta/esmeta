#!/usr/bin/env python3
"""Propose triage verdicts for failures whose test text the ledger lacks.

A changed oracle rewrites the interaction tests, so the ledger, keyed by test
text, no longer knows their failures.  For each such failure of RUN, this
script looks up the verdict the ledger gives the same engine on the same
original program and test kind in REF, and compares the two symptoms.

It never edits the ledger.  It writes
  PREFIX.md    one block per proposal, for a person to review
  PREFIX.json  {engine: {program: verdict}} of the proposals, to merge after
               review (e.g. keep only the "same" ones)

  python3 experiment/oracle-triage-proposal.py RUN.json --ref REF.json \\
      --out logs/oracle-check/triage-proposal
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import re
import sys
from collections import Counter
from pathlib import Path

HOME = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(HOME / "experiment"))
from rq1_table import Defects, PREFIX, ledger_key, UNCOUNTED  # noqa: E402

spec = importlib.util.spec_from_file_location("ev", HOME / "experiment" / "oracle-eval.py")
ev = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ev)


def symptom(failure: dict) -> str:
    """category and message with numbers and logged entries blanked"""
    msg = failure.get("stderr") or failure.get("stdout") or ""
    msg = re.sub(r"\[.*?\]", "[...]", msg, flags=re.S)
    msg = re.sub(r"\d+", "N", msg)
    return f"{failure.get('category')}: {msg.strip()[:160]}"


def index(doc: dict, ledger: dict, originals, injected) -> dict:
    """(engine, stem, kind) -> list of (bug, verdicts)"""
    out: dict = {}
    for engine, result in doc["engines"].items():
        e = PREFIX[engine]
        for bug in result["bugs"]:
            v = ledger.get(e, {}).get(ledger_key(bug["program"]), "?")
            verdicts = [v] if isinstance(v, str) else v
            for name in ev.test_of(bug, originals, injected) or []:
                out.setdefault((e, *ev.kind_stem(name)), []).append((bug, verdicts))
    return out


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("run", type=Path)
    ap.add_argument("--ref", type=Path, required=True)
    ap.add_argument("--out", type=Path, default=HOME / "logs/oracle-check/triage-proposal")
    args = ap.parse_args()

    ledger = Defects().ledger
    originals, injected = ev.stems()
    ref = index(json.loads(args.ref.read_text()), ledger, originals, injected)
    run = index(json.loads(args.run.read_text()), ledger, originals, injected)

    proposals: dict[str, dict[str, object]] = {}
    blocks, stats = [], Counter()
    for key, entries in sorted(run.items()):
        engine, stem, kind = key
        for bug, verdicts in entries:
            if not any(v.startswith("?") for v in verdicts):
                continue  # the ledger knows this text
            stats["unknown"] += 1
            known = [
                (b, vs) for b, vs in ref.get(key, [])
                if not any(v.startswith("?") for v in vs)
            ]
            if not known:
                stats["no reference verdict"] += 1
                blocks.append((engine, stem, kind, None, "no reference verdict", bug, None))
                continue
            rbug, rverdicts = known[0]
            s_new, s_ref = symptom(bug["failures"][0]), symptom(rbug["failures"][0])
            match = "same" if s_new == s_ref else (
                "similar" if s_new.split(":")[0] == s_ref.split(":")[0] else "different"
            )
            stats[match] += 1
            proposed = rverdicts[0] if len(rverdicts) == 1 else rverdicts
            proposals.setdefault(engine, {})[ledger_key(bug["program"])] = proposed
            blocks.append((engine, stem, kind, proposed, match, bug, rbug))

    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.with_suffix(".json").write_text(
        json.dumps(proposals, indent=1, sort_keys=True, ensure_ascii=False) + "\n"
    )
    counted = Counter(
        (match, v if isinstance(v, str) else ",".join(v))
        for e, s, k, v, match, *_ in blocks if v is not None
    )
    lines = [
        f"# Triage proposals for {args.run.name} (reference: {args.ref.name})",
        "",
        "| | count |", "|---|---|",
        *[f"| {k} | {v} |" for k, v in stats.most_common()],
        "",
        "## Proposed verdicts by symptom match",
        "",
        "| match | verdict | failures |", "|---|---|---|",
        *[f"| {m} | {v} | {c} |" for (m, v), c in sorted(counted.items())],
        "",
        "## Details",
    ]
    order = {"different": 0, "similar": 1, "no reference verdict": 2, "same": 3}
    for engine, stem, kind, verdict, match, bug, rbug in sorted(blocks, key=lambda b: (order[b[4]], b[0], b[1])):
        lines += [
            "",
            f"### {engine} / program {stem} ({kind}) -> {verdict} [{match}]",
            "",
            f"- new: `{symptom(bug['failures'][0])}`",
        ]
        if rbug is not None:
            lines.append(f"- ref: `{symptom(rbug['failures'][0])}`")
    args.out.with_suffix(".md").write_text("\n".join(lines) + "\n")
    print(dict(stats))
    print(f"wrote {args.out.with_suffix('.md')} and {args.out.with_suffix('.json')}")


if __name__ == "__main__":
    main()
