#!/usr/bin/env python3

# python3 experiment/rq2_table.py [--draft] [--tex ../synth262-paper/fse27/numbers/rq2_table.tex]
#
# The numbers of RQ2: the full solver against the runs that drop one extension,
# structural types for properties (-solve:no-shape) and built-in call templates
# (-solve:no-template). Five-run means of solving time, branch coverage,
# programs, and defects with final-state assertions only and with the
# interaction-based oracle. Defects count as in rq1_table.py.

from __future__ import annotations

import argparse
import json
import re
import sys

from pathlib import Path

from results import INTERACTION, PLAIN, covered, instrumented, mann_whitney, tests, Defects, Tex, cell, conform_time, generation_time, mean, programs, runs, solver_summary, table, universe

SETTINGS = {"solve": "Synth262", "noshape": "w/o structural", "notemplate": "w/o template"}


def main() -> int:
    parser = argparse.ArgumentParser(description="the numbers of RQ2")
    parser.add_argument("--draft", action="store_true", help="add untriaged failures to the ledger")
    parser.add_argument("--tex", type=Path, help="write the paper's number macros here")
    parser.add_argument("--coverage-only", action="store_true", help="update generation metrics only")
    args = parser.parse_args()
    defects = None if args.coverage_only else Defects()

    counted = []
    for prefix, label in SETTINGS.items():
        rs = runs(prefix)
        s = [{**solver_summary(r), "wall_time": generation_time(r)["real"]} for r in rs]
        plain = defects.group([r / PLAIN["solve"] for r in rs]) if defects else None
        aware = defects.group([r / INTERACTION for r in rs]) if defects else None
        counted.append((label, s, [programs(r / "programs") for r in rs], plain, aware))
    rows = [
        [label, cell(mean([x["wall_time"] for x in s]), time=True), cell(mean([x["coverage"] for x in s])),
         cell(mean(ps))] + ([] if args.coverage_only else
         [cell(mean(plain[0]), plain[1]), cell(mean(aware[0]), aware[1])])
        for label, s, ps, plain, aware in counted
    ]
    table("RQ2: run means", ["", "time", "coverage", "programs"] +
          ([] if args.coverage_only else ["w/o interaction-based", "w/ interaction-based"]), rows)

    # Synth262's own row comes from rq1_table.py
    tex = Tex("rq2_table.py")
    for (label, s, ps, plain, aware), name in zip(counted[1:], ("NoShape", "NoTemplate")):
        tex.seconds(f"{name}Time", mean([x["wall_time"] for x in s]))
        tex.mean(f"Num{name}CoveredAll", mean([x["coverage"] for x in s]))
        tex.mean(f"Num{name}Programs", mean(ps))
        if not args.coverage_only:
            prefix = "noshape" if name == "NoShape" else "notemplate"
            tex.seconds(f"{name}OracleTime", mean([conform_time(r, "injectionMs") for r in runs(prefix)]))
            tex.mean(f"NumDefects{name}PlainMean", mean(plain[0]), plain[1])
            tex.mean(f"NumDefects{name}OracleMean", mean(aware[0]), aware[1])
            tex.mean(f"Num{name}Tests", mean([tests(r) for r in runs(prefix)]))
    targets = universe(runs("solve")[0])
    if any(universe(r) != targets for prefix in SETTINGS for r in runs(prefix)):
        raise ValueError("ablation runs use different target universes")
    tex.count("NumSynthTemplates", mean([s["templates"] for s in counted[0][1]]))
    # the final-state-only injector, timed separately in each run (timing-final-state.json)
    for prefix, name in (("solve", "Plain"), ("noshape", "NoShapePlain"), ("notemplate", "NoTemplatePlain"), ("fuzz", "FuzzPlain")):
        files = [r / "timing-final-state.json" for r in runs(prefix)]
        values = [json.loads(f.read_text())["injectionMs"] if f.is_file() else None for f in files]
        tex.seconds(f"{name}OracleTime", None if None in values else mean(values) / 1000)
    tex.count("NumNoShapeTemplates", mean([s["templates"] for s in counted[1][1]]))
    # target outcomes that explain each loss (Synth262, NoShape, NoTemplate)
    used = {"Synth": ("fail-verify", "fail-reify", "timeout"), "NoShape": ("fail-verify", "timeout"),
            "NoTemplate": ("fail-reify",)}
    for (label, s, *_), name in zip(counted, ("Synth", "NoShape", "NoTemplate")):
        for status in used[name]:
            macro = "".join(w.title() for w in status.split("-"))
            tex.mean(f"Num{name}{macro}", mean([x.get(status, 0) for x in s]))
    # coverage of each variant against Synth262: every run against every run,
    # and the sides one union covers but the other does not
    full = [covered(r / "branch-coverage.json") for r in runs("solve")]
    union = set().union(*full)
    for prefix, name in (("noshape", "NoShape"), ("notemplate", "NoTemplate")):
        variant = [covered(r / "branch-coverage.json") for r in runs(prefix)]
        tex.pct(f"Pct{name}Drop", mean([len(x) for x in full]) - mean([len(x) for x in variant]),
                mean([len(x) for x in full]))
        other = set().union(*variant)
        tex.count(f"Num{name}Lost", len(union - other))
        tex.count(f"Num{name}Gained", len(other - union))
    if not args.coverage_only:
        # defects only the instrumented programs reproduce, and the programs the
        # interaction-based oracle instruments
        for prefix, name in (("solve", "Synth"), ("fuzz", "Fuzz")):
            rs, plain_log = runs(prefix), PLAIN["fuzz" if prefix == "fuzz" else "solve"]
            only = [len((defects.found(r / INTERACTION) or set()) - (defects.found(r / plain_log) or set())) for r in rs]
            tex.mean(f"Num{name}InteractionOnly", mean(only))
        # final-state tests: the programs the injector keeps
        for prefix, name in (("solve", "Synth"), ("noshape", "NoShape"), ("notemplate", "NoTemplate"), ("fuzz", "Fuzz")):
            tex.mean(f"Num{name}PlainTests", mean([instrumented(r) for r in runs(prefix)]))
        source = mean([programs(r / "programs") for r in runs("solve")])
        kept = mean([instrumented(r) for r in runs("solve")])
        if None not in (source, kept):
            tex.pct("PctSynthSkipped", source - kept, source)
        # rows the oracle adds to the fuzzer in any run, and whether Synth262 finds them too
        fuzz_plain = set().union(*(defects.found(r / PLAIN["fuzz"]) or set() for r in runs("fuzz")))
        fuzz_aware = set().union(*(defects.found(r / INTERACTION) or set() for r in runs("fuzz")))
        synth_all = set().union(*(defects.found(r / log) or set() for r in runs("solve") for log in (PLAIN["solve"], INTERACTION)))
        tex.count("NumFuzzOracleNew", len(fuzz_aware - fuzz_plain))
        if (fuzz_aware - fuzz_plain) - synth_all:
            print(f"  fuzzer rows only the oracle adds and Synth262 misses: {sorted((fuzz_aware - fuzz_plain) - synth_all)}")
        # opaque candidates the tagging run reports, which drive the oracle cost
        for prefix, name in (("solve", "Synth"), ("fuzz", "Fuzz")):
            sites = []
            for r in runs(prefix):
                found = re.search(r"sites probed[^:]*: (\d+)", (r / "summary-interaction").read_text())
                sites.append(int(found.group(1)) if found else None)
            tex.mean(f"Num{name}Opaque", mean(sites))
    if not args.coverage_only:
        tex.mean("NumFuzzTests", mean([tests(r) for r in runs("fuzz")]))
    tex.write(args.tex)

    if not args.coverage_only:
        defects.report(args.draft)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (FileNotFoundError, ValueError) as err:
        print(f"error: {err}", file=sys.stderr)
        raise SystemExit(1)
