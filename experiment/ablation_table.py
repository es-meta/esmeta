#!/usr/bin/env python3

# python3 experiment/ablation_table.py
#
# The ablation table: the full solver against the runs that drop one part of
# input synthesis, the object structure in the type domain (-solve:no-shape)
# and the call templates derived from the specification (-solve:no-template).
# Five-run means of covered target sides, timeouts, programs, mean program
# length in characters, and solving time, read from each run's summary and
# programs. Dropping the amplifier changes only the bug counts, which
# reprod_bug.py gives.

from __future__ import annotations

import argparse
import os
import re
import statistics
from pathlib import Path

import box

SETTINGS = {"solve": "Synth262", "noshape": "- structure", "notemplate": "- template"}


def seconds(text: str) -> int:
    parts = [int(p) for p in text.split(":")]
    return sum(p * 60**i for i, p in enumerate(reversed(parts)))


def row(run: Path) -> dict[str, float]:
    summary = run.joinpath("summary").read_text()
    programs = list(run.joinpath("programs").glob("*.js"))
    return {
        "covered": int(re.search(r"Reached targets: (\d+)/", summary).group(1)),
        "timeout": int(re.search(r"timeout\s+(\d+)", summary).group(1)),
        "programs": len(programs),
        "length": statistics.mean(len(p.read_text()) for p in programs),
        "time": seconds(re.search(r"Solving: (\S+)", summary).group(1)),
    }


def main() -> int:
    home = Path(os.environ.get("ESMETA_HOME", Path(__file__).resolve().parents[1]))
    p = argparse.ArgumentParser(description="Five-run means per ablation setting.")
    p.add_argument("-d", "--data", type=Path, default=home / "experiment" / "data")
    args = p.parse_args()
    print(f"{'setting':12}{'covered':>9}{'timeout':>9}{'programs':>10}{'length':>8}{'time':>8}")
    for prefix, label in SETTINGS.items():
        rows = [row(r) for r in box.config_runs(args.data, prefix)]
        mean = {k: statistics.mean(r[k] for r in rows) for k in rows[0]}
        t = int(round(mean["time"]))
        print(f"{label:12}{mean['covered']:9.0f}{mean['timeout']:9.0f}{mean['programs']:10.0f}"
              f"{mean['length']:8.0f}{t // 60:5d}:{t % 60:02d}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
