#!/usr/bin/env python3
"""Compile generic/direct representative shards with comparable accounting."""

from __future__ import annotations

import argparse
import pathlib
import resource
import subprocess
import time


def compile_source(label: str, source: pathlib.Path, output: pathlib.Path, coqc: list[str]):
    output.parent.mkdir(parents=True, exist_ok=True)
    before = resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss
    started = time.monotonic()
    completed = subprocess.run(
        [*coqc, "-noglob", "-o", str(output), str(source)],
        text=True,
        capture_output=True,
    )
    wall = time.monotonic() - started
    after = resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss
    if completed.returncode:
        print(completed.stdout, end="")
        print(completed.stderr, end="")
        raise SystemExit(f"{label} representative compile failed")
    data = source.read_bytes()
    return {
        "label": label,
        "source_bytes": len(data),
        "source_lines": data.count(b"\n"),
        "wall_seconds": wall,
        # ru_maxrss is KiB on Linux and bytes on macOS.  Ratios are unit-free.
        "peak_rss": max(before, after),
        "vo_bytes": output.stat().st_size,
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("generic", type=pathlib.Path)
    parser.add_argument("direct", type=pathlib.Path)
    parser.add_argument("--output-dir", type=pathlib.Path, required=True)
    parser.add_argument("--coqc", nargs="+", required=True)
    args = parser.parse_args()
    for source in (args.generic, args.direct):
        if not source.is_file():
            raise SystemExit(f"missing representative source: {source}")
    generic = compile_source(
        "generic", args.generic, args.output_dir / "generic.vo", args.coqc
    )
    direct = compile_source(
        "direct", args.direct, args.output_dir / "direct.vo", args.coqc
    )
    for result in (generic, direct):
        print(
            "{label}: source={source_bytes}B/{source_lines} lines "
            "wall={wall_seconds:.3f}s peak-rss={peak_rss} vo={vo_bytes}B".format(
                **result
            )
        )
    wall_ratio = direct["wall_seconds"] / max(generic["wall_seconds"], 1e-9)
    rss_ratio = direct["peak_rss"] / max(generic["peak_rss"], 1)
    print(f"direct/generic: wall={wall_ratio:.2f}x peak-rss={rss_ratio:.2f}x")
    if wall_ratio > 2.0 or rss_ratio > 2.0:
        print("GATE: over 2x; full direct compile remains opt-in")
        return 2
    print("GATE: representative shard is within the 2x resource threshold")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
