#!/usr/bin/env python3
import argparse
import os
import re
import shlex
import subprocess
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Optional, List, Tuple

FILENAME_RE = re.compile(
    r"^(?P<branch>\d+)-(?P<truth>true|false)\.json$", re.IGNORECASE
)
COVERED_RE = re.compile(r"Covered\s+.+?\s+with\s+(?P<iters>\d+)\s+iters", re.IGNORECASE)
FAILED_RE = re.compile(r"\[ESMeta v[^\]]+\]\s+Failed to cover", re.IGNORECASE)
ESMETA_HOME = (
    Path(os.getenv("ESMETA_HOME") or Path(__file__).resolve().parent.parent)
    .expanduser()
    .resolve()
)


@dataclass
class RunResult:
    file: str
    branch: int
    truth: str
    target: str
    run_idx: int
    status: str  # "Covered" | "Timeout" | "Unknown"
    iters: Optional[int]
    last_line: str


def last_nonempty_line(text: str) -> str:
    lines = [ln.rstrip("\n") for ln in text.splitlines()]
    for ln in reversed(lines):
        if ln.strip():
            return ln.strip()
    return ""


def run_once(
    esmeta: str,
    duration: int,
    json_path: Path,
    target: str,
    branch: int,
    truth: str,
    run_idx: int,
    verbose: bool,
) -> RunResult:
    esmeta_argv = shlex.split(esmeta)
    argv = esmeta_argv + [
        "mutate",
        "-mutate:mutator=TargetMutator",
        f"-mutate:target={target}",
        f"-mutate:duration={duration}",
        str(json_path),
        "-silent",
    ]

    def _run(argv_list: list[str]) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            argv_list,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            check=False,
        )

    try:
        proc = _run(argv)
    except OSError:
        shell = os.environ.get("SHELL", "/bin/bash")
        cmd_str = " ".join(shlex.quote(x) for x in argv)
        proc = _run([shell, "-lc", cmd_str])

    out = proc.stdout or ""
    if verbose and out:
        print(out, end="" if out.endswith("\n") else "\n")

    ll = last_nonempty_line(out)

    status, iters = "Unknown", None
    if FAILED_RE.search(ll):
        status, iters = "Timeout", None
    else:
        m = COVERED_RE.search(ll)
        if m:
            status, iters = "Covered", int(m.group("iters"))

    return RunResult(
        file=json_path.name,
        branch=branch,
        truth=truth,
        target=target,
        run_idx=run_idx,
        status=status,
        iters=iters,
        last_line=ll,
    )


def run_file(
    esmeta: str,
    duration: int,
    runs: int,
    p: Path,
    branch: int,
    truth: str,
    verbose: bool,
) -> List[RunResult]:
    target = f"Branch[{branch}]:{truth}"
    results: List[RunResult] = []
    for i in range(1, runs + 1):
        rr = run_once(
            esmeta,
            duration,
            p,
            target,
            branch=branch,
            truth=truth,
            run_idx=i,
            verbose=verbose,
        )
        results.append(rr)
        if rr.status == "Timeout":
            break
    return results


def format_iters_list(iters: List[int]) -> str:
    return "[ " + ", ".join(str(x) for x in sorted(iters)) + " ]"


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--dir", default="./samples")
    ap.add_argument("--runs", type=int, default=10)
    ap.add_argument("--duration", type=int, default=300)
    ap.add_argument("--esmeta", default=str(ESMETA_HOME / "bin" / "esmeta"))
    ap.add_argument("--out", default="sample-results")
    ap.add_argument("--verbose", action="store_true")
    args = ap.parse_args()

    d = Path(args.dir)
    if not d.exists() or not d.is_dir():
        raise SystemExit(f"Directory not found: {d}")

    files: List[Tuple[Path, int, str]] = []
    for p in sorted(d.iterdir()):
        if not p.is_file():
            continue
        m = FILENAME_RE.match(p.name)
        if not m:
            continue
        branch = int(m.group("branch"))
        truth = "T" if m.group("truth").lower() == "true" else "F"
        files.append((p, branch, truth))

    if not files:
        raise SystemExit(f"No matching file in {d}")

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)

    with out_path.open("w", encoding="utf-8") as f:
        for i, (p, branch, truth) in enumerate(files):
            id, condStr = p.stem.split("-", 1)
            label = f"Branch[{id}]:{'T' if condStr.lower() == 'true' else 'F'}"

            results = run_file(
                args.esmeta,
                args.duration,
                args.runs,
                p,
                branch,
                truth,
                verbose=args.verbose,
            )

            saw_timeout = any(r.status == "Timeout" for r in results)
            if saw_timeout:
                f.write(f'{label}: "TIMEOUT"\n')
                f.flush()
            else:
                iters_list = [
                    r.iters
                    for r in results
                    if r.status == "Covered" and r.iters is not None
                ]
                if iters_list:
                    sorted_iters_list = format_iters_list([int(x) for x in iters_list])
                    f.write(f"{label}: {sorted_iters_list}\n")
                else:
                    f.write(f'{label}: "UNKNOWN"\n')
                f.flush()

            msg = f"[{i}] {label}: "
            if saw_timeout:
                msg += "TIMEOUT"
            else:
                msg += f"{len([r for r in results if r.status == 'Covered'])} covered"
            print(msg)

    print(f"\nDone. Wrote: {out_path.resolve()}")


if __name__ == "__main__":
    raise SystemExit(main())
