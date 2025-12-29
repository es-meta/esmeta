#!/usr/bin/env python3
import argparse
import csv
import os
import re
import shlex
import subprocess
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Optional, List, Tuple

FILENAME_RE = re.compile(r"^(?P<branch>\d+)-(?P<truth>true|false)\.json$", re.IGNORECASE)
COVERED_RE = re.compile(r"Covered\s+.+?\s+with\s+(?P<iters>\d+)\s+iters", re.IGNORECASE)
FAILED_RE = re.compile(r"\[ESMeta v[^\]]+\]\s+Failed to cover", re.IGNORECASE)

@dataclass
class RunResult:
    file: str
    branch: int
    truth: str
    target: str
    run_idx: int
    status: str
    iters: Optional[int]
    returncode: int
    elapsed_sec: float
    last_line: str

def last_nonempty_line(text: str) -> str:
    lines = [ln.rstrip("\n") for ln in text.splitlines()]
    for ln in reversed(lines):
        if ln.strip():
            return ln.strip()
    return ""

def run_once(esmeta: str, duration: int, json_path: Path, target: str,
            branch: int, truth: str, run_idx: int) -> RunResult:
    esmeta_argv = shlex.split(esmeta)  # "bash .../esmeta" 허용
    argv = esmeta_argv + [
        "mutate",
        "-mutate:mutator=TargetMutator",
        f"-mutate:target={target}",
        f"-mutate:duration={duration}",
        str(json_path),
        "-silent"
    ]

    t0 = time.time()

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
        # shebang 없는 스크립트 등: bash로 fallback
        shell = os.environ.get("SHELL", "/bin/bash")
        cmd_str = " ".join(shlex.quote(x) for x in argv)
        proc = _run([shell, "-lc", cmd_str])

    elapsed = time.time() - t0
    out = proc.stdout or ""
    print(out)
    ll = last_nonempty_line(out)

    status = "Unknown"
    iters = None
    if FAILED_RE.search(ll):
        status = "Timeout"
    else:
        m = COVERED_RE.search(ll)
        if m:
            status = "Covered"
            iters = int(m.group("iters"))

    return RunResult(
        file=json_path.name,
        branch=branch,
        truth=truth,
        target=target,
        run_idx=run_idx,
        status=status,
        iters=iters,
        returncode=proc.returncode,
        elapsed_sec=elapsed,
        last_line=ll,
    )

def run_file(esmeta: str, duration: int, runs: int, p: Path, branch: int, truth: str) -> List[RunResult]:
    """한 파일을 runs번(Timeout 나오면 즉시 중단) 돌리고 결과 리스트 반환"""
    target = f"Branch[{branch}]:{truth}"
    results: List[RunResult] = []
    for i in range(1, runs + 1):
        rr = run_once(esmeta, duration, p, target, branch=branch, truth=truth, run_idx=i)
        results.append(rr)
        if rr.status == "Timeout":
            break
    return results

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dir", default="./experiments/samples")
    ap.add_argument("--runs", type=int, default=10)
    ap.add_argument("--duration", type=int, default=300)
    ap.add_argument("--esmeta", default="/home/d01c2/esmeta/bin/esmeta")
    ap.add_argument("--out", default="dump.csv")
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
        raise SystemExit(f"No matching files like '286-true.json' in: {d}")

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)

    with out_path.open("w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow([
            "file", "branch", "truth", "target",
            "run_idx", "status", "iters",
            "returncode", "elapsed_sec", "last_line"
        ])

        total = 0
        # 파일 단위 순차 실행
        for p, branch, truth in files:
            results = run_file(args.esmeta, args.duration, args.runs, p, branch, truth)

            for rr in results:
                w.writerow([
                    rr.file, rr.branch, rr.truth, rr.target,
                    rr.run_idx, rr.status, ("" if rr.iters is None else rr.iters),
                    rr.returncode, f"{rr.elapsed_sec:.3f}", rr.last_line
                ])
                f.flush()

                total += 1
                print(f"[{total}] {rr.file} run {rr.run_idx} -> {rr.status}"
                      + (f" (iters={rr.iters})" if rr.iters is not None else ""))

            if results and results[-1].status == "Timeout":
                print(f"  -> Timeout detected; stopped further runs for {results[-1].file}")

    print(f"\nDone. Wrote: {out_path.resolve()}")

if __name__ == "__main__":
    main()