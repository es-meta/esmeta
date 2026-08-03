#!/usr/bin/env python3
"""Benchmark and cross-check persistent Test262 payload workers.

The fixed fixture is replayed through the worker's line protocol.  Timing is
the host-observed RUN/RESULT round trip; worker startup and warmup are excluded.
When a candidate is supplied, every normalized verdict line must exactly match
the reference.  Normalization replaces only the elapsed field.
"""

from __future__ import annotations

import argparse
import math
import re
import select
import statistics
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path


SCRIPT_DIR = Path(__file__).resolve().parent
DEFAULT_FIXTURE = SCRIPT_DIR / "build" / "perf-fixture" / "fixture.tsv"
DEFAULT_REFERENCE = SCRIPT_DIR / "fvitree-worker"


@dataclass(frozen=True)
class FixtureEntry:
    global_index: int
    local_index: int
    payload_path: Path
    rel_name: str


@dataclass
class BenchmarkResult:
    label: str
    durations: list[float]
    per_test: dict[FixtureEntry, list[float]]
    verdicts: dict[FixtureEntry, str]


def parse_nonnegative(field: str, source: str, line_number: int) -> int:
    try:
        value = int(source, 10)
    except ValueError as error:
        raise ValueError(
            f"fixture line {line_number}: {field} is not an integer: {source!r}"
        ) from error
    if value < 0:
        raise ValueError(
            f"fixture line {line_number}: {field} must be nonnegative: {value}"
        )
    return value


def load_fixture(path: Path) -> list[FixtureEntry]:
    path = path.resolve()
    entries: list[FixtureEntry] = []
    for line_number, raw_line in enumerate(
        path.read_text(encoding="utf-8").splitlines(), start=1
    ):
        line = raw_line.rstrip("\r\n")
        if not line or line.startswith("#"):
            continue
        fields = line.split("\t", 3)
        if len(fields) != 4:
            raise ValueError(
                f"fixture line {line_number}: expected four tab-separated "
                "fields: globalIndex, localModuleIndex, payloadFile, relName"
            )
        global_source, local_source, payload_source, rel_name = fields
        global_index = parse_nonnegative(
            "globalIndex", global_source, line_number
        )
        local_index = parse_nonnegative(
            "localModuleIndex", local_source, line_number
        )
        if not rel_name:
            raise ValueError(f"fixture line {line_number}: empty relName")
        payload_path = (path.parent / payload_source).resolve()
        if not payload_path.is_file():
            raise ValueError(
                f"fixture line {line_number}: payload does not exist: "
                f"{payload_path}"
            )
        entries.append(
            FixtureEntry(global_index, local_index, payload_path, rel_name)
        )
    if not entries:
        raise ValueError(f"fixture has no entries: {path}")
    return entries


def read_line(
    process: subprocess.Popen[str], timeout: float, context: str
) -> str:
    assert process.stdout is not None
    readable, _, _ = select.select([process.stdout], [], [], timeout)
    if not readable:
        raise TimeoutError(f"{context} timed out after {timeout:.3f}s")
    line = process.stdout.readline()
    if line == "":
        raise RuntimeError(
            f"{context}: worker exited with status {process.poll()}"
        )
    return line.rstrip("\r\n")


def decode_hex(field: str, source: str) -> str:
    try:
        return bytes.fromhex(source).decode("utf-8")
    except (UnicodeDecodeError, ValueError) as error:
        raise RuntimeError(f"invalid {field} hex: {error}") from error


def normalize_verdict(line: str) -> str:
    match = re.fullmatch(
        r"(\S+\s+)([+-]?(?:\d+(?:\.\d*)?|\.\d+)s)(\s+.*)", line
    )
    if match is None:
        raise RuntimeError(f"malformed verdict line: {line!r}")
    return f"{match.group(1)}<elapsed>{match.group(3)}"


class Worker:
    def __init__(self, executable: Path, startup_timeout: float):
        self.executable = executable.resolve()
        if not self.executable.is_file():
            raise ValueError(f"worker does not exist: {self.executable}")
        self.process = subprocess.Popen(
            [str(self.executable)],
            cwd=SCRIPT_DIR,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=None,
            text=True,
            encoding="utf-8",
            errors="strict",
            bufsize=1,
        )
        try:
            greeting = read_line(self.process, startup_timeout, "worker startup")
            if greeting != "READY 1":
                raise RuntimeError(f"invalid worker greeting: {greeting!r}")
        except Exception:
            self.close(force=True)
            raise

    def run(
        self, entry: FixtureEntry, fuel: int, timeout: float
    ) -> tuple[float, str]:
        assert self.process.stdin is not None
        request = " ".join(
            (
                "RUN",
                str(fuel),
                str(entry.local_index),
                str(entry.global_index),
                "0",
                entry.rel_name.encode("utf-8").hex(),
                str(entry.payload_path).encode("utf-8").hex(),
            )
        )
        started = time.perf_counter()
        self.process.stdin.write(request + "\n")
        self.process.stdin.flush()
        response = read_line(
            self.process,
            timeout,
            f"RUN {entry.local_index:03d} {entry.rel_name}",
        )
        elapsed = time.perf_counter() - started
        kind, separator, payload = response.partition(" ")
        if not separator:
            raise RuntimeError(f"malformed worker response: {response!r}")
        message = decode_hex(kind, payload)
        if kind == "ERROR":
            raise RuntimeError(
                f"worker error for {entry.rel_name}: {message}"
            )
        if kind != "RESULT":
            raise RuntimeError(f"unexpected worker response: {response!r}")
        return elapsed, normalize_verdict(message)

    def close(self, force: bool = False) -> None:
        process = self.process
        if process.poll() is not None:
            return
        try:
            if not force and process.stdin is not None:
                process.stdin.write("QUIT\n")
                process.stdin.flush()
                bye = read_line(process, 5.0, "worker shutdown")
                if bye != "BYE":
                    raise RuntimeError(f"invalid worker shutdown: {bye!r}")
                process.wait(timeout=5.0)
                return
        finally:
            if process.poll() is None:
                process.terminate()
                try:
                    process.wait(timeout=5.0)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait()


def benchmark(
    label: str,
    executable: Path,
    entries: list[FixtureEntry],
    fuel: int,
    warmup: int,
    repeat: int,
    timeout: float,
    startup_timeout: float,
) -> BenchmarkResult:
    durations: list[float] = []
    per_test = {entry: [] for entry in entries}
    verdicts: dict[FixtureEntry, str] = {}
    worker = Worker(executable, startup_timeout)
    try:
        for iteration in range(warmup + repeat):
            measured = iteration >= warmup
            for entry in entries:
                elapsed, verdict = worker.run(entry, fuel, timeout)
                previous = verdicts.setdefault(entry, verdict)
                if verdict != previous:
                    raise RuntimeError(
                        f"{label} produced an unstable verdict for "
                        f"{entry.rel_name}:\n  {previous}\n  {verdict}"
                    )
                if measured:
                    durations.append(elapsed)
                    per_test[entry].append(elapsed)
    finally:
        worker.close()
    return BenchmarkResult(label, durations, per_test, verdicts)


def percentile(values: list[float], percent: float) -> float:
    ordered = sorted(values)
    if len(ordered) == 1:
        return ordered[0]
    position = (len(ordered) - 1) * percent
    lower = math.floor(position)
    upper = math.ceil(position)
    if lower == upper:
        return ordered[lower]
    fraction = position - lower
    return ordered[lower] + (ordered[upper] - ordered[lower]) * fraction


def print_result(result: BenchmarkResult, entries: list[FixtureEntry]) -> None:
    values = result.durations
    print(f"worker\t{result.label}")
    print(f"samples\t{len(values)}")
    print(f"total_seconds\t{sum(values):.6f}")
    print(f"mean_seconds\t{statistics.fmean(values):.6f}")
    print(f"p50_seconds\t{percentile(values, 0.50):.6f}")
    print(f"p95_seconds\t{percentile(values, 0.95):.6f}")
    print("per_test\tlocal\tglobal\tmean_seconds\tp50_seconds\tp95_seconds\tpath")
    for entry in entries:
        per_test = result.per_test[entry]
        print(
            "per_test\t"
            f"{entry.local_index:03d}\t{entry.global_index}\t"
            f"{statistics.fmean(per_test):.6f}\t"
            f"{percentile(per_test, 0.50):.6f}\t"
            f"{percentile(per_test, 0.95):.6f}\t{entry.rel_name}"
        )


def compare_verdicts(
    reference: BenchmarkResult,
    candidate: BenchmarkResult,
    entries: list[FixtureEntry],
) -> None:
    for entry in entries:
        expected = reference.verdicts[entry]
        actual = candidate.verdicts[entry]
        if actual != expected:
            raise RuntimeError(
                f"candidate verdict differs for {entry.rel_name}:\n"
                f"  reference: {expected}\n"
                f"  candidate: {actual}"
            )


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--fixture", type=Path, default=DEFAULT_FIXTURE)
    parser.add_argument("--reference", type=Path, default=DEFAULT_REFERENCE)
    parser.add_argument("--candidate", type=Path)
    parser.add_argument("--fuel", type=int, default=100_000_000)
    parser.add_argument("--warmup", type=int, default=1)
    parser.add_argument("--repeat", type=int, default=3)
    parser.add_argument("--timeout", type=float, default=900.0)
    parser.add_argument("--startup-timeout", type=float, default=30.0)
    args = parser.parse_args(argv)
    if args.fuel <= 0:
        parser.error("--fuel must be positive")
    if args.warmup < 0:
        parser.error("--warmup must be nonnegative")
    if args.repeat <= 0:
        parser.error("--repeat must be positive")
    if args.timeout <= 0 or args.startup_timeout <= 0:
        parser.error("timeouts must be positive")
    return args


def main(argv: list[str] | None = None) -> int:
    args = parse_args(sys.argv[1:] if argv is None else argv)
    entries = load_fixture(args.fixture)
    reference = benchmark(
        "reference",
        args.reference,
        entries,
        args.fuel,
        args.warmup,
        args.repeat,
        args.timeout,
        args.startup_timeout,
    )
    print_result(reference, entries)
    if args.candidate is not None:
        candidate = benchmark(
            "candidate",
            args.candidate,
            entries,
            args.fuel,
            args.warmup,
            args.repeat,
            args.timeout,
            args.startup_timeout,
        )
        compare_verdicts(reference, candidate, entries)
        print()
        print_result(candidate, entries)
        print(
            "speedup\t"
            f"{sum(reference.durations) / sum(candidate.durations):.6f}"
        )
        print("verdicts\tIDENTICAL")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError, TimeoutError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(1)
