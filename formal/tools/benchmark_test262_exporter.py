#!/usr/bin/env python3
"""Benchmark parallel Test262 export while proving byte-for-byte equivalence."""

from __future__ import annotations

import argparse
import csv
import filecmp
import importlib.util
import shutil
import sys
import tempfile
from collections import Counter
from dataclasses import dataclass
from pathlib import Path


FORMAL = Path(__file__).resolve().parents[1]
ROOT = FORMAL.parent
RUNNER_PATH = FORMAL / "run-test262-full.py"


def _load_runner():
    spec = importlib.util.spec_from_file_location(
        "run_test262_full_benchmark", RUNNER_PATH
    )
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot import runner from {RUNNER_PATH}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


runner = _load_runner()


@dataclass(frozen=True)
class ExportSnapshot:
    root: Path
    manifest: Path
    payload_dir: Path
    payload_names: tuple[str, ...]
    dispositions: Counter[str]

    @classmethod
    def capture(cls, destination: Path) -> "ExportSnapshot":
        if not runner.MANIFEST.is_file():
            raise RuntimeError(f"export did not create {runner.MANIFEST}")
        destination.mkdir(parents=True, exist_ok=True)
        manifest = destination / runner.MANIFEST.name
        payload_dir = destination / "payload"
        payload_dir.mkdir()
        shutil.copyfile(runner.MANIFEST, manifest)
        payload_names = tuple(
            path.name for path in sorted(runner.PAYLOAD_DIR.glob("T*.fvt"))
        )
        for name in payload_names:
            shutil.copyfile(runner.PAYLOAD_DIR / name, payload_dir / name)
        dispositions = read_dispositions(manifest)
        emitted = dispositions["EMITTED"]
        if emitted != len(payload_names):
            raise RuntimeError(
                "manifest/payload inconsistency: "
                f"EMITTED={emitted}, payloads={len(payload_names)}"
            )
        return cls(
            destination,
            manifest,
            payload_dir,
            payload_names,
            dispositions,
        )


def read_dispositions(manifest: Path) -> Counter[str]:
    with manifest.open(encoding="utf-8", newline="") as stream:
        rows = (line for line in stream if not line.startswith("#"))
        return Counter(
            row["disposition"]
            for row in csv.DictReader(rows, delimiter="\t")
        )


def compare_snapshots(
    serial: ExportSnapshot, parallel: ExportSnapshot
) -> list[str]:
    errors: list[str] = []
    if serial.manifest.read_bytes() != parallel.manifest.read_bytes():
        errors.append("manifest bytes differ")
    if serial.dispositions != parallel.dispositions:
        errors.append(
            "disposition counts differ: "
            f"serial={dict(serial.dispositions)}, "
            f"parallel={dict(parallel.dispositions)}"
        )
    if serial.payload_names != parallel.payload_names:
        serial_only = sorted(set(serial.payload_names) - set(parallel.payload_names))
        parallel_only = sorted(set(parallel.payload_names) - set(serial.payload_names))
        errors.append(
            "payload filenames differ: "
            f"serial-only={serial_only[:5]}, parallel-only={parallel_only[:5]}"
        )
    for name in sorted(set(serial.payload_names) & set(parallel.payload_names)):
        if not filecmp.cmp(
            serial.payload_dir / name,
            parallel.payload_dir / name,
            shallow=False,
        ):
            errors.append(f"payload bytes differ: {name}")
            break
    return errors


def _copy_current_outputs(destination: Path) -> None:
    destination.mkdir(parents=True, exist_ok=True)
    if runner.MANIFEST.is_file():
        shutil.copyfile(runner.MANIFEST, destination / "manifest")
    payload_dir = destination / "payload"
    payload_dir.mkdir()
    if runner.PAYLOAD_DIR.is_dir():
        for path in runner.PAYLOAD_DIR.glob("T*.fvt"):
            shutil.copyfile(path, payload_dir / path.name)


def _restore_outputs(source: Path) -> None:
    runner.PAYLOAD_DIR.mkdir(parents=True, exist_ok=True)
    for path in runner.PAYLOAD_DIR.glob("T*.fvt"):
        path.unlink()
    for path in (source / "payload").glob("T*.fvt"):
        shutil.copyfile(path, runner.PAYLOAD_DIR / path.name)
    saved_manifest = source / "manifest"
    if saved_manifest.is_file():
        shutil.copyfile(saved_manifest, runner.MANIFEST)
    elif runner.MANIFEST.exists():
        runner.MANIFEST.unlink()


def run_export(
    *,
    classpath: str,
    export_jobs: int,
    expected_pool_size: int,
    prefix: str | None,
    offset: int,
    count: int,
    timeout: float,
    startup_timeout: float,
    log_dir: Path,
) -> float:
    with runner.PersistentExporter(
        prefix,
        classpath,
        export_jobs,
        expected_pool_size,
        log_dir,
        startup_timeout,
    ) as exporter:
        result = exporter.export(offset, count, timeout)
    if result.timed_out:
        raise RuntimeError(
            f"export_jobs={export_jobs} timed out after {result.elapsed:.3f}s"
        )
    if result.returncode != 0:
        raise RuntimeError(
            f"export_jobs={export_jobs} failed with status {result.returncode}:\n"
            f"{result.output[-2000:]}"
        )
    return result.elapsed


def resolve_classpath(build_timeout: float, state_dir: Path) -> str:
    fingerprint = runner.source_fingerprint()
    options = argparse.Namespace(build_timeout=build_timeout)
    return runner.resolve_exporter_classpath(options, state_dir, fingerprint)


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--offset", type=int, default=0)
    parser.add_argument("--count", type=int, default=160)
    parser.add_argument("--parallel-jobs", type=int, default=12)
    parser.add_argument("--min-speedup", type=float, default=2.0)
    parser.add_argument("--prefix")
    parser.add_argument("--export-timeout", type=float, default=900.0)
    parser.add_argument("--startup-timeout", type=float, default=300.0)
    parser.add_argument("--build-timeout", type=float, default=900.0)
    args = parser.parse_args(argv)
    for name in ("offset", "count", "parallel_jobs"):
        minimum = 0 if name == "offset" else 1
        if getattr(args, name) < minimum:
            parser.error(f"--{name.replace('_', '-')} must be >= {minimum}")
    for name in (
        "min_speedup",
        "export_timeout",
        "startup_timeout",
        "build_timeout",
    ):
        if getattr(args, name) <= 0:
            parser.error(f"--{name.replace('_', '-')} must be positive")
    return args


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    targets = runner.load_inventory(runner.INVENTORY, args.prefix)
    if args.offset + args.count > len(targets):
        raise RuntimeError(
            f"selection [{args.offset}, {args.offset + args.count}) exceeds "
            f"pool size {len(targets)}"
        )

    with tempfile.TemporaryDirectory(
        prefix="test262-export-benchmark-"
    ) as directory:
        work = Path(directory)
        backup = work / "original"
        _copy_current_outputs(backup)
        try:
            classpath = resolve_classpath(args.build_timeout, work)
            serial_elapsed = run_export(
                classpath=classpath,
                export_jobs=1,
                expected_pool_size=len(targets),
                prefix=args.prefix,
                offset=args.offset,
                count=args.count,
                timeout=args.export_timeout,
                startup_timeout=args.startup_timeout,
                log_dir=work / "serial-logs",
            )
            serial = ExportSnapshot.capture(work / "serial")
            parallel_elapsed = run_export(
                classpath=classpath,
                export_jobs=args.parallel_jobs,
                expected_pool_size=len(targets),
                prefix=args.prefix,
                offset=args.offset,
                count=args.count,
                timeout=args.export_timeout,
                startup_timeout=args.startup_timeout,
                log_dir=work / "parallel-logs",
            )
            parallel = ExportSnapshot.capture(work / "parallel")
            errors = compare_snapshots(serial, parallel)
        finally:
            _restore_outputs(backup)

    speedup = serial_elapsed / parallel_elapsed
    counts = ", ".join(
        f"{name}={count}" for name, count in sorted(serial.dispositions.items())
    )
    print(
        f"selection offset={args.offset} count={args.count} pool={len(targets)}"
    )
    print(f"dispositions {counts}")
    print(
        f"serial={serial_elapsed:.3f}s parallel({args.parallel_jobs})="
        f"{parallel_elapsed:.3f}s speedup={speedup:.2f}x"
    )
    if errors:
        for error in errors:
            print(f"FAIL: {error}", file=sys.stderr)
        return 1
    print("equivalence: manifest and payload filenames/bytes are identical")
    if speedup < args.min_speedup:
        print(
            f"FAIL: speedup {speedup:.2f}x is below "
            f"required {args.min_speedup:.2f}x",
            file=sys.stderr,
        )
        return 2
    print(f"PASS: speedup meets required {args.min_speedup:.2f}x")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError, ValueError) as error:
        print(f"ERROR: {error}", file=sys.stderr)
        raise SystemExit(1)
