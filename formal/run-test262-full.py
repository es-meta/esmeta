#!/usr/bin/env python3
"""Resumable orchestration for the modular Test262 pipeline."""

from __future__ import annotations

import argparse
import base64
import csv
import hashlib
import json
import os
import queue
import re
import select
import signal
import subprocess
import sys
import tempfile
import threading
import time
from collections import Counter, namedtuple
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import Protocol


ROOT = Path(__file__).resolve().parent.parent
FORMAL = ROOT / "formal"
INVENTORY = FORMAL / "validation" / "test262-inventory.tsv"
MANIFEST = FORMAL / "validation" / "test262-shard.tsv"
PAYLOAD_DIR = (FORMAL / "validation" / "payload").resolve()
BASE_STAMP = FORMAL / "validation" / "test262-base.json"
EXPORTER_CLASSPATH_STAMP = (
    FORMAL / "build" / "test262-exporter-classpath.json"
)
RESULT_HEADER = (
    "globalIndex\trelName\tstatus\tstage\tshardOffset\tlocalModuleIndex"
    "\telapsedSeconds\treason\n"
)
FINAL_STATUSES = {
    "ESMETA_FAILED",
    "NOT_REPRESENTABLE",
    "PASS",
    "MISMATCH",
    "UNSUPPORTED",
    "TIMEOUT",
    "CRASH",
    "BUILD_ERROR",
}
SOURCE_PATHS = (
    "build.sbt",
    "project",
    "src/main/scala",
    # ESMeta loads manual algorithms, host-function IR, Test262 policy, and
    # Unicode tables from here while exporting the specification/payloads.
    # Include every resource file, including .ir/.algo and extensionless data.
    "src/main/resources",
    # Executable Rocq dependency closure of ExtractCore.v.  Keep this list
    # explicit: proof/regression files in formal/*.v can change while a long
    # Test262 campaign is running without changing the extracted semantics.
    "formal/TyModel.v",
    "formal/Fragment.v",
    "formal/TestEncoding.v",
    "formal/Domain.v",
    "formal/Events.v",
    "formal/Semantics.v",
    "formal/ITreeExec.v",
    "formal/ExtractionConfig.v",
    "formal/ITreeCore.v",
    "formal/SpecAlgorithmITree.v",
    "formal/ExtractCore.v",
    "formal/Makefile",
    "formal/run-test262-full.py",
    "formal/itree_test_runtime.ml",
    "formal/with-itree-build-lock.py",
    "formal/payload_codec.ml",
    "formal/payload_worker.ml",
    "formal/prepare-native-core.py",
    "formal/spec_snapshot_writer.ml",
    "formal/validation/Spec.v",
)
TYMODEL_MANIFEST = "formal/_CoqProject"
SPEC_MANIFEST = "formal/validation/SpecSources.mk"
TYMODEL_BLOCK_START = "# BEGIN GENERATED FVTyModel SHARDS"
TYMODEL_BLOCK_END = "# END GENERATED FVTyModel SHARDS"
TYMODEL_SHARD_RE = re.compile(
    r"TyModel(?:Base|Parent[0-9]{2}|Bindings[0-9]{2})[.]v"
)
TYMODEL_IMPORT_RE = re.compile(
    r"^From ESMetaFV Require Import "
    r"(TyModel(?:Base|Parent[0-9]{2}|Bindings[0-9]{2}))[.]$",
    re.MULTILINE,
)
SPEC_SOURCE_RE = re.compile(
    r"validation/spec/"
    r"(?:SpecFuncs(?:_[0-9]{4})?|SpecGlobals|SpecHeap(?:_[0-9]{4})?)[.]v"
)
SPEC_FACADE_IMPORT_RE = {
    prefix: re.compile(rf"\b{prefix}_[0-9]{{4}}\b")
    for prefix in ("SpecFuncs", "SpecHeap")
}
SUBMODULE_PATHS = ("client", "ecma262", "tests/test262")
SOURCE_SUFFIXES = {
    ".conf",
    ".json",
    ".ml",
    ".mli",
    ".properties",
    ".py",
    ".sbt",
    ".scala",
    ".sh",
    ".v",
    ".yaml",
    ".yml",
}
IGNORED_DIRS = {
    ".bloop",
    ".cache",
    ".git",
    ".metals",
    ".omc",
    ".scala-build",
    "build",
    "logs",
    "node_modules",
    "out",
    "target",
}

Target = namedtuple("Target", "pool_offset global_index rel_name")
Result = namedtuple(
    "Result",
    "global_index rel_name status stage shard_offset local_index elapsed reason",
)
CommandResult = namedtuple("CommandResult", "returncode timed_out elapsed output")


def clean_field(value: object) -> str:
    return str(value).replace("\t", " ").replace("\r", " ").replace("\n", " ")


def atomic_write(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary = tempfile.mkstemp(
        prefix=f".{path.name}.", dir=path.parent, text=True
    )
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8", newline="") as stream:
            stream.write(content)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
    finally:
        try:
            os.unlink(temporary)
        except FileNotFoundError:
            pass


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        while chunk := stream.read(1024 * 1024):
            digest.update(chunk)
    return digest.hexdigest()


def load_inventory(path: Path, prefix: str | None) -> list[Target]:
    targets: list[tuple[int, str]] = []
    seen_indices: set[int] = set()
    seen_names: set[str] = set()
    with path.open(encoding="utf-8", newline="") as stream:
        rows = (line for line in stream if not line.startswith("#"))
        for row in csv.DictReader(rows, delimiter="\t"):
            if row["disposition"] != "TARGET":
                continue
            global_index = int(row["globalTargetIndex"])
            rel_name = row["relName"]
            if global_index in seen_indices or rel_name in seen_names:
                raise ValueError(
                    "inventory has duplicate target identity: "
                    f"{global_index} {rel_name!r}"
                )
            seen_indices.add(global_index)
            seen_names.add(rel_name)
            if prefix is not None and not rel_name.startswith(prefix):
                continue
            targets.append((global_index, rel_name))
    targets.sort()
    return [
        Target(pool_offset, global_index, rel_name)
        for pool_offset, (global_index, rel_name) in enumerate(targets)
    ]


def read_results(
    results_dir: Path,
    selection: list[Target] | None = None,
) -> dict[int, Result]:
    results: dict[int, Result] = {}
    selected_names = (
        {target.global_index: target.rel_name for target in selection}
        if selection is not None
        else None
    )
    if not results_dir.exists():
        return results
    for path in sorted(results_dir.glob("*.tsv")):
        with path.open(encoding="utf-8", newline="") as stream:
            row = next(csv.DictReader(stream, delimiter="\t"))
        result = Result(
            int(row["globalIndex"]),
            row["relName"],
            row["status"],
            row["stage"],
            int(row["shardOffset"]),
            row["localModuleIndex"],
            float(row["elapsedSeconds"]),
            row["reason"],
        )
        if result.status not in FINAL_STATUSES:
            raise ValueError(f"invalid checkpoint status in {path}: {result.status}")
        if result.global_index in results:
            raise ValueError(
                f"duplicate checkpoint global index in {path}: "
                f"{result.global_index}"
            )
        if (
            selected_names is not None
            and selected_names.get(result.global_index) != result.rel_name
        ):
            raise RuntimeError(
                f"checkpoint does not belong to this campaign: {path} has "
                f"{result.global_index} {result.rel_name!r}"
            )
        results[result.global_index] = result
    return results


def result_row(result: Result) -> str:
    values = (
        result.global_index,
        result.rel_name,
        result.status,
        result.stage,
        result.shard_offset,
        result.local_index,
        f"{result.elapsed:.3f}",
        result.reason,
    )
    return "\t".join(clean_field(value) for value in values) + "\n"


def write_result(results_dir: Path, result: Result) -> None:
    if result.status not in FINAL_STATUSES:
        raise ValueError(f"invalid result status: {result.status}")
    atomic_write(
        results_dir / f"{result.global_index:05d}.tsv",
        RESULT_HEADER + result_row(result),
    )


def write_aggregate(state_dir: Path, selection: list[Target]) -> None:
    results = sorted(
        read_results(state_dir / "results", selection).values()
    )
    atomic_write(
        state_dir / "results.tsv",
        RESULT_HEADER + "".join(result_row(result) for result in results),
    )
    counts = Counter(result.status for result in results)
    lines = [
        "Full Test262 modular run",
        f"completed\t{len(results)}",
        f"expected\t{len(selection)}",
        f"remaining\t{max(0, len(selection) - len(results))}",
    ]
    lines.extend(f"{status}\t{counts[status]}" for status in sorted(FINAL_STATUSES))
    atomic_write(state_dir / "summary.txt", "\n".join(lines) + "\n")


def _included_source(path: Path) -> bool:
    is_resource = path.parts[:3] == ("src", "main", "resources")
    return not _ignored_path(path) and (
        is_resource or path.suffix in SOURCE_SUFFIXES
    )


def _ignored_path(path: Path) -> bool:
    return any(part in IGNORED_DIRS for part in path.parts)


class Digest(Protocol):
    def update(self, data: bytes, /) -> None: ...


def _hash_file(digest: Digest, root: Path, path: Path) -> None:
    relative = path.relative_to(root).as_posix()
    digest.update(f"file\0{relative}\0".encode("utf-8"))
    with path.open("rb") as stream:
        while chunk := stream.read(1024 * 1024):
            digest.update(chunk)
    digest.update(b"\0")


def _hash_git_output(
    digest: Digest, repository: Path, command: list[str]
) -> None:
    digest.update(f"git\0{' '.join(command)}\0".encode("utf-8"))
    process = subprocess.Popen(
        ["git", "-C", str(repository), *command],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    assert process.stdout is not None
    while chunk := process.stdout.read(1024 * 1024):
        digest.update(chunk)
    _, error = process.communicate()
    if process.returncode != 0:
        raise RuntimeError(
            f"cannot fingerprint {repository}: "
            f"{error.decode('utf-8', errors='replace').strip()}"
        )


def _require_contiguous_shards(
    paths: list[str], prefix: str, digits: int
) -> None:
    pattern = re.compile(rf"{re.escape(prefix)}([0-9]{{{digits}}})[.]v")
    indices = sorted(
        int(match.group(1))
        for path in paths
        if (match := pattern.fullmatch(path)) is not None
    )
    if not indices or indices != list(range(len(indices))):
        raise RuntimeError(
            f"generated source manifest has non-contiguous {prefix} shards: "
            f"{indices}"
        )


def _manifest_block(path: Path, start: str, end: str) -> list[str]:
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except OSError as error:
        raise RuntimeError(
            f"required generated source manifest is missing: {path}"
        ) from error
    if lines.count(start) != 1 or lines.count(end) != 1:
        raise RuntimeError(
            f"generated source manifest must contain exactly one {start!r} / "
            f"{end!r} block: {path}"
        )
    start_index = lines.index(start)
    end_index = lines.index(end)
    if end_index <= start_index:
        raise RuntimeError(f"generated source manifest block is malformed: {path}")
    return [
        line.strip()
        for line in lines[start_index + 1 : end_index]
        if line.strip()
    ]


def _generated_tymodel_sources(root: Path) -> set[Path]:
    """Resolve and validate the generated TyModel source closure."""
    formal = root / "formal"

    tymodel_manifest_entries = _manifest_block(
        root / TYMODEL_MANIFEST, TYMODEL_BLOCK_START, TYMODEL_BLOCK_END
    )
    if (
        len(tymodel_manifest_entries) < 2
        or tymodel_manifest_entries[0] != "TyModelBase.v"
        or tymodel_manifest_entries[-1] != "TyModel.v"
        or len(tymodel_manifest_entries) != len(set(tymodel_manifest_entries))
    ):
        raise RuntimeError(
            "generated TyModel manifest must contain unique shards followed by TyModel.v"
        )
    tymodel_entries = tymodel_manifest_entries[:-1]
    if any(TYMODEL_SHARD_RE.fullmatch(entry) is None for entry in tymodel_entries):
        raise RuntimeError(
            "generated TyModel manifest contains an unexpected shard source"
        )
    _require_contiguous_shards(tymodel_entries, "TyModelParent", 2)
    _require_contiguous_shards(tymodel_entries, "TyModelBindings", 2)

    tymodel_facade = formal / "TyModel.v"
    try:
        imported = [
            f"{name}.v"
            for name in TYMODEL_IMPORT_RE.findall(
                tymodel_facade.read_text(encoding="utf-8")
            )
        ]
    except OSError as error:
        raise RuntimeError(
            f"required generated facade is missing: {tymodel_facade}"
        ) from error
    if imported != tymodel_entries:
        raise RuntimeError(
            "generated TyModel manifest does not match TyModel.v imports: "
            f"manifest={tymodel_entries!r}, imports={imported!r}"
        )

    sources = {formal / entry for entry in tymodel_manifest_entries}
    missing = sorted(path for path in sources if not path.is_file())
    if missing:
        raise RuntimeError(
            "generated semantic source closure is incomplete: "
            + ", ".join(str(path) for path in missing)
        )
    return sources


def _generated_spec_sources(root: Path) -> set[Path]:
    """Resolve and validate the generated Spec source closure."""
    formal = root / "formal"

    spec_manifest = root / SPEC_MANIFEST
    try:
        spec_text = spec_manifest.read_text(encoding="utf-8")
    except OSError as error:
        raise RuntimeError(
            f"required generated source manifest is missing: {spec_manifest}"
        ) from error
    spec_prefix = (
        "# AUTO-GENERATED by FVInitState; do not edit.\n"
        "SPEC_GENERATED_SOURCES := \\\n"
    )
    if not spec_text.startswith(spec_prefix) or not spec_text.endswith("\n"):
        raise RuntimeError(
            f"generated Spec source manifest is malformed: {spec_manifest}"
        )
    body_lines = spec_text.removeprefix(spec_prefix).splitlines()
    if (
        not body_lines
        or any(not line.startswith("  ") for line in body_lines)
        or any(not line.endswith(" \\") for line in body_lines[:-1])
        or body_lines[-1].endswith(" \\")
    ):
        raise RuntimeError(
            f"generated Spec source manifest has malformed continuations: {spec_manifest}"
        )
    spec_entries = [line.strip().removesuffix(" \\").strip() for line in body_lines]
    required_spec_facades = {
        "validation/spec/SpecFuncs.v",
        "validation/spec/SpecGlobals.v",
        "validation/spec/SpecHeap.v",
    }
    if (
        len(spec_entries) != len(set(spec_entries))
        or any(SPEC_SOURCE_RE.fullmatch(entry) is None for entry in spec_entries)
        or not required_spec_facades.issubset(spec_entries)
    ):
        raise RuntimeError(
            "generated Spec manifest must contain unique Funcs/Globals/Heap sources"
        )
    _require_contiguous_shards(spec_entries, "validation/spec/SpecFuncs_", 4)
    _require_contiguous_shards(spec_entries, "validation/spec/SpecHeap_", 4)

    for prefix, import_pattern in SPEC_FACADE_IMPORT_RE.items():
        facade = formal / "validation" / "spec" / f"{prefix}.v"
        try:
            imported = {
                f"validation/spec/{module}.v"
                for module in import_pattern.findall(
                    facade.read_text(encoding="utf-8")
                )
            }
        except OSError as error:
            raise RuntimeError(
                f"required generated facade is missing: {facade}"
            ) from error
        manifested = {
            entry
            for entry in spec_entries
            if re.fullmatch(
                rf"validation/spec/{prefix}_[0-9]{{4}}[.]v", entry
            )
        }
        if imported != manifested:
            raise RuntimeError(
                f"generated {prefix} facade does not match SpecSources.mk: "
                f"manifest={sorted(manifested)!r}, imports={sorted(imported)!r}"
            )

    sources = {formal / entry for entry in spec_entries}
    sources.update({formal / "validation" / "Spec.v", spec_manifest})
    missing = sorted(path for path in sources if not path.is_file())
    if missing:
        raise RuntimeError(
            "generated semantic source closure is incomplete: "
            + ", ".join(str(path) for path in missing)
        )
    return sources


def _generated_semantic_sources(root: Path) -> set[Path]:
    """Resolve and validate every generated Rocq source in the semantic closure."""
    return _generated_tymodel_sources(root) | _generated_spec_sources(root)


def _source_fingerprint(root: Path, include_generated_spec: bool) -> str:
    digest = hashlib.sha256()
    source_files = _generated_tymodel_sources(root)
    if include_generated_spec:
        source_files.update(_generated_spec_sources(root))
    for relative in SOURCE_PATHS:
        if not include_generated_spec and relative == "formal/validation/Spec.v":
            continue
        path = root / relative
        if path.is_dir():
            source_files.update(
                candidate
                for candidate in path.rglob("*")
                if candidate.is_file()
                and _included_source(candidate.relative_to(root))
            )
        elif path.is_file():
            source_files.add(path)
    for path in sorted(source_files):
        _hash_file(digest, root, path)

    for relative in SUBMODULE_PATHS:
        repository = root / relative
        if not repository.is_dir():
            raise RuntimeError(
                f"required source checkout is missing: {repository}"
            )
        digest.update(f"repository\0{relative}\0".encode("utf-8"))
        _hash_git_output(digest, repository, ["rev-parse", "HEAD"])
        _hash_git_output(digest, repository, ["diff", "--binary", "HEAD", "--"])
        untracked = subprocess.run(
            [
                "git",
                "-C",
                str(repository),
                "ls-files",
                "--others",
                "--exclude-standard",
                "-z",
            ],
            check=True,
            capture_output=True,
        ).stdout
        for raw_name in sorted(name for name in untracked.split(b"\0") if name):
            name = raw_name.decode("utf-8", errors="surrogateescape")
            path = repository / name
            if path.is_file() and not _ignored_path(path.relative_to(root)):
                _hash_file(digest, root, path)
    return digest.hexdigest()


def source_fingerprint(root: Path = ROOT) -> str:
    """Hash semantic/export/build sources plus submodule revisions and dirt."""
    return _source_fingerprint(root, include_generated_spec=True)


def generator_input_fingerprint(root: Path = ROOT) -> str:
    """Hash FVInitState inputs, excluding the generated Spec closure."""
    return _source_fingerprint(root, include_generated_spec=False)


def campaign_data(
    selection: list[Target],
    args: argparse.Namespace,
    fingerprint: str | None = None,
) -> dict[str, object]:
    digest = hashlib.sha256()
    for target in selection:
        digest.update(
            f"{target.global_index}\t{target.rel_name}\n".encode("utf-8")
        )
    return {
        "version": 6,
        "inventory": str(INVENTORY.relative_to(ROOT)),
        "prefix": args.prefix,
        "indices": args.indices,
        "start": args.start,
        "count": args.count,
        "selected": len(selection),
        "selectionSha256": digest.hexdigest(),
        "sourceSha256": fingerprint or source_fingerprint(),
        "shardSize": args.shard_size,
        "jobs": args.jobs,
        "exportJobs": args.export_jobs,
        "fuel": args.fuel,
        "exportTimeout": args.export_timeout,
        "buildTimeout": args.build_timeout,
        "runTimeout": args.run_timeout,
        "workerMode": args.worker_mode,
    }


def ensure_source_unchanged(
    state_dir: Path,
    expected_fingerprint: str,
    stage: str,
) -> None:
    drift_path = state_dir / "source-drift.json"
    if drift_path.exists():
        raise RuntimeError(
            f"campaign was invalidated by source drift: {drift_path}; "
            "use a new state directory"
        )
    actual_fingerprint = source_fingerprint()
    if actual_fingerprint == expected_fingerprint:
        return
    details = {
        "stage": stage,
        "expectedSourceSha256": expected_fingerprint,
        "actualSourceSha256": actual_fingerprint,
    }
    atomic_write(
        drift_path,
        json.dumps(details, indent=2, sort_keys=True) + "\n",
    )
    raise RuntimeError(
        f"campaign source changed during {stage}; checkpoints are "
        f"quarantined by {drift_path}; use a new state directory"
    )


def ensure_campaign(
    state_dir: Path,
    selection: list[Target],
    args: argparse.Namespace,
    fingerprint: str | None = None,
    create: bool = True,
) -> None:
    expected = campaign_data(selection, args, fingerprint)
    path = state_dir / "campaign.json"
    drift_path = state_dir / "source-drift.json"
    if drift_path.exists():
        raise RuntimeError(
            f"campaign was invalidated by source drift: {drift_path}; "
            "use a new state directory"
        )
    if path.exists():
        actual = json.loads(path.read_text(encoding="utf-8"))
        if actual.get("version") != expected["version"]:
            raise RuntimeError(
                f"campaign metadata is obsolete: {path}; use a new state "
                "directory because old checkpoints do not record the current "
                "source and execution-policy identity"
            )
        if actual.get("sourceSha256") != expected["sourceSha256"]:
            raise RuntimeError(
                f"campaign source fingerprint changed: {path}; use a new "
                "state directory (or restore the original source/Test262 "
                "checkout) instead of reusing these checkpoints"
            )
        if actual != expected:
            raise RuntimeError(
                f"state directory belongs to a different campaign: {path}; "
                "use a separate state directory"
            )
        read_results(state_dir / "results", selection)
        return

    if read_results(state_dir / "results"):
        raise RuntimeError(
            f"state directory has checkpoints but no campaign metadata: "
            f"{state_dir}; use a new state directory because their source "
            "identity cannot be verified"
        )
    if not create:
        return
    atomic_write(path, json.dumps(expected, indent=2, sort_keys=True) + "\n")


def pending_ranges(
    pool: list[Target], completed: set[int], shard_size: int
) -> list[list[Target]]:
    ranges: list[list[Target]] = []
    current: list[Target] = []
    for target in pool:
        if target.global_index in completed:
            if current:
                ranges.append(current)
                current = []
            continue
        if current and (
            len(current) >= shard_size
            or target.pool_offset != current[-1].pool_offset + 1
        ):
            ranges.append(current)
            current = []
        current.append(target)
    if current:
        ranges.append(current)
    return ranges


def run_command(command: list[str], cwd: Path, timeout: float) -> CommandResult:
    started = time.monotonic()
    environment = os.environ.copy()
    environment["ESMETA_HOME"] = str(ROOT)
    process = subprocess.Popen(
        command,
        cwd=cwd,
        env=environment,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        start_new_session=True,
    )
    timed_out = False
    try:
        output, _ = process.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        timed_out = True
        os.killpg(process.pid, signal.SIGTERM)
        try:
            output, _ = process.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGKILL)
            output, _ = process.communicate()
    return CommandResult(
        process.returncode, timed_out, time.monotonic() - started, output
    )


def terminate_process(process: subprocess.Popen) -> None:
    def send(signal_number: int) -> None:
        try:
            os.killpg(process.pid, signal_number)
        except ProcessLookupError:
            return
        except PermissionError:
            try:
                process.send_signal(signal_number)
            except ProcessLookupError:
                return

    if process.poll() is None:
        send(signal.SIGTERM)
        try:
            process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            send(signal.SIGKILL)
            process.wait()
    if process.stdin is not None:
        try:
            process.stdin.close()
        except OSError:
            pass
    if process.stdout is not None:
        try:
            process.stdout.close()
        except OSError:
            pass


def persistent_exporter_command(
    prefix: str | None,
    classpath: str,
    export_jobs: int,
) -> list[str]:
    command = [
        "java",
        "-Xms1g",
        "-Xmx8g",
        "-Xss4m",
        "-Dfile.encoding=utf8",
        "-cp",
        classpath,
        "esmeta.fv.FVInitState",
        "--test262-server",
    ]
    if prefix:
        command.append(prefix)
    command.extend(
        (
            "--reuse-test262-base",
            "--payload-only",
            f"--test262-export-jobs={export_jobs}",
        )
    )
    return command


def parse_exporter_protocol(line: str) -> list[str] | None:
    plain = re.sub(r"\x1b\[[0-9;?]*[ -/]*[@-~]", "", line)
    fields = plain.strip().split()
    try:
        marker = fields.index("FVEXPORT")
    except ValueError:
        return None
    return fields[marker + 1 :]


class PersistentExporter:
    """One JVM that keeps ESMeta's CFG and Test262 corpus loaded."""

    def __init__(
        self,
        prefix: str | None,
        classpath: str,
        export_jobs: int,
        expected_pool_size: int,
        log_dir: Path,
        startup_timeout: float,
    ):
        self.command = persistent_exporter_command(
            prefix, classpath, export_jobs
        )
        self.expected_pool_size = expected_pool_size
        self.startup_timeout = startup_timeout
        log_dir.mkdir(parents=True, exist_ok=True)
        self._log = (log_dir / "exporter-session.log").open(
            "a", encoding="utf-8", newline=""
        )
        self.process: subprocess.Popen[str] | None = None
        self._line_queue: queue.Queue[str | None] = queue.Queue()
        self._reader_thread: threading.Thread | None = None
        self._spawn()

    def _drain_stdout(
        self,
        process: subprocess.Popen[str],
        line_queue: queue.Queue[str | None],
    ) -> None:
        assert process.stdout is not None
        try:
            for line in process.stdout:
                self._log.write(line)
                self._log.flush()
                line_queue.put(line.rstrip("\r\n"))
        finally:
            line_queue.put(None)

    def _readline(self, timeout: float) -> str | None:
        try:
            line = self._line_queue.get(timeout=max(0.0, timeout))
        except queue.Empty:
            return None
        if line is None:
            assert self.process is not None
            raise RuntimeError(
                "persistent exporter exited with status "
                f"{self.process.poll()}"
            )
        return line

    def _read_protocol(
        self,
        timeout: float,
        output: list[str],
    ) -> list[str] | None:
        deadline = time.monotonic() + timeout
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                return None
            line = self._readline(remaining)
            if line is None:
                return None
            output.append(line)
            protocol = parse_exporter_protocol(line)
            if protocol is not None:
                return protocol

    def _spawn(self) -> None:
        self._log.write(f"command: {' '.join(self.command)}\n")
        self._log.flush()
        self.process = subprocess.Popen(
            self.command,
            cwd=ROOT,
            env={**os.environ, "ESMETA_HOME": str(ROOT)},
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            encoding="utf-8",
            errors="strict",
            bufsize=1,
            start_new_session=True,
        )
        self._line_queue = queue.Queue()
        self._reader_thread = threading.Thread(
            target=self._drain_stdout,
            args=(self.process, self._line_queue),
            name="test262-exporter-output",
            daemon=True,
        )
        self._reader_thread.start()
        output: list[str] = []
        try:
            greeting = self._read_protocol(self.startup_timeout, output)
            expected = ["READY", "1", str(self.expected_pool_size)]
            if greeting is None:
                raise RuntimeError(
                    "persistent exporter startup timed out after "
                    f"{self.startup_timeout}s"
                )
            if greeting != expected:
                raise RuntimeError(
                    "persistent exporter sent invalid greeting: "
                    f"{greeting!r}; expected {expected!r}"
                )
        except Exception:
            self._terminate()
            raise

    def _terminate(self) -> None:
        process = self.process
        self.process = None
        if process is not None:
            terminate_process(process)
        reader = self._reader_thread
        self._reader_thread = None
        if reader is not None:
            reader.join(timeout=2)

    @staticmethod
    def _decode_error(encoded: str) -> str:
        try:
            padded = encoded + "=" * (-len(encoded) % 4)
            return base64.urlsafe_b64decode(padded).decode("utf-8")
        except (UnicodeDecodeError, ValueError) as error:
            raise RuntimeError(
                f"persistent exporter returned invalid error text: {error}"
            ) from error

    def export(self, offset: int, count: int, timeout: float) -> CommandResult:
        started = time.monotonic()
        output: list[str] = []
        try:
            if self.process is None or self.process.poll() is not None:
                self._terminate()
                self._spawn()
            assert self.process is not None
            assert self.process.stdin is not None
            self.process.stdin.write(f"EXPORT {offset} {count}\n")
            self.process.stdin.flush()
            response = self._read_protocol(timeout, output)
            if response is None:
                self._terminate()
                return CommandResult(
                    -signal.SIGTERM,
                    True,
                    time.monotonic() - started,
                    "\n".join(output) + ("\n" if output else ""),
                )
            if len(response) < 3:
                raise RuntimeError(
                    f"persistent exporter sent malformed response: {response!r}"
                )
            kind, response_offset, response_count = response[:3]
            if (response_offset, response_count) != (
                str(offset),
                str(count),
            ):
                raise RuntimeError(
                    "persistent exporter response identity mismatch: "
                    f"{response!r}"
                )
            if kind == "DONE" and len(response) == 4:
                int(response[3])
                return CommandResult(
                    0,
                    False,
                    time.monotonic() - started,
                    "\n".join(output) + "\n",
                )
            if kind == "ERROR" and len(response) == 4:
                reason = self._decode_error(response[3])
                return CommandResult(
                    1,
                    False,
                    time.monotonic() - started,
                    "\n".join(output) + f"\n{reason}\n",
                )
            raise RuntimeError(
                f"persistent exporter sent unknown response: {response!r}"
            )
        except (BrokenPipeError, OSError, RuntimeError) as error:
            self._terminate()
            return CommandResult(
                1,
                False,
                time.monotonic() - started,
                "\n".join(output)
                + ("\n" if output else "")
                + f"{type(error).__name__}: {error}\n",
            )

    def close(self) -> None:
        process = self.process
        if process is not None and process.poll() is None:
            try:
                assert process.stdin is not None
                process.stdin.write("QUIT\n")
                process.stdin.flush()
                output: list[str] = []
                self._read_protocol(5, output)
            except (BrokenPipeError, OSError, RuntimeError):
                pass
        self._terminate()
        try:
            self._log.close()
        except OSError:
            pass

    def __enter__(self) -> "PersistentExporter":
        return self

    def __exit__(self, *_):
        self.close()


class PayloadWorker:
    """One persistent core-only OCaml process."""

    def __init__(
        self,
        index: int,
        log_dir: Path,
        startup_timeout: float,
        executable: Path,
    ):
        self.index = index
        self.startup_timeout = startup_timeout
        self.executable = executable.resolve()
        log_dir.mkdir(parents=True, exist_ok=True)
        self._stderr = (log_dir / f"worker-{index:02d}.stderr.log").open("ab")
        self.process: subprocess.Popen[str] | None = None
        self._spawn()

    def _readline(self, timeout: float) -> str | None:
        assert self.process is not None
        assert self.process.stdout is not None
        readable, _, _ = select.select(
            [self.process.stdout], [], [], timeout
        )
        if not readable:
            return None
        line = self.process.stdout.readline()
        if line == "":
            raise RuntimeError(
                f"worker {self.index} exited with status "
                f"{self.process.poll()}"
            )
        return line.rstrip("\r\n")

    def _spawn(self) -> None:
        self.process = subprocess.Popen(
            [str(self.executable)],
            cwd=FORMAL,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=self._stderr,
            text=True,
            encoding="utf-8",
            errors="strict",
            bufsize=1,
            start_new_session=True,
        )
        try:
            greeting = self._readline(self.startup_timeout)
            if greeting is None:
                raise RuntimeError(
                    f"worker {self.index} startup timed out after "
                    f"{self.startup_timeout}s"
                )
            if greeting != "READY 1":
                raise RuntimeError(
                    f"worker {self.index} sent invalid greeting: {greeting!r}"
                )
        except Exception:
            self._terminate()
            raise

    def _terminate(self) -> None:
        process = self.process
        self.process = None
        if process is not None:
            terminate_process(process)

    def restart(self) -> None:
        self._terminate()
        self._spawn()

    @staticmethod
    def _decode_hex(field: str, source: str) -> str:
        try:
            return bytes.fromhex(source).decode("utf-8")
        except (UnicodeDecodeError, ValueError) as error:
            raise RuntimeError(
                f"worker returned invalid {field} hex: {error}"
            ) from error

    def run(
        self,
        fuel: int,
        local_index: int,
        global_index: int,
        expected_name: str,
        payload_path: Path,
        timeout: float,
    ) -> CommandResult:
        started = time.monotonic()
        assert self.process is not None
        assert self.process.stdin is not None
        request = " ".join(
            (
                "RUN",
                str(fuel),
                str(local_index),
                str(global_index),
                "0",
                expected_name.encode("utf-8").hex(),
                str(payload_path).encode("utf-8").hex(),
            )
        )
        try:
            self.process.stdin.write(request + "\n")
            self.process.stdin.flush()
            response = self._readline(timeout)
            if response is None:
                self.restart()
                return CommandResult(
                    -signal.SIGTERM,
                    True,
                    time.monotonic() - started,
                    "",
                )
            kind, separator, encoded = response.partition(" ")
            if not separator:
                raise RuntimeError(
                    f"worker {self.index} sent malformed response: "
                    f"{response!r}"
                )
            message = self._decode_hex("response", encoded)
            if kind == "RESULT":
                return CommandResult(
                    0, False, time.monotonic() - started, message + "\n"
                )
            if kind == "ERROR":
                return CommandResult(
                    1, False, time.monotonic() - started, message + "\n"
                )
            raise RuntimeError(
                f"worker {self.index} sent unknown response {kind!r}"
            )
        except (BrokenPipeError, OSError, RuntimeError) as error:
            try:
                self.restart()
            except Exception as restart_error:
                error = RuntimeError(
                    f"{error}; worker restart failed: {restart_error}"
                )
            return CommandResult(
                1,
                False,
                time.monotonic() - started,
                f"{type(error).__name__}: {error}\n",
            )

    def close(self) -> None:
        process = self.process
        if process is not None and process.poll() is None:
            try:
                assert process.stdin is not None
                process.stdin.write("QUIT\n")
                process.stdin.flush()
                self._readline(2)
            except (BrokenPipeError, OSError, RuntimeError):
                pass
        self._terminate()
        try:
            self._stderr.close()
        except OSError:
            pass


class PayloadWorkerPool:
    def __init__(
        self,
        count: int,
        log_dir: Path,
        startup_timeout: float,
        executable: Path,
    ):
        self.executable = executable.resolve()
        self.workers: list[PayloadWorker] = []
        self.available: queue.Queue[PayloadWorker] = queue.Queue()
        try:
            for index in range(count):
                worker = PayloadWorker(
                    index, log_dir, startup_timeout, self.executable
                )
                self.workers.append(worker)
                self.available.put(worker)
        except Exception:
            self.close()
            raise

    def run(self, *args, **kwargs) -> CommandResult:
        worker = self.available.get()
        try:
            return worker.run(*args, **kwargs)
        finally:
            self.available.put(worker)

    def close(self) -> None:
        for worker in self.workers:
            worker.close()
        self.workers.clear()

    def __enter__(self) -> "PayloadWorkerPool":
        return self

    def __exit__(self, *_):
        self.close()


def parse_manifest(path: Path) -> list[dict[str, str]]:
    with path.open(encoding="utf-8", newline="") as stream:
        rows = (line for line in stream if not line.startswith("#"))
        return list(csv.DictReader(rows, delimiter="\t"))


def validate_manifest(
    rows: list[dict[str, str]], group: list[Target]
) -> dict[int, dict[str, str]]:
    expected = {
        target.global_index: target.rel_name for target in group
    }
    actual: dict[int, dict[str, str]] = {}
    for row in rows:
        global_index = int(row["globalIndex"])
        if global_index in actual:
            raise ValueError(
                f"export manifest repeats global index {global_index}"
            )
        actual[global_index] = row
    if set(actual) != set(expected):
        raise ValueError(
            "export manifest indices do not match selected shard: "
            f"expected={sorted(expected)} actual={sorted(actual)}"
        )
    for global_index, rel_name in expected.items():
        if actual[global_index]["relName"] != rel_name:
            raise ValueError(
                "export manifest path does not match selected shard: "
                f"{global_index} expected {rel_name!r}, got "
                f"{actual[global_index]['relName']!r}"
            )
    return actual


def resolve_payload_path(relative: str) -> Path:
    if not relative or relative == "-":
        raise ValueError("emitted manifest row has no payload file")
    candidate = (FORMAL / relative).resolve()
    try:
        candidate.relative_to(PAYLOAD_DIR)
    except ValueError as error:
        raise ValueError(
            f"payload path escapes {PAYLOAD_DIR}: {relative!r}"
        ) from error
    if not candidate.is_file():
        raise ValueError(f"payload file does not exist: {candidate}")
    return candidate


def classify_driver_output(
    output: str,
    returncode: int,
    expected_id: str,
    expected_rel_name: str,
) -> tuple[str, str]:
    verdict_lines = [
        line.strip() for line in output.splitlines() if line.startswith("T")
    ]
    verdict_line = verdict_lines[0] if len(verdict_lines) == 1 else ""
    if returncode != 0:
        reason = (
            verdict_line
            or output.strip()
            or f"driver exited with status {returncode}"
        )
        return "BUILD_ERROR", reason
    if len(verdict_lines) != 1:
        return (
            "BUILD_ERROR",
            f"driver emitted {len(verdict_lines)} verdict lines; expected 1",
        )
    fields = verdict_line.split()
    if not fields or fields[0] != expected_id:
        return (
            "BUILD_ERROR",
            f"driver verdict id does not match {expected_id}: {verdict_line}",
        )
    name_pattern = (
        rf"(?<!\S){re.escape(expected_rel_name)}(?:\s+\(|$)"
    )
    if re.search(name_pattern, verdict_line) is None:
        return (
            "BUILD_ERROR",
            "driver verdict path does not match "
            f"{expected_rel_name}: {verdict_line}",
        )
    if len(fields) < 4:
        return "BUILD_ERROR", f"malformed driver verdict: {verdict_line}"
    verdict = fields[3]
    if verdict == "PASS":
        return "PASS", ""
    if verdict in {
        "RESULT-DIFFERS",
        "PRINTS-DIFFER",
        "AST-ALIASES-DIFFER",
    }:
        return "MISMATCH", verdict_line
    if verdict == "UNSUPPORTED-EFFECT":
        return "UNSUPPORTED", verdict_line
    if verdict == "OUT-OF-FUEL":
        return "TIMEOUT", verdict_line
    if verdict == "CRASH":
        return "CRASH", verdict_line
    return "BUILD_ERROR", f"unknown driver verdict {verdict}: {verdict_line}"


class RunLock:
    def __init__(self, path: Path):
        self.path = path

    def __enter__(self):
        self.path.parent.mkdir(parents=True, exist_ok=True)
        for _ in range(2):
            try:
                descriptor = os.open(
                    self.path, os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o644
                )
                with os.fdopen(descriptor, "w") as stream:
                    stream.write(f"{os.getpid()}\n")
                return self
            except FileExistsError:
                try:
                    pid = int(self.path.read_text().strip())
                    os.kill(pid, 0)
                except (FileNotFoundError, ProcessLookupError, ValueError):
                    self.path.unlink(missing_ok=True)
                    continue
                raise RuntimeError(f"another runner is active (pid {pid})")
        raise RuntimeError(f"could not acquire lock {self.path}")

    def __exit__(self, *_):
        self.path.unlink(missing_ok=True)


def save_log(path: Path, command: list[str], result: CommandResult) -> None:
    header = (
        f"command: {' '.join(command)}\n"
        f"returncode: {result.returncode}\n"
        f"timedOut: {str(result.timed_out).lower()}\n"
        f"elapsedSeconds: {result.elapsed:.3f}\n\n"
    )
    atomic_write(path, header + result.output)


def bootstrap_inventory(args: argparse.Namespace) -> str:
    # TyModel is an independently tracked input and must be valid before the
    # exporter is allowed to regenerate anything.  In contrast, Spec is an
    # FVInitState output: a missing, partial, or malformed closure means the
    # empty bootstrap needs to be retried.
    _generated_tymodel_sources(ROOT)
    try:
        _generated_spec_sources(ROOT)
    except RuntimeError as error:
        spec_closure_error: RuntimeError | None = error
        current_fingerprint = None
    else:
        spec_closure_error = None
        current_fingerprint = source_fingerprint()
    stamp: dict[str, object] = {}
    try:
        stamp = json.loads(BASE_STAMP.read_text(encoding="utf-8"))
    except (FileNotFoundError, json.JSONDecodeError):
        pass
    spec_exists = (FORMAL / "validation" / "Spec.v").is_file()
    inventory_exists = INVENTORY.is_file()
    reuse = (
        current_fingerprint is not None
        and spec_exists
        and inventory_exists
        and stamp.get("version") == 1
        and stamp.get("sourceSha256") == current_fingerprint
        and stamp.get("inventorySha256") == file_sha256(INVENTORY)
    )
    if reuse:
        assert current_fingerprint is not None
        return current_fingerprint
    generator_fingerprint = generator_input_fingerprint(ROOT)
    command = [
        "sbt",
        "runMain esmeta.fv.FVInitState --test262-shard 0 0 --payload-only",
    ]
    result = run_command(command, ROOT, args.export_timeout)
    prior_closure = (
        "\npre-bootstrap generated Spec closure was invalid: "
        f"{spec_closure_error}"
        if spec_closure_error is not None
        else ""
    )
    if result.timed_out:
        raise RuntimeError(
            f"inventory bootstrap timed out after {args.export_timeout}s"
            f"{prior_closure}"
        )
    if result.returncode != 0:
        raise RuntimeError(
            "inventory bootstrap failed with status "
            f"{result.returncode}:\n{result.output[-2000:]}{prior_closure}"
        )
    after_bootstrap_fingerprint = generator_input_fingerprint(ROOT)
    if after_bootstrap_fingerprint != generator_fingerprint:
        raise RuntimeError(
            "FVInitState generator inputs changed during inventory bootstrap; "
            "refusing to stamp mixed generated output"
        )
    if not INVENTORY.exists():
        raise RuntimeError(
            f"inventory bootstrap succeeded but did not create {INVENTORY}"
        )
    current_fingerprint = source_fingerprint()
    after_full_fingerprint = generator_input_fingerprint(ROOT)
    if after_full_fingerprint != generator_fingerprint:
        raise RuntimeError(
            "FVInitState generator inputs changed while validating generated "
            "output; refusing to stamp mixed generated output"
        )
    atomic_write(
        BASE_STAMP,
        json.dumps(
            {
                "version": 1,
                "sourceSha256": current_fingerprint,
                "inventorySha256": file_sha256(INVENTORY),
            },
            indent=2,
            sort_keys=True,
        )
        + "\n",
    )
    return current_fingerprint


def valid_exporter_classpath(classpath: object) -> bool:
    if not isinstance(classpath, str):
        return False
    entries = classpath.split(os.pathsep)
    return len(entries) > 1 and all(
        entry and Path(entry).exists() for entry in entries
    )


def extract_exporter_classpath(output: str) -> str:
    for raw_line in reversed(output.splitlines()):
        line = re.sub(r"\x1b\[[0-9;?]*[ -/]*[@-~]", "", raw_line).strip()
        if valid_exporter_classpath(line):
            return line
    raise RuntimeError("sbt did not emit a valid Compile/fullClasspath")


def resolve_exporter_classpath(
    args: argparse.Namespace,
    state_dir: Path,
    fingerprint: str,
) -> str:
    stamp: dict[str, object] = {}
    if EXPORTER_CLASSPATH_STAMP.is_file():
        try:
            stamp = json.loads(
                EXPORTER_CLASSPATH_STAMP.read_text(encoding="utf-8")
            )
        except (json.JSONDecodeError, OSError):
            stamp = {}
    classpath = stamp.get("classpath")
    if (
        stamp.get("version") == 1
        and stamp.get("sourceSha256") == fingerprint
        and valid_exporter_classpath(classpath)
    ):
        assert isinstance(classpath, str)
        return classpath

    command = ["sbt", "--error", "export Compile / fullClasspath"]
    result = run_command(command, ROOT, args.build_timeout)
    save_log(state_dir / "logs" / "exporter-classpath.log", command, result)
    if result.timed_out:
        raise RuntimeError(
            "exporter classpath query timed out after "
            f"{args.build_timeout}s"
        )
    if result.returncode != 0:
        raise RuntimeError(
            "exporter classpath query failed with status "
            f"{result.returncode}:\n{result.output[-2000:]}"
        )
    classpath = extract_exporter_classpath(result.output)
    atomic_write(
        EXPORTER_CLASSPATH_STAMP,
        json.dumps(
            {
                "version": 1,
                "sourceSha256": fingerprint,
                "classpath": classpath,
            },
            indent=2,
            sort_keys=True,
        )
        + "\n",
    )
    return classpath


def build_payload_worker(
    args: argparse.Namespace,
    state_dir: Path,
) -> Path:
    target = (
        "fvitree-worker-native"
        if args.worker_mode == "native"
        else "fvitree-worker"
    )
    command = [
        "python3",
        "./with-itree-build-lock.py",
        str(args.jobs),
        target,
    ]
    result = run_command(command, FORMAL, args.build_timeout)
    save_log(state_dir / "logs" / "build-worker.log", command, result)
    if result.timed_out:
        raise RuntimeError(
            f"payload worker build timed out after {args.build_timeout}s"
        )
    if result.returncode != 0:
        raise RuntimeError(
            "payload worker build failed with status "
            f"{result.returncode}:\n{result.output[-2000:]}"
        )
    executable = FORMAL / target
    if not executable.is_file():
        raise RuntimeError(
            f"payload worker build succeeded but did not create {executable}"
        )
    return executable


def checkpoint(
    state_dir: Path,
    result: Result,
) -> None:
    write_result(state_dir / "results", result)
    print(
        f"[{result.global_index:05d}] {result.status:<17} "
        f"{result.rel_name}",
        flush=True,
    )


def execute_emitted_test(
    target: Target,
    local_index: str,
    payload_path: Path,
    offset: int,
    args: argparse.Namespace,
    state_dir: Path,
    workers: PayloadWorkerPool,
) -> Result:
    worker_name = (
        "fvitree-worker-native"
        if args.worker_mode == "native"
        else "fvitree-worker"
    )
    run_command_line = [
        worker_name,
        str(args.fuel),
        local_index,
        str(target.global_index),
        str(payload_path),
    ]
    run_result = workers.run(
        args.fuel,
        int(local_index),
        target.global_index,
        target.rel_name,
        payload_path,
        args.run_timeout,
    )
    if run_result.timed_out:
        status, reason = (
            "TIMEOUT",
            f"runtime timeout after {args.run_timeout}s",
        )
    else:
        status, reason = classify_driver_output(
            run_result.output,
            run_result.returncode,
            f"T{int(local_index):03d}",
            target.rel_name,
        )
    if status != "PASS":
        save_log(
            state_dir / "logs" / f"run-{target.global_index:05d}.log",
            run_command_line,
            run_result,
        )
    return Result(
        target.global_index,
        target.rel_name,
        status,
        "run",
        offset,
        local_index,
        run_result.elapsed,
        reason,
    )


def execute_range(
    group: list[Target],
    args: argparse.Namespace,
    state_dir: Path,
    exporter: PersistentExporter,
    workers: PayloadWorkerPool,
) -> None:
    offset = group[0].pool_offset
    count = len(group)
    export_command = ["persistent-exporter", "EXPORT", str(offset), str(count)]
    export_result = exporter.export(offset, count, args.export_timeout)
    save_log(
        state_dir / "logs" / f"export-{offset:05d}-{count}.log",
        export_command,
        export_result,
    )
    if export_result.timed_out or export_result.returncode != 0:
        reason = (
            f"export timed out after {args.export_timeout}s"
            if export_result.timed_out
            else f"export exited with status {export_result.returncode}"
        )
        raise RuntimeError(
            f"transient shard export failure at offset {offset}: {reason}; "
            "no result checkpoints were written, so rerunning can retry it"
        )

    rows = parse_manifest(MANIFEST)
    try:
        rows_by_global = validate_manifest(rows, group)
    except (KeyError, TypeError, ValueError) as error:
        for target in group:
            checkpoint(
                state_dir,
                Result(
                    target.global_index,
                    target.rel_name,
                    "BUILD_ERROR",
                    "export",
                    offset,
                    "-",
                    export_result.elapsed,
                    f"invalid export manifest: {error}",
                ),
            )
        return
    emitted: list[tuple[Target, str, Path]] = []
    for target in group:
        row = rows_by_global[target.global_index]
        if row["disposition"] in {"ESMETA_FAILED", "NOT_REPRESENTABLE"}:
            checkpoint(
                state_dir,
                Result(
                    target.global_index,
                    target.rel_name,
                    row["disposition"],
                    "export",
                    offset,
                    "-",
                    export_result.elapsed,
                    row["reason"],
                ),
            )
        elif row["disposition"] == "EMITTED":
            try:
                payload_path = resolve_payload_path(row["payloadFile"])
            except (KeyError, ValueError) as error:
                checkpoint(
                    state_dir,
                    Result(
                        target.global_index,
                        target.rel_name,
                        "BUILD_ERROR",
                        "export",
                        offset,
                        row["localModuleIndex"],
                        export_result.elapsed,
                        f"invalid payload manifest: {error}",
                    ),
                )
            else:
                emitted.append(
                    (target, row["localModuleIndex"], payload_path)
                )
        else:
            checkpoint(
                state_dir,
                Result(
                    target.global_index,
                    target.rel_name,
                    "BUILD_ERROR",
                    "export",
                    offset,
                    row["localModuleIndex"],
                    export_result.elapsed,
                    f"unknown export disposition: {row['disposition']}",
                ),
            )
    if not emitted:
        return

    with ThreadPoolExecutor(max_workers=args.jobs) as executor:
        futures = {
            executor.submit(
                execute_emitted_test,
                target,
                local_index,
                payload_path,
                offset,
                args,
                state_dir,
                workers,
            ): (target, local_index)
            for target, local_index, payload_path in emitted
        }
        for future in as_completed(futures):
            target, local_index = futures[future]
            try:
                result = future.result()
            except Exception as error:
                result = Result(
                    target.global_index,
                    target.rel_name,
                    "BUILD_ERROR",
                    "run",
                    offset,
                    local_index,
                    0.0,
                    f"runner exception: {type(error).__name__}: {error}",
                )
            checkpoint(state_dir, result)


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--shard-size", type=int, default=1280)
    parser.add_argument("--jobs", type=int, default=1)
    parser.add_argument(
        "--export-jobs",
        type=int,
        help="parallel ESMeta oracle jobs (default: same as --jobs)",
    )
    parser.add_argument("--fuel", type=int, default=100_000_000)
    parser.add_argument("--export-timeout", type=float, default=900)
    parser.add_argument("--build-timeout", type=float, default=1800)
    parser.add_argument("--run-timeout", type=float, default=900)
    parser.add_argument(
        "--worker-mode",
        choices=("native", "bytecode"),
        default="native",
        help="use the snapshot-backed native worker (default) or bytecode fallback",
    )
    parser.add_argument("--prefix")
    parser.add_argument(
        "--indices",
        help="comma-separated global target indices (exclusive with prefix/start/count)",
    )
    parser.add_argument("--start", type=int, default=0)
    parser.add_argument("--count", type=int)
    parser.add_argument(
        "--state-dir", type=Path, default=FORMAL / "logs" / "test262-full"
    )
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument(
        "--smoke",
        action="store_true",
        help="process at most two target tests with shards of at most two",
    )
    args = parser.parse_args(argv)
    if args.indices is not None:
        try:
            parsed_indices = [
                int(value.strip())
                for value in args.indices.split(",")
                if value.strip()
            ]
        except ValueError:
            parser.error("--indices must be a comma-separated list of integers")
        if not parsed_indices:
            parser.error("--indices must contain at least one index")
        if any(index < 0 for index in parsed_indices):
            parser.error("--indices values must be non-negative")
        if len(set(parsed_indices)) != len(parsed_indices):
            parser.error("--indices must not contain duplicates")
        if args.prefix is not None or args.start != 0 or args.count is not None:
            parser.error("--indices is exclusive with --prefix, --start, and --count")
        args.indices = sorted(parsed_indices)
    if args.export_jobs is None:
        args.export_jobs = args.jobs
    positive = (
        "shard_size",
        "jobs",
        "export_jobs",
        "fuel",
        "export_timeout",
        "build_timeout",
        "run_timeout",
    )
    for name in positive:
        if getattr(args, name) <= 0:
            parser.error(f"--{name.replace('_', '-')} must be positive")
    if args.start < 0 or (args.count is not None and args.count < 0):
        parser.error("--start and --count must be non-negative")
    if args.smoke:
        if args.indices is not None:
            args.indices = args.indices[:2]
        else:
            args.count = min(args.count if args.count is not None else 2, 2)
        args.shard_size = min(args.shard_size, 2)
    return args


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv if argv is not None else sys.argv[1:])
    state_dir = args.state_dir.resolve()

    # Export paths remain shared, so one repository-wide lock excludes other
    # campaigns.  The JVM and extracted core are each started once; persistent
    # processes then exchange immutable per-test payloads.
    with RunLock(FORMAL / "build" / "test262-full.lock"):
        fingerprint = bootstrap_inventory(args)
        pool = load_inventory(INVENTORY, args.prefix)
        if args.indices is not None:
            by_global_index = {target.global_index: target for target in pool}
            missing = [
                index for index in args.indices if index not in by_global_index
            ]
            if missing:
                raise ValueError(
                    "requested indices are not in the target pool: "
                    + ",".join(map(str, missing))
                )
            selection = [by_global_index[index] for index in args.indices]
        else:
            end = (
                len(pool)
                if args.count is None
                else min(len(pool), args.start + args.count)
            )
            selection = pool[args.start:end]
        ensure_campaign(
            state_dir,
            selection,
            args,
            fingerprint,
            create=not args.dry_run,
        )
        completed = set(read_results(state_dir / "results", selection))
        ranges = pending_ranges(selection, completed, args.shard_size)

        print(
            f"pool={len(pool)} selected={len(selection)} completed="
            f"{sum(t.global_index in completed for t in selection)} "
            f"pending={sum(map(len, ranges))} shards={len(ranges)} "
            f"jobs={args.jobs} export-jobs={args.export_jobs}"
        )
        if args.dry_run:
            for group in ranges:
                print(
                    f"offset={group[0].pool_offset} count={len(group)} "
                    f"global={group[0].global_index}..{group[-1].global_index}"
                )
            return 0

        state_dir.mkdir(parents=True, exist_ok=True)
        write_aggregate(state_dir, selection)
        if not ranges:
            return 0
        ensure_source_unchanged(
            state_dir, fingerprint, "before payload worker build"
        )
        worker_executable = build_payload_worker(args, state_dir)
        exporter_classpath = resolve_exporter_classpath(
            args, state_dir, fingerprint
        )
        ensure_source_unchanged(
            state_dir, fingerprint, "after runtime builds"
        )
        with (
            PersistentExporter(
                args.prefix,
                exporter_classpath,
                args.export_jobs,
                len(pool),
                state_dir / "logs",
                args.export_timeout,
            ) as exporter,
            PayloadWorkerPool(
                args.jobs,
                state_dir / "logs",
                min(120.0, args.build_timeout),
                worker_executable,
            ) as workers,
        ):
            for group in ranges:
                ensure_source_unchanged(
                    state_dir, fingerprint, "before shard execution"
                )
                execute_range(group, args, state_dir, exporter, workers)
                ensure_source_unchanged(
                    state_dir, fingerprint, "after shard execution"
                )
                write_aggregate(state_dir, selection)
        write_aggregate(state_dir, selection)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (csv.Error, KeyError, OSError, RuntimeError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(2)
