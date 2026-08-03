import importlib.util
import sys
import tempfile
import unittest
from pathlib import Path


SCRIPT = Path(__file__).parents[1] / "tools" / "benchmark_test262_exporter.py"
SPEC = importlib.util.spec_from_file_location("benchmark_test262_exporter", SCRIPT)
benchmark = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = benchmark
SPEC.loader.exec_module(benchmark)


class BenchmarkTest262ExporterTest(unittest.TestCase):
    def test_defaults(self):
        args = benchmark.parse_args([])

        self.assertEqual(args.offset, 0)
        self.assertEqual(args.count, 160)
        self.assertEqual(args.parallel_jobs, 12)
        self.assertEqual(args.min_speedup, 2.0)

    def test_snapshot_comparison_checks_manifest_names_and_bytes(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            serial = self._snapshot(root / "serial", b"same", {"T000.fvt": b"a"})
            parallel = self._snapshot(
                root / "parallel", b"same", {"T000.fvt": b"a"}
            )
            self.assertEqual(benchmark.compare_snapshots(serial, parallel), [])

            (parallel.payload_dir / "T000.fvt").write_bytes(b"b")
            self.assertIn(
                "payload bytes differ: T000.fvt",
                benchmark.compare_snapshots(serial, parallel),
            )

    def test_snapshot_comparison_reports_disposition_difference(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            serial = self._snapshot(root / "serial", b"serial", {})
            parallel = self._snapshot(root / "parallel", b"parallel", {})
            serial = benchmark.ExportSnapshot(
                serial.root,
                serial.manifest,
                serial.payload_dir,
                serial.payload_names,
                benchmark.Counter({"EMITTED": 1}),
            )

            errors = benchmark.compare_snapshots(serial, parallel)

            self.assertIn("manifest bytes differ", errors)
            self.assertTrue(
                any(error.startswith("disposition counts differ") for error in errors)
            )

    @staticmethod
    def _snapshot(root: Path, manifest: bytes, payloads: dict[str, bytes]):
        payload_dir = root / "payload"
        payload_dir.mkdir(parents=True)
        manifest_path = root / "test262-shard.tsv"
        manifest_path.write_bytes(manifest)
        for name, content in payloads.items():
            (payload_dir / name).write_bytes(content)
        return benchmark.ExportSnapshot(
            root,
            manifest_path,
            payload_dir,
            tuple(sorted(payloads)),
            benchmark.Counter(),
        )


if __name__ == "__main__":
    unittest.main()
