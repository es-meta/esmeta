import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
GENERATOR = ROOT / "formal" / "gen-extract-shard.sh"


class GenExtractShardTest(unittest.TestCase):
    def test_generates_ocaml_aggregate_from_checked_payload_modules(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            tests = work / "Tests.v"
            output = work / "ExtractShard.v"
            split = work / "drivers"
            tests.write_text(
                "From ESMetaFV.validation.itree Require Import T000 T007.\n",
                encoding="utf-8",
            )

            subprocess.run(
                [GENERATOR, tests, output, split],
                check=True,
                cwd=ROOT / "formal",
            )

            self.assertEqual(
                (split / "Tests.ml").read_text(encoding="utf-8"),
                "(* AUTO-GENERATED aggregate of Rocq-extracted "
                "Test262 payloads. *)\n"
                "let tests =\n"
                "  T000.test_000 ::\n"
                "  T007.test_007 ::\n"
                "  []\n",
            )


if __name__ == "__main__":
    unittest.main()
