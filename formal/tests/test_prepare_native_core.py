import importlib.util
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch


SCRIPT = Path(__file__).resolve().parents[1] / "prepare-native-core.py"
SPEC = importlib.util.spec_from_file_location("prepare_native_core", SCRIPT)
assert SPEC is not None and SPEC.loader is not None
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class SplitSpecModuleTest(unittest.TestCase):
    def test_generated_spec_data_is_replaced_by_snapshot(self) -> None:
        for name in (
            "Spec.ml",
            "Spec.mli",
            "SpecFuncs.ml",
            "SpecFuncs_0092.mli",
            "SpecGlobals.ml",
            "SpecHeap.ml",
            "SpecHeap_0083.mli",
        ):
            with self.subTest(name=name):
                self.assertTrue(MODULE.is_split_spec_module(Path(name)))

    def test_non_spec_runtime_modules_are_preserved(self) -> None:
        for name in (
            "Fragment.ml",
            "SpecAlgorithmITree.ml",
            "SpecFuncs_backup.ml",
            "SpecFuncs_00000.ml",
            "SpecHeap_0083_extra.mli",
            "SpecHeap_083.mli",
            "TyModelBindings00.ml",
        ):
            with self.subTest(name=name):
                self.assertFalse(MODULE.is_split_spec_module(Path(name)))

    def test_snapshot_facades_preserve_extracted_module_owners(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "core"
            output = root / "native-core"
            source.mkdir()
            (source / ".extracted").touch()
            (source / "Fragment.ml").write_text("runtime\n", encoding="utf-8")
            (source / "SpecAlgorithmITree.ml").write_text(
                "open SpecFuncs\nopen SpecGlobals\nopen SpecHeap\n",
                encoding="utf-8",
            )
            for name in (
                "Spec.ml",
                "SpecFuncs.ml",
                "SpecFuncs_0000.ml",
                "SpecGlobals.ml",
                "SpecHeap.ml",
                "SpecHeap_0000.ml",
            ):
                (source / name).write_text("generated data\n", encoding="utf-8")

            with patch.object(MODULE, "SOURCE", source), patch.object(
                MODULE, "OUTPUT", output
            ):
                self.assertEqual(MODULE.main(), 0)

            self.assertTrue((output / "SpecAlgorithmITree.ml").is_file())
            self.assertFalse((output / "SpecFuncs_0000.ml").exists())
            self.assertFalse((output / "SpecHeap_0000.ml").exists())
            for name, contents in MODULE.SPEC_COMPAT_MODULES.items():
                with self.subTest(name=name):
                    self.assertEqual(
                        (output / name).read_text(encoding="utf-8"), contents
                    )


if __name__ == "__main__":
    unittest.main()
