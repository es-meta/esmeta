import importlib.util
import sys
import tempfile
import unittest
from pathlib import Path


FORMATTER = Path(__file__).parents[1] / "tools" / "format_itree_shape.py"
SPEC = importlib.util.spec_from_file_location("format_itree_shape", FORMATTER)
formatter = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = formatter
SPEC.loader.exec_module(formatter)


class FormatITreeShapeTest(unittest.TestCase):
    def test_tau_run_reports_covered_execution_steps(self):
        raw = """ESMetaFV closed ITree shape dump
program: T000  built-ins/Array/isArray/example.js
instruction markers enabled for IR function: RunJobs

        12  Tau x 20
        32  Vis IO(esmeta.trace.enter, ScriptEvaluation)
... truncated after 2 shape lines at execution step 33 ...
"""
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            raw_path = work / "trace.log"
            raw_path.write_text(raw, encoding="utf-8")

            trace = formatter.parse_raw_trace(raw_path)
            report = formatter.render_report(
                trace,
                work / "missing-Spec.v",
                work / "missing-test262",
                [],
                False,
            )

        self.assertEqual(trace.truncated_at, 33)
        self.assertEqual(formatter.terminal_summary(trace)[0], "INCOMPLETE")
        self.assertIn("silent internal steps 12..31", report)
        self.assertIn("next recorded event is step 32", report)
        self.assertIn("not milliseconds", report)
        self.assertIn("not N JavaScript or IR instructions", report)

    def test_terminal_return_and_balanced_call_are_recognized(self):
        raw = """ESMetaFV closed ITree shape dump
program: T007  language/example.js

         0  Vis IO(esmeta.trace.enter, <entry>)
         1  Tau x 3
         4  Vis IO(esmeta.trace.exit, <entry>)
         5  Ret(VUndef)
"""
        with tempfile.TemporaryDirectory() as directory:
            raw_path = Path(directory) / "trace.log"
            raw_path.write_text(raw, encoding="utf-8")
            trace = formatter.parse_raw_trace(raw_path)

        self.assertEqual(formatter.terminal_summary(trace)[0], "COMPLETED")
        self.assertEqual(trace.call_roots[0].name, "<entry>")
        self.assertEqual(trace.call_roots[0].span, 5)
        self.assertEqual(sum(event.tau_count for event in trace.events), 3)

    def test_static_ir_preserves_sequence_branch_and_loop_paths(self):
        spec_source = (
            'mkFunc false "RunJobs" nil None '
            '(ISeq ((ICall (LName "x") (ERef (Ref (VGlobal "Foo") nil)) nil) '
            ':: (IIf (EBool true) (IReturn EUndef) '
            '(IWhile (EBool true) INop)) :: nil)).\n'
        )
        with tempfile.TemporaryDirectory() as directory:
            spec_path = Path(directory) / "Spec.v"
            spec_path.write_text(spec_source, encoding="utf-8")

            result = formatter.static_ir(spec_path, "RunJobs")

        self.assertIsNotNone(result)
        _, instructions = result
        self.assertEqual(
            [(item.path, item.kind) for item in instructions],
            [
                ("body", "ISeq"),
                ("body.0", "ICall"),
                ("body.1", "IIf"),
                ("body.1.then", "IReturn"),
                ("body.1.else", "IWhile"),
                ("body.1.else.body", "INop"),
            ],
        )


if __name__ == "__main__":
    unittest.main()
