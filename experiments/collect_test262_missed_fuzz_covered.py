#!/usr/bin/env python3
import argparse
import json
import os
import re
import sys
from collections import defaultdict
from pathlib import Path

# base directory paths
ESMETA_HOME = (
    Path(os.getenv("ESMETA_HOME") or Path(__file__).resolve().parent.parent)
    .expanduser()
    .resolve()
)
TEST262TEST_LOG_DIR = ESMETA_HOME / "logs" / "test262" / "recent"
FUZZ_LOG_DIR = ESMETA_HOME / "logs" / "fuzz" / "recent"

# input data paths
FUZZ_BRANCH_COVERAGE_PATH = FUZZ_LOG_DIR / "branch-coverage.json"
FUZZ_MINIMAL_DIR = FUZZ_LOG_DIR / "minimal"
TEST262_TARGET_CONDS_PATH = TEST262TEST_LOG_DIR / "target-conds.json"
TEST262_BRANCH_COVERAGE_PATH = TEST262TEST_LOG_DIR / "branch-coverage.json"

# -------------------------
# helper functions
# -------------------------


def load_json(p: Path):
    return json.load(p.open("r", encoding="utf-8"))


def js_one_lining(text: str) -> str:
    text = text.lstrip("\ufeff")  # remove BOM (UTF-8 with BOM)
    text = text.replace("\r\n", "\n").replace("\r", "\n")  # unify linebreak to LF
    text = text.replace("\n", " ").replace("\t", " ")  # one lining
    text = re.sub(r"\s+", " ", text)  # unify whitespaces
    text = text.strip()  # trim
    text = re.sub(r"^(?:(?:\"use strict\")\s*;?)\s*", "", text)  # remove "use strict"
    return text.strip()  # trim


def extract_branch_coverage(entry: dict):
    try:  # get branch id
        branch = int(entry["condView"]["cond"]["branch"])
    except Exception:
        branch = None

    try:  # get cond branch (true or false)
        cond = bool(entry["condView"]["cond"]["cond"])
    except Exception:
        cond = None

    try:  # get script filename (in minimal directory)
        script = str(entry["script"])
    except Exception:
        script = None

    return branch, cond, script


def read_js(minimal_dir: Path, script_name: str) -> tuple[str, bool]:
    p = minimal_dir / script_name
    if not p.exists() or not p.is_file():
        return f"<MISSING {p.as_posix()}>", False
    try:
        return p.read_text(encoding="utf-8", errors="replace"), True
    except Exception as e:
        return f"<READ_ERROR {p.as_posix()} : {e}>", False


def build_fuzz_code_map(
    fuzz_entries: list[dict], target_set: set[int], minimal_dir: Path
):
    fuzz = defaultdict(dict)

    for e in fuzz_entries:
        if not isinstance(e, dict):
            continue

        branch, cond, script = extract_branch_coverage(e)
        if branch is None or cond is None or script is None:
            continue
        if branch not in target_set:
            continue

        js_raw, ok = read_js(minimal_dir, script)
        if not ok:
            continue

        code = js_one_lining(js_raw).strip()
        if not code:
            continue

        fuzz[branch].setdefault(cond, code)
    return fuzz


def build_test262_cov(entries: list[dict]):
    cov = defaultdict(lambda: {True: [], False: []})
    for e in entries:
        try:
            b = int(e["condView"]["cond"]["branch"])
            cond = bool(e["condView"]["cond"]["cond"])
            script = str(e["script"])
        except Exception:
            continue
        cov[b][cond].append(script)
    return cov


def annotate_summary_only_gaps(
    fuzz: dict[int, dict[bool, str]], test262_entries: list[dict]
) -> str:
    test262 = build_test262_cov(test262_entries)

    lines: list[str] = []

    for b in sorted(fuzz.keys()):
        if True not in fuzz[b] or False not in fuzz[b]:
            continue

        code_t = fuzz[b][True].strip()
        code_f = fuzz[b][False].strip()
        if not code_t or not code_f:
            continue

        t_scripts = test262[b][True]
        f_scripts = test262[b][False]

        if t_scripts and f_scripts:
            continue

        lines.append(f"Branch[{b}]:T : {code_t}")
        lines.append(f"Branch[{b}]:F : {code_f}")

        local_prefix = (ESMETA_HOME / "tests").as_posix().rstrip("/") + "/"

        if t_scripts:
            refined_path = t_scripts[0].replace("\\", "/").removeprefix(local_prefix)
            lines.append(
                f'Branch[{b}]   : Test262 covered true branch with "{refined_path}"'
            )
        elif f_scripts:
            refined_path = f_scripts[0].replace("\\", "/").removeprefix(local_prefix)
            lines.append(
                f'Branch[{b}]   : Test262 covered false branch with "{refined_path}"'
            )
        else:
            lines.append(
                f"Branch[{b}]   : Test262 covered neither true nor false branch"
            )

        lines.append("")

    return "\n".join(lines).rstrip() + "\n"


# -------------------------
# entry function
# -------------------------


def main():
    ap = argparse.ArgumentParser(
        description="Collect branches where fuzz covers both sides but Test262 does not, and emit annotated summary."
    )
    ap.add_argument(
        "-o",
        "--output",
        type=Path,
        default=Path("-"),
        help="Output file path (default: stdout). Use '-' for stdout.",
    )
    args = ap.parse_args()

    # Load test262 target conds
    if not TEST262_TARGET_CONDS_PATH.exists():
        print(f"ERROR: not found: {TEST262_TARGET_CONDS_PATH}", file=sys.stderr)
        raise SystemExit(2)

    targets = load_json(TEST262_TARGET_CONDS_PATH)
    if not isinstance(targets, list) or not all(isinstance(x, int) for x in targets):
        print(
            "ERROR: target-conds.json must be a JSON list of integers (branch numbers).",
            file=sys.stderr,
        )
        raise SystemExit(2)
    target_set = set(targets)

    # Load fuzz branch coverage
    if not FUZZ_BRANCH_COVERAGE_PATH.exists():
        print(f"ERROR: not found: {FUZZ_BRANCH_COVERAGE_PATH}", file=sys.stderr)
        raise SystemExit(2)

    fuzz_cov = load_json(FUZZ_BRANCH_COVERAGE_PATH)
    if not isinstance(fuzz_cov, list):
        print("ERROR: branch-coverage.json must be a JSON list.", file=sys.stderr)
        raise SystemExit(2)

    fuzz_map = build_fuzz_code_map(fuzz_cov, target_set, FUZZ_MINIMAL_DIR)

    # Load test262 branch coverage
    if not TEST262_BRANCH_COVERAGE_PATH.exists():
        print(f"ERROR: not found: {TEST262_BRANCH_COVERAGE_PATH}", file=sys.stderr)
        raise SystemExit(2)

    test262_entries = load_json(TEST262_BRANCH_COVERAGE_PATH)
    if not isinstance(test262_entries, list):
        print(
            "ERROR: test262-branch-coverage.json must be a JSON list.", file=sys.stderr
        )
        raise SystemExit(2)

    final_s = annotate_summary_only_gaps(fuzz_map, test262_entries)

    if str(args.output) == "-":
        sys.stdout.write(final_s)
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(final_s, encoding="utf-8")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
