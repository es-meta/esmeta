#!/usr/bin/env python3

# python3 experiment/rq1_table.py [--draft] [--tex ../synth262-paper/fse27/numbers/rq1_table.tex]
#
# The table of RQ1: each test suite's time, branch coverage, programs, and
# defects, as five-run means over experiment/data. rq1_venn.py and
# rq2_table.py share the helpers here.
#
# A defect is a row of experiment/bugs.json. A failure on the frozen engines
# counts as the row experiment/conform/triage.json gives it; `fp:` and `new:`
# do not count. --draft adds unseen failures to the ledger as `?` guesses for a
# person to confirm. data.sh unpack restores the conform logs; to redo them on
# Linux x86-64, the platform they were made on:
#
#   experiment/engine-install.sh    # the frozen engines, in ~/frozen
#   export ESMETA_HOME=$PWD JAVA_OPTS="-Xmx32g -Xss4m -Duser.home=$HOME/frozen/home"
#   for r in experiment/data/solve-? experiment/data/fuzz-?; do
#     bin/esmeta conform-test $r/injected -conform-test:injected -conform-test:out=$r/conform-interaction.json
#   done
# The conform-programs/minimal reports are the original-test subset of that run.


from __future__ import annotations

import argparse
import json
import os
import re
import statistics
import sys
from pathlib import Path

HOME = Path(os.environ.get("ESMETA_HOME", Path(__file__).resolve().parents[1]))
DATA = HOME / "experiment" / "data"
TABLE = HOME / "experiment" / "bugs.json"
BUGS = HOME / "bugs"
LEDGER = HOME / "experiment" / "conform" / "triage.json"

# the log of each oracle: final-state assertions only, and the interaction-based one
PLAIN = {"solve": "conform-programs.json", "fuzz": "conform-minimal.json"}
INTERACTION = "conform-interaction.json"
NOT_A_BUG = {"invalid"}
UNCOUNTED = ("fp:", "new:")
# the line that ends the logging-proxy prelude of an interaction test
PRELUDE_END = "  return L;\n})();\n"

# conform-test engine id -> row name prefix
PREFIX = {
    "v8": "v8",
    "javascriptcore": "jsc",
    "graaljs": "graal",
    "spidermonkey": "sm",
    "xs": "xs",
    "quickjs": "qjs",
}
WORD_RE = re.compile(r"[A-Za-z_$][\w$]*")
DECL_RE = re.compile(r"\b(?:var|let|const|function|class)\s+([A-Za-z_$][\w$]*)")
KEYWORDS = {"var", "let", "const", "function", "class", "new", "typeof", "undefined", "use", "strict"}
BRANCH_RE = re.compile(r"\bBranch\[(\d+)\]:(T|F)\b")


# ------------------------------------------------------------------------------
# runs


def runs(prefix: str) -> list[Path]:
    found = sorted(p for p in DATA.glob(f"{prefix}-*") if p.is_dir())
    if not found:
        raise FileNotFoundError(f"no {prefix} runs under {DATA}; run experiment/data.sh unpack")
    return found


def seconds(text: str) -> int:
    parts = [int(p) for p in text.split(":")]
    return sum(p * 60**i for i, p in enumerate(reversed(parts)))


def solver_summary(run: Path) -> dict[str, int]:
    """solving time, covered targets, and branch coverage (targets plus the
    sides the programs reach outside them)"""
    text = (run / "summary").read_text(encoding="utf-8")
    found = re.search(r"Reached targets: (\d+)/(\d+)", text)
    reached, universe = int(found.group(1)), int(found.group(2))
    outside = int(re.search(r"Observed outside target set: (\d+)", text).group(1))
    return {
        "time": seconds(re.search(r"Solving: (\S+)", text).group(1)),
        "targets": reached,
        "universe": universe,
        "coverage": reached + outside,
    }


def fuzz_at(run: Path, hours: float) -> dict[str, int]:
    """branch coverage and minimal programs at the first row past `hours`"""
    lines = (run / "summary.tsv").read_text(encoding="utf-8").splitlines()
    head = lines[0].split("\t")
    rows = [dict(zip(head, line.split("\t"))) for line in lines[1:]]
    row = next((r for r in rows if int(r["time(ms)"]) >= hours * 3_600_000), rows[-1])
    return {"coverage": int(row["branch(#)"]), "programs": int(row["minimal(#)"])}


def programs(directory: Path) -> int | None:
    return len(list(directory.glob("*.js"))) if directory.is_dir() else None


def mean(values: list) -> float | None:
    """the mean, or None when a run lacks the number"""
    return None if not values or None in values else statistics.mean(values)


# ------------------------------------------------------------------------------
# defects


def bug_status() -> dict[str, str]:
    """row -> status, the rows upstream does not consider defects left out"""
    doc = json.loads(TABLE.read_text(encoding="utf-8"))
    return {n: status for status, names in doc["status"].items() if status not in NOT_A_BUG for n in names}


def bug_rows() -> set[str]:
    return set(bug_status())


def ledger_key(program: str) -> str:
    """the program as the ledger stores it: an interaction test without the
    logging-proxy prelude every such test shares"""
    head, sep, rest = program.partition(PRELUDE_END)
    return (rest if sep and "logState" in head else program).strip()


def failures(doc: dict) -> set[tuple[str, str]]:
    """(engine, program) per failure, divergences included"""
    out = set()
    for engine, result in doc["engines"].items():
        for bug in result["bugs"]:
            out.add((PREFIX[engine], ledger_key(bug["program"])))
    for d in doc.get("divergences", []):
        for engine in d["odd"]:
            out.add((PREFIX[engine], ledger_key(d["program"])))
    return out


def names(program: str) -> set[str]:
    """what a program refers to, less what it declares itself"""
    words = set(WORD_RE.findall(program))
    return words - set(DECL_RE.findall(program)) - KEYWORDS - {w for w in words if w.startswith("__res")}


def suggest(engine: str, program: str, stored: dict[str, str]) -> str:
    """the most specific row whose reproduction's names the program all uses"""
    # ponytail: word tokens, not a parse; a person confirms every guess
    used = names(program)
    fits = [
        (len(need), row)
        for row, text in stored.items()
        if row.startswith(engine + "-") and (need := names(text)) <= used
    ]
    return "?" + (max(fits)[1] if fits else "")


class Defects:
    """counts the rows each conform log reproduces through the triage ledger"""

    def __init__(self) -> None:
        self.rows = bug_rows()
        self.ledger: dict[str, dict[str, str]] = (
            json.loads(LEDGER.read_text(encoding="utf-8")) if LEDGER.is_file() else {}
        )
        self.seen: set[tuple[str, str]] = set()
        self.pending: set[tuple[str, str]] = set()

    def per_run(self, log: Path) -> int | None:
        """how many rows one log reproduces; None when the log is not there yet"""
        found = self.found(log)
        return None if found is None else len(found)

    def found(self, log: Path) -> set[str] | None:
        """the rows one log reproduces; None when the log is not there yet"""
        if not log.is_file():
            return None
        fs = failures(json.loads(log.read_text(encoding="utf-8")))
        self.seen |= fs
        found = set()
        for engine, program in fs:
            # one failure may show several defects, so a verdict is a row or a list of them
            verdicts = self.ledger.get(engine, {}).get(program, "?")
            if isinstance(verdicts, str):
                verdicts = [verdicts]
            if any(v.startswith("?") for v in verdicts):
                self.pending.add((engine, program))
            for verdict in verdicts:
                if not verdict.startswith(("?",) + UNCOUNTED):
                    if verdict not in self.rows:
                        raise ValueError(f"ledger names a row the table does not list: {verdict}")
                    found.add(verdict)
        return found

    def draft(self) -> int:
        """adds every unseen failure as a guess; returns how many"""
        stored = {
            p.stem: p.read_text(encoding="utf-8")
            for source in ("solver", "fuzzer")
            for p in (BUGS / source).glob("*.js")
            if p.stem in self.rows
        }
        added = 0
        for engine, program in sorted(self.seen):
            if program not in self.ledger.setdefault(engine, {}):
                self.ledger[engine][program] = suggest(engine, program, stored)
                added += 1
        LEDGER.parent.mkdir(parents=True, exist_ok=True)
        LEDGER.write_text(
            json.dumps(self.ledger, indent=1, sort_keys=True, ensure_ascii=False) + "\n",
            encoding="utf-8",
        )
        return added

    def group(self, logs: list[Path]) -> tuple[list[int | None], bool]:
        """rows per log, and whether any of their failures awaits triage"""
        before = len(self.pending)
        counts = [self.per_run(log) for log in logs]
        return counts, len(self.pending) > before or any(
            f in self.pending for log in logs if log.is_file()
            for f in failures(json.loads(log.read_text(encoding="utf-8")))
        )

    def report(self, draft: bool) -> None:
        if draft:
            print(f"\nadded {self.draft()} failure(s) to {LEDGER}; review every `?` entry")
        elif self.pending:
            print(f"\n* {len(self.pending)} failure(s) not triaged; run with --draft and review {LEDGER}")


# ------------------------------------------------------------------------------
# printing


def cell(value, pending: bool = False, time: bool = False) -> str:
    if value is None:
        return "?"
    if time:
        t = int(round(value))
        return f"{t // 3600}:{t // 60 % 60:02d}:{t % 60:02d}" if t >= 3600 else f"{t // 60}:{t % 60:02d}"
    text = f"{value:,.0f}" if float(value).is_integer() or value >= 100 else f"{value:,.1f}"
    return text + ("*" if pending else "")


def table(title: str, head: list[str], rows: list[list[str]]) -> None:
    widths = [max(len(r[i]) for r in [head] + rows) for i in range(len(head))]
    print(title)
    for r in [head] + rows:
        print("  " + r[0].ljust(widths[0]) + "".join(c.rjust(w + 3) for c, w in zip(r[1:], widths[1:])))


class Tex:
    """number macros for the paper, written to one generated file; a number
    that is missing or still awaits triage comes out red"""

    def __init__(self, source: str) -> None:
        self.source = source
        self.macros: dict[str, str] = {}

    def put(self, name: str, text: str | None, pending: bool = False) -> None:
        if text is None:
            text = "?"
            pending = True
        self.macros[name] = f"\\inred{{{text}}}" if pending else text

    def count(self, name: str, value: float | None, pending: bool = False) -> None:
        self.put(name, None if value is None else f"{round(value):,}", pending)

    def pct(self, name: str, part: float | None, whole: int) -> None:
        self.put(name, None if part is None else f"{100 * part / whole:.1f}\\%")

    def time(self, name: str, seconds: float | None) -> None:
        if seconds is None:
            return self.put(name, None)
        t = round(seconds)
        self.put(name, f"{t // 60}\\,min {t % 60}\\,s" if t < 3600 else f"{t / 3600:g}\\,h")

    def write(self, path: Path | None) -> None:
        if path is None:
            return
        lines = [f"% generated by esmeta experiment/{self.source}; do not edit, rerun it"]
        lines += [f"\\newcommand{{\\{n}}}{{{v}\\xspace}}" for n, v in self.macros.items()]
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("\n".join(lines) + "\n", encoding="utf-8")
        print(f"wrote {len(self.macros)} macros to {path}")


# ------------------------------------------------------------------------------
# RQ1


def covered(path: Path) -> set[tuple[int, str]]:
    out = set()
    for entry in json.loads(path.read_text(encoding="utf-8")):
        cond = entry["condView"]["cond"]
        branch = cond["branch"]
        branch = branch.get("id") if isinstance(branch, dict) else branch
        out.add((int(branch), "T" if cond["cond"] else "F"))
    return out


def main() -> int:
    parser = argparse.ArgumentParser(description="the table of RQ1")
    parser.add_argument("--draft", action="store_true", help="add untriaged failures to the ledger")
    parser.add_argument("--tex", type=Path, help="write the paper's number macros here")
    args = parser.parse_args()
    defects = Defects()

    solve, fuzz = runs("solve"), runs("fuzz")
    s = [solver_summary(r) for r in solve]
    plain, p1 = defects.group([r / PLAIN["solve"] for r in solve])
    aware, p2 = defects.group([r / INTERACTION for r in solve])
    fplain, p3 = defects.group([r / PLAIN["fuzz"] for r in fuzz])
    faware, p4 = defects.group([r / INTERACTION for r in fuzz])
    test262 = covered(DATA / "test262" / "branch-coverage.json")

    rows = [
        ["Synth262", cell(None), cell(mean([x["coverage"] for x in s])),
         cell(mean([programs(r / "injected") for r in solve])),
         cell(mean(aware), p2)],
        ["  w/o interaction-based", cell(mean([x["time"] for x in s]), time=True),
         cell(mean([x["coverage"] for x in s])), cell(mean([programs(r / "programs") for r in solve])),
         cell(mean(plain), p1)],
    ]
    for hours in (1, 10, 50):
        at = [fuzz_at(r, hours) for r in fuzz]
        rows.append([f"JEST {hours} h", cell(hours * 3600, time=True),
                     cell(mean([x["coverage"] for x in at])), cell(mean([x["programs"] for x in at])),
                     cell(mean(fplain), p3) if hours == 50 else "--"])
    rows.append(["  w/ interaction-based", cell(50 * 3600, time=True),
                 cell(mean([fuzz_at(r, 50)["coverage"] for r in fuzz])),
                 cell(mean([programs(r / "injected") for r in fuzz])),
                 cell(mean(faware), p4)])
    rows.append(["Test262", "--", cell(len(test262)), "?", "?"])
    table("RQ1: five-run means (time of Synth262 = solving + interaction-based oracle, not logged)",
          ["", "time", "coverage", "programs", "defects"], rows)
    available = sum(x is not None for x in aware + faware)
    if available < len(aware) + len(faware):
        print(f"  interaction-based logs present in {available} of {len(aware) + len(faware)} runs")

    tex = Tex("rq1_table.py")
    tex.count("NumSynthCoverageRuns", len(solve))
    tex.time("SolveSearchTime", mean([x["time"] for x in s]))
    tex.count("NumSynthCoveredAll", mean([x["coverage"] for x in s]))
    tex.count("NumSynthPrograms", mean([programs(r / "programs") for r in solve]))
    tex.count("NumSynthProgramsOracle", mean([programs(r / "injected") for r in solve]))
    tex.count("NumBugsPlainMean", mean(plain), p1)
    tex.count("NumBugsOracleMean", mean(aware), p2)
    # rows only the interaction-based oracle reproduces, per run
    only = [
        None if a is None or p is None else len(a - p)
        for a, p in zip((defects.found(r / INTERACTION) for r in solve), (defects.found(r / PLAIN["solve"]) for r in solve))
    ]
    tex.count("NumInteractionOnlyBugs", mean(only), p1 or p2)
    for hours, name in ((1, "OneHour"), (10, "TenHours"), (50, "All")):
        at = [fuzz_at(r, hours) for r in fuzz]
        tex.count(f"NumFuzzCovered{name}", mean([x["coverage"] for x in at]))
        tex.count(f"NumFuzzPrograms{name}", mean([x["programs"] for x in at]))
    tex.count("NumFuzzProgramsOracle", mean([programs(r / "injected") for r in fuzz]))
    tex.count("NumBugsFuzzPlainMean", mean(fplain), p3)
    tex.count("NumBugsFuzzOracleMean", mean(faware), p4)
    tex.count("NumTestCoveredAll", len(test262))
    tex.write(args.tex)

    defects.report(args.draft)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (FileNotFoundError, ValueError) as err:
        print(f"error: {err}", file=sys.stderr)
        raise SystemExit(1)
