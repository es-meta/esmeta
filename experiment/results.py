"""Shared readers, defect matching, and LaTeX output for experiment results."""

from __future__ import annotations

import json
import os
import re
import statistics
from pathlib import Path

HOME = Path(os.environ.get("ESMETA_HOME", Path(__file__).resolve().parents[1]))
DATA = HOME / "experiment" / "data"
TABLE = HOME / "experiment" / "bugs.json"
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
        "templates": int(re.search(r"Template generation: \d+ entries, (\d+) templates", text)[1]),
        **{status: int(count) for status, count in re.findall(
            r"^  (pass|fail-verify|fail-reify|unsolved|timeout|error)\s+(\d+)", text, re.M)},
    }


def fuzz_at(run: Path, hours: float) -> dict[str, int]:
    """branch coverage and minimal programs at the first row past `hours`"""
    lines = (run / "summary.tsv").read_text(encoding="utf-8").splitlines()
    head = lines[0].split("\t")
    rows = [dict(zip(head, line.split("\t"))) for line in lines[1:]]
    row = next((r for r in rows if int(r["time(ms)"]) >= hours * 3_600_000), None)
    if row is None:
        raise ValueError(f"{run}: no coverage measurement at {hours:g} hours")
    return {"coverage": int(row["branch(#)"]), "programs": int(row["minimal(#)"])}


def programs(directory: Path) -> int | None:
    return len(list(directory.glob("*.js"))) if directory.is_dir() else None


def tests(run: Path) -> int | None:
    """the tests the interaction-based conform run injected"""
    log = run / INTERACTION
    # Older reports may omit the test count.
    count = json.loads(log.read_text(encoding="utf-8")).get("tests") if log.is_file() else None
    return count if count is not None else programs(run / "injected")


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
    # Guesses use tokens; every suggested classification requires review.
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
        self.pending_by_log: dict[Path, set[tuple[str, str]]] = {}

    def per_run(self, log: Path) -> int | None:
        """how many rows one log reproduces; None when the log is not there yet"""
        found = self.found(log)
        return None if found is None else len(found)

    def found(self, log: Path) -> set[str] | None:
        """the rows one log reproduces; None when the log is not there yet"""
        self.pending_by_log[log] = set()
        if log.is_file():
            doc = json.loads(log.read_text(encoding="utf-8"))
        elif log.name in PLAIN.values() and (log.parent / INTERACTION).is_file():
            # An interaction run also executes the originals; reuse those results.
            doc = json.loads((log.parent / INTERACTION).read_text(encoding="utf-8"))
            directory = "programs" if log.name == PLAIN["solve"] else "minimal"
            source = log.parent / directory
            if not source.is_dir():
                raise FileNotFoundError(f"missing original programs: {source}")
            originals = {p.read_text().strip() for p in source.glob("*.js")}
            if not originals:
                raise ValueError(f"no original JavaScript programs in {source}")
            for result in doc["engines"].values():
                result["bugs"] = [b for b in result["bugs"] if b["program"].strip() in originals]
            doc["divergences"] = [d for d in doc.get("divergences", []) if d["program"].strip() in originals]
        else:
            return None
        fs = failures(doc)
        self.seen |= fs
        found = set()
        for engine, program in fs:
            # one failure may show several defects, so a verdict is a row or a list of them
            verdicts = self.ledger.get(engine, {}).get(program, "?")
            if isinstance(verdicts, str):
                verdicts = [verdicts]
            if any(v.startswith("?") for v in verdicts):
                self.pending.add((engine, program))
                self.pending_by_log[log].add((engine, program))
            for verdict in verdicts:
                if not verdict.startswith(("?",) + UNCOUNTED):
                    if verdict not in self.rows:
                        raise ValueError(f"ledger names a row the table does not list: {verdict}")
                    found.add(verdict)
        return found

    def draft(self) -> int:
        """adds every unseen failure as a guess; returns how many"""
        # the shortest program the ledger already gives each row
        stored: dict[str, str] = {}
        for programs in self.ledger.values():
            for program, verdicts in programs.items():
                for row in [verdicts] if isinstance(verdicts, str) else verdicts:
                    if row in self.rows and len(program) < len(stored.get(row, program + " ")):
                        stored[row] = program
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
        counts = [self.per_run(log) for log in logs]
        return counts, any(self.pending_by_log[log] for log in logs)

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
    """number macros for the paper, written to one generated file; a missing
    or untriaged number comes out as ?. The paper colors every generated number itself
    (\\genvalue in macros.tex), and untriaged failures are reported on stdout."""

    def __init__(self, source: str) -> None:
        self.source = source
        self.macros: dict[str, str] = {}

    def put(self, name: str, text: str | None, pending: bool = False) -> None:
        self.macros[name] = "?" if text is None or pending else text

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
        text = path.read_text() if path.exists() else ""
        for name, value in self.macros.items():
            pattern = r"^\\newcommand\{\\" + re.escape(name) + r"\}.*$"
            line = f"\\newcommand{{\\{name}}}{{{value}\\xspace}}"
            if re.search(pattern, text, re.M):
                text = re.sub(pattern, lambda _: line, text, flags=re.M)
            else:
                text += line + "\n"
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")
        print(f"wrote {len(self.macros)} macros to {path}")


def covered(path: Path) -> set[tuple[int, str]]:
    out = set()
    for entry in json.loads(path.read_text(encoding="utf-8")):
        cond = entry["condView"]["cond"]
        branch = cond["branch"]
        branch = branch.get("id") if isinstance(branch, dict) else branch
        out.add((int(branch), "T" if cond["cond"] else "F"))
    return out


def universe(run: Path) -> set[tuple[int, str]]:
    """Target sides listed in the solver's final summary."""
    text = (run / "summary").read_text(encoding="utf-8")
    return {(int(branch), side) for branch, side in BRANCH_RE.findall(text)}
