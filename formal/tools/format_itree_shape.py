#!/usr/bin/env python3
"""Turn a raw ESMetaFV ITree shape dump into a reader-oriented report.

The extracted runner intentionally emits a small, lossless event vocabulary.
That is useful for debugging, but a long trace hides the relationship between
the Test262 source, the specification IR, and the closed ITree.  This tool adds
that missing presentation layer without changing the extracted semantics or
the active Test262 campaign fingerprint.
"""

from __future__ import annotations

import argparse
import re
import textwrap
from dataclasses import dataclass, field
from pathlib import Path
from typing import Iterable, Iterator, Sequence


FORMAL = Path(__file__).resolve().parents[1]
ROOT = FORMAL.parent
DEFAULT_SPEC = FORMAL / "validation" / "Spec.v"
DEFAULT_TEST262_ROOT = ROOT / "tests" / "test262" / "test"

NODE_RE = re.compile(r"^\s*(\d+)\s{2}(.+)$")
PROGRAM_RE = re.compile(r"^program:\s+(T\d+)\s+(.+)$")
TRACE_TARGET_RE = re.compile(
    r"^instruction markers enabled for IR function:\s+(.+)$"
)
ENTER_RE = re.compile(r"^Vis IO\(esmeta\.trace\.enter, (.+)\)$")
EXIT_RE = re.compile(r"^Vis IO\(esmeta\.trace\.exit, (.+)\)$")
TAU_RE = re.compile(r"^Tau x (\d+)$")
MARKER_RE = re.compile(
    r"^Vis IO\(esmeta\.print, \$ESMetaFV\.trace\.inst:(.+):([^:()]+)\)$"
)
TRUNCATED_RE = re.compile(
    r"^\.\.\. truncated after (\d+) shape lines at execution step (\d+) \.\.\.$"
)

INSTRUCTION_KINDS = {
    "INop",
    "ISeq",
    "IExpr",
    "ILet",
    "IAssign",
    "IIf",
    "IWhile",
    "ICall",
    "IReturn",
    "IAssert",
    "IPrint",
    "IPush",
    "IPop",
    "IExpand",
    "IDelete",
    "ISdoCall",
}

NOISY_CALLS = {
    "NormalCompletion",
    "Completion",
    "__APPEND_LIST__",
    "__FLAT_LIST__",
    "__NEW_OBJ__",
}


@dataclass(frozen=True)
class Event:
    step: int
    description: str
    raw: str
    tau_count: int = 0
    call_action: str | None = None
    call_name: str | None = None
    marker_path: str | None = None
    marker_kind: str | None = None


@dataclass
class CallNode:
    name: str
    start: int
    parent: CallNode | None = None
    end: int | None = None
    children: list[CallNode] = field(default_factory=list)

    @property
    def span(self) -> int | None:
        if self.end is None:
            return None
        return self.end - self.start + 1

    def ancestry(self) -> list[str]:
        names: list[str] = []
        node: CallNode | None = self
        while node is not None:
            names.append(node.name)
            node = node.parent
        names.reverse()
        return names


@dataclass
class RawTrace:
    path: Path
    header: list[str]
    test_id: str
    program: str
    trace_target: str | None
    events: list[Event]
    call_roots: list[CallNode]
    call_nodes: list[CallNode]
    max_call_depth: int
    truncated_at: int | None
    raw_text: str

    @property
    def terminal(self) -> Event | None:
        for event in reversed(self.events):
            if event.description.startswith(
                ("Ret(", "Vis Take", "Vis Choose", "Crash(")
            ):
                return event
        return None


@dataclass(frozen=True)
class StaticInstruction:
    path: str
    kind: str
    detail: str
    depth: int


Term = str | list["Term"]


def parse_event(step: int, description: str, raw: str) -> Event:
    if match := TAU_RE.match(description):
        return Event(step, description, raw, tau_count=int(match.group(1)))
    if match := ENTER_RE.match(description):
        return Event(
            step,
            description,
            raw,
            call_action="enter",
            call_name=match.group(1),
        )
    if match := EXIT_RE.match(description):
        return Event(
            step,
            description,
            raw,
            call_action="exit",
            call_name=match.group(1),
        )
    if match := MARKER_RE.match(description):
        return Event(
            step,
            description,
            raw,
            marker_path=match.group(1),
            marker_kind=match.group(2),
        )
    return Event(step, description, raw)


def build_call_tree(
    events: Sequence[Event],
) -> tuple[list[CallNode], list[CallNode], int]:
    roots: list[CallNode] = []
    nodes: list[CallNode] = []
    stack: list[CallNode] = []
    max_depth = 0

    for event in events:
        if event.call_action == "enter" and event.call_name is not None:
            parent = stack[-1] if stack else None
            node = CallNode(event.call_name, event.step, parent)
            if parent is None:
                roots.append(node)
            else:
                parent.children.append(node)
            nodes.append(node)
            stack.append(node)
            max_depth = max(max_depth, len(stack))
        elif event.call_action == "exit" and event.call_name is not None:
            matching = next(
                (
                    index
                    for index in range(len(stack) - 1, -1, -1)
                    if stack[index].name == event.call_name
                ),
                None,
            )
            if matching is not None:
                stack[matching].end = event.step
                del stack[matching:]

    return roots, nodes, max_depth


def parse_raw_trace(path: Path) -> RawTrace:
    raw_text = path.read_text(encoding="utf-8")
    header: list[str] = []
    events: list[Event] = []
    test_id = "<unknown>"
    program = "<unknown>"
    trace_target: str | None = None
    truncated_at: int | None = None

    for line in raw_text.splitlines():
        if match := PROGRAM_RE.match(line):
            test_id, program = match.groups()
        elif match := TRACE_TARGET_RE.match(line):
            trace_target = match.group(1)

        if match := NODE_RE.match(line):
            step = int(match.group(1))
            description = match.group(2)
            events.append(parse_event(step, description, line))
        elif match := TRUNCATED_RE.match(line):
            truncated_at = int(match.group(2))
        elif not events:
            header.append(line)

    roots, nodes, max_depth = build_call_tree(events)
    return RawTrace(
        path=path,
        header=header,
        test_id=test_id,
        program=program,
        trace_target=trace_target,
        events=events,
        call_roots=roots,
        call_nodes=nodes,
        max_call_depth=max_depth,
        truncated_at=truncated_at,
        raw_text=raw_text,
    )


def tokenize_term(source: str) -> list[str]:
    tokens: list[str] = []
    index = 0
    while index < len(source):
        char = source[index]
        if char.isspace():
            index += 1
        elif source.startswith("::", index):
            tokens.append("::")
            index += 2
        elif char in "()":
            tokens.append(char)
            index += 1
        elif char == '"':
            start = index
            index += 1
            while index < len(source):
                if source[index] != '"':
                    index += 1
                elif index + 1 < len(source) and source[index + 1] == '"':
                    index += 2
                else:
                    index += 1
                    break
            tokens.append(source[start:index])
        else:
            start = index
            while (
                index < len(source)
                and not source[index].isspace()
                and source[index] not in "()"
                and not source.startswith("::", index)
            ):
                index += 1
            tokens.append(source[start:index])
    return tokens


def parse_terms(tokens: Sequence[str]) -> list[Term]:
    def parse_one(index: int) -> tuple[Term, int]:
        token = tokens[index]
        if token != "(":
            return token, index + 1
        items: list[Term] = []
        index += 1
        while index < len(tokens) and tokens[index] != ")":
            item, index = parse_one(index)
            items.append(item)
        if index >= len(tokens):
            raise ValueError("unterminated parenthesized term")
        return items, index + 1

    terms: list[Term] = []
    index = 0
    while index < len(tokens):
        term, index = parse_one(index)
        terms.append(term)
    return terms


def head(term: Term) -> str | None:
    if isinstance(term, list) and term and isinstance(term[0], str):
        return term[0]
    return term if isinstance(term, str) else None


def atoms(term: Term) -> Iterator[str]:
    if isinstance(term, str):
        yield term
    else:
        for child in term:
            yield from atoms(child)


def quoted_values(term: Term) -> list[str]:
    return [
        atom[1:-1].replace('""', '"')
        for atom in atoms(term)
        if len(atom) >= 2 and atom.startswith('"') and atom.endswith('"')
    ]


def child_groups(term: Term) -> list[list[Term]]:
    if not isinstance(term, list):
        return []
    return [item for item in term if isinstance(item, list)]


def instruction_children(term: Term) -> list[tuple[str, Term]]:
    if not isinstance(term, list):
        return []
    kind = head(term)
    if kind == "ISeq" and len(term) >= 2:
        sequence = term[1]
        if not isinstance(sequence, list):
            return []
        children = [
            child
            for child in sequence
            if head(child) in INSTRUCTION_KINDS
        ]
        return [(str(index), child) for index, child in enumerate(children)]
    if kind == "IIf" and len(term) >= 4:
        branches: list[tuple[str, Term]] = []
        if head(term[2]) in INSTRUCTION_KINDS:
            branches.append(("then", term[2]))
        if head(term[3]) in INSTRUCTION_KINDS:
            branches.append(("else", term[3]))
        return branches
    if kind == "IWhile" and len(term) >= 3 and head(term[2]) in INSTRUCTION_KINDS:
        return [("body", term[2])]
    return []


def first_instruction(term: Term) -> list[Term] | None:
    if isinstance(term, list):
        if head(term) in INSTRUCTION_KINDS:
            return term
        for child in term:
            found = first_instruction(child)
            if found is not None:
                return found
    return None


def local_or_global(term: Term) -> str | None:
    flat = list(atoms(term))
    for marker, label in (("VGlobal", "global"), ("LName", "local")):
        if marker in flat:
            index = flat.index(marker)
            for atom in flat[index + 1 :]:
                if atom.startswith('"') and atom.endswith('"'):
                    return f"{label} {atom[1:-1]}"
    if "LTemp" in flat:
        index = flat.index("LTemp")
        for atom in flat[index + 1 :]:
            if atom.startswith('"') and atom.endswith('"'):
                return f"temp {atom[1:-1]}"
    return None


def short_expr(term: Term) -> str:
    kind = head(term)
    values = quoted_values(term)
    if kind is None:
        return "<term>"
    if kind in {"EUndef", "ENull"}:
        return {"EUndef": "undefined", "ENull": "null"}[kind]
    if kind == "EBool":
        flat = list(atoms(term))
        return flat[1] if len(flat) > 1 else "boolean"
    if kind == "EClo":
        return f"closure {values[0]}" if values else "closure"
    if kind == "ERecord":
        return f"new {values[0]} record" if values else "new record"
    if kind == "ERef":
        reference = local_or_global(term) or "reference"
        fields = values[1:] if values and values[0] in reference else values
        if fields:
            reference += "." + ".".join(fields[-2:])
        return reference
    if kind == "ETypeCheck":
        flat = list(atoms(term))
        expected = next(
            (atom[1:] for atom in reversed(flat) if atom.startswith("T")),
            "type",
        )
        reference = local_or_global(term) or "value"
        return f"{reference} is {expected}"
    if kind == "ESizeOf":
        return f"size({local_or_global(term) or 'value'})"
    if kind == "EBinary":
        flat = list(atoms(term))
        operator = flat[1] if len(flat) > 1 else "binary-op"
        refs: list[str] = []
        for child in child_groups(term):
            reference = local_or_global(child)
            if reference and reference not in refs:
                refs.append(reference)
        operands = ", ".join(refs[:2]) or "values"
        return f"{operator}({operands})"
    if kind == "EUnary":
        flat = list(atoms(term))
        operator = flat[1] if len(flat) > 1 else "unary-op"
        return f"{operator}(value)"
    if values:
        return f"{kind}({', '.join(values[:2])})"
    return kind


def short_instruction(term: Term) -> str:
    kind = head(term) or "<instruction>"
    if not isinstance(term, list):
        return kind
    if kind == "ICall" and len(term) >= 4:
        destination = local_or_global(term[1]) or "result"
        function = short_expr(term[2])
        return f"{destination} := call {function}"
    if kind == "ISdoCall" and len(term) >= 4:
        destination = local_or_global(term[1]) or "result"
        methods = quoted_values(term[3])
        method = methods[0] if methods else short_expr(term[3])
        return f"{destination} := SDO call {method}"
    if kind == "ILet" and len(term) >= 3:
        names = quoted_values(term[1])
        name = names[0] if names else "local"
        return f"let {name} := {short_expr(term[2])}"
    if kind == "IAssign" and len(term) >= 3:
        target = local_or_global(term[1]) or "target"
        return f"{target} := {short_expr(term[2])}"
    if kind == "IAssert" and len(term) >= 2:
        return f"assert {short_expr(term[1])}"
    if kind == "IIf" and len(term) >= 2:
        return f"if {short_expr(term[1])}"
    if kind == "IWhile" and len(term) >= 2:
        return f"while {short_expr(term[1])}"
    if kind == "IReturn" and len(term) >= 2:
        return f"return {short_expr(term[1])}"
    if kind == "IPop" and len(term) >= 4:
        destination = local_or_global(term[1]) or "value"
        source = short_expr(term[2])
        side = "front" if head(term[3]) == "true" else "back"
        return f"{destination} := pop {side} from {source}"
    if kind == "IPush" and len(term) >= 4:
        side = "front" if head(term[3]) == "true" else "back"
        return f"push {short_expr(term[1])} to {side} of {short_expr(term[2])}"
    if kind == "ISeq":
        return "sequence"
    if kind == "IExpr" and len(term) >= 2:
        return f"evaluate {short_expr(term[1])}"
    return kind


def find_function_line(spec_path: Path, function: str) -> tuple[int, str] | None:
    pattern = re.compile(
        rf'^\s*mkFunc\s+(?:true|false)\s+"{re.escape(function)}"(?:\s|$)'
    )
    with spec_path.open(encoding="utf-8") as stream:
        for number, line in enumerate(stream, 1):
            if pattern.search(line):
                return number, line.rstrip("\n")
    return None


def static_ir(
    spec_path: Path, function: str
) -> tuple[int, list[StaticInstruction]] | None:
    found = find_function_line(spec_path, function)
    if found is None:
        return None
    line_number, source = found
    terms = parse_terms(tokenize_term(source.rstrip().removesuffix(".")))
    body = first_instruction(terms)
    if body is None:
        return None

    result: list[StaticInstruction] = []

    def walk(term: Term, path: str, depth: int) -> None:
        kind = head(term) or "<instruction>"
        result.append(
            StaticInstruction(path, kind, short_instruction(term), depth)
        )
        for suffix, child in instruction_children(term):
            walk(child, f"{path}.{suffix}", depth + 1)

    walk(body, "body", 0)
    return line_number, result


def extract_description(source: str) -> str | None:
    match = re.search(r"(?m)^description:\s*(.+?)\s*$", source)
    return match.group(1) if match else None


def derived_focus_patterns(program: str) -> list[str]:
    stem = Path(program).stem
    candidates = re.findall(r"[A-Za-z][A-Za-z0-9]+", stem)
    parts = [part for part in program.split("/") if re.search(r"[A-Za-z]", part)]
    candidates.extend(parts[-3:])
    ignored = {
        "built",
        "ins",
        "prototype",
        "language",
        "expressions",
        "test",
        "tests",
        "js",
    }
    result: list[str] = []
    for candidate in candidates:
        normalized = re.sub(r"[^A-Za-z0-9]", "", candidate)
        if len(normalized) >= 4 and normalized.lower() not in ignored:
            if normalized.lower() not in {item.lower() for item in result}:
                result.append(normalized)
    return result


def format_span(node: CallNode) -> str:
    end = "?" if node.end is None else str(node.end)
    span = "?" if node.span is None else f"{node.span:,} steps"
    return f"[{node.start:,}..{end}] {span}"


def selected_children(node: CallNode, limit: int = 10) -> tuple[list[CallNode], int]:
    material = [
        child
        for child in node.children
        if child.name not in NOISY_CALLS
        and (child.span is None or child.span >= 20)
    ]
    if len(material) <= limit:
        return material, 0
    ranked = sorted(
        material,
        key=lambda child: child.span if child.span is not None else -1,
        reverse=True,
    )[:limit]
    keep = {id(child) for child in ranked}
    return [child for child in material if id(child) in keep], len(material) - limit


def phase_lines(root: CallNode | None) -> list[str]:
    if root is None:
        return ["  <no balanced function-entry trace found>"]
    lines = [f"  {root.name}  {format_span(root)}"]
    children, omitted = selected_children(root, limit=14)
    for child in children:
        lines.append(f"    -> {child.name}  {format_span(child)}")
        grandchildren, hidden = selected_children(child, limit=8)
        for grandchild in grandchildren:
            lines.append(
                f"       -> {grandchild.name}  {format_span(grandchild)}"
            )
        if hidden:
            lines.append(f"       ... {hidden} smaller/repetitive calls folded")
    if omitted:
        lines.append(f"    ... {omitted} smaller/repetitive calls folded")
    return lines


def focus_lines(
    trace: RawTrace, patterns: Sequence[str], limit: int = 20
) -> list[str]:
    lowered = [pattern.lower() for pattern in patterns]
    matches = [
        node
        for node in trace.call_nodes
        if any(pattern in node.name.lower() for pattern in lowered)
    ]
    lines: list[str] = []
    for node in matches[:limit]:
        path = " -> ".join(node.ancestry())
        lines.append(f"  {format_span(node)}")
        lines.extend(
            f"    {part}"
            for part in textwrap.wrap(path, width=100, subsequent_indent="  ")
        )
    if len(matches) > limit:
        lines.append(f"  ... {len(matches) - limit} more matching calls folded")
    return lines or ["  <no matching function calls observed>"]


def terminal_summary(trace: RawTrace) -> tuple[str, str]:
    terminal = trace.terminal
    if terminal is not None:
        description = terminal.description
        if description.startswith("Ret("):
            return "COMPLETED", f"normal return at step {terminal.step:,}: {description}"
        if description.startswith("Vis Take"):
            return "UNSUPPORTED", f"undefined behavior/Take at step {terminal.step:,}"
        if description.startswith("Vis Choose"):
            return "UNSUPPORTED", f"Choose at step {terminal.step:,}"
        return "CRASHED", f"{description} at step {terminal.step:,}"
    if trace.truncated_at is not None:
        return (
            "INCOMPLETE",
            f"shape limit reached at step {trace.truncated_at:,}; outcome was not observed",
        )
    return "UNKNOWN", "no terminal event was recorded"


def render_report(
    trace: RawTrace,
    spec_path: Path,
    test262_root: Path,
    focus: Sequence[str],
    include_raw: bool,
) -> str:
    state, outcome = terminal_summary(trace)
    tau_steps = sum(event.tau_count for event in trace.events)
    visible_events = sum(event.tau_count == 0 for event in trace.events)
    markers = [event for event in trace.events if event.marker_path is not None]
    tau_example_index = next(
        (
            index
            for index, event in enumerate(trace.events)
            if event.tau_count >= 10
        ),
        None,
    )
    if tau_example_index is None:
        tau_example_index = next(
            (
                index
                for index, event in enumerate(trace.events)
                if event.tau_count > 1
            ),
            None,
        )
    if tau_example_index is None:
        tau_example_index = next(
            (
                index
                for index, event in enumerate(trace.events)
                if event.tau_count > 0
            ),
            None,
        )
    tau_example: str | None = None
    if tau_example_index is not None:
        event = trace.events[tau_example_index]
        last_tau_step = event.step + event.tau_count - 1
        next_step = (
            trace.events[tau_example_index + 1].step
            if tau_example_index + 1 < len(trace.events)
            else None
        )
        next_record = (
            f"; the next recorded event is step {next_step:,}"
            if next_step is not None
            else ""
        )
        tau_example = (
            f"  Example: raw `{event.step} Tau x {event.tau_count}` means silent "
            f"internal steps {event.step:,}..{last_tau_step:,}{next_record}."
        )
    instruction_data = (
        static_ir(spec_path, trace.trace_target)
        if trace.trace_target and spec_path.is_file()
        else None
    )
    static_by_path = {
        item.path: item for item in instruction_data[1]
    } if instruction_data else {}

    source_path = test262_root / trace.program
    source = source_path.read_text(encoding="utf-8") if source_path.is_file() else None
    description = extract_description(source) if source else None
    patterns = list(focus) or derived_focus_patterns(trace.program)
    if "ScriptEvaluation" not in patterns:
        patterns.insert(0, "ScriptEvaluation")

    lines = [
        "ESMetaFV ITree execution — readable report",
        "=" * 44,
        f"Status       : {state} — {outcome}",
        f"Test         : {trace.test_id}  {trace.program}",
        f"Purpose      : {description or '<not found in Test262 metadata>'}",
        f"Trace target : {trace.trace_target or '<instruction markers disabled>'}",
        f"Raw trace    : {trace.path.resolve()}",
        "",
        "What is being executed",
        "----------------------",
        "  Test262 JavaScript is parsed into an AST. It is not compiled into one",
        "  standalone IR function. The ECMAScript specification IR (RunJobs,",
        "  ScriptEvaluation, Evaluation algorithms, built-ins, ...) interprets that",
        "  AST, and the closed denotation of those IR functions is the ITree below.",
        "",
        "  JS source -> ESMeta AST -> specification IR -> closed ITree -> terminal value",
        "",
        "Execution summary",
        "-----------------",
        f"  Last recorded execution step : {trace.events[-1].step if trace.events else 0:,}",
        f"  Internal computation (Tau)   : {tau_steps:,} steps in compressed runs",
        f"  Visible trace records        : {visible_events:,}",
        f"  Function calls entered       : {len(trace.call_nodes):,}",
        f"  Maximum call depth           : {trace.max_call_depth:,}",
        f"  IR instruction markers       : {len(markers):,}",
        "",
        "How to read this report",
        "-----------------------",
        "  IR      static instruction from the operational semantics",
        "  ->      function call; matching returns are summarized by a [start..end] span",
        "  Tau x N N consecutive silent/internal ITree transitions; not milliseconds",
        "          and not N JavaScript or IR instructions",
        "  Ret     normal terminal value; Take/Choose are unsupported effects",
    ]
    if tau_example is not None:
        lines.append(tau_example)
    lines.append("")

    if source is not None:
        lines.extend(
            [
                "Test262 JavaScript body",
                "-----------------------",
                "  ESMeta injects the Test262 harness separately. This is the test file body:",
                f"  source: {source_path.resolve()}",
                "",
            ]
        )
        for number, source_line in enumerate(source.splitlines(), 1):
            lines.append(f"  {number:4d} | {source_line}")
        lines.append("")

    lines.extend(["Static specification IR", "-----------------------"])
    if instruction_data is None:
        lines.append("  <IR function was not found in the supplied Spec.v>")
    else:
        line_number, instructions = instruction_data
        lines.append(
            f"  {trace.trace_target} from {spec_path.resolve()}:{line_number}"
        )
        lines.append("  Human outline; expressions are intentionally abbreviated:")
        lines.append("")
        for instruction in instructions:
            indent = "  " + "  " * instruction.depth
            lines.append(
                f"{indent}{instruction.path:<28} {instruction.kind:<8} {instruction.detail}"
            )
    lines.append("")

    lines.extend(["Observed IR instruction trace", "-----------------------------"])
    if not markers:
        lines.append("  <no instruction markers observed>")
    else:
        for event in markers:
            assert event.marker_path is not None
            detail = static_by_path.get(event.marker_path)
            suffix = f" — {detail.detail}" if detail is not None else ""
            lines.append(
                f"  [{event.step:>7,}] {event.marker_path:<30} "
                f"{event.marker_kind or 'IR'}{suffix}"
            )
    lines.append("")

    entry = next((node for node in trace.call_roots if node.name == "<entry>"), None)
    if entry is None and trace.call_roots:
        entry = trace.call_roots[0]
    lines.extend(["Major execution phases", "----------------------"])
    lines.extend(phase_lines(entry))
    lines.append("")

    lines.extend(
        [
            "Focused dynamic call paths",
            "--------------------------",
            "  Match patterns: " + ", ".join(patterns),
        ]
    )
    lines.extend(focus_lines(trace, patterns))
    lines.append("")

    lines.extend(
        [
            "Terminal observation",
            "--------------------",
            f"  {state}: {outcome}",
            "  This is an ITree shape result. The Test262 differential verdict is",
            "  reported separately by the extracted test runner.",
            "",
        ]
    )

    if include_raw:
        lines.extend(
            [
                "Raw trace (lossless)",
                "--------------------",
                trace.raw_text.rstrip("\n"),
                "",
            ]
        )
    else:
        lines.extend(
            [
                "Raw trace omitted from this readable view.",
                f"See: {trace.path.resolve()}",
                "Use --include-raw to append it verbatim.",
                "",
            ]
        )

    return "\n".join(lines)


def parse_args(argv: Sequence[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("raw_log", type=Path)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--spec", type=Path, default=DEFAULT_SPEC)
    parser.add_argument(
        "--test262-root", type=Path, default=DEFAULT_TEST262_ROOT
    )
    parser.add_argument(
        "--focus",
        action="append",
        default=[],
        help="case-insensitive function-name substring; may be repeated",
    )
    parser.add_argument("--include-raw", action="store_true")
    return parser.parse_args(argv)


def default_output(raw_log: Path) -> Path:
    name = raw_log.name
    if name.endswith(".raw.log"):
        name = name[: -len(".raw.log")] + ".readable.log"
    elif name.endswith(".log"):
        name = name[: -len(".log")] + ".readable.log"
    else:
        name += ".readable.log"
    return raw_log.with_name(name)


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_args(argv)
    trace = parse_raw_trace(args.raw_log)
    output = args.output or default_output(args.raw_log)
    report = render_report(
        trace,
        args.spec,
        args.test262_root,
        args.focus,
        args.include_raw,
    )
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(report, encoding="utf-8")
    print(output.resolve())
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
