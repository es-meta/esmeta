#!/usr/bin/env python3

# python3 experiment/spec_growth.py -o img/growth.pdf --macros
#
# Read at the edition tags, and never by date: TC39 cuts each edition on its
# own branch and never merges it back, so no lineage carries them all and
# walking one reports drafts of a document nobody published.
#
# Nothing is checked out either. ESMeta builds from ecma262/spec.html, so this
# reads the objects in place with `git show`.

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path

# the clause that opens the built-in half of the document
ANCHOR = "sec-ecmascript-standard-built-in-objects"
MEMORY_MODEL = "sec-memory-model"
EDITION = re.compile(r"^es20\d\d$")
TOP_CLAUSE = re.compile(r'^<emu-clause id="([a-z0-9-]+)"', re.M)
ALGORITHM = re.compile(r"<emu-alg\b([^>]*)>(.*?)</emu-alg>", re.S)
# `emu-alg:not([example])`, as the extractor selects
EXAMPLE = re.compile(r"\bexample\b")
STEP = re.compile(r"^\s*\d+\.\s", re.M)
TOKEN = re.compile(r"<(/?)emu-clause\b([^>]*)>|<emu-alg\b([^>]*)>", re.S)
TITLE = re.compile(r"\s*<h1>(.*?)</h1>", re.S)

# Okabe-Ito, as in experiment/venn.py
LANGUAGE, BUILTIN, INK, MUTED = "#0072b2", "#e69f00", "#1a1a1a", "#6b7280"


def git(repo: Path, *args: str) -> str:
    out = subprocess.run(["git", "-C", str(repo), *args], capture_output=True, text=True)
    return "" if out.returncode else out.stdout


def is_api(attrs: str, title: str) -> bool:
    """a built-in function or accessor, marked only since ES2026"""
    if 'type="built-in function"' in attrs:
        return True
    if "aoid=" in attrs or "type=" in attrs:
        return False
    if title.startswith("[[") or ":" in title:
        return False
    return title.startswith(("get ", "set ")) or "(" in title


def is_sdo(attrs: str, title: str) -> bool:
    """a syntax-directed operation, marked only since ES2021"""
    if 'type="sdo"' in attrs:
        return True
    if "aoid=" in attrs or "type=" in attrs:
        return False
    return "Semantics:" in title and "Early Errors" not in title


def steps(spec: Path, tag: str) -> tuple[int, int, int, int] | None:
    """numbered steps in the language and the library, and their SDOs and APIs"""
    text = git(spec, "show", f"{tag}:spec.html")
    if not text:
        return None
    tops = [(m.start(), m.group(1)) for m in TOP_CLAUSE.finditer(text)]
    cut = next((off for off, name in tops if name == ANCHOR), None)
    if cut is None:
        return None
    # the memory model and the annexes follow the library
    ends = [off for off, name in tops if name == MEMORY_MODEL and off > cut]
    end = min(ends + [text.find("<emu-annex", cut)])
    language = builtin = sdos = 0
    apis = set()
    stack: list[tuple[int, str, str]] = []
    for m in TOKEN.finditer(text):
        if m.group(3) is None:
            if m.group(1):
                stack.pop()
            else:
                title = TITLE.match(text, m.end())
                name = re.sub(r"<[^>]+>|\s+", " ", title.group(1)).strip() if title else ""
                stack.append((m.start(), m.group(2), name))
            continue
        if EXAMPLE.search(m.group(3)):
            continue
        alg = ALGORITHM.match(text, m.start())
        n = len(STEP.findall(alg.group(2))) if alg else 0
        if m.start() < cut:
            language += n
            sdos += bool(stack and is_sdo(stack[-1][1], stack[-1][2]))
        elif m.start() < end:
            builtin += n
            if stack and is_api(stack[-1][1], stack[-1][2]):
                apis.add(stack[-1][0])
    return language, builtin, sdos, len(apis)


def editions(spec: Path) -> list[dict]:
    out = []
    for tag in sorted(t for t in git(spec, "tag").split() if EDITION.match(t)):
        counted = steps(spec, tag)
        if counted is None:
            continue
        out.append({
            "tag": tag, "year": int(tag[2:]),
            "language": counted[0], "builtin": counted[1],
            "sdos": counted[2], "apis": counted[3],
        })
    return out



def draw(rows: list[dict], out: Path, width_pt: float) -> None:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.ticker import MultipleLocator

    plt.rcParams.update({
        "font.family": "serif",
        "font.serif": ["Linux Libertine O", "Times New Roman", "DejaVu Serif"],
        "font.size": 8,
        "axes.labelcolor": INK, "text.color": INK,
        "xtick.color": INK, "ytick.color": INK, "axes.edgecolor": INK,
        "axes.spines.top": False, "axes.spines.right": False,
        "pdf.fonttype": 42,
    })
    inch = width_pt / 72
    fig, axes = plt.subplots(1, 2, figsize=(inch, inch * 0.32))
    years = [r["year"] for r in rows]
    for axis, keys in ((axes[0], ("sdos", "apis")), (axes[1], ("language", "builtin"))):
        for key, color, label in zip(keys, (LANGUAGE, BUILTIN), ("language", "built-in library")):
            axis.plot(years, [r[key] for r in rows], color=color, linewidth=1.1, label=label)
    for axis, keys, ylabel, tick in (
        (axes[0], ("sdos", "apis"), "algorithms", 200),
        (axes[1], ("language", "builtin"), "algorithm steps", 2000),
    ):
        axis.set_ylabel(ylabel, fontsize=10)
        axis.yaxis.set_major_locator(MultipleLocator(tick))
        axis.yaxis.set_major_formatter(lambda v, _: f"{v / 1000:.1f}k")
        axis.tick_params(length=2.5, width=0.6)
        axis.tick_params(labelsize=10)
        axis.set_xticks(years)
        axis.set_xticklabels([f"ES{y}" if i % 5 == 0 else "" for i, y in enumerate(years)])
        for spine in axis.spines.values():
            spine.set_linewidth(0.6)
        axis.margins(x=0.04)
        top = max(r[key] for r in rows for key in keys)
        axis.set_ylim(0, -(-top // tick) * tick)
    axes[1].legend(frameon=False, loc="lower right", handlelength=1.6, fontsize=9)
    fig.tight_layout(pad=0.3)
    out.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out, format="pdf", bbox_inches="tight", pad_inches=0.01)


def main() -> int:
    home = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(
        description="Plot where ECMA-262 has grown, language against built-ins.",
    )
    parser.add_argument("--spec", type=Path, default=home / "ecma262")
    parser.add_argument("-o", "--out", type=Path, default=home / "experiment" / "growth.pdf")
    parser.add_argument("--width", type=float, default=395.0, help="figure width in points (395 is acmsmall's text width)")
    parser.add_argument("--macros", action="store_true")
    parser.add_argument("--no-plot", action="store_true")
    args = parser.parse_args()

    if not (args.spec / ".git").exists():
        print(f"error: not a git checkout: {args.spec}", file=sys.stderr)
        return 1

    rows = editions(args.spec)
    if len(rows) < 2:
        print(f"error: no edition tags in {args.spec}", file=sys.stderr)
        return 1
    first, last = rows[0], rows[-1]
    grew_l = last["language"] - first["language"]
    grew_b = last["builtin"] - first["builtin"]
    share = grew_b * 100 / (grew_l + grew_b)
    grew_s = last["sdos"] - first["sdos"]
    grew_a = last["apis"] - first["apis"]

    print(f"  {'':9}{'steps':>20}{'algorithms':>20}")
    print(f"  {'edition':9}{'language':>10}{'built-in':>10}{'SDO':>10}{'API':>10}")
    for r in rows:
        print(
            f"  {r['tag']:9}{r['language']:>10,}{r['builtin']:>10,}"
            f"{r['sdos']:>10,}{r['apis']:>10,}",
        )
    print(
        f"\n  since {first['tag']}: language +{grew_l:,} steps, built-in +{grew_b:,}; "
        f"built-ins are {share:.0f}% of the growth",
    )
    print(f"  since {first['tag']}: language +{grew_s:,} SDO algorithms, built-in +{grew_a:,} API algorithms")

    if args.macros:
        print()
        for name, value in (
            ("SpecFirstEdition", str(first["year"])),
            ("SpecLastEdition", str(last["year"])),
            ("NumStepsLangFirst", f"{first['language']:,}"),
            ("NumStepsBuiltinFirst", f"{first['builtin']:,}"),
            ("NumStepsLangLast", f"{last['language']:,}"),
            ("NumStepsBuiltinLast", f"{last['builtin']:,}"),
            ("PctBuiltinOfGrowth", f"{share:.0f}\\%"),
            ("NumAlgsLangFirst", f"{first['sdos']:,}"),
            ("NumAlgsLangLast", f"{last['sdos']:,}"),
            ("NumAlgsBuiltinFirst", f"{first['apis']:,}"),
            ("NumAlgsBuiltinLast", f"{last['apis']:,}"),
        ):
            print(f"\\newcommand{{\\{name}}}{{{value}\\xspace}}")

    if not args.no_plot:
        draw(rows, args.out, args.width)
        print(f"\n  wrote {args.out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
