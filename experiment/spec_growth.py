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
EDITION = re.compile(r"^es20\d\d$")
TOP_CLAUSE = re.compile(r'^<emu-clause id="([a-z0-9-]+)"', re.M)
ALGORITHM = re.compile(r"<emu-alg\b([^>]*)>(.*?)</emu-alg>", re.S)
# `emu-alg:not([example])`, as the extractor selects
EXAMPLE = re.compile(r"\bexample\b")
STEP = re.compile(r"^\s*\d+\.\s", re.M)

# Okabe-Ito, as in experiment/venn.py
LANGUAGE, BUILTIN, INK, MUTED = "#0072b2", "#e69f00", "#1a1a1a", "#6b7280"


def git(repo: Path, *args: str) -> str:
    out = subprocess.run(["git", "-C", str(repo), *args], capture_output=True, text=True)
    return "" if out.returncode else out.stdout


def steps(spec: Path, tag: str) -> tuple[int, int] | None:
    """numbered algorithm steps, in the language clauses and in the library"""
    text = git(spec, "show", f"{tag}:spec.html")
    if not text:
        return None
    tops = [(m.start(), m.group(1)) for m in TOP_CLAUSE.finditer(text)]
    cut = next((off for off, name in tops if name == ANCHOR), None)
    if cut is None:
        return None
    language = builtin = 0
    for alg in ALGORITHM.finditer(text):
        if EXAMPLE.search(alg.group(1)):
            continue
        n = len(STEP.findall(alg.group(2)))
        if alg.start() >= cut:
            builtin += n
        else:
            language += n
    return language, builtin


def editions(spec: Path) -> list[dict]:
    out = []
    for tag in sorted(t for t in git(spec, "tag").split() if EDITION.match(t)):
        counted = steps(spec, tag)
        if counted is None:
            continue
        out.append({
            "tag": tag, "year": int(tag[2:]),
            "language": counted[0], "builtin": counted[1],
        })
    return out



def draw(rows: list[dict], out: Path, width_pt: float) -> None:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    plt.rcParams.update({
        "font.family": "serif",
        "font.serif": ["Linux Libertine O", "Times New Roman", "DejaVu Serif"],
        "font.size": 8,
        "axes.edgecolor": MUTED, "axes.labelcolor": INK, "text.color": INK,
        "xtick.color": MUTED, "ytick.color": MUTED,
        "axes.spines.top": False, "axes.spines.right": False,
        "pdf.fonttype": 42,
    })
    inch = width_pt / 72
    fig, axis = plt.subplots(figsize=(inch, inch * 0.62))
    years = [r["year"] for r in rows]
    for key, color, label in (
        ("language", LANGUAGE, "language"),
        ("builtin", BUILTIN, "built-in library"),
    ):
        axis.plot(
            years, [r[key] for r in rows],
            color=color, linewidth=1.4, marker="o", markersize=2.8, label=label,
        )
    axis.set_ylabel("algorithm steps")
    axis.grid(axis="y", color=MUTED, alpha=0.18, linewidth=0.5)
    axis.set_axisbelow(True)
    axis.set_xticks(years[::2])
    axis.set_xticklabels([f"ES{y}" for y in years[::2]])
    axis.legend(frameon=False, fontsize=7.5, loc="upper left")
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
    parser.add_argument("--width", type=float, default=240.0, help="figure width in points")
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

    print(f"  {'edition':9}{'language':>10}{'built-in':>10}")
    for r in rows:
        print(f"  {r['tag']:9}{r['language']:>10,}{r['builtin']:>10,}")
    print(
        f"\n  since {first['tag']}: language +{grew_l:,} steps, built-in +{grew_b:,}; "
        f"built-ins are {share:.0f}% of the growth",
    )

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
        ):
            print(f"\\newcommand{{\\{name}}}{{{value}\\xspace}}")

    if not args.no_plot:
        draw(rows, args.out, args.width)
        print(f"\n  wrote {args.out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
