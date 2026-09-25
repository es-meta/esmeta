#!/usr/bin/env python3

# python3 experiment/edit_distance.py
#
# Share of the covered branch sides each tool covers, by type edit distance.

from __future__ import annotations

import argparse
import csv
import sys
from pathlib import Path

import box
import venn
import venn_coverage

INK = venn.INK
HATCH = {"0fs": "//////"}
PRIORITY = ["solve", "1fs", "0fs"]

AXIS = "type edit distance"
MEASURE_HINT = 'sbt "Test/runMain esmeta.solver.EditDistance RUN/branch-coverage.json"'


def distance_map(path: Path) -> tuple[set, dict]:
    """the sides a run covers, and the edits of those whose witness replayed"""
    covered, edits = set(), {}
    with path.open(encoding="utf-8") as f:
        for row in csv.DictReader(f, delimiter="\t"):
            side = (int(row["branch"]), venn_coverage.parse_side(row["side"]))
            covered.add(side)
            if row["status"] == "ok":
                edits[side] = int(row["edits"])
    return covered, edits


def shares(runs: dict[str, list[Path]], universe: set) -> list[dict]:
    """per distance, each run's covered share of the sides any tool covers"""
    configs = sorted(runs, key=PRIORITY.index)
    n = min(len(v) for v in runs.values())
    table: dict[int, dict] = {}
    for i in range(n):
        measured = {c: distance_map(runs[c][i]) for c in configs}
        covered = {c: measured[c][0] & universe for c in configs}
        counts: dict[int, dict] = {}
        for side in set().union(*covered.values()):
            owner = next(c for c in configs if side in covered[c])
            depth = measured[owner][1].get(side)
            if depth is None:
                continue
            row = counts.setdefault(depth, {"all": 0, **{c: 0 for c in configs}})
            row["all"] += 1
            for c in configs:
                row[c] += side in covered[c]
        for depth, row in counts.items():
            entry = table.setdefault(depth, {"depth": depth, "all": [], **{c: [] for c in configs}})
            entry["all"].append(row["all"])
            for c in configs:
                entry[c].append(row[c] * 100 / row["all"])
    return [table[d] for d in sorted(table)]


def draw_bars(rows: list[dict], configs: list[str], out: Path, width_pt: float) -> None:
    """side-by-side bars of the mean share each tool covers, runs as a range"""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.colors import to_rgba

    plt.rcParams.update({
        "font.family": "serif",
        "font.serif": ["Linux Libertine O", "Times New Roman", "DejaVu Serif"],
        "font.size": 8,
        "axes.labelcolor": INK, "text.color": INK,
        "xtick.color": INK, "ytick.color": INK, "axes.edgecolor": INK,
        "axes.spines.top": False, "axes.spines.right": False,
        "pdf.fonttype": 42,
        "hatch.linewidth": 0.4,
    })
    inch = width_pt / 72
    fig, axis = plt.subplots(figsize=(inch, inch * 0.75))
    width = 0.8 / len(configs)
    handles = []
    for i, config in enumerate(configs):
        colour = venn.colour_of(box.label(config).split(" (")[0], i)
        xs = [j + (i - (len(configs) - 1) / 2) * width for j in range(len(rows))]
        runs = [r[config] for r in rows]
        means = [sum(v) / len(v) for v in runs]
        low = [m - min(v) for m, v in zip(means, runs)]
        high = [max(v) - m for m, v in zip(means, runs)]
        wash = axis.bar(xs, means, width=width * 0.92,
                 color=to_rgba(colour, venn.FILL_OPACITY),
                 edgecolor=venn.darker(colour), linewidth=0.6,
                 yerr=[low, high], error_kw={"elinewidth": 0.5, "capsize": 1.2, "ecolor": INK})
        handle = wash
        if config in HATCH:
            hatch = axis.bar(xs, means, width=width * 0.92, color="none", linewidth=0,
                             edgecolor=venn.darker(colour), hatch=HATCH[config])
            handle = (wash, hatch)
        handles.append((handle, box.label(config)))
    axis.set_xticks(range(len(rows)))
    axis.set_xticklabels([
        f"{r['depth']}\n{round(sum(r['all']) / len(r['all']))}"
        for r in rows
    ])
    axis.tick_params(axis="x", labelsize=6)
    n = len(rows[0]["all"])
    axis.set_xlabel(f"{AXIS} (covered sides below, {n}-run mean)")
    axis.set_ylabel("covered share (%)")
    axis.set_ylim(0, 108)
    axis.set_yticks(range(0, 101, 20))
    axis.legend(*zip(*handles), frameon=False, loc="upper center", bbox_to_anchor=(0.5, 1.16),
                ncol=len(configs), handlelength=1.0, fontsize=6.5)
    axis.tick_params(length=2.5, width=0.6)
    for spine in axis.spines.values():
        spine.set_linewidth(0.6)
    fig.tight_layout(pad=0.3)
    out.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out, format="pdf", bbox_inches="tight", pad_inches=0.01)


def main() -> int:
    home = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(
        description="Count who covers each branch side, by type edit distance.",
    )
    parser.add_argument("-d", "--data", type=Path, default=home / "experiment" / "data",
                        help="run directories, each with its measured edit-distance.tsv")
    parser.add_argument("--configs", default="solve,0fs,1fs",
                        help="run prefixes, in the order the bars stand")
    parser.add_argument("-s", "--solver-log", type=Path, default=None,
                        help="solver run whose summary gives the universe (default: solve-1)")
    parser.add_argument("-o", "--out", type=Path,
                        default=home / "experiment" / "edit-distance.pdf")
    parser.add_argument("--width", type=float, default=240.0)
    parser.add_argument("--no-plot", action="store_true")
    args = parser.parse_args()

    configs = args.configs.split(",")
    runs = {}
    for c in configs:
        runs[c] = [run / "edit-distance.tsv" for run in box.config_runs(args.data, c)]
        missing = [t for t in runs[c] if not t.exists()]
        if missing:
            print(f"error: no {missing[0]}; run {MEASURE_HINT}", file=sys.stderr)
            return 1
    solver_log = args.solver_log or box.config_runs(args.data, "solve")[0]
    universe, _, _, _ = venn_coverage.load_solver_sets(solver_log, {"pass"})
    rows = shares(runs, universe)

    print(f"  {'distance':>8}{'sides':>12}" + "".join(f"{c:>18}" for c in configs))
    for r in rows:
        shares_by_config = "".join(
            f"{sum(r[c]) / len(r[c]):>8.1f} ({min(r[c]):.0f}-{max(r[c]):.0f})".rjust(18)
            for c in configs)
        sides = f"{min(r['all']):,}-{max(r['all']):,}"
        print(f"  {r['depth']:>8}{sides:>12}{shares_by_config}")

    if not args.no_plot:
        draw_bars(rows, configs, args.out, args.width)
        print(f"\n  wrote {args.out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
