#!/usr/bin/env python3

"""Box plots of one number per run, for print."""

from __future__ import annotations

import json
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator

import venn

PT = 1 / 72  # inches per point


def config_runs(data: Path, config: str) -> list[Path]:
    runs = sorted(p for p in data.glob(f"{config}-*") if p.is_dir())
    if not runs:
        raise FileNotFoundError(f"no {config} runs under {data}; run experiment/data.sh unpack")
    return runs


def label(config: str, amplified: bool | None = None) -> str:
    """the tool names the Venn legends use: as shipped, Synth262 has the amplifier and JEST does not"""
    if config == "solve":
        return "Synth262 - amplifier" if amplified is False else "Synth262"
    return "JEST + amplifier" if amplified else "JEST"


def render(
    out: Path,
    groups: dict[str, list[int]],
    ylabel: str,
    width_pt: float,
    integer: bool,
    total: int | None = None,
) -> None:
    """one box per group with its runs as dots; a total puts % on the left axis"""
    if total:
        groups = {k: [100 * v / total for v in vs] for k, vs in groups.items()}
    plt.rcParams.update({"font.family": "serif", "font.size": 8, "pdf.fonttype": 42})
    fig, ax = plt.subplots(figsize=(width_pt * PT, width_pt * PT * 0.75))
    labels = list(groups)
    colours = [venn.colour_of(k.split(" ")[0], i) for i, k in enumerate(labels)]
    boxes = ax.boxplot(
        [groups[k] for k in labels],
        tick_labels=[k.replace(" + ", "\n+ ").replace(" - ", "\n\u2212 ") for k in labels],
        widths=0.5,
        patch_artist=True,
        showfliers=False,
        medianprops={"color": venn.INK, "linewidth": 1.0},
    )
    for patch, colour in zip(boxes["boxes"], colours):
        patch.set(facecolor=colour, alpha=venn.FILL_OPACITY, edgecolor=venn.darker(colour))
    for i, (label, colour) in enumerate(zip(labels, colours), start=1):
        ax.scatter([i] * len(groups[label]), groups[label], s=8, color=venn.darker(colour), zorder=3)
    ax.set_ylabel(ylabel)
    if integer:
        ax.yaxis.set_major_locator(MaxNLocator(integer=True))
    if total:
        right = ax.secondary_yaxis("right", functions=(lambda p: p * total / 100, lambda c: c * 100 / total))
        right.set_ylabel("# covered targets")
        ax.spines["top"].set_visible(False)
    else:
        ax.spines[["top", "right"]].set_visible(False)
    fig.tight_layout()
    fig.savefig(out)
    plt.close(fig)


def write_json(path: Path, doc: dict) -> None:
    path.write_text(json.dumps(doc, indent=2, sort_keys=True) + "\n", encoding="utf-8")
