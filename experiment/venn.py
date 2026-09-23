#!/usr/bin/env python3

"""Venn diagrams for print."""

from __future__ import annotations

import html
import math
import subprocess
from dataclasses import dataclass

# Okabe-Ito, the colourblind-safe set; readable in greyscale and in print
INK = "#1a1a1a"
MUTED = "#6b7280"
PALETTE = ["#0072b2", "#e69f00", "#009e73"]  # blue, orange, green
# a set keeps its colour across figures, whichever others it is drawn with
COLOUR = {"Synth262": PALETTE[0], "ESMeta fuzzer": PALETTE[1], "Test262": PALETTE[2]}


def colour_of(name: str, i: int) -> str:
    return COLOUR.get(name, PALETTE[i % len(PALETTE)])


def darker(colour: str, share: float = 0.35) -> str:
    """the colour mixed with that share of black, for its outline"""
    rgb = [int(colour[k:k + 2], 16) for k in (1, 3, 5)]
    return "#" + "".join(f"{round(c * (1 - share)):02x}" for c in rgb)
FONT = "Linux Libertine O, Linux Libertine, Libertine, Times New Roman, serif"
# a wash light enough that the overlaps still read; each outline is its own
# colour, darker, so the circles stay apart where they cross
FILL_OPACITY = 0.30
STROKE_PT = 0.5
LEADER_PT = 0.6


@dataclass
class Circle:
    name: str
    x: float
    y: float
    r: float


def set_sizes(names: list[str], regions: dict[frozenset[str], int]) -> dict[str, int]:
    return {
        n: sum(c for key, c in regions.items() if n in key) for n in names
    }


# centre distance as a share of the mean radius, by the number of circles:
# as close as the thinnest region still holds its number
SPREAD = {2: 0.8, 3: 0.7}


def place(
    names: list[str],
    regions: dict[frozenset[str], int],
    unit: float,
) -> list[Circle]:
    """areas are the set sizes; the overlaps are drawn evenly, not to scale,
    since three circles cannot size seven regions and a wrong area misleads"""
    sizes = set_sizes(names, regions)
    radii = [math.sqrt(max(sizes[n], 1) * unit / math.pi) for n in names]
    d = SPREAD.get(len(names), 1.0) * sum(radii) / len(radii)
    spots = [(0.0, 0.0), (d, 0.0), (d / 2, d * math.sqrt(3) / 2)]
    return [Circle(n, x, y, r) for n, (x, y), r in zip(names, spots, radii)]


def _grid(circles: list[Circle], steps: int):
    lo_x = min(c.x - c.r for c in circles)
    hi_x = max(c.x + c.r for c in circles)
    lo_y = min(c.y - c.r for c in circles)
    hi_y = max(c.y + c.r for c in circles)
    step = max(hi_x - lo_x, hi_y - lo_y) / steps
    ny = int((hi_y - lo_y) / step) + 1
    nx = int((hi_x - lo_x) / step) + 1
    for iy in range(ny):
        for ix in range(nx):
            yield lo_x + ix * step, lo_y + iy * step
    return


def _key(circles: list[Circle], px: float, py: float) -> frozenset[str]:
    return frozenset(
        c.name for c in circles if math.hypot(px - c.x, py - c.y) <= c.r
    )


def _clearance(circles: list[Circle], px: float, py: float) -> float:
    """room a label has: distance to the nearest boundary"""
    return min(abs(math.hypot(px - c.x, py - c.y) - c.r) for c in circles)


def anchors(
    circles: list[Circle],
    keys: list[frozenset[str]],
    steps: int = 320,
) -> dict[frozenset[str], tuple[list[tuple[float, float, float]], float, float]]:
    """each region's sampled points with their room, and its centroid"""
    points: dict[frozenset[str], list[tuple[float, float, float]]] = {}
    wanted = set(keys)
    for px, py in _grid(circles, steps):
        key = _key(circles, px, py)
        if key in wanted:
            points.setdefault(key, []).append((px, py, _clearance(circles, px, py)))
    return {
        key: (pts, sum(p[0] for p in pts) / len(pts), sum(p[1] for p in pts) / len(pts))
        for key, pts in points.items()
    }


def _box(x: float, y: float, text: str, size: float, anchor: str, margin: float = 0.0):
    """the rectangle a piece of text occupies"""
    w = _text_width(text, size)
    left = x - w if anchor == "end" else x if anchor == "start" else x - w / 2
    return (
        left - margin,
        y - size * 0.72 - margin,
        left + w + margin,
        y + size * 0.30 + margin,
    )


def _hits(box, others) -> bool:
    for other in others:
        if (
            box[0] < other[2]
            and other[0] < box[2]
            and box[1] < other[3]
            and other[1] < box[3]
        ):
            return True
    return False


def _crosses(x1: float, y1: float, x2: float, y2: float, others) -> bool:
    """does a leader cross a placed label"""
    for i in range(1, 21):
        t = i / 21
        px, py = x1 + (x2 - x1) * t, y1 + (y2 - y1) * t
        for box in others:
            if box[0] <= px <= box[2] and box[1] <= py <= box[3]:
                return True
    return False


def _text_width(label: str, size: float) -> float:
    """rough text width"""
    return len(label) * size * 0.52


def _text_room(label: str, size: float) -> float:
    """radius a centred number needs"""
    return math.hypot(_text_width(label, size), size * 0.72) / 2


def _drop(anchor: str, below: bool, size: float) -> float:
    """baseline offset from a leader's tip"""
    if anchor != "middle":
        return size * 0.3
    return size * 0.95 if below else -size * 0.25


def _segments_cross(a, b) -> bool:
    """do two leaders cross"""
    def side(x1, y1, x2, y2, x, y):
        return (x2 - x1) * (y - y1) - (y2 - y1) * (x - x1)
    d1 = side(*a, b[0], b[1])
    d2 = side(*a, b[2], b[3])
    d3 = side(*b, a[0], a[1])
    d4 = side(*b, a[2], a[3])
    return d1 * d2 < 0 and d3 * d4 < 0


def _leave(px, py, angle, circles, span):
    """distance along the angle at which the ray has left every circle"""
    cos, sin = math.cos(angle), math.sin(angle)
    step = span * 0.01
    d = 0.0
    while d < span * 2:
        x, y = px + cos * d, py + sin * d
        if all(math.hypot(x - c.x, y - c.y) >= c.r for c in circles):
            return d
        d += step
    return d


def _layout(circles, regions, keys, spots, labels, names, order, size, gap, span, title, legend_on):
    """every count on a leader outside the circles, legend at the left,
    title underneath; returns the frame and the placed pieces"""
    mid_x = sum(c.x for c in circles) / len(circles)
    mid_y = sum(c.y for c in circles) / len(circles)
    lo_cx = min(c.x - c.r for c in circles)
    hi_cx = max(c.x + c.r for c in circles)
    lo_cy = min(c.y - c.r for c in circles)
    hi_cy = max(c.y + c.r for c in circles)
    clear = size * 0.45

    # legend: a column of circle swatches, left of the drawing
    legend_w = size * 1.5 + max(_text_width(labels[n], size) for n in names)
    legend_x = lo_cx - gap * 2 - legend_w
    legend_y0 = mid_y - size * 1.5 * (len(names) - 1) / 2
    legend = [(legend_x, legend_y0 + i * size * 1.5, n) for i, n in enumerate(order)]
    taken = [
        (legend_x - clear, legend_y0 - size, legend_x + legend_w + clear, legend[-1][1] + size)
    ]
    if not legend_on:
        legend, taken = [], []
    inside, leaders = [], []
    for key in sorted(keys, key=lambda k: (-regions[k], sorted(k))):
        if key not in spots or regions[key] == 0:
            continue
        pts, cx, cy = spots[key]
        count = f"{regions[key]:,}"
        width = _text_width(count, size)
        # a region with room to spare carries its number itself, as near its
        # centroid as the room allows, since that is where the eye looks
        fits = [p for p in pts if p[2] >= _text_room(count, size) * 1.6]
        if fits:
            px, py, _ = min(fits, key=lambda p: math.hypot(p[0] - cx, p[1] - cy))
            inside.append((px, py, count))
            continue
        px, py, room = max(pts, key=lambda p: p[2])
        # the region every set shares points straight up, as the eye expects
        whole = key == frozenset(names)
        base = -math.pi / 2 if whole else math.atan2(py - mid_y, px - mid_x)
        turns = (0.0,) if whole else (0.0, 0.3, -0.3, 0.6, -0.6, 0.9, -0.9, 1.2, -1.2, 1.5, -1.5)
        best = None
        for turn in turns:
            angle = base + turn
            exit_d = _leave(px, py, angle, circles, span)
            for extra in (1.0, 1.8, 2.8, 4.0):
                d = exit_d + gap * extra
                tx, ty = px + math.cos(angle) * d, py + math.sin(angle) * d
                anchor = "end" if math.cos(angle) < -0.3 else "start" if math.cos(angle) > 0.3 else "middle"
                left = tx - width if anchor == "end" else tx if anchor == "start" else tx - width / 2
                # a number under a leader hangs below its tip, one above sits on it
                by = ty + _drop(anchor, ty > py, size)
                box = (left - clear, by - size * 0.72 - clear, left + width + clear, by + size * 0.3 + clear)
                # text must clear the circles too, not just the other text
                on_circle = any(
                    math.hypot(x - c.x, y - c.y) < c.r
                    for x in (box[0], (box[0] + box[2]) / 2, box[2])
                    for y in (box[1], (box[1] + box[3]) / 2, box[3])
                    for c in circles
                )
                penalty = (
                    1000 * _hits(box, taken)
                    + 1000 * on_circle
                    + 300 * _crosses(px, py, tx, ty, taken)
                    + 300 * any(_segments_cross((px, py, tx, ty), l[:4]) for l in leaders)
                    + 10 * d / gap
                    + abs(turn)
                )
                if best is None or penalty < best[0]:
                    best = (penalty, tx, ty, anchor, box)
        _, tx, ty, anchor, box = best
        taken.append(box)
        leaders.append((px, py, tx, ty, count, anchor))

    # title under everything
    title_y = max([hi_cy] + [b[3] for b in taken]) + gap + size * 0.9
    if title:
        taken.append(_box(mid_x, title_y, title, size, "middle", clear))

    xs = [lo_cx, hi_cx] + [b[0] for b in taken] + [b[2] for b in taken]
    ys = [lo_cy, hi_cy] + [b[1] for b in taken] + [b[3] for b in taken]
    pad = span * 0.02
    return (
        min(xs) - pad, max(xs) + pad, min(ys) - pad, max(ys) + pad,
        legend, (mid_x, title_y), inside, leaders,
    )


def render(
    names: list[str],
    regions: dict[frozenset[str], int],
    labels: dict[str, str] | None = None,
    width_pt: float = 240.0,
    font_pt: float = 8.0,
    title: str = "",
    description: str = "",
    legend_on: bool = True,
    legend_order: list[str] | None = None,
) -> str:
    """sized in points for a paper; the legend lists the sets in
    `legend_order`, which need not follow where the circles sit"""
    labels = labels or {n: n for n in names}
    order = legend_order or names
    circles = place(names, regions, 1.0)
    keys = [k for k in regions if k]
    spots = anchors(circles, keys)
    span = max(
        max(c.x + c.r for c in circles) - min(c.x - c.r for c in circles),
        max(c.y + c.r for c in circles) - min(c.y - c.r for c in circles),
    )
    gap = span * 0.05
    # text is sized in points, not as a fraction of the drawing, so widening
    # the figure enlarges the circles and leaves the type at the paper's size.
    # The frame depends on the type and the scale depends on the frame, so the
    # two settle over a few passes.
    scale = width_pt / span
    for _ in range(4):
        size = font_pt / scale
        lo_x, hi_x, lo_y, hi_y, legend, (title_x, title_y), inside, leaders = _layout(
            circles, regions, keys, spots, labels, names, order, size, gap, span, title, legend_on,
        )
        scale = width_pt / (hi_x - lo_x)
    size = font_pt / scale
    w, h = hi_x - lo_x, hi_y - lo_y

    def fx(v: float) -> str:
        return f"{(v - lo_x) * scale:.2f}"

    def fy(v: float) -> str:
        return f"{(v - lo_y) * scale:.2f}"

    def fs(v: float) -> str:
        return f"{v * scale:.2f}"

    sizes = set_sizes(names, regions)
    desc = description or (
        f"Venn diagram over {sum(regions.values()):,} items, circle areas "
        "proportional to set sizes and region areas not; "
        + ", ".join(f"{labels[n]} {sizes[n]:,}" for n in names) + "."
    )
    out = [
        '<?xml version="1.0" encoding="UTF-8"?>',
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width_pt:.1f}pt" '
        f'height="{h * scale:.1f}pt" viewBox="0 0 {w * scale:.2f} {h * scale:.2f}" '
        'role="img" aria-labelledby="t d">',
        f"  <title id=\"t\">{html.escape(title or 'Venn diagram')}</title>",
        f'  <desc id="d">{html.escape(desc)}</desc>',
        f'  <g font-family="{FONT}" fill="{INK}" font-size="{font_pt:.2f}">',
    ]
    for i, circle in enumerate(circles):
        out.append(
            f'    <circle cx="{fx(circle.x)}" cy="{fy(circle.y)}" r="{fs(circle.r)}" '
            f'fill="{colour_of(circle.name, i)}" fill-opacity="{FILL_OPACITY}" '
            f'stroke="{darker(colour_of(circle.name, i))}" '
            f'stroke-width="{STROKE_PT:.2f}" />',
        )
    for px, py, count in inside:
        out.append(
            f'    <text x="{fx(px)}" y="{fy(py)}" text-anchor="middle" '
            f'dominant-baseline="central">{count}</text>',
        )
    for px, py, tx, ty, count, anchor in leaders:
        out.append(
            f'    <line x1="{fx(px)}" y1="{fy(py)}" x2="{fx(tx)}" y2="{fy(ty)}" '
            f'stroke="{INK}" stroke-width="{LEADER_PT:.2f}" />',
        )
        nudge = {"start": size * 0.25, "end": -size * 0.25}.get(anchor, 0.0)
        out.append(
            f'    <text x="{fx(tx + nudge)}" y="{fy(ty + _drop(anchor, ty > py, size))}" '
            f'text-anchor="{anchor}">{count}</text>',
        )
    for lx, ly, name in legend:
        colour = colour_of(name, names.index(name))
        out.append(
            f'    <circle cx="{fx(lx + size * 0.55)}" cy="{fy(ly)}" r="{fs(size * 0.55)}" '
            f'fill="{colour}" fill-opacity="{FILL_OPACITY}" '
            f'stroke="{darker(colour)}" '
            f'stroke-width="{STROKE_PT:.2f}" />',
        )
        out.append(
            f'    <text x="{fx(lx + size * 1.5)}" y="{fy(ly)}" '
            f'dominant-baseline="central">{html.escape(labels[name])}</text>',
        )
    if title:
        out.append(
            f'    <text x="{fx(title_x)}" y="{fy(title_y)}" text-anchor="middle">'
            f'{html.escape(title)}</text>',
        )
    out.append("  </g>")
    out.append("</svg>")
    return "\n".join(out) + "\n"


def write(path, svg: str) -> None:
    """PDF through rsvg-convert (brew install librsvg); an .svg path keeps the SVG"""
    if (path.suffix.lower() == ".svg"):
        path.write_text(svg, encoding="utf-8")
    else:
        subprocess.run(
            ["rsvg-convert", "-f", "pdf", "-o", str(path)],
            input=svg.encode("utf-8"), check=True,
        )
