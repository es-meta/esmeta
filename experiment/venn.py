#!/usr/bin/env python3

"""Area-proportional Venn diagrams, drawn for print.

Each circle's area is its set's size, and the centre distances are solved so
that every pairwise overlap covers its own area too. Two circles can always be
placed exactly; three circles have three distances to satisfy three pairwise
overlaps, so the pairs are exact and whatever the triple region then covers is
reported as residual rather than silently trusted.

Region labels carry the true counts either way. `fit_report` says how far the
drawing is from the numbers so a caption can admit it.
"""

from __future__ import annotations

import html
import math
from dataclasses import dataclass

# Okabe-Ito, the colourblind-safe set; readable in greyscale and in print
INK = "#1a1a1a"
MUTED = "#6b7280"
PALETTE = [
    ("#0072b2", "#00537f"),  # blue
    ("#e69f00", "#a37100"),  # orange
    ("#009e73", "#006f51"),  # green
]
FONT = "Linux Libertine O, Linux Libertine, Libertine, Times New Roman, serif"
FILL_OPACITY = 0.32


@dataclass
class Circle:
    name: str
    x: float
    y: float
    r: float


def lens_area(r1: float, r2: float, d: float) -> float:
    """area covered by both circles when their centres are d apart"""
    if d >= r1 + r2:
        return 0.0
    if d <= abs(r1 - r2):
        return math.pi * min(r1, r2) ** 2
    a1 = math.acos((d * d + r1 * r1 - r2 * r2) / (2 * d * r1))
    a2 = math.acos((d * d + r2 * r2 - r1 * r1) / (2 * d * r2))
    return (
        r1 * r1 * (a1 - math.sin(2 * a1) / 2)
        + r2 * r2 * (a2 - math.sin(2 * a2) / 2)
    )


def distance_for(r1: float, r2: float, target: float) -> float:
    """the centre distance whose overlap has the given area"""
    lo, hi = abs(r1 - r2), r1 + r2
    if target <= 0:
        return hi
    if target >= math.pi * min(r1, r2) ** 2:
        return lo
    for _ in range(80):
        mid = (lo + hi) / 2
        if lens_area(r1, r2, mid) > target:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2


def set_sizes(names: list[str], regions: dict[frozenset[str], int]) -> dict[str, int]:
    return {
        n: sum(c for key, c in regions.items() if n in key) for n in names
    }


def pair_size(a: str, b: str, regions: dict[frozenset[str], int]) -> int:
    return sum(c for key, c in regions.items() if a in key and b in key)


def place(
    names: list[str],
    regions: dict[frozenset[str], int],
    unit: float,
) -> list[Circle]:
    """circles whose areas are the set sizes and whose overlaps are the pairs

    `unit` is the drawing area one element takes up, so a radius is
    sqrt(count * unit / pi).
    """
    sizes = set_sizes(names, regions)
    radii = [math.sqrt(max(sizes[n], 1) * unit / math.pi) for n in names]
    if len(names) == 1:
        return [Circle(names[0], 0.0, 0.0, radii[0])]

    d01 = distance_for(radii[0], radii[1], pair_size(names[0], names[1], regions) * unit)
    if len(names) == 2:
        return [
            Circle(names[0], 0.0, 0.0, radii[0]),
            Circle(names[1], d01, 0.0, radii[1]),
        ]

    d02 = distance_for(radii[0], radii[2], pair_size(names[0], names[2], regions) * unit)
    d12 = distance_for(radii[1], radii[2], pair_size(names[1], names[2], regions) * unit)
    # trilateration; a triangle inequality this tight can fail, and then the
    # third circle lands on the axis as close as the other two allow
    x = (d01 * d01 + d02 * d02 - d12 * d12) / (2 * d01)
    y = math.sqrt(max(0.0, d02 * d02 - x * x))
    return [
        Circle(names[0], 0.0, 0.0, radii[0]),
        Circle(names[1], d01, 0.0, radii[1]),
        Circle(names[2], x, y, radii[2]),
    ]


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
    """distance to the nearest circle boundary, which is how much room a label has"""
    return min(abs(math.hypot(px - c.x, py - c.y) - c.r) for c in circles)


def anchors(
    circles: list[Circle],
    keys: list[frozenset[str]],
    steps: int = 320,
) -> dict[frozenset[str], tuple[float, float, float]]:
    """the roomiest point of each region, with the room it has"""
    best: dict[frozenset[str], tuple[float, float, float]] = {}
    wanted = set(keys)
    for px, py in _grid(circles, steps):
        key = _key(circles, px, py)
        if key not in wanted:
            continue
        room = _clearance(circles, px, py)
        if key not in best or room > best[key][2]:
            best[key] = (px, py, room)
    return best


def measured_areas(
    circles: list[Circle],
    keys: list[frozenset[str]],
    steps: int = 320,
) -> dict[frozenset[str], float]:
    """what the drawing actually covers, by sampling"""
    lo_x = min(c.x - c.r for c in circles)
    hi_x = max(c.x + c.r for c in circles)
    lo_y = min(c.y - c.r for c in circles)
    hi_y = max(c.y + c.r for c in circles)
    step = max(hi_x - lo_x, hi_y - lo_y) / steps
    cell = step * step
    out = {k: 0.0 for k in keys}
    for px, py in _grid(circles, steps):
        key = _key(circles, px, py)
        if key in out:
            out[key] += cell
    return out


def fit_report(
    circles: list[Circle],
    regions: dict[frozenset[str], int],
    unit: float,
) -> list[str]:
    """regions the drawing cannot size correctly, worst first"""
    keys = [k for k in regions if k]
    drawn = measured_areas(circles, keys)
    off = []
    for key in keys:
        want = regions[key] * unit
        got = drawn.get(key, 0.0)
        if want <= 0 and got <= 0:
            continue
        scale = max(want, got, 1e-9)
        error = abs(got - want) / scale
        if error > 0.02:
            shown = round(got / unit)
            off.append((error, f"{' & '.join(sorted(key))}: {regions[key]} drawn as ~{shown}"))
    off.sort(reverse=True)
    return [line for _, line in off]


def _box(x: float, y: float, text: str, size: float, anchor: str, margin: float = 0.0):
    """the rectangle a piece of text occupies, for keeping labels apart"""
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
    """whether a leader line runs through a label already placed"""
    for i in range(1, 21):
        t = i / 21
        px, py = x1 + (x2 - x1) * t, y1 + (y2 - y1) * t
        for box in others:
            if box[0] <= px <= box[2] and box[1] <= py <= box[3]:
                return True
    return False


def _text_width(label: str, size: float) -> float:
    """roughly how wide a run of text is, enough to keep it inside the frame"""
    return len(label) * size * 0.52


def _text_room(label: str, size: float) -> float:
    """the radius a centred number needs, in drawing units"""
    return math.hypot(_text_width(label, size), size * 0.72) / 2


def _frame(
    circles, regions, keys, spots, labels, names,
    count_size, label_size, gap, span, has_outside,
):
    """where every piece of text goes, and the frame that holds it all"""
    sizes = set_sizes(names, regions)
    mid_x = sum(c.x for c in circles) / len(circles)
    mid_y = sum(c.y for c in circles) / len(circles)

    placed = []
    for circle in circles:
        dx, dy = circle.x - mid_x, circle.y - mid_y
        norm = math.hypot(dx, dy) or 1.0
        if len(circles) == 2:
            dx, dy, norm = (-1.0 if circle.x < mid_x else 1.0), -0.35, 1.06
        ux, uy = dx / norm, dy / norm
        # a label that sits above or below its circle is centred on it, since
        # running the text sideways from there would cross the outline
        anchor = "middle" if abs(ux) < 0.4 else "end" if ux < 0 else "start"
        reach = circle.r + gap * (1.6 if anchor == "middle" else 1.0)
        drop = label_size * (0.72 if uy > 0 else -0.10) if anchor == "middle" else 0.0
        placed.append(
            (
                circle.x + ux * reach,
                circle.y + uy * reach + drop,
                labels[circle.name],
                anchor,
                circle.name,
            ),
        )

    # a region too small for its number gets the number outside, on a leader,
    # pushed out until it clears the set labels and the leaders before it
    clear = count_size * 0.45  # keep labels from touching
    taken = [
        _box(x, y, f"{labels[n]} ({sizes[n]:,})", label_size, a, clear)
        for x, y, _t, a, n in placed
    ]
    inside, leaders = [], []
    for key in sorted(keys, key=lambda k: (len(k), sorted(k))):
        if key not in spots:
            continue
        px, py, room = spots[key]
        text = f"{regions[key]:,}"
        if room >= _text_room(text, count_size):
            inside.append((px, py, text))
            continue
        dx, dy = px - mid_x, py - mid_y
        norm = math.hypot(dx, dy) or 1.0
        base = math.atan2(dy, dx)
        # every direction may be crowded, so score them all and take the least
        # bad: never overlapping beats never crossing beats staying short
        best = None
        for reach in (2.6, 3.4, 4.4, 5.6, 7.0, 8.6):
            for turn in (0.0, 0.26, -0.26, 0.52, -0.52, 0.85, -0.85, 1.2, -1.2):
                angle = base + turn
                tx = px + math.cos(angle) * gap * reach
                ty = py + math.sin(angle) * gap * reach
                anchor = "end" if math.cos(angle) < 0 else "start"
                box = _box(tx, ty, text, count_size, anchor, clear)
                penalty = (
                    1000 * _hits(box, taken)
                    + 300 * _crosses(px, py, tx, ty, taken)
                    + 10 * reach
                    + abs(turn)
                )
                if best is None or penalty < best[0]:
                    best = (penalty, tx, ty, anchor, box)
        _, tx, ty, anchor, box = best
        taken.append(box)
        leaders.append((px, py, tx, ty, text, anchor))

    def span_of(x, text, name, anchor, size):
        width = _text_width(text if name is None else f"{text} ({sizes[name]:,})", size)
        if anchor == "end":
            return x - width, x
        if anchor == "start":
            return x, x + width
        return x - width / 2, x + width / 2

    edges = [span_of(p[0], p[2], p[4], p[3], label_size) for p in placed]
    edges += [span_of(l[2], l[4], None, l[5], count_size) for l in leaders]
    xs = (
        [c.x - c.r for c in circles]
        + [c.x + c.r for c in circles]
        + [e for pair in edges for e in pair]
    )
    ys = (
        [c.y - c.r for c in circles]
        + [c.y + c.r for c in circles]
        + [p[1] - label_size * 0.78 for p in placed]
        + [p[1] + label_size * 0.28 for p in placed]
        + [l[3] - count_size * 0.6 for l in leaders]
        + [l[3] + count_size * 0.6 for l in leaders]
    )
    pad = span * 0.02
    lo_y, hi_y = min(ys) - pad, max(ys) + pad
    if has_outside:
        hi_y += label_size * 2.0
    return min(xs) - pad, max(xs) + pad, lo_y, hi_y, placed, inside, leaders


def render(
    names: list[str],
    regions: dict[frozenset[str], int],
    labels: dict[str, str] | None = None,
    outside: int | None = None,
    outside_label: str = "neither",
    width_pt: float = 340.0,
    font_pt: float = 8.5,
    title: str = "",
    description: str = "",
) -> str:
    """an area-proportional Venn, sized in points for inclusion in a paper"""
    labels = labels or {n: n for n in names}
    unit = 1.0  # solved in element units, scaled to points at the end
    circles = place(names, regions, unit)
    keys = [k for k in regions if k]

    spots = anchors(circles, keys)
    span = max(
        max(c.x + c.r for c in circles) - min(c.x - c.r for c in circles),
        max(c.y + c.r for c in circles) - min(c.y - c.r for c in circles),
    )
    gap = span * 0.030
    # text is sized in points, not as a fraction of the drawing, so widening
    # the figure enlarges the circles and leaves the type at the paper's size.
    # The frame depends on the type and the scale depends on the frame, so the
    # two settle over a few passes.
    count_size = label_size = span * 0.05
    scale = width_pt / (max(c.x + c.r for c in circles) - min(c.x - c.r for c in circles))

    for _ in range(3):
        count_size = label_size = font_pt / scale
        lo_x, hi_x, lo_y, hi_y, placed, inside, leaders = _frame(
            circles, regions, keys, spots, labels, names,
            count_size, label_size, gap, span, outside is not None,
        )
        scale = width_pt / (hi_x - lo_x)
    # the frame settled a hair away from the last sizes; take the scale as
    # final so the emitted type is exactly font_pt
    count_size = label_size = font_pt / scale
    w, h = hi_x - lo_x, hi_y - lo_y
    pad = span * 0.02
    sizes = set_sizes(names, regions)

    def fx(v: float) -> str:
        return f"{(v - lo_x) * scale:.2f}"

    def fy(v: float) -> str:
        return f"{(v - lo_y) * scale:.2f}"

    def fs(v: float) -> str:
        return f"{v * scale:.2f}"

    total = sum(regions.values()) + (outside or 0)
    desc = description or (
        f"Area-proportional Venn diagram over {total:,} items; "
        + ", ".join(
            f"{labels[n]} {set_sizes(names, regions)[n]:,}" for n in names
        )
        + "."
    )
    out = [
        '<?xml version="1.0" encoding="UTF-8"?>',
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width_pt:.1f}pt" '
        f'height="{h * scale:.1f}pt" viewBox="0 0 {w * scale:.2f} {h * scale:.2f}" '
        'role="img" aria-labelledby="t d">',
        f"  <title id=\"t\">{html.escape(title or 'Venn diagram')}</title>",
        f'  <desc id="d">{html.escape(desc)}</desc>',
        f'  <g font-family="{FONT}" fill="{INK}">',
    ]
    for i, circle in enumerate(circles):
        fill, stroke = PALETTE[i % len(PALETTE)]
        out.append(
            f'    <circle cx="{fx(circle.x)}" cy="{fy(circle.y)}" r="{fs(circle.r)}" '
            f'fill="{fill}" fill-opacity="{FILL_OPACITY}" stroke="{stroke}" '
            f'stroke-width="{fs(span * 0.0035)}" />',
        )
    for px, py, text in inside:
        out.append(
            f'    <text x="{fx(px)}" y="{fy(py)}" font-size="{fs(count_size)}" '
            'text-anchor="middle" dominant-baseline="central">'
            f"{html.escape(text)}</text>",
        )
    for px, py, tx, ty, text, anchor in leaders:
        pad_x = fs(gap * 0.28) if anchor == "start" else f"-{fs(gap * 0.28)}"
        out.append(
            f'    <line x1="{fx(px)}" y1="{fy(py)}" x2="{fx(tx)}" y2="{fy(ty)}" '
            f'stroke="{MUTED}" stroke-width="{fs(span * 0.002)}" />',
        )
        out.append(
            f'    <text x="{float(fx(tx)) + float(pad_x):.2f}" y="{fy(ty)}" '
            f'font-size="{fs(count_size)}" text-anchor="{anchor}" '
            f'dominant-baseline="central">{html.escape(text)}</text>',
        )
    for px, py, text, anchor, name in placed:
        size = sizes[name]
        out.append(
            f'    <text x="{fx(px)}" y="{fy(py)}" font-size="{fs(label_size)}" '
            f'text-anchor="{anchor}">{html.escape(text)}'
            f'<tspan fill="{MUTED}" font-size="{fs(label_size * 0.85)}" '
            f'dx="{fs(label_size * 0.3)}">'
            f"({size:,})</tspan></text>",
        )
    if outside is not None:
        out.append(
            f'    <text x="{fx(hi_x - pad)}" y="{fy(hi_y - pad)}" '
            f'font-size="{fs(label_size * 0.88)}" fill="{MUTED}" text-anchor="end">'
            f"{html.escape(outside_label)}: {outside:,}</text>",
        )
    out.append("  </g>")
    out.append("</svg>")
    return "\n".join(out) + "\n"
