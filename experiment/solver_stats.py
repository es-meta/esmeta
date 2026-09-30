#!/usr/bin/env python3

# python3 experiment/solver_stats.py [logs/solver/solve-...] [-o OUT_DIR]
#
# Summarize `stats.jsonl`, `templates.json`, and the witness programs of one
# `esmeta solve -solve:log` run. Writes `report.md`, CSV tables, and PNG
# figures into OUT_DIR (default: <run>/stats).

import argparse
import json
import re
from collections import Counter
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

BUDGET = 100  # Solver.maxCandidatesPerPath
TIMEOUT_MS = 10_000  # Solver.solveTimeLimit

# categorical slots in fixed order (reference palette, light mode)
C = ["#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300",
     "#4a3aa7", "#e34948"]
GRAY = "#8a8985"
STATUSES = ["pass", "fail-verify", "fail-reify", "unsolved", "timeout", "error"]
OUTCOMES = ["pass", "fail", "no-candidate", "timeout"]

plt.rcParams.update({
    "figure.dpi": 150, "savefig.bbox": "tight", "font.size": 9,
    "axes.spines.top": False, "axes.spines.right": False,
    "axes.grid": True, "grid.color": "#e6e5e0", "grid.linewidth": 0.6,
    "axes.axisbelow": True, "axes.edgecolor": "#9a9993",
})


# -----------------------------------------------------------------------------
# loading
# -----------------------------------------------------------------------------
def load(run: Path):
    targets, entries, paths = [], [], []
    for line in (run / "stats.jsonl").read_text().splitlines():
        t = json.loads(line)
        tid = f"{t['branch']}:{'T' if t['side'] else 'F'}"
        es = t.pop("entries")
        t.update(id=tid, nTried=len(es))
        for ei, e in enumerate(es):
            ps = e.pop("paths")
            e.update(target=tid, side=t["side"], rank=ei, nPaths=len(ps))
            for p in ps:
                p.update(target=tid, side=t["side"], entry=e["name"], rank=ei)
                paths.append(p)
            entries.append(e)
        targets.append(t)
    T, E, P = pd.DataFrame(targets), pd.DataFrame(entries), pd.DataFrame(paths)
    # per-target aggregates
    agg = P.groupby("target").agg(
        paths=("index", "size"), symPaths=("symPaths", "sum"),
        executed=("executed", "sum"), generated=("generated", "sum"),
        errors=("errors", "sum"), incidental=("incidental", "sum"),
        execMs=("execMs", "sum"))
    T = T.merge(agg, left_on="id", right_index=True, how="left")
    T[agg.columns] = T[agg.columns].fillna(0)
    T["sideName"] = np.where(T["side"], "true", "false")
    T["perEntryMs"] = TIMEOUT_MS / T["nEntries"]
    P["budgetHit"] = P["generated"] >= BUDGET
    return T, E, P


def describe(s: pd.Series) -> dict:
    s = s.dropna()
    if s.empty:
        return {}
    q = s.quantile([0.25, 0.5, 0.75, 0.9, 0.99])
    return {"n": len(s), "sum": s.sum(), "mean": s.mean(), "min": s.min(),
            "p25": q[0.25], "median": q[0.5], "p75": q[0.75],
            "p90": q[0.9], "p99": q[0.99], "max": s.max()}


def dist_table(rows: dict) -> pd.DataFrame:
    return pd.DataFrame({k: describe(v) for k, v in rows.items()}).T


def fmt(df: pd.DataFrame) -> str:
    try:
        return df.to_markdown(floatfmt=",.1f")  # needs `tabulate`
    except ImportError:
        return "```\n" + df.to_string(float_format="{:,.1f}".format) + "\n```"


# -----------------------------------------------------------------------------
# templates and programs
# -----------------------------------------------------------------------------
def template_stats(run: Path):
    path = run / "templates.json"
    if not path.exists():
        return None, None
    data = json.loads(path.read_text())
    rows = []
    for slot, forms in data.items():
        for f in forms:
            rows.append({"slot": slot, "template": f["template"],
                         "holes": len(f["holes"]),
                         "relation": "relation" in f})
    return pd.DataFrame(rows), data


def template_callee(template: str) -> re.Pattern:
    # the template text before its first hole, used to find nested uses
    prefix = re.split(r"#(?:THIS|NEW_TARGET|VAR|[0-9])", template)[0]
    return re.compile(r"(?<![\w.$])" + re.escape(prefix))


def program_stats(run: Path, templates: pd.DataFrame | None):
    cov_path = run / "branch-coverage.json"
    if not cov_path.exists():
        return None
    cov = json.loads(cov_path.read_text())
    callees = [template_callee(t) for t in templates["template"].unique()] \
        if templates is not None else []
    rows, cache = [], {}
    for c in cov:
        script = c["script"]
        if script not in cache:
            js = (run / script).read_text()
            uses = sum(len(c.findall(js)) for c in callees)
            top = 1 if any(c.match(js) for c in callees) else 0
            cache[script] = {"chars": len(js), "templateUses": uses - top,
                             "construct": js.count("Reflect.construct("),
                             "accessor": js.count(".get.call(")
                             + js.count(".set.call("),
                             "arrow": js.count("=>"),
                             "script": script}
        cond = c["condView"]["cond"]
        rows.append({"target": f"{cond['branch']}:{'T' if cond['cond'] else 'F'}",
                     **cache[script]})
    return pd.DataFrame(rows)


# -----------------------------------------------------------------------------
# figures
# -----------------------------------------------------------------------------
def save(fig, out: Path, name: str):
    fig.savefig(out / f"{name}.png")
    plt.close(fig)


def log_bins(maxv: float, n: int = 30):
    return np.unique(np.logspace(0, np.log10(max(maxv, 2)), n).round())


def figures(T, E, P, Tm, prog, out: Path):
    own = T[~T["reused"]]
    # 1. executions per path (the per-path budget spike at 100)
    fig, ax = plt.subplots(figsize=(6, 3))
    for i, o in enumerate(OUTCOMES):
        s = P.loc[P["outcome"] == o, "executed"]
        if len(s):
            ax.hist(s, bins=np.arange(0, BUDGET + 2), histtype="step",
                    linewidth=1.5, color=C[i], label=f"{o} ({len(s):,})")
    ax.set_yscale("log")
    ax.set_xlabel("programs executed on the path")
    ax.set_ylabel("paths (log)")
    ax.set_title("Executions per path, by path outcome")
    ax.legend(frameon=False)
    save(fig, out, "01-exec-per-path")

    # 2. paths and executions per target side, by final status
    for col, name, label in [("paths", "02-paths-per-target", "paths tried"),
                             ("executed", "03-exec-per-target",
                              "programs executed")]:
        fig, ax = plt.subplots(figsize=(6, 3))
        bins = np.concatenate([[0], log_bins(own[col].max() + 1)])
        for i, st in enumerate(STATUSES):
            s = own.loc[own["status"] == st, col]
            if len(s):
                ax.hist(s + 0.5, bins=bins + 0.5, histtype="step",
                        linewidth=1.5, color=C[i], label=f"{st} ({len(s):,})")
        ax.set_xscale("log")
        ax.set_yscale("log")
        ax.set_xlabel(f"{label} per target side (log)")
        ax.set_ylabel("target sides (log)")
        ax.set_title(f"{label.capitalize()} per target side, by status")
        ax.legend(frameon=False, fontsize=8)
        save(fig, out, name)

    # 4. box plots: per target side, split by branch side (true/false)
    fig, axes = plt.subplots(1, 3, figsize=(8, 3))
    for ax, col in zip(axes, ["paths", "executed", "ms"]):
        data = [own.loc[own["side"] == s, col] + 1 for s in (True, False)]
        ax.boxplot(data, tick_labels=["true", "false"], showfliers=False,
                   medianprops={"color": C[1]})
        ax.set_yscale("log")
        ax.set_title({"paths": "paths", "executed": "executions",
                      "ms": "time (ms)"}[col])
    fig.suptitle("Per target side, by branch side (log, whiskers 1.5 IQR)")
    save(fig, out, "04-by-side")

    # 5. cumulative solved targets vs executions and vs time
    passed = own[own["status"] == "pass"]
    fig, axes = plt.subplots(1, 2, figsize=(8, 3))
    for ax, col, label in [(axes[0], "executed", "programs executed"),
                           (axes[1], "ms", "time (ms)")]:
        s = np.sort(passed[col].values)
        ax.step(s, np.arange(1, len(s) + 1) / len(own) * 100, color=C[0],
                where="post")
        ax.set_xscale("symlog" if col == "executed" else "log")
        ax.set_xlabel(f"{label} until pass")
        ax.set_ylabel("% of solved-by-own target sides")
    fig.suptitle("How quickly target sides are covered")
    save(fig, out, "05-cdf-until-pass")

    # 6. where the passing program comes from
    pp = P[P["outcome"] == "pass"]
    fig, axes = plt.subplots(1, 3, figsize=(9, 3))
    for ax, col, label in [(axes[0], "rank", "entry rank (0 = nearest)"),
                           (axes[1], "index", "path index in entry"),
                           (axes[2], "executed", "program index in path")]:
        vc = pp[col].value_counts().sort_index()
        head = vc[vc.index < 20]
        ax.bar(head.index, head.values, color=C[0], width=0.8)
        rest = vc[vc.index >= 20].sum()
        ax.set_title(f"{label}\n(>=20: {rest:,})", fontsize=9)
        ax.set_yscale("log")
    fig.suptitle("Position of the covering program", y=1.08)
    save(fig, out, "06-pass-position")

    # 7. entries: how many exist vs how many tried, and per-entry budget
    fig, axes = plt.subplots(1, 2, figsize=(8, 3))
    axes[0].scatter(own["nEntries"], own["nTried"], s=6, alpha=0.3,
                    color=C[0], linewidths=0)
    axes[0].set_xscale("log")
    axes[0].set_yscale("log")
    axes[0].set_xlabel("entry functions available")
    axes[0].set_ylabel("entry functions tried")
    axes[1].hist(T["perEntryMs"], bins=np.logspace(0, 4, 40), color=C[2])
    axes[1].set_xscale("log")
    axes[1].set_xlabel("per-entry time budget (ms, log)")
    axes[1].set_ylabel("target sides")
    fig.suptitle("Entry functions per target side")
    save(fig, out, "07-entries")

    # 8. time breakdown
    search = E["searchMs"].sum()
    execute = P["execMs"].sum()
    total = E["ms"].sum()
    parts = {"symbolic search": search, "program execution": execute,
             "synthesis + other": max(total - search - execute, 0)}
    fig, ax = plt.subplots(figsize=(6, 1.6))
    left = 0
    for i, (k, v) in enumerate(parts.items()):
        ax.barh(0, v / total * 100, left=left, color=C[i], edgecolor="white",
                linewidth=2, label=f"{k} {v / total * 100:.1f}%")
        left += v / total * 100
    ax.set_yticks([])
    ax.set_xlim(0, 100)
    ax.set_xlabel("% of solver time over all target sides (CPU-thread time)")
    ax.legend(frameon=False, ncol=3, bbox_to_anchor=(0.5, 1.5),
              loc="upper center", fontsize=8)
    ax.grid(False)
    save(fig, out, "08-time-breakdown")

    # 9. templates per slot
    if Tm is not None:
        vc = Tm.groupby("slot").size().sort_values()
        fig, ax = plt.subplots(figsize=(6, max(2.5, len(vc) * 0.14)))
        ax.barh(vc.index, vc.values, color=C[0], height=0.7)
        ax.set_xlabel("templates")
        ax.set_title(f"Object construction templates per internal slot "
                     f"({Tm['template'].nunique()} distinct)")
        ax.tick_params(axis="y", labelsize=6)
        save(fig, out, "09-templates-per-slot")

    # 10. witness programs: size and nested templates
    if prog is not None:
        p = prog.drop_duplicates("script")
        fig, axes = plt.subplots(1, 2, figsize=(8, 3))
        axes[0].hist(p["chars"], bins=log_bins(p["chars"].max(), 40),
                     color=C[0])
        axes[0].set_xscale("log")
        axes[0].set_xlabel("program length (chars, log)")
        axes[0].set_ylabel("distinct programs")
        vc = p["templateUses"].value_counts().sort_index()
        axes[1].bar(vc.index, vc.values, color=C[1])
        axes[1].set_xlabel("nested template calls")
        axes[1].set_yscale("log")
        fig.suptitle("Covering programs")
        save(fig, out, "10-programs")

    # 11. dedup and duplicate symbolic paths
    fig, axes = plt.subplots(1, 2, figsize=(8, 3))
    dup = (P["symPaths"] - 1).clip(lower=0)
    vc = dup.value_counts().sort_index()
    axes[0].bar(vc.index[:30], vc.values[:30], color=C[2])
    axes[0].set_yscale("log")
    axes[0].set_xlabel("duplicate symbolic paths skipped before a path")
    axes[0].set_ylabel("paths")
    lost = (P["generated"] - P["executed"])[P["outcome"] == "fail"]
    axes[1].hist(lost, bins=np.arange(0, BUDGET + 2), color=C[3])
    axes[1].set_yscale("log")
    axes[1].set_xlabel("duplicate programs dropped (failed paths)")
    fig.suptitle("Redundancy")
    save(fig, out, "11-redundancy")


# -----------------------------------------------------------------------------
# report
# -----------------------------------------------------------------------------
def report(run, T, E, P, Tm, prog, out: Path) -> str:
    own = T[~T["reused"]]
    L = [f"# Solver statistics: `{run.name}`\n"]
    summary = run / "summary"
    if summary.exists():
        L += ["```", *summary.read_text().split("\n\n")[0].splitlines(), "```\n"]

    L.append("## Overview\n")
    covered_other = ((T["status"] != "pass") & T["covered"]).sum()
    rows = {
        "target sides": len(T),
        "  covered before own turn (reused)": int(T["reused"].sum()),
        "  solved by own search": len(own),
        "  own status != pass but covered by another target's program":
            int(covered_other),
        "entry functions tried (sum)": len(E),
        "symbolic paths found (incl. duplicate invocations)":
            int(E["symPaths"].sum()),
        "distinct paths tried (invocations)": len(P),
        "paths with no candidate program": int((P["outcome"] == "no-candidate")
                                               .sum()),
        "paths reaching the per-path budget (100)": int(P["budgetHit"].sum()),
        "programs generated (pre-dedup, within budget)":
            int(P["generated"].sum()),
        "programs executed": int(P["executed"].sum()),
        "executions cut off by the deadline": int(P["aborted"].sum()),
        "executions that threw / hit the 2 s coverage limit":
            int(P["errors"].sum()),
        "branch sides first covered incidentally": int(P["incidental"].sum()),
        "solver thread time (s)": round(E["ms"].sum() / 1000, 1),
        "  symbolic search (s)": round(E["searchMs"].sum() / 1000, 1),
        "  program execution (s)": round(P["execMs"].sum() / 1000, 1),
    }
    L.append(fmt(pd.DataFrame(rows.items(), columns=["metric", "value"])
                 .set_index("metric")))
    L.append("")

    L.append("## Path outcomes\n")
    L.append(fmt(P["outcome"].value_counts().to_frame("paths")))
    L.append("")

    L.append("## Distributions (own-search target sides)\n")
    L.append(fmt(dist_table({
        "entries available / target": own["nEntries"],
        "entries tried / target": own["nTried"],
        "per-entry budget (ms)": own["perEntryMs"],
        "paths / target": own["paths"],
        "paths / entry": E["nPaths"],
        "executions / path": P["executed"],
        "executions / target": own["executed"],
        "time / target (ms)": own["ms"],
        "exec time / program (ms)": P["execMs"] / P["executed"]
                                      .replace(0, np.nan),
    })))
    L.append("")

    for key, label in [("status", "final status"), ("sideName", "branch side")]:
        L.append(f"## By {label}\n")
        g = own.groupby(key)
        tab = pd.DataFrame({
            "targets": g.size(),
            "paths sum": g["paths"].sum(),
            "paths median": g["paths"].median(),
            "paths mean": g["paths"].mean(),
            "paths max": g["paths"].max(),
            "exec sum": g["executed"].sum(),
            "exec median": g["executed"].median(),
            "exec mean": g["executed"].mean(),
            "exec p90": g["executed"].quantile(0.9),
            "exec max": g["executed"].max(),
            "time median (ms)": g["ms"].median(),
        })
        if key == "sideName":
            tab["pass %"] = own.groupby(key)["covered"].mean() * 100
        L.append(fmt(tab))
        L.append("")

    if key == "sideName":
        L.append("## Status x branch side (all target sides)\n")
        L.append(fmt(pd.crosstab(T["status"], T["sideName"], margins=True)))
        L.append("")

    pp = P[P["outcome"] == "pass"]
    L.append("## Covering program position (own-search passes)\n")
    L.append(fmt(pd.DataFrame({
        "on first entry": [(pp["rank"] == 0).mean() * 100],
        "on first path of entry": [(pp["index"] == 0).mean() * 100],
        "first program of path": [(pp["executed"] == 1).mean() * 100],
        "within 10 programs": [(pp["executed"] <= 10).mean() * 100],
    }, index=["% of passes"]).T))
    L.append("")
    exec_until = own.loc[own["status"] == "pass", "executed"]
    L.append(fmt(dist_table({"executions until pass / target": exec_until,
                             "time until pass (ms)":
                                 own.loc[own["status"] == "pass", "ms"]})))
    L.append("")

    L.append("## Hardest target functions (most unsolved own-search sides)\n")
    bad = own[~own["covered"]].groupby("func").agg(
        unsolved=("id", "size"), exec_mean=("executed", "mean"),
        paths_mean=("paths", "mean")).sort_values("unsolved", ascending=False)
    L.append(fmt(bad.head(20)))
    L.append("")

    L.append("## Most expensive target sides (executions)\n")
    L.append(fmt(own.sort_values("executed", ascending=False)
                 [["id", "func", "status", "nEntries", "nTried", "paths",
                   "executed", "ms"]].head(15).set_index("id")))
    L.append("")

    L.append("## Entry functions that produce covering programs\n")
    wins = pp["entry"].str.removeprefix("INTRINSICS.").value_counts()
    L.append(f"{wins.size} distinct entry functions cover at least one side.\n")
    L.append(fmt(wins.head(20).to_frame("passes")))
    L.append("")

    if Tm is not None:
        L.append("## Object construction templates\n")
        per_slot = Tm.groupby("slot").size()
        L.append(fmt(pd.DataFrame({
            "internal slots with a template": [per_slot.size],
            "(slot, template) pairs": [len(Tm)],
            "distinct templates": [Tm["template"].nunique()],
            "templates with a relation": [Tm.drop_duplicates("template")
                                          ["relation"].sum()],
            "templates via Reflect.construct": [Tm.drop_duplicates("template")
                                                ["template"].str.startswith(
                                                    "Reflect.construct").sum()],
        }, index=["value"]).T))
        L.append("")
        L.append(fmt(dist_table({"templates / slot": per_slot,
                                 "holes / template":
                                     Tm.drop_duplicates("template")["holes"]})))
        L.append("")
        L.append(fmt(per_slot.sort_values(ascending=False).head(15)
                     .to_frame("templates")))
        L.append("")

    if prog is not None:
        p = prog.drop_duplicates("script")
        L.append("## Covering programs (branch-coverage.json)\n")
        L.append(fmt(pd.DataFrame({
            "covered branch sides": [len(prog)],
            "distinct programs": [len(p)],
            "programs using >= 1 nested template": [(p["templateUses"] > 0)
                                                    .sum()],
            "programs with Reflect.construct": [(p["construct"] > 0).sum()],
            "programs with accessor .get/.set.call": [(p["accessor"] > 0).sum()],
            "programs with an arrow function": [(p["arrow"] > 0).sum()],
        }, index=["value"]).T))
        L.append("")
        L.append(fmt(dist_table({"length (chars)": p["chars"],
                                 "nested template calls": p["templateUses"],
                                 "sides covered / program":
                                     prog.groupby("script").size()})))
        L.append("")

    L.append("## Figures\n")
    for f in sorted(out.glob("*.png")):
        L.append(f"![{f.stem}]({f.name})")
    return "\n".join(L) + "\n"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("run", nargs="?", default="logs/solver/recent")
    ap.add_argument("-o", "--out")
    args = ap.parse_args()
    run = Path(args.run).resolve()
    out = Path(args.out) if args.out else run / "stats"
    out.mkdir(parents=True, exist_ok=True)
    T, E, P = load(run)
    Tm, _ = template_stats(run)
    prog = program_stats(run, Tm)
    figures(T, E, P, Tm, prog, out)
    T.drop(columns=[]).to_csv(out / "targets.csv", index=False)
    E.to_csv(out / "entries.csv", index=False)
    P.to_csv(out / "paths.csv", index=False)
    (out / "report.md").write_text(report(run, T, E, P, Tm, prog, out))
    print(f"wrote {out}/report.md")


if __name__ == "__main__":
    main()
