"""Figures for the counsellor-score validity analysis report.
PHI-safe: opaque counsellor_id + numeric n_messages + rubric item booleans only.
Writes PNGs to manuscript_outputs/. Light-theme, print-friendly.
"""
import os
import numpy as np
import pandas as pd
import pyarrow.parquet as pq
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

np.random.seed(7)
OUT = "evals/analysis/manuscript_outputs"
os.makedirs(OUT, exist_ok=True)
DATA = "evals/retention_satisfaction_analysis/data"

ACC, WARM, AQUA, VIOLET, GREY = "#2a78d6", "#e0663a", "#1baf7a", "#5a4aae", "#9aa3af"
plt.rcParams.update({
    "figure.dpi": 150, "savefig.dpi": 150, "font.size": 9,
    "axes.spines.top": False, "axes.spines.right": False,
    "axes.edgecolor": "#c9cdd4", "axes.labelcolor": "#333", "text.color": "#222",
    "xtick.color": "#555", "ytick.color": "#555", "axes.titleweight": "600",
    "axes.grid": True, "grid.color": "#eceef1", "grid.linewidth": 0.8,
})

trim = pq.read_table(f"{DATA}/deployment_trimmed_20260921.parquet").to_pandas()
full = pq.read_table(f"{DATA}/deployment_full_dataset_20260921.parquet",
                     columns=["conversation_uid", "n_messages"]).to_pandas()
for d in (trim, full):
    d["conversation_uid"] = d["conversation_uid"].astype(str)
df = trim.merge(full, on="conversation_uid", how="inner")
df["cid"] = df["counsellor_id"].astype(str)
df = df.dropna(subset=["n_messages", "fractional_score", "cid"])
df = df[df["cid"].str.lower().ne("nan") & df["cid"].ne("")].copy()
df["n_messages"] = df["n_messages"].astype(int)
df["frac"] = df["fractional_score"].astype(float)
DIMS = {"Productivity": ["P1","P2","P3","P4","P5","P6","P8"],
        "Micro-skills": ["M1","M2","M3","M4","M5"],
        "Style & tone": ["S1","S2","S3","S4","S5","S6","S7"]}
for dim, its in DIMS.items():
    df[dim] = df[its].astype(float).mean(axis=1)


def binned(sub, col, step=5, lo=20, hi=140):
    xs, ms, cis = [], [], []
    for a in range(lo, hi + 1, step):
        s = sub[(sub["n_messages"] >= a) & (sub["n_messages"] < a + step)][col].dropna()
        if len(s) < 15:
            continue
        xs.append(a + step / 2); ms.append(s.mean())
        cis.append(1.96 * s.std(ddof=1) / np.sqrt(len(s)))
    return np.array(xs), np.array(ms), np.array(cis)


# ---------- FIG 1: length distribution + score vs length ----------
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(9, 3.2))
d150 = df[df["n_messages"] <= 150]["n_messages"]  # drop ~1% tail instead of piling it into one bar
ax1.hist(d150, bins=range(20, 152, 2), color=ACC, alpha=.85)
ax1.set_title("Conversation length distribution", loc="left")
ax1.set_xlabel("messages per conversation"); ax1.set_ylabel("conversations")
ax1.axvline(20, color="#333", ls="--", lw=1)
ax1.text(24, ax1.get_ylim()[1]*.9, "already floored at 20  ·  tail >150 (~1%) not shown", fontsize=7.5, color="#333")

x, m, ci = binned(df, "frac")
ax2.fill_between(x, m-ci, m+ci, color=ACC, alpha=.18)
ax2.plot(x, m, color=ACC, lw=2)
ax2.set_title("Mean audit score rises with length", loc="left")
ax2.set_xlabel("messages per conversation"); ax2.set_ylabel("fractional_score")
ax2.set_ylim(0.55, 1.0)
ax2.annotate("0.64 @ 20 msgs", (x[0], m[0]), (x[0]+12, m[0]-.02), fontsize=8,
             color=ACC, arrowprops=dict(arrowstyle="-", color=ACC, lw=.8))
ax2.annotate("0.92 @ 60+", (x[9], m[9]), (x[9]-4, m[9]+.03), fontsize=8, color=ACC)
fig.tight_layout(); fig.savefig(f"{OUT}/fig1_length_score.png"); plt.close(fig)

# ---------- FIG 2: reliability vs N + length-luck ----------
def var_comp(y):
    grand = df[y].mean(); grp = df.groupby("cid")[y]; ni, mi = grp.size(), grp.mean()
    k, N = len(ni), len(df)
    msb = (ni*(mi-grand)**2).sum()/(k-1); msw = ((df[y]-df["cid"].map(mi))**2).sum()/(N-k)
    n0 = (N-(ni**2).sum()/N)/(k-1); return max(0,(msb-msw)/n0), msw
vb, vw = var_comp("frac")
ns = np.arange(2, 101)
model_R = vb/(vb+vw/ns)
vc = df["cid"].value_counts()
emp_ns, emp_R = [], []
for n in (5, 10, 20, 30, 50):
    elig = vc[vc >= 2*n].index
    cs = []
    for _ in range(25):
        a, b = [], []
        for cid in elig:
            s = df[df["cid"] == cid]["frac"].sample(2*n).values
            a.append(s[:n].mean()); b.append(s[n:].mean())
        cs.append(np.corrcoef(a, b)[0, 1])
    emp_ns.append(n); emp_R.append(np.mean(cs))

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(9, 3.2))
ax1.plot(ns, model_R, color=GREY, lw=1.6, label="model (variance components)")
ax1.plot(emp_ns, emp_R, "o-", color=ACC, lw=2, ms=5, label="empirical (split-sample)")
ax1.axvline(20, color="#333", ls="--", lw=1); ax1.axhline(0.8, color=WARM, ls=":", lw=1)
ax1.text(21, 0.6, "min N = 20", fontsize=8, color="#333")
ax1.text(60, 0.81, "R = 0.8", fontsize=8, color=WARM)
ax1.set_title("Reliability of a counsellor mean vs. N", loc="left")
ax1.set_xlabel("conversations behind the average"); ax1.set_ylabel("reliability")
ax1.set_ylim(0.4, 1.0); ax1.legend(fontsize=7.5, frameon=False, loc="lower right")

pool = df.groupby(np.minimum(df["n_messages"],150)//5)["frac"].transform("mean").values
drift = [np.std([np.random.choice(pool, n, replace=False).mean() for _ in range(2000)]) for n in ns]
ax2.plot(ns, np.array(drift)*100, color=WARM, lw=2)
ax2.axvline(20, color="#333", ls="--", lw=1)
ax2.set_title("Score drift from length luck alone", loc="left")
ax2.set_xlabel("conversations behind the average"); ax2.set_ylabel("SD of mean (points)")
ax2.annotate("±3 pts by N=20", (20, drift[18]*100), (30, drift[18]*100+1),
             fontsize=8, color=WARM, arrowprops=dict(arrowstyle="-", color=WARM, lw=.8))
fig.tight_layout(); fig.savefig(f"{OUT}/fig2_reliability.png"); plt.close(fig)

# ---------- FIG 3: per-dimension score vs length (small multiples) ----------
risk = df[df["X0"] == True].copy()
risk["Safety"] = risk[["X1","X2","X3","X4"]].astype(float).mean(axis=1)
panels = [("Productivity", df, ACC), ("Micro-skills", df, AQUA),
          ("Style & tone", df, VIOLET), ("Safety (risk cases)", risk, WARM)]
fig, axes = plt.subplots(2, 2, figsize=(8, 5.4), sharex=True, sharey=True)
xf, mf, _ = binned(df, "frac")
for ax, (name, sub, col) in zip(axes.ravel(), panels):
    colname = "Safety" if name.startswith("Safety") else name
    x, m, ci = binned(sub, colname)
    ax.plot(xf, mf, color=GREY, lw=1.2, ls="--", label="overall")
    ax.fill_between(x, m-ci, m+ci, color=col, alpha=.18)
    ax.plot(x, m, color=col, lw=2, label=name)
    sw = (sub[sub["n_messages"]>=60][colname].mean() - sub[sub["n_messages"]<=24][colname].mean())
    ax.set_title(f"{name}   (Δ{sw:+.2f})", loc="left", fontsize=9)
    ax.set_ylim(0.1, 1.0)
for ax in axes[1]:
    ax.set_xlabel("messages")
for ax in axes[:, 0]:
    ax.set_ylabel("mean score")
axes[0,0].legend(fontsize=7, frameon=False, loc="lower right")
fig.suptitle("Every dimension climbs with length — least for Style, most for Safety",
             x=.01, ha="left", fontsize=10, weight="600")
fig.tight_layout(); fig.savefig(f"{OUT}/fig3_dimensions.png"); plt.close(fig)

print("wrote fig1_length_score.png, fig2_reliability.png, fig3_dimensions.png to", OUT)
