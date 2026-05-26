"""
mixology_benchmark_viz.py
─────────────────────────
Generates a 4-panel 300 dpi figure benchmarking 8 sentiment lexicons
on the Mixology politics corpus.

Usage:
    python mixology_benchmark_viz.py

Output:
    mixology_benchmark.png  (300 dpi, ~3400 × 2400 px)
    mixology_benchmark.pdf  (vector, for publications)

Requirements:
    pip install matplotlib numpy
"""

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.ticker as mticker
import numpy as np

# ══════════════════════════════════════════════════════════════════════════
# DATA — update these constants with your own corpus results
#
# After running pipeline_300k.R, copy values from the following R objects:
#
#   TOK_COV       <- coverage_tbl$coverage * 100   (ordered as LEXICONS)
#   PCT_POS       <- benchmark$pct_positive        (ordered as LEXICONS)
#   PCT_NEG       <- benchmark$pct_negative        (ordered as LEXICONS)
#   PCT_AMB       <- benchmark$pct_ambiguous       (ordered as LEXICONS)
#   NEG_BIAS      <- benchmark$neg_bias            (ordered as LEXICONS)
#   SCORE_COV     <- perf_score$score_coverage     (ordered as LEXICONS)
#   SCORE_CLASSIF <- perf_score$score_classif      (ordered as LEXICONS)
#   SCORE_BALANCE <- perf_score$score_balance      (ordered as LEXICONS)
#   SCORE_GLOBAL  <- perf_score$score_global       (ordered as LEXICONS)
#
# Keep LEXICONS in this exact order — all lists must match it:
# ══════════════════════════════════════════════════════════════════════════

LEXICONS = [
    "General Inquirer",
    "MPQA Subjectivity",
    "Bing Liu",
    "NRC",
    "AFINN",
    "Loughran-McDonald",
    "Mixology Covid",
    "Mixology",
]

IS_MIXO = [False, False, False, False, False, False, True, True]

# Number of terms per lexicon — fixed, no need to update
N_TERMS = [4206, 6884, 6783, 6456, 2477, 3917, 4166, 16528]

# ── Update the values below with your corpus results ─────────────────────

# Token coverage (%) — from: coverage_tbl$coverage * 100
TOK_COV = [6.4, 20.4, 13.6, 19.7, 13.8, 7.6, 65.9, 73.6]

# Tweet-level polarity (% of matched tweets) — from: benchmark$pct_*
PCT_POS = [40.1, 35.8, 30.8, 41.7, 29.3, 22.6, 33.4, 30.7]
PCT_NEG = [47.4, 43.3, 54.7, 40.2, 55.2, 61.8, 50.6, 56.0]
PCT_AMB = [0.0,  20.9, 14.5, 18.1, 15.5, 15.6,  6.3,  6.5]

# Negative bias — from: benchmark$neg_bias
NEG_BIAS = [1.15, 1.15, 1.56, 0.99, 1.55, 2.40, 1.14, 1.23]

# Synthetic performance score — from: perf_score$score_*
SCORE_COV     = [6.4,  20.4, 13.6, 19.7, 13.8, 7.6,  65.9, 73.6]
SCORE_CLASSIF = [47.3, 80.7, 71.0, 81.5, 72.0, 52.8, 99.9, 99.9]
SCORE_BALANCE = [87.3, 86.9, 64.2, 99.1, 64.5, 41.6, 87.4, 81.5]
SCORE_GLOBAL  = [34.9, 51.8, 40.9, 54.1, 41.4, 28.0, 80.4, 83.1]

# ── Palette ───────────────────────────────────────────────────────────────────

COL_GENERAL = "#378ADD"   # blue — general lexicons
COL_MIXO    = "#1D9E75"   # teal — Mixology lexicons
COL_POS     = "#1D9E75"
COL_NEG     = "#D85A30"
COL_AMB     = "#888780"
COL_COV     = "#378ADD"
COL_CLS     = "#1D9E75"
COL_BAL     = "#BA7517"
COL_BIAS_OK = "#1D9E75"
COL_BIAS_MD = "#BA7517"
COL_BIAS_HI = "#D85A30"

FONT = "DejaVu Sans"

# ── Layout ────────────────────────────────────────────────────────────────────

fig = plt.figure(figsize=(14, 10), dpi=300)
fig.patch.set_facecolor("white")

gs = fig.add_gridspec(
    2, 2,
    hspace=0.42, wspace=0.38,
    left=0.10, right=0.97,
    top=0.91, bottom=0.07,
)

ax1 = fig.add_subplot(gs[0, 0])   # token coverage
ax2 = fig.add_subplot(gs[0, 1])   # polarity distribution
ax3 = fig.add_subplot(gs[1, 0])   # negative bias
ax4 = fig.add_subplot(gs[1, 1])   # synthetic score

# Sort order (by token coverage ascending for ax1, consistent elsewhere)
order = np.argsort(TOK_COV)
lex_sorted  = [LEXICONS[i]  for i in order]
mixo_sorted = [IS_MIXO[i]   for i in order]

def colors_by_type(order):
    return [COL_MIXO if IS_MIXO[i] else COL_GENERAL for i in order]

def subtitle(ax, text):
    ax.set_title(text, fontsize=8, color="#555555",
                 fontname=FONT, pad=2, loc="left")

def panel_label(ax, letter):
    ax.text(-0.12, 1.06, letter, transform=ax.transAxes,
            fontsize=13, fontweight="bold", fontname=FONT,
            va="top", ha="left", color="#222222")

# ── Panel A — Token coverage ──────────────────────────────────────────────────

y      = np.arange(len(LEXICONS))
vals   = [TOK_COV[i] for i in order]
cols   = colors_by_type(order)

bars = ax1.barh(y, vals, color=cols, height=0.6, edgecolor="white", linewidth=0.4)

for bar_, val in zip(bars, vals):
    ax1.text(val + 0.8, bar_.get_y() + bar_.get_height() / 2,
             f"{val:.1f}%", va="center", ha="left",
             fontsize=7.5, fontname=FONT, color="#333333")

ax1.set_yticks(y)
ax1.set_yticklabels(lex_sorted, fontsize=8.5, fontname=FONT)
ax1.set_xlabel("Token coverage (%)", fontsize=8.5, fontname=FONT)
ax1.set_xlim(0, 90)
ax1.xaxis.set_major_formatter(mticker.FormatStrFormatter("%g%%"))
ax1.spines[["top", "right"]].set_visible(False)
ax1.tick_params(axis="both", labelsize=8)

panel_label(ax1, "A")
ax1.set_title("Token coverage by lexicon", fontsize=10, fontweight="bold",
              fontname=FONT, pad=8, loc="left")
subtitle(ax1, "% of cleaned corpus tokens matched")

legend_els = [
    mpatches.Patch(facecolor=COL_GENERAL, label="General-purpose lexicons"),
    mpatches.Patch(facecolor=COL_MIXO,    label="Mixology lexicons"),
]
ax1.legend(handles=legend_els, fontsize=7.5, frameon=False,
           loc="lower right", handlelength=1.2)

# ── Panel B — Polarity distribution ──────────────────────────────────────────

order_pol = np.argsort(PCT_NEG)
lex_pol   = [LEXICONS[i] for i in order_pol]
pos_pol   = [PCT_POS[i]  for i in order_pol]
neg_pol   = [PCT_NEG[i]  for i in order_pol]
amb_pol   = [PCT_AMB[i]  for i in order_pol]

y = np.arange(len(LEXICONS))
h = 0.22

ax2.barh(y + h,   pos_pol, height=h, color=COL_POS, label="Positive",  edgecolor="white", linewidth=0.3)
ax2.barh(y,       neg_pol, height=h, color=COL_NEG, label="Negative",  edgecolor="white", linewidth=0.3)
ax2.barh(y - h,   amb_pol, height=h, color=COL_AMB, label="Ambiguous", edgecolor="white", linewidth=0.3)

ax2.set_yticks(y)
ax2.set_yticklabels(lex_pol, fontsize=8.5, fontname=FONT)
ax2.set_xlabel("% of classified tweets", fontsize=8.5, fontname=FONT)
ax2.set_xlim(0, 80)
ax2.xaxis.set_major_formatter(mticker.FormatStrFormatter("%g%%"))
ax2.spines[["top", "right"]].set_visible(False)
ax2.tick_params(axis="both", labelsize=8)
ax2.legend(fontsize=7.5, frameon=False, loc="lower right",
           handlelength=1.2, ncol=3)

panel_label(ax2, "B")
ax2.set_title("Tweet-level polarity distribution", fontsize=10,
              fontweight="bold", fontname=FONT, pad=8, loc="left")
subtitle(ax2, "Matched tweets only — sorted by % negative")

# ── Panel C — Negative bias ───────────────────────────────────────────────────

order_bias = np.argsort(NEG_BIAS)
lex_bias   = [LEXICONS[i]  for i in order_bias]
bias_vals  = [NEG_BIAS[i]  for i in order_bias]

def bias_color(v):
    if v < 1.2: return COL_BIAS_OK
    if v < 1.6: return COL_BIAS_MD
    return COL_BIAS_HI

bias_cols = [bias_color(v) for v in bias_vals]
y = np.arange(len(LEXICONS))

bars = ax3.barh(y, bias_vals, color=bias_cols, height=0.6,
                edgecolor="white", linewidth=0.4)
ax3.axvline(1.0, color="#333333", linewidth=0.8, linestyle="--", alpha=0.6)
ax3.text(1.01, len(LEXICONS) - 0.3, "balanced", fontsize=7,
         color="#555555", fontname=FONT, va="top")

for bar_, val in zip(bars, bias_vals):
    ax3.text(val + 0.02, bar_.get_y() + bar_.get_height() / 2,
             f"{val:.2f}", va="center", ha="left",
             fontsize=7.5, fontname=FONT, color="#333333")

ax3.set_yticks(y)
ax3.set_yticklabels(lex_bias, fontsize=8.5, fontname=FONT)
ax3.set_xlabel("Negative tokens / Positive tokens", fontsize=8.5, fontname=FONT)
ax3.set_xlim(0, 3.0)
ax3.spines[["top", "right"]].set_visible(False)
ax3.tick_params(axis="both", labelsize=8)

legend_els = [
    mpatches.Patch(facecolor=COL_BIAS_OK, label="< 1.2  balanced"),
    mpatches.Patch(facecolor=COL_BIAS_MD, label="1.2–1.6  moderate"),
    mpatches.Patch(facecolor=COL_BIAS_HI, label="> 1.6  high bias"),
]
ax3.legend(handles=legend_els, fontsize=7.5, frameon=False,
           loc="lower right", handlelength=1.2)

panel_label(ax3, "C")
ax3.set_title("Negative bias", fontsize=10, fontweight="bold",
              fontname=FONT, pad=8, loc="left")
subtitle(ax3, "neg_tokens / pos_tokens — dashed = perfectly balanced")

# ── Panel D — Synthetic performance score ─────────────────────────────────────

order_sc  = np.argsort(SCORE_GLOBAL)
lex_sc    = [LEXICONS[i]      for i in order_sc]
sc_cov    = [0.5 * SCORE_COV[i]     for i in order_sc]
sc_cls    = [0.3 * SCORE_CLASSIF[i] for i in order_sc]
sc_bal    = [0.2 * SCORE_BALANCE[i] for i in order_sc]
sc_global = [SCORE_GLOBAL[i]        for i in order_sc]

y    = np.arange(len(LEXICONS))
left = np.zeros(len(LEXICONS))

b1 = ax4.barh(y, sc_cov, height=0.6, color=COL_COV,
              edgecolor="white", linewidth=0.4, label="Coverage (×0.5)")
left += np.array(sc_cov)

b2 = ax4.barh(y, sc_cls, height=0.6, color=COL_CLS, left=left,
              edgecolor="white", linewidth=0.4, label="Classification rate (×0.3)")
left += np.array(sc_cls)

b3 = ax4.barh(y, sc_bal, height=0.6, color=COL_BAL, left=left,
              edgecolor="white", linewidth=0.4, label="Balance (×0.2)")

for i, (total, bar_) in enumerate(zip(sc_global, b1)):
    ax4.text(total + 0.5, bar_.get_y() + bar_.get_height() / 2,
             f"{total:.1f}", va="center", ha="left",
             fontsize=7.5, fontname=FONT,
             fontweight="bold" if [IS_MIXO[j] for j in order_sc][i] else "normal",
             color="#1D9E75" if [IS_MIXO[j] for j in order_sc][i] else "#333333")

ax4.set_yticks(y)
ax4.set_yticklabels(lex_sc, fontsize=8.5, fontname=FONT)
ax4.set_xlabel("Score", fontsize=8.5, fontname=FONT)
ax4.set_xlim(0, 105)
ax4.spines[["top", "right"]].set_visible(False)
ax4.tick_params(axis="both", labelsize=8)
ax4.legend(fontsize=7.5, frameon=False, loc="lower right",
           handlelength=1.2)

panel_label(ax4, "D")
ax4.set_title("Synthetic performance score", fontsize=10, fontweight="bold",
              fontname=FONT, pad=8, loc="left")
subtitle(ax4, "Coverage 50% + Classification 30% + Balance 20%")

# ── Main title ────────────────────────────────────────────────────────────────

fig.text(
    0.03, 0.975,
    "Benchmark of 8 sentiment lexicons — political measures corpus (n = 4,371 tweets)",
    fontsize=11, fontweight="bold", fontname=FONT,
    va="top", ha="left", color="#111111"
)
fig.text(
    0.03, 0.957,
    "Mixology project · Dierickx, L. (2022) · ohmybox.info · "
    "Negation window = 3 tokens · Mixology lexicons: weighted scoring",
    fontsize=7.5, fontname=FONT, va="top", ha="left", color="#666666"
)

# ── Save ──────────────────────────────────────────────────────────────────────

fig.savefig("mixology_benchmark.png", dpi=300, bbox_inches="tight",
            facecolor="white")
fig.savefig("mixology_benchmark.pdf", dpi=300, bbox_inches="tight",
            facecolor="white")

print("Saved: mixology_benchmark.png (300 dpi)")
print("Saved: mixology_benchmark.pdf (vector)")
