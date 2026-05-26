"""
mixology_pipeline_schema.py
────────────────────────────
Generates a publication-ready 300 dpi schema of the Mixology
sentiment analysis pipeline (6 stages).

Usage:
    python mixology_pipeline_schema.py

Output:
    mixology_pipeline_schema.png  (300 dpi)
    mixology_pipeline_schema.pdf  (vector, for publications)

Requirements:
    pip install matplotlib

This script contains no data constants to update — the pipeline
stages, term counts, and methodological labels are fixed.
Only edit the subtitle text if your corpus differs from the default
(300k+ tweets, Western Europe, December 2021).
"""

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.patches import FancyBboxPatch
import numpy as np

# ── Palette ───────────────────────────────────────────────────────────────────
GRAY_FILL   = "#F1EFE8"; GRAY_EDGE   = "#5F5E5A"; GRAY_TXT  = "#2C2C2A"
BLUE_FILL   = "#E6F1FB"; BLUE_EDGE   = "#185FA5"; BLUE_TXT  = "#0C447C"
TEAL_FILL   = "#E1F5EE"; TEAL_EDGE   = "#0F6E56"; TEAL_TXT  = "#085041"
PURP_FILL   = "#EEEDFE"; PURP_EDGE   = "#534AB7"; PURP_TXT  = "#3C3489"
CORAL_FILL  = "#FAECE7"; CORAL_EDGE  = "#993C1D"; CORAL_TXT = "#712B13"
ARROW_COL   = "#444441"
SUB_COL     = "#5F5E5A"
CAPTION_COL = "#888780"
INNER_ALPHA = 0.55

FONT = "DejaVu Sans"

# ── Figure setup ──────────────────────────────────────────────────────────────
FIG_W, FIG_H = 10, 14.2
fig, ax = plt.subplots(figsize=(FIG_W, FIG_H), dpi=300)
fig.patch.set_facecolor("white")
ax.set_xlim(0, 10); ax.set_ylim(-0.6, 13.4)
ax.axis("off")

# ── Helpers ───────────────────────────────────────────────────────────────────

def rbox(ax, x, y, w, h, fill, edge, radius=0.18, lw=0.8, alpha=1.0):
    box = FancyBboxPatch(
        (x, y), w, h,
        boxstyle=f"round,pad=0,rounding_size={radius}",
        linewidth=lw, edgecolor=edge, facecolor=fill, alpha=alpha,
        zorder=2
    )
    ax.add_patch(box)

def title_sub(ax, cx, y_title, y_sub, title, sub, tcol, scol=None):
    ax.text(cx, y_title, title, ha="center", va="center",
            fontsize=9, fontweight="bold", color=tcol, fontname=FONT, zorder=3)
    if sub:
        ax.text(cx, y_sub, sub, ha="center", va="center",
                fontsize=7.5, color=scol or SUB_COL, fontname=FONT, zorder=3)

def arrow(ax, x, y1, y2, col=ARROW_COL):
    ax.annotate("", xy=(x, y2), xytext=(x, y1),
                arrowprops=dict(arrowstyle="-|>", color=col,
                                lw=1.2, mutation_scale=10))

def stage_label(ax, y, n, label, col):
    ax.text(0.18, y, f"Stage {n}", ha="center", va="center",
            fontsize=7, fontweight="bold", color=col,
            fontname=FONT, zorder=3,
            bbox=dict(boxstyle="round,pad=0.18", fc=col, ec="none", alpha=0.12))
    ax.text(0.18, y - 0.22, label, ha="center", va="center",
            fontsize=6.5, color=col, fontname=FONT, zorder=3)

# ── Stage positions (y = top of box, heights vary) ────────────────────────────
LEFT  = 0.30
RIGHT = 9.70
W     = RIGHT - LEFT
MID   = (LEFT + RIGHT) / 2

STAGES = [
    # (y_top, height, fill, edge, txt, title, sub, stage_n, stage_label)
    (12.10, 0.72, GRAY_FILL,  GRAY_EDGE,  GRAY_TXT,
     "Stage 1 — Raw corpus",
     "300k+ English tweets  ·  Twitter API  ·  Dec. 2021  ·  Western Europe",
     1, "raw corpus"),

    (10.70,  0.72, GRAY_FILL,  GRAY_EDGE,  GRAY_TXT,
     "Stage 2 — Preprocessing",
     "Lowercase  ·  remove URLs, mentions, hashtags, punctuation",
     2, "preprocessing"),

    (9.10,  1.32, BLUE_FILL,  BLUE_EDGE,  BLUE_TXT,
     "Stage 3 — Tokenisation",
     None,
     3, "tokenisation"),

    (6.90,  1.90, TEAL_FILL,  TEAL_EDGE,  TEAL_TXT,
     "Stage 4 — Lexicon matching",
     None,
     4, "lexicon matching"),

    (4.60,  1.90, PURP_FILL,  PURP_EDGE,  PURP_TXT,
     "Stage 5 — Scoring and polarity classification",
     None,
     5, "scoring"),

    (2.30,  1.90, CORAL_FILL, CORAL_EDGE, CORAL_TXT,
     "Stage 6 — Comparative evaluation",
     None,
     6, "evaluation"),
]

# ── Draw main stage boxes ─────────────────────────────────────────────────────
for (yt, h, fill, edge, txt, title, sub, sn, slabel) in STAGES:
    yb = yt - h
    rbox(ax, LEFT, yb, W, h, fill, edge, radius=0.18, lw=1.0)
    y_title = (yt + yb) / 2 + (0.18 if sub else 0)
    y_sub   = y_title - 0.34
    title_sub(ax, MID, y_title, y_sub, title, sub, txt)
    stage_label(ax, (yt + yb) / 2, sn, slabel, edge)

# ── Arrows between stages ─────────────────────────────────────────────────────
gaps = [
    (12.10 - 0.72, 10.70),    # S1 -> S2
    (10.70 - 0.72,  9.10),    # S2 -> S3
    (9.10  - 1.32,  6.90),    # S3 -> S4
    (6.90  - 1.90,  4.60),    # S4 -> S5
    (4.60  - 1.90,  2.30),    # S5 -> S6
]
for (y1, y2) in gaps:
    mid_y = (y1 + y2) / 2
    arrow(ax, MID, y1 - 0.04, y2 + 0.04)

# ── Stage 3 inner boxes ───────────────────────────────────────────────────────
# Three sub-boxes inside tokenisation
s3_yb = 9.10 - 1.32
inner_h = 0.54
inner_y = s3_yb + 0.12
inner_tops = [
    (LEFT + 0.12, 2.55, "Split on whitespace", ""),
    (LEFT + 2.90, 2.55, "Stop word removal",   "350 custom terms"),
    (LEFT + 5.68, 2.55, "Negation marking",    "window = 3 tokens"),
]
for (ix, iw, ititle, isub) in inner_tops:
    rbox(ax, ix, inner_y, iw, inner_h, BLUE_FILL, BLUE_EDGE,
         radius=0.10, lw=0.6, alpha=INNER_ALPHA)
    cy = inner_y + inner_h / 2
    ax.text(ix + iw/2, cy + (0.10 if isub else 0), ititle,
            ha="center", va="center", fontsize=7.5,
            fontweight="bold", color=BLUE_TXT, fontname=FONT, zorder=4)
    if isub:
        ax.text(ix + iw/2, cy - 0.16, isub,
                ha="center", va="center", fontsize=6.8,
                color=BLUE_EDGE, fontname=FONT, zorder=4)

# ── Stage 4 inner boxes ───────────────────────────────────────────────────────
s4_yb = 6.90 - 1.90
inner_y4 = s4_yb + 0.14
inner_h4 = 1.00
LEX_BOXES = [
    (LEFT + 0.12, 2.10, "General\nInquirer", "4,206 terms"),
    (LEFT + 2.40, 2.10, "MPQA · Bing\nNRC · AFINN",  "6 dictionaries"),
    (LEFT + 4.68, 2.10, "Loughran-\nMcDonald", "3,917 terms"),
    (LEFT + 6.96, 2.28, "Mixology\nCovid + Mixology", "4,166 / 16,528"),
]
for (ix, iw, ititle, isub) in LEX_BOXES:
    rbox(ax, ix, inner_y4, iw, inner_h4, TEAL_FILL, TEAL_EDGE,
         radius=0.10, lw=0.6, alpha=INNER_ALPHA)
    cy = inner_y4 + inner_h4 / 2
    for di, line in enumerate(ititle.split("\n")):
        offset = 0.14 if len(ititle.split("\n")) > 1 else 0
        ax.text(ix + iw/2, cy + offset - di*0.28, line,
                ha="center", va="center", fontsize=7.2,
                fontweight="bold", color=TEAL_TXT, fontname=FONT, zorder=4)
    ax.text(ix + iw/2, inner_y4 + 0.18, isub,
            ha="center", va="center", fontsize=6.5,
            color=TEAL_EDGE, fontname=FONT, zorder=4)

# ── Stage 5 inner boxes ───────────────────────────────────────────────────────
s5_yb = 4.60 - 1.90
inner_y5 = s5_yb + 0.14
inner_h5 = 1.00
SCORE_BOXES = [
    (LEFT + 0.12, 2.80, "Token-level score",     "pos / neg / amb count"),
    (LEFT + 3.12, 2.80, "Corpus frequency weight","log-norm. 0.5 → 3.0"),
    (LEFT + 6.12, 2.80, "Tweet polarity",         "positive / negative / none"),
]
for (ix, iw, ititle, isub) in SCORE_BOXES:
    rbox(ax, ix, inner_y5, iw, inner_h5, PURP_FILL, PURP_EDGE,
         radius=0.10, lw=0.6, alpha=INNER_ALPHA)
    cy = inner_y5 + inner_h5 / 2
    ax.text(ix + iw/2, cy + 0.12, ititle,
            ha="center", va="center", fontsize=7.5,
            fontweight="bold", color=PURP_TXT, fontname=FONT, zorder=4)
    ax.text(ix + iw/2, cy - 0.16, isub,
            ha="center", va="center", fontsize=6.8,
            color=PURP_EDGE, fontname=FONT, zorder=4)

# ── Stage 6 inner boxes ───────────────────────────────────────────────────────
s6_yb = 2.30 - 1.90
inner_y6 = s6_yb + 0.14
inner_h6 = 1.00
EVAL_BOXES = [
    (LEFT + 0.12, 2.80, "Token coverage",           "6% → 74% range"),
    (LEFT + 3.12, 2.80, "Negative bias",             "neg / pos token ratio"),
    (LEFT + 6.12, 2.80, "Synthetic performance",     "cov. 50% + cls. 30%\n+ balance 20%"),
]
for (ix, iw, ititle, isub) in EVAL_BOXES:
    rbox(ax, ix, inner_y6, iw, inner_h6, CORAL_FILL, CORAL_EDGE,
         radius=0.10, lw=0.6, alpha=INNER_ALPHA)
    cy = inner_y6 + inner_h6 / 2
    ax.text(ix + iw/2, cy + 0.12, ititle,
            ha="center", va="center", fontsize=7.5,
            fontweight="bold", color=CORAL_TXT, fontname=FONT, zorder=4)
    for di, line in enumerate(isub.split("\n")):
        offset = 0.06 if len(isub.split("\n")) > 1 else 0
        ax.text(ix + iw/2, cy - 0.10 - di*0.22 + offset, line,
                ha="center", va="center", fontsize=6.8,
                color=CORAL_EDGE, fontname=FONT, zorder=4)

# ── Legend ────────────────────────────────────────────────────────────────────
legend_items = [
    (GRAY_FILL,  GRAY_EDGE,  "Corpus stages"),
    (BLUE_FILL,  BLUE_EDGE,  "Text processing"),
    (TEAL_FILL,  TEAL_EDGE,  "Lexicon resources"),
    (PURP_FILL,  PURP_EDGE,  "Scoring"),
    (CORAL_FILL, CORAL_EDGE, "Evaluation"),
]
lx = LEFT + 0.1
ly = -0.35
for i, (fc, ec, label) in enumerate(legend_items):
    bx = lx + i * 1.84
    bp = FancyBboxPatch((bx, ly), 0.28, 0.18,
                        boxstyle="round,pad=0,rounding_size=0.04",
                        linewidth=0.6, edgecolor=ec, facecolor=fc, zorder=3)
    ax.add_patch(bp)
    ax.text(bx + 0.36, ly + 0.09, label, va="center", fontsize=6.5,
            color=SUB_COL, fontname=FONT, zorder=3)

# ── Title and caption ─────────────────────────────────────────────────────────
ax.text(MID, 13.00,
        "Mixology sentiment analysis pipeline",
        ha="center", va="center", fontsize=11, fontweight="bold",
        color="#111111", fontname=FONT)
ax.text(MID, 12.72,
        "8 lexicons compared  ·  negation window = 3 tokens  ·  "
        "Dierickx, L. (2022)  ·  ohmybox.info",
        ha="center", va="center", fontsize=7.5,
        color=CAPTION_COL, fontname=FONT)

# ── Save ──────────────────────────────────────────────────────────────────────
plt.tight_layout(pad=0)
fig.savefig("mixology_pipeline_schema.png", dpi=300, bbox_inches="tight",
            facecolor="white")
fig.savefig("mixology_pipeline_schema.pdf", dpi=300, bbox_inches="tight",
            facecolor="white")

print("Saved: mixology_pipeline_schema.png (300 dpi)")
print("Saved: mixology_pipeline_schema.pdf (vector)")
