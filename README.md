# mixology <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![R version](https://img.shields.io/badge/R-%3E%3D4.0.0-blue.svg)](https://cran.r-project.org/)
[![Project Status: Active](https://img.shields.io/badge/status-active-brightgreen.svg)](https://ohmybox.info)
<!-- badges: end -->

**mixology** is an R package providing eight sentiment lexicons and helper
functions for comparative opinion mining. It is based on the analysis of public
opinion expressed on Twitter during the Covid-19 crisis, focusing on
Western European accounts (December 2021).

---

## Lexicons

All lexicons share a common three-category polarity scheme —
**positive**, **negative**, **ambiguous** — enabling straightforward
cross-lexicon comparison. The six general-purpose dictionaries have been
harmonised from their original formats.

| Short name | Full name | Terms | Positive | Negative | Ambiguous |
|---|---|---|---|---|---|
| `inquirer` | General Inquirer | 4,206 | 1,915 (45.5%) | 2,291 (54.5%) | — |
| `subjectivity` | MPQA Subjectivity | 6,884 | 2,298 (33.4%) | 4,147 (60.2%) | 439 (6.4%) |
| `bing` | Bing Liu | 6,783 | 2,005 (29.6%) | 4,778 (70.4%) | — |
| `nrc` | NRC Emotion Lexicon | 6,456 | 2,772 (43.0%) | 3,601 (55.8%) | 83 (1.3%) |
| `afinn` | AFINN | 2,477 | 878 (35.4%) | 1,598 (64.5%) | 1 (0.0%) |
| `loughran` | Loughran-McDonald | 3,917 | 354 (9.0%) | 3,250 (83.0%) | 313 (8.0%) |
| `covid` | **Mixology Covid Lexicon** | **4,166** | **1,953 (46.9%)** | **1,924 (46.2%)** | **289 (6.9%)** |
| `mixology` | **Mixology Lexicon** | **16,528** | **5,716 (34.6%)** | **9,655 (58.4%)** | **1,157 (7.0%)** |

The **Mixology Covid Lexicon** was built by manually reviewing 4,500 frequent
tokens from a corpus of 596,619 English tweets (Western Europe,
December 2021), cross-referenced against the six general-purpose dictionaries
using bigram and trigram context. The **Mixology Lexicon** merges all seven
resources after manual conflict resolution.

Unlike general-purpose dictionaries, which carry a systematic surplus of
negative terms, the Covid lexicon has a near-balanced distribution, reducing
the negative bias typically observed when applying off-the-shelf resources to
domain-specific corpora.

---

## Installation

```r
remotes::install_github("laurence001/mixology")
```

**Dependencies:** dplyr, tidytext, stringr, tibble, rlang (all on CRAN).

---

## Quick start

```r
library(mixology)
library(dplyr)

tweets <- c(
  "The lockdown is terrible and completely unjustified",
  "The vaccine rollout has been excellent, great progress",
  "I am not sure about the booster at all"
)

# 1. Compare all eight lexicons at once
compare_lexicons(tweets)

# 2. Score with one lexicon
mixology_sentiment(tweets, lexicon = "covid")

# 3. Weighted + negation-aware
mixology_sentiment(tweets,
  lexicon         = "covid",
  weighted        = TRUE,
  handle_negation = TRUE
)

# 4. Coverage diagnostic before full analysis
lexicon_coverage(tweets)

# 5. Find terms with conflicting polarities across lexicons
lexicon_conflicts(c("covid", "bing", "nrc"))
```

---

## Functions

| Function | Description |
|---|---|
| `get_lexicon(name)` | Return any of the eight lexicons as a tibble |
| `mixology_lexicon_names()` | List available lexicon names and labels |
| `mixology_tokenize(text, ...)` | Tokenise; remove stop words |
| `mixology_negation(tokens, window)` | Mark tokens within a negation window |
| `mixology_sentiment(text, ...)` | Full pipeline: tokenise → match → score |
| `compare_lexicons(text, ...)` | Run all (or selected) lexicons; return summary or long-format results |
| `lexicon_coverage(text, ...)` | Quick coverage diagnostic per lexicon |
| `lexicon_conflicts(lexicons, ...)` | Terms with conflicting polarities across lexicons |

### Output columns from `mixology_sentiment()`

| Column | Description |
|---|---|
| `doc_id` | Document index |
| `n_tokens` | Tokens after stop word removal |
| `n_matched` | Tokens matched in the lexicon |
| `coverage` | `n_matched / n_tokens` |
| `score_positive` | Sum of (weighted) positive matches |
| `score_negative` | Sum of (weighted) negative matches |
| `score_ambiguous` | Sum of (weighted) ambiguous matches |
| `score_net` | `score_positive − score_negative` |
| `polarity` | Dominant polarity: `positive`, `negative`, `ambiguous`, or `none` |

---

## Package structure

```
mixology/
├── DESCRIPTION
├── NAMESPACE
├── LICENSE
├── README.md
├── R/
│   ├── data.R              # Roxygen documentation for all 10 datasets
│   └── functions.R         # All exported functions
├── data-raw/
│   ├── prepare_data.R      # Run once to generate .rds files
│   ├── inquirer.csv
│   ├── subjectivity.csv
│   ├── bing.csv
│   ├── nrc.csv
│   ├── afinn.csv
│   ├── loughran.csv
│   ├── mixology_covid_lexicon_v3.csv
│   ├── mixology_lexicon_v3.csv
│   ├── stop_words_en_v3.csv
│   └── negative_en.csv
└── inst/
    ├── data/               # .rds datasets (10 files, loaded at runtime)
    ├── getting_started.Rmd # Introductory vignette
    ├── pipeline.R          # Basic usage examples
    └── pipeline_300k.R     # Full pipeline for large corpora
```

---

## R pipelines

Two ready-to-use R scripts are bundled with the package and can be opened
directly from RStudio:

```r
# Basic usage and function examples
file.edit(system.file("pipeline.R", package = "mixology"))

# Full pipeline for large corpora (300k+ tweets), with:
#   - chunk-based processing
#   - Covid vs Mixology comparison
#   - benchmark across all 8 lexicons
#   - coverage-corrected inter-lexicon stability (simple and strict variants)
#   - synthetic performance score
file.edit(system.file("pipeline_300k.R", package = "mixology"))
```

---

## Design notes

**Ambiguity over neutrality.** Terms that can be positive or negative
depending on context (e.g. *tested positive*, *government*, *restrictions*)
are labelled `ambiguous` rather than excluded or forced into a binary.
This is particularly relevant for Covid-related discourse, where standard
polarities are frequently reversed.

**Harmonisation of general lexicons.** Original categories were mapped as
follows before merging:

- *AFINN*: scores binarised (positive / negative); 0 → ambiguous
- *Loughran*: constraining + litigious → negative; uncertainty + superfluous → ambiguous
- *NRC*: anger + fear + sadness + disgust → negative; anticipation + trust + joy → positive; surprise → ambiguous
- *MPQA*: neutral + both → ambiguous

**Corpus weights.** The `weight` column (Mixology lexicons only) encodes
log-normalised token frequency from the corpus of 596,619 English tweets
(Western Europe, December 2021):

```
weight = 0.5 + (log(freq + 1) / log(max_freq + 1)) × 2.5
```

Range: 0.5 (unseen) to 3.0 (most frequent term, *vaccine*). Weights can be
recomputed from any corpus — see the vignette.

---

## Known limitations

- Scoring is **unigram-based**; multi-word expressions are not handled.
- Negation uses a **fixed sliding window** and does not resolve scope across
  clause boundaries.
- Weights are derived from a **single sub-corpus** and may not transfer to
  other domains or time periods.
- Lexicons cover **English only**. French resources are in development.

---

## Contributing

Corrections, additions, and annotation feedback are welcome via
[GitHub Issues](https://github.com/laurence001/mixology/issues). Please
include the term, the suggested polarity, and a usage example from the corpus.

---

## Citation

If you use this package in your research, please cite the companion paper:

```bibtex
@article{dierickx2026mixology,
  author  = {Dierickx, Laurence},
  title   = {Wrong dictionary, wrong answer? A domain-adapted lexicon framework
             for crisis sentiment analysis},
  journal = {Computational Communication Research},
  year    = {2026},
  url     = {https://journal.computationalcommunication.org}
}
```

To cite the package itself:

```bibtex
@misc{dierickx2026mixologypkg,
  author  = {Dierickx, Laurence},
  title   = {mixology: Sentiment Analysis Lexicons and Tools for Crisis
             Communication Research},
  year    = {2026},
  version = {0.3.0},
  url     = {https://github.com/laurence001/mixology}
}
```

---

## Licence

Data: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) —
free to use and adapt with attribution.  
Code: MIT.
