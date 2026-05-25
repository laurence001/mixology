# Mixology

# <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![R version](https://img.shields.io/badge/R-%3E%3D4.0.0-blue.svg)](https://cran.r-project.org/)
[![Project Status: Active](https://img.shields.io/badge/status-active-brightgreen.svg)](https://ohmybox.info)
<!-- badges: end -->

**mixology** is an R package providing domain-specific sentiment analysis
lexicons and helper functions developed as part of a research project that analyses
public opinion expressed on Twitter during the Covid-19 crisis, with a focus
on Western European accounts (December 2021).

The package is built around two core resources: a manually annotated,
Covid-specific lexicon and a larger merged lexicon combining six
general-purpose English sentiment dictionaries. Both include corpus-based
frequency weights to support more nuanced scoring.

---

## Background

Standard sentiment lexicons — Bing, NRC, MPQA, and others — were not designed
to handle the vocabulary of a health and political crisis. Terms such as
*lockdown*, *antivax*, *booster*, or *sanitary pass* are either absent from
those resources or carry a generic polarity that does not reflect how they are
actually used in pandemic-related discourse.

The Mixology Covid Lexicon was built in two stages. First, the 4,000 most
frequent tokens were extracted from a corpus of 311,882 English tweets about
vaccination (after stop word removal). Each term was then cross-referenced
against six merged general-purpose dictionaries and reviewed manually, with
bigram and trigram context consulted to resolve ambiguous cases. The full
process covered 14,446 terms.

A key design choice was to favour **ambiguity** over neutrality: terms that
can be positive or negative depending on context (e.g. *tested positive*,
*government*) are labelled `"ambiguous"` rather than forced into a binary
polarity. This results in a near-balanced distribution between positive and
negative terms — unlike general dictionaries, which typically contain a
significant surplus of negative entries.

---

## Datasets

| Dataset | Description | Terms | Positive | Negative | Ambiguous |
|---|---|---|---|---|---|
| `mixology_covid_lexicon` | Manually annotated Covid-specific lexicon | 4,166 | 1,953 (46.9%) | 1,924 (46.2%) | 289 (6.9%) |
| `mixology_lexicon` | Merged general + Covid lexicon | 16,528 | 5,716 (34.6%) | 9,655 (58.4%) | 1,157 (7.0%) |
| `stop_words_en` | Custom English stop word list | 350 | — | — | — |
| `negations_en` | English negation markers | 22 | — | — | — |

Both lexicons include two additional columns:

- **`weight`** — log-normalised corpus frequency (range 0.5–3.0). Terms unseen
  in the reference corpus receive a prior weight of 0.5; the most frequent term
  receives 3.0. Formula: `0.5 + (log(freq + 1) / log(max_freq + 1)) * 2.5`.
- **`freq_corpus`** — raw token frequency in the reference corpus (political
  measures sub-corpus, n = 4,371 tweets).

The six general-purpose dictionaries used to build the merged Mixology Lexicon
are: General Inquirer, MPQA Subjectivity Lexicon, Bing, NRC, Afinn, and
Loughran. Polarity categories were harmonised before merging (e.g. Afinn scores
binarised, Loughran financial categories mapped to positive/negative/ambiguous).

---

## Installation

```r
# Install from GitHub:
remotes::install_github("laurence001/mixology")

# Or load locally during development:
devtools::load_all("path/to/mixology")
```

**Dependencies:** dplyr, tidytext, stringr, tibble, rlang (all available on CRAN).

---

## Quick start

```r
library(mixology)

tweets <- c(
  "The lockdown is terrible and completely unjustified",
  "The vaccine rollout has been excellent, real progress",
  "I am not convinced by the booster at all"
)

# 1. Basic unweighted scoring
mixology_sentiment(tweets, lexicon = "covid", weighted = FALSE)

# 2. Weighted scoring (corpus-frequency weights)
mixology_sentiment(tweets, lexicon = "covid", weighted = TRUE)

# 3. Weighted + negation-aware ("not convinced" -> polarity reversed)
mixology_sentiment(tweets,
  lexicon         = "covid",
  weighted        = TRUE,
  handle_negation = TRUE,
  negation_window = 3
)

# 4. Compare both lexicons
r_covid <- mixology_sentiment(tweets, lexicon = "covid")
r_full  <- mixology_sentiment(tweets, lexicon = "full")
```

The output tibble contains one row per document with columns `n_tokens`,
`n_matched`, `coverage`, `score_positive`, `score_negative`,
`score_ambiguous`, `score_net`, and `polarity`.

---

## Functions

| Function | Description |
|---|---|
| `get_lexicon(lexicon)` | Returns `mixology_covid_lexicon` or `mixology_lexicon` as a tibble |
| `mixology_tokenize(text, ...)` | Tokenises a character vector; removes stop words |
| `mixology_negation(tokens_tbl, window)` | Marks tokens within a negation window |
| `mixology_sentiment(text, ...)` | Full pipeline: tokenise → match → score |

All functions accept a `custom_stopwords` or `custom_negations` argument for
corpus-specific adjustments.

---

## Applying to a data frame

```r
library(dplyr)

corpus <- read.csv("your_corpus.csv")  # requires a 'text' column

scores <- mixology_sentiment(
  corpus$text,
  lexicon         = "covid",
  weighted        = TRUE,
  handle_negation = TRUE
)

corpus <- bind_cols(corpus, scores |> select(-doc_id))
```

---

## Recomputing weights from your own corpus

The default weights were computed from the Mixology political measures
sub-corpus. To adapt them to a different corpus:

```r
library(dplyr)

# your_freq: a named integer vector  (names = tokens, values = counts)
max_f <- max(your_freq)

lex <- get_lexicon("covid") |>
  mutate(
    freq_corpus = coalesce(your_freq[word], 0L),
    weight      = 0.5 + (log(freq_corpus + 1) / log(max_f + 1)) * 2.5
  )
```

---

## Package structure

```
mixology/
├── R/
│   ├── data.R          # Roxygen documentation for all datasets
│   └── functions.R     # get_lexicon(), mixology_tokenize(),
│                       # mixology_negation(), mixology_sentiment()
├── data/               # .rda datasets (built by data-raw/prepare_data.R)
├── data-raw/
│   ├── prepare_data.R              # Run once to generate .rda from CSV
│   ├── mixology_covid_lexicon_v3.csv
│   ├── mixology_lexicon_v3.csv
│   ├── stop_words_en_v3.csv
│   └── negative_en.csv
├── inst/
│   └── getting_started.Rmd        # Introductory vignette
├── DESCRIPTION
└── NAMESPACE
```

---

## Known limitations

- Scoring is currently **unigram-based**: multi-word expressions and
  idioms are not handled.
- **Negation handling** uses a fixed sliding window and does not resolve
  scope across clause boundaries.
- Weights are derived from a **single sub-corpus** (political measures,
  Western Europe, December 2021) and may not transfer well to other domains
  or time periods.
- The lexicons cover **English only**. French resources are in development.

---

## Contributing

Corrections, additions, and feedback are welcome via
[GitHub Issues](https://github.com/yourname/mixology/issues). If you
identify terms that are miscategorised or missing, please open an issue
with the term, the suggested polarity, and a usage example from the corpus.

---

## Citation

If you use these resources in your research, please cite:

```bibtex
@misc{dierickx2026mixology,
  author = {Dierickx, Laurence},
  title  = {Mixology: Sentiment Analysis Lexicons for Covid-19 Crisis Communication},
  year   = {2026}
}
```

---

## Licence

Data: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) —
free to use and adapt with attribution.  
Code: MIT.
