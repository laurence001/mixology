# ══════════════════════════════════════════════════════════════════════════════
# Mixology — Full sentiment analysis pipeline
# Corpus: English tweets, Western Europe, Dec. 2021
# No ground truth — internal evaluation metrics only
#
# !! IMPORTANT: run this script top to bottom in a single session.
#    Parts 2–5 depend on objects created in Part 1 (all_scores_long,
#    benchmark, etc.). Running sections out of order will produce errors.
#
# Pipeline structure:
#   Part 1 — Covid vs Mixology (pair comparison)
#   Part 2 — Benchmark across all 8 lexicons
#   Part 3 — Comparative evaluation
#             3a. Token coverage
#             3b. Classification rate
#             3c. Negative bias
#             3d. Coverage-corrected inter-lexicon stability (two variants)
#             3e. Synthetic performance score
#   Part 4 — Visualisations (ggplot2)
#   Part 5 — Export (R CSVs + Python figure update helper)
#
# Python figures (300 dpi PNG + PDF vector):
#   Part 5 writes a ready-to-run update script — python_update.R — that
#   patches the data constants in both Python files automatically.
# ══════════════════════════════════════════════════════════════════════════════

library(mixology)
library(dplyr)
library(tidyr)
library(ggplot2)


# ── 0. Parameters ─────────────────────────────────────────────────────────────

CHUNK_SIZE        <- 5000   # tweets per chunk — reduce if RAM is limited
NEGATION_WINDOW   <- 3
WEIGHTED          <- TRUE   # corpus-frequency weights for covid and mixology
SEED              <- 42
MAX_CHUNK_FAILURES <- 3     # abort if more than this many chunks fail

# Load your corpus — adapt the line below:
# df <- read.csv("your_corpus.csv", stringsAsFactors = FALSE)
tweet <- sample_corpus_vaccin$text   # <-- replace with your object name
N     <- length(tweet)
cat("Corpus:", N, "tweets\n")


# ── Helper: safe chunk scorer ─────────────────────────────────────────────────
# Scores a character vector in fixed-size chunks to keep memory manageable.
#
# Robustness guarantees:
#   - Empty chunks (all-empty strings) are handled gracefully.
#   - Any chunk that throws an error is filled with all-zero rows and logged.
#   - If the number of failed chunks exceeds MAX_CHUNK_FAILURES, the function
#     stops with an informative error rather than silently returning bad data.

score_chunks <- function(tweets, lexicon_name, weighted = TRUE,
                          handle_negation = TRUE,
                          chunk_size      = CHUNK_SIZE,
                          max_failures    = MAX_CHUNK_FAILURES) {
  n        <- length(tweets)
  chunks   <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  total    <- length(chunks)
  results  <- vector("list", total)
  n_failed <- 0L

  for (i in seq_along(chunks)) {
    if (i %% 10 == 0 || i == total)
      cat(sprintf("  [%s] chunk %d / %d\n", lexicon_name, i, total))

    idx   <- chunks[[i]]
    chunk <- tweets[idx]

    res <- tryCatch(
      mixology_sentiment(
        chunk,
        lexicon         = lexicon_name,
        weighted        = weighted,
        handle_negation = handle_negation,
        negation_window = NEGATION_WINDOW
      ),
      error = function(e) {
        n_failed <<- n_failed + 1L
        message(sprintf(
          "  !! chunk %d of '%s' failed (%s). Filled with zeros [%d/%d allowed].",
          i, lexicon_name, conditionMessage(e), n_failed, max_failures
        ))
        tibble::tibble(
          doc_id          = seq_along(idx),
          n_tokens        = 0L,
          n_matched       = 0L,
          coverage        = 0,
          score_positive  = 0,
          score_negative  = 0,
          score_ambiguous = 0,
          score_net       = 0,
          polarity        = "none"
        )
      }
    )

    if (n_failed > max_failures)
      stop(sprintf(
        "Too many chunk failures for lexicon '%s' (%d > %d allowed). ",
        lexicon_name, n_failed, max_failures,
        "Check your corpus for encoding issues or empty rows."
      ))

    # Remap 1-based chunk indices back to original corpus indices.
    # res$doc_id is always 1..length(chunk), so idx[res$doc_id] is safe
    # even when some tweets in the chunk are empty.
    res$doc_id <- idx[res$doc_id]
    results[[i]] <- res
  }

  if (n_failed > 0)
    message(sprintf(
      "  [%s] completed with %d failed chunk(s) — those rows show polarity = 'none'.",
      lexicon_name, n_failed
    ))

  dplyr::bind_rows(results)
}


# ══════════════════════════════════════════════════════════════════════════════
# PART 1 — Covid vs Mixology
# The two Mixology-specific lexicons are compared directly using corpus-
# frequency weighting and negation handling, to assess how their different
# vocabulary sizes and near-balanced polarity distributions affect results.
# ══════════════════════════════════════════════════════════════════════════════

cat("\n── Part 1: Covid vs Mixology ──\n")

scores_covid <- score_chunks(tweet, "covid",    weighted = WEIGHTED)
scores_mixo  <- score_chunks(tweet, "mixology", weighted = WEIGHTED)

# Join for direct tweet-level comparison
comparison_12 <- scores_covid |>
  select(doc_id, n_tokens,
         cov_covid = coverage,
         net_covid = score_net,
         pol_covid = polarity) |>
  left_join(
    scores_mixo |>
      select(doc_id,
             cov_mixo = coverage,
             net_mixo = score_net,
             pol_mixo = polarity),
    by = "doc_id"
  )

# ── 1a. Descriptive summary ───────────────────────────────────────────────────

# Guard against a lexicon matching zero positive tokens across the corpus.
# This can happen on very small or domain-unusual corpora.
.safe_bias <- function(neg, pos) {
  if (pos == 0) {
    warning("Zero positive token matches — neg_bias is undefined; returning NA.")
    return(NA_real_)
  }
  neg / pos
}

summary_12 <- tibble(
  lexicon       = c("Mixology Covid", "Mixology"),
  n_tweets      = N,
  mean_coverage = c(mean(comparison_12$cov_covid),
                    mean(comparison_12$cov_mixo)),
  pct_positive  = c(mean(comparison_12$pol_covid == "positive") * 100,
                    mean(comparison_12$pol_mixo  == "positive") * 100),
  pct_negative  = c(mean(comparison_12$pol_covid == "negative") * 100,
                    mean(comparison_12$pol_mixo  == "negative") * 100),
  pct_ambiguous = c(mean(comparison_12$pol_covid == "ambiguous") * 100,
                    mean(comparison_12$pol_mixo  == "ambiguous") * 100),
  pct_none      = c(mean(comparison_12$pol_covid == "none") * 100,
                    mean(comparison_12$pol_mixo  == "none") * 100),
  mean_net      = c(mean(comparison_12$net_covid),
                    mean(comparison_12$net_mixo)),
  neg_bias      = c(
    .safe_bias(sum(scores_covid$score_negative),
               sum(scores_covid$score_positive)),
    .safe_bias(sum(scores_mixo$score_negative),
               sum(scores_mixo$score_positive))
  )
) |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

cat("\nCovid vs Mixology summary:\n")
print(summary_12)

# ── 1b. Inter-lexicon agreement ───────────────────────────────────────────────
# Proportion of matched tweets where both lexicons assign the same polarity.
# Tweets classified as "none" by either lexicon are excluded.

accord <- comparison_12 |>
  filter(pol_covid != "none", pol_mixo != "none") |>
  mutate(agree = pol_covid == pol_mixo)

cat(sprintf(
  "\nCovid / Mixology agreement: %.1f%% (%d / %d classified tweets)\n",
  mean(accord$agree) * 100, sum(accord$agree), nrow(accord)
))

cat("\nConfusion matrix (rows = Covid, columns = Mixology):\n")
print(table(Covid = accord$pol_covid, Mixology = accord$pol_mixo))

# Divergent tweets — useful for manual inspection and error analysis
divergent <- comparison_12 |>
  filter(pol_covid != "none", pol_mixo != "none",
         pol_covid != pol_mixo) |>
  mutate(text = tweet[doc_id])

cat(sprintf("\n%d divergent tweets (%.1f%%)\n",
    nrow(divergent), 100 * nrow(divergent) / nrow(accord)))


# ══════════════════════════════════════════════════════════════════════════════
# PART 2 — Benchmark: all 8 lexicons
# The 6 general-purpose lexicons are scored without weighting for a fair
# cross-lexicon comparison. The Mixology lexicons from Part 1 are reused.
#
# Produces: all_scores_long — used by Parts 3, 4, and 5.
# ══════════════════════════════════════════════════════════════════════════════

cat("\n── Part 2: Benchmark — 8 lexicons ──\n")

all_lexicons   <- names(mixology_lexicon_names())
other_lexicons <- setdiff(all_lexicons, c("covid", "mixology"))

scores_others <- lapply(other_lexicons, function(lex) {
  cat(sprintf("  Scoring: %s\n", lex))
  res <- score_chunks(tweet, lex, weighted = FALSE, handle_negation = TRUE)
  res$lexicon <- lex
  res
})
names(scores_others) <- other_lexicons

# Long-format table: one row per tweet × lexicon
# This object is used by Parts 3d, 4d, and 5.
all_scores_long <- bind_rows(
  scores_covid |> mutate(lexicon = "covid"),
  scores_mixo  |> mutate(lexicon = "mixology"),
  bind_rows(scores_others)
) |>
  mutate(lexicon_label = mixology_lexicon_names()[lexicon])

# ── 2a. Aggregate benchmark table ─────────────────────────────────────────────

benchmark <- all_scores_long |>
  group_by(lexicon, lexicon_label) |>
  summarise(
    n_tweets      = n(),
    n_matched     = sum(n_matched > 0),
    mean_coverage = mean(coverage),
    pct_none      = mean(polarity == "none") * 100,
    pct_positive  = mean(polarity == "positive" & n_matched > 0) /
                    mean(n_matched > 0) * 100,
    pct_negative  = mean(polarity == "negative" & n_matched > 0) /
                    mean(n_matched > 0) * 100,
    pct_ambiguous = mean(polarity == "ambiguous" & n_matched > 0) /
                    mean(n_matched > 0) * 100,
    mean_net      = mean(score_net),
    neg_bias      = {
      pos <- sum(score_positive)
      neg <- sum(score_negative)
      if (pos == 0) NA_real_ else round(neg / pos, 3)
    },
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  arrange(desc(mean_coverage))

cat("\nBenchmark table:\n")
print(benchmark, n = 8)


# ══════════════════════════════════════════════════════════════════════════════
# PART 3 — Comparative evaluation (no ground truth)
# ══════════════════════════════════════════════════════════════════════════════

cat("\n── Part 3: Comparative evaluation ──\n")

# ── 3a. Token coverage ────────────────────────────────────────────────────────
# A higher coverage means more corpus tokens are recognised — critical for
# domain-specific corpora where general lexicons miss many relevant terms.

cat("\nToken coverage (% corpus tokens matched):\n")
coverage_tbl <- lexicon_coverage(tweet)
print(coverage_tbl |> arrange(desc(coverage)))

# ── 3b. Classification rate ───────────────────────────────────────────────────
# Tweets classified as "none" contribute nothing to the analysis.
# A lexicon that leaves 50%+ of tweets unclassified is of limited use.

classif_rate <- benchmark |>
  select(lexicon_label, mean_coverage, pct_none) |>
  mutate(pct_classified = round(100 - pct_none, 1)) |>
  arrange(desc(pct_classified))

cat("\nClassification rate:\n")
print(classif_rate)

# ── 3c. Negative bias ─────────────────────────────────────────────────────────
# General lexicons contain more negative than positive terms by design.
# A bias > 1.6 means the lexicon will systematically over-classify tweets as
# negative regardless of their actual content. NA = zero positive matches.

cat("\nNegative bias (neg_tokens / pos_tokens):\n")
benchmark |>
  select(lexicon_label, neg_bias) |>
  mutate(rating = case_when(
    is.na(neg_bias)  ~ "undefined (no positive matches)",
    neg_bias < 1.2   ~ "balanced",
    neg_bias < 1.6   ~ "slight bias",
    neg_bias < 2.0   ~ "biased",
    TRUE             ~ "strongly biased"
  )) |>
  arrange(neg_bias) |>
  print()

# ── 3d. Coverage-corrected inter-lexicon stability ────────────────────────────
#
# Why coverage correction matters
# ────────────────────────────────
# Raw pairwise agreement (% tweets where two lexicons agree on polarity) is
# inflated for large lexicons: a lexicon that classifies nearly all tweets
# will always share a large intersection with any other lexicon, making it
# appear artificially stable. This reflects verbosity, not genuine coherence.
#
# Correction principle
# ─────────────────────
# The raw agreement is weighted by the relative overlap between the two
# lexicons' covered tweet sets. If A covers 65% of tweets and B covers
# only 13%, their intersection is small; the correction penalises the pair
# proportionally, shifting focus from "who classifies the most" to "who
# classifies consistently at equivalent information".
#
# Two variants:
#
#   Simple (recommended for main results):
#     S_AB = agreement × (C_intersection / max(C_A, C_B))
#     Anchors to the larger lexicon; straightforward to interpret.
#
#   Strict (recommended for supplementary / appendix):
#     S*_AB = agreement × (C_intersection / sqrt(C_A × C_B))
#     Geometric mean denominator; penalises asymmetric pairs more heavily
#     and rewards pairs that are both large and coherent.
#
# Interpretation shift:
#   Before → "which lexicon is the most covering and coherent?"
#   After  → "which lexicon is the most coherent per unit of shared
#              information?"

set.seed(SEED)
sample_idx <- sample(seq_len(N), min(10000, N))

# Pre-filter all_scores_long to the sample once — avoids 56 full-table scans
# in the pairwise loop (2.4M rows × 28 pairs without this = very slow).
all_scores_sample <- all_scores_long |>
  filter(doc_id %in% sample_idx)

# Per-lexicon tweet coverage on the sample — used as C_A, C_B in the formulas
coverage_indiv <- all_scores_sample |>
  group_by(lexicon) |>
  summarise(cov = mean(n_matched > 0), .groups = "drop") |>
  tibble::deframe()

cat("\nIndividual tweet coverage on sample (10k):\n")
print(sort(coverage_indiv, decreasing = TRUE))

# Pre-split sample by lexicon for fast pair lookups
sample_by_lex <- split(
  all_scores_sample |> select(lexicon, doc_id, polarity),
  all_scores_sample$lexicon
)

# Internal helper: retrieve matched-only rows for one lexicon from the
# pre-split sample. Called by both stability functions to avoid duplication.
.lex_matched <- function(lex_name, sample_by_lex, col_name) {
  sample_by_lex[[lex_name]] |>
    filter(polarity != "none") |>
    select(doc_id, !!col_name := polarity)
}

# Internal helper: shared setup for both stability variants.
# Returns list(joined, agreement, c_intersection, c_a, c_b) or NA if empty.
.stability_base <- function(lex_a, lex_b, idx, sample_by_lex, coverage_indiv) {
  a      <- .lex_matched(lex_a, sample_by_lex, "pol_a")
  b      <- .lex_matched(lex_b, sample_by_lex, "pol_b")
  joined <- inner_join(a, b, by = "doc_id")
  if (nrow(joined) == 0) return(NULL)
  list(
    agreement      = mean(joined$pol_a == joined$pol_b),
    c_intersection = nrow(joined) / length(idx),
    c_a            = coverage_indiv[[lex_a]],
    c_b            = coverage_indiv[[lex_b]]
  )
}

# Simple coverage-corrected stability
#   S_AB = agreement x (C_intersection / max(C_A, C_B))
pairwise_stability_simple <- function(lex_a, lex_b, idx,
                                      sample_by_lex, coverage_indiv) {
  b <- .stability_base(lex_a, lex_b, idx, sample_by_lex, coverage_indiv)
  if (is.null(b)) return(NA_real_)
  b$agreement * (b$c_intersection / max(b$c_a, b$c_b))
}

# Strict coverage-corrected stability (geometric mean denominator)
#   S*_AB = agreement x (C_intersection / sqrt(C_A x C_B))
#   Penalises asymmetric pairs more heavily than the simple variant.
pairwise_stability_strict <- function(lex_a, lex_b, idx,
                                      sample_by_lex, coverage_indiv) {
  b <- .stability_base(lex_a, lex_b, idx, sample_by_lex, coverage_indiv)
  if (is.null(b)) return(NA_real_)
  b$agreement * (b$c_intersection / sqrt(b$c_a * b$c_b))
}

pairs   <- combn(all_lexicons, 2, simplify = FALSE)
n_pairs <- length(pairs)
pair_results <- vector("list", n_pairs)

cat(sprintf("\nComputing stability for %d pairs (sample n = %d)...\n",
            n_pairs, length(sample_idx)))

for (k in seq_along(pairs)) {
  p <- pairs[[k]]
  if (k %% 5 == 0 || k == n_pairs)
    cat(sprintf("  pair %d / %d: %s vs %s\n",
                k, n_pairs,
                mixology_lexicon_names()[p[1]],
                mixology_lexicon_names()[p[2]]))

  pair_results[[k]] <- tibble(
    lex_a = mixology_lexicon_names()[p[1]],
    lex_b = mixology_lexicon_names()[p[2]],
    stability_simple = round(
      pairwise_stability_simple(p[1], p[2], sample_idx,
                                sample_by_lex, coverage_indiv) * 100, 2),
    stability_strict = round(
      pairwise_stability_strict(p[1], p[2], sample_idx,
                                sample_by_lex, coverage_indiv) * 100, 2)
  )
}

agree_tbl_cov <- bind_rows(pair_results) |>
  arrange(desc(stability_simple))

cat("\nCoverage-corrected inter-lexicon stability (sample 10k):\n")
cat("  Simple  = agreement × (C_intersection / max(C_A, C_B))\n")
cat("  Strict  = agreement × (C_intersection / sqrt(C_A × C_B))\n\n")
print(agree_tbl_cov, n = 28)

# ── 3e. Synthetic performance score ───────────────────────────────────────────
# Combines three internal metrics into a single score:
#   Coverage (50%)            — how much of the corpus is reached
#   Classification rate (30%) — how many tweets receive a polarity label
#   Balance (20%)             — proximity to an unbiased neg/pos ratio
#
# NA neg_bias (zero positive matches) receives a balance score of 0
# rather than crashing or producing a misleading value.

perf_score <- benchmark |>
  select(lexicon, lexicon_label, mean_coverage, pct_none, neg_bias) |>
  mutate(
    score_coverage = mean_coverage * 100,
    score_classif  = 100 - pct_none,
    score_balance  = if_else(
      is.na(neg_bias), 0,
      100 / (1 + abs(neg_bias - 1))
    ),
    score_global = round(
      0.5 * score_coverage +
      0.3 * score_classif  +
      0.2 * score_balance, 1
    )
  ) |>
  select(lexicon_label, score_coverage, score_classif,
         score_balance, score_global) |>
  mutate(across(where(is.numeric), \(x) round(x, 1))) |>
  arrange(desc(score_global))

cat("\nSynthetic performance score:\n")
cat("  Weights: coverage 50% | classification rate 30% | balance 20%\n")
if (any(is.na(benchmark$neg_bias)))
  cat("  Note: balance score = 0 for any lexicon with NA neg_bias",
      "(zero positive token matches).\n")
cat("\n")
print(perf_score)


# ══════════════════════════════════════════════════════════════════════════════
# PART 4 — Visualisations
# ══════════════════════════════════════════════════════════════════════════════

cat("\n── Part 4: Visualisations ──\n")

# ── 4a. Token coverage ────────────────────────────────────────────────────────

p1 <- benchmark |>
  mutate(
    lexicon_label = reorder(lexicon_label, mean_coverage),
    is_mixo       = lexicon %in% c("covid", "mixology")
  ) |>
  ggplot(aes(x = lexicon_label, y = mean_coverage * 100, fill = is_mixo)) +
  geom_col() +
  scale_fill_manual(
    values = c("FALSE" = "steelblue", "TRUE" = "#1D9E75"),
    labels = c("General-purpose lexicons", "Mixology lexicons"),
    name   = NULL
  ) +
  coord_flip() +
  labs(
    title    = "Token coverage by lexicon",
    subtitle = paste0("n = ", format(N, big.mark = ","), " tweets"),
    x = NULL, y = "Coverage (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p1)

# ── 4b. Polarity distribution ─────────────────────────────────────────────────

p2 <- benchmark |>
  select(lexicon_label, pct_positive, pct_negative, pct_ambiguous) |>
  pivot_longer(starts_with("pct_"),
               names_to = "polarity", values_to = "pct") |>
  mutate(
    polarity = recode(polarity,
                      pct_positive  = "Positive",
                      pct_negative  = "Negative",
                      pct_ambiguous = "Ambiguous"),
    lexicon_label = reorder(lexicon_label,
                            ifelse(polarity == "Negative", pct, 0))
  ) |>
  ggplot(aes(x = lexicon_label, y = pct, fill = polarity)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(
    "Positive"  = "#1D9E75",
    "Negative"  = "#D85A30",
    "Ambiguous" = "#888780"
  )) +
  coord_flip() +
  labs(
    title    = "Polarity distribution by lexicon",
    subtitle = "% of classified tweets (matched docs only)",
    x = NULL, y = "%", fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p2)

# ── 4c. Synthetic performance score ───────────────────────────────────────────

p3 <- perf_score |>
  mutate(lexicon_label = reorder(lexicon_label, score_global)) |>
  pivot_longer(c(score_coverage, score_classif, score_balance),
               names_to = "component", values_to = "value") |>
  mutate(component = recode(component,
    score_coverage = "Coverage (×0.5)",
    score_classif  = "Classification rate (×0.3)",
    score_balance  = "Balance (×0.2)"
  )) |>
  ggplot(aes(x = lexicon_label, y = value, fill = component)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c(
    "Coverage (×0.5)"            = "#378ADD",
    "Classification rate (×0.3)" = "#1D9E75",
    "Balance (×0.2)"             = "#BA7517"
  )) +
  coord_flip() +
  labs(
    title    = "Synthetic performance score by lexicon",
    subtitle = "Coverage 50% + classification 30% + balance 20%",
    x = NULL, y = "Score", fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p3)

# ── 4d. Coverage-corrected stability heatmap ──────────────────────────────────
# Uses the simple corrected stability score. Unlike a raw agreement heatmap,
# this does not reward large lexicons for classifying more tweets — a high
# value here means genuine coherence at equivalent information.

agree_matrix_cov <- agree_tbl_cov |>
  select(lex_a, lex_b, agreement = stability_simple) |>
  bind_rows(
    agree_tbl_cov |>
      select(lex_a = lex_b, lex_b = lex_a, agreement = stability_simple)
  ) |>
  bind_rows(tibble(
    lex_a     = unique(agree_tbl_cov$lex_a),
    lex_b     = unique(agree_tbl_cov$lex_a),
    agreement = 100
  ))

p4 <- agree_matrix_cov |>
  ggplot(aes(x = lex_a, y = lex_b, fill = agreement)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = round(agreement, 0)), size = 2.8, colour = "white") +
  scale_fill_gradient(low = "#D85A30", high = "#1D9E75",
                      name = "Corrected stability") +
  labs(
    title    = "Coverage-corrected inter-lexicon stability",
    subtitle = paste0(
      "S = agreement × (C_intersection / max(C_A, C_B)) — sample 10k tweets\n",
      "Penalises large lexicons that inflate raw agreement through coverage alone"
    ),
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

print(p4)


# ══════════════════════════════════════════════════════════════════════════════
# PART 5 — Export
# ══════════════════════════════════════════════════════════════════════════════

write.csv(benchmark,      "benchmark_all_lexicons.csv",     row.names = FALSE)
write.csv(perf_score,     "performance_scores.csv",         row.names = FALSE)
write.csv(agree_tbl_cov,  "stability_corrected.csv",        row.names = FALSE)
write.csv(comparison_12,  "covid_vs_mixology_doclevel.csv", row.names = FALSE)
write.csv(
  divergent |> select(doc_id, text, pol_covid, pol_mixo, net_covid, net_mixo),
  "divergent_tweets.csv", row.names = FALSE
)

# Full doc-level scores (large file on 300k tweets — uncomment if needed):
# write.csv(all_scores_long, "all_scores_doclevel.csv", row.names = FALSE)

# ── Automatic Python figure updater ───────────────────────────────────────────
# Writes a small R script that patches the data constants in both Python files
# with your corpus results. Run it with: source("update_python_figures.R")
# then: python mixology_benchmark_viz.py / python mixology_pipeline_schema.py

lex_order <- c("General Inquirer", "MPQA Subjectivity", "Bing Liu",
               "NRC", "AFINN", "Loughran-McDonald",
               "Mixology Covid", "Mixology")

bm <- benchmark |>
  mutate(lexicon_label = factor(lexicon_label, levels = lex_order)) |>
  arrange(lexicon_label)

ps <- perf_score |>
  mutate(lexicon_label = factor(lexicon_label, levels = lex_order)) |>
  arrange(lexicon_label)

ct <- coverage_tbl |>
  mutate(lexicon_label = factor(lexicon_label, levels = lex_order)) |>
  arrange(lexicon_label)

.fmt <- function(x) paste0("[", paste(round(x, 1), collapse = ", "), "]")

update_script <- sprintf(
'# update_python_figures.R — auto-generated by pipeline_300k.R
# Patches data constants in mixology_benchmark_viz.py with your corpus results.
# Run: source("update_python_figures.R")

.EXPECTED_CONSTANTS <- c(
  "TOK_COV = ", "PCT_POS = ", "PCT_NEG = ", "PCT_AMB = ",
  "NEG_BIAS = ", "SCORE_COV = ", "SCORE_CLASSIF = ",
  "SCORE_BALANCE = ", "SCORE_GLOBAL = "
)

.patch <- function(file, pattern, replacement) {
  if (!file.exists(file))
    stop("File not found: ", file,
         "
Copy it to your working directory first:
",
         "  file.copy(system.file(\"mixology_benchmark_viz.py\",",
         " package = \"mixology\"), \".\")"
    )
  lines <- readLines(file)
  idx   <- grep(pattern, lines, fixed = TRUE)
  if (length(idx) == 0)
    stop("Pattern not found in ", file, ": "", pattern, ""
",
         "The file may have been manually edited. Check the constant name.")
  if (length(idx) > 1)
    stop("Pattern matched ", length(idx), " lines in ", file,
         ": "", pattern, ""
Expected exactly one match.")
  lines[idx] <- replacement
  writeLines(lines, file)
  message("  Updated: ", pattern)
}

# Resolve Python file path — prefer installed package, fall back to local copy.
# If neither exists, stop with a clear instruction.
viz <- system.file("mixology_benchmark_viz.py", package = "mixology")
if (!nzchar(viz)) {
  viz <- "mixology_benchmark_viz.py"
  if (!file.exists(viz))
    stop(
      "mixology_benchmark_viz.py not found.
",
      "Copy it to your working directory with:
",
      "  file.copy(system.file("mixology_benchmark_viz.py",",
      " package = "mixology"), ".")"
    )
}

# Verify all expected constants exist before patching anything.
# This catches manual edits to the Python file before any writes occur.
lines_check <- readLines(viz)
missing <- Filter(
  function(p) length(grep(p, lines_check, fixed = TRUE)) == 0,
  .EXPECTED_CONSTANTS
)
if (length(missing) > 0)
  stop(
    "The following constants were not found in ", viz, ":
",
    paste0("  ", missing, collapse = "
"), "
",
    "The file may have been manually edited. Restore it from the package
",
    "with: file.copy(system.file("mixology_benchmark_viz.py",",
    " package = "mixology"), ".", overwrite = TRUE)"
  )

.patch(viz, "TOK_COV = ",      paste0("TOK_COV = ",      .fmt(ct$coverage * 100)))
.patch(viz, "PCT_POS = ",      paste0("PCT_POS = ",      .fmt(bm$pct_positive)))
.patch(viz, "PCT_NEG = ",      paste0("PCT_NEG = ",      .fmt(bm$pct_negative)))
.patch(viz, "PCT_AMB = ",      paste0("PCT_AMB = ",      .fmt(bm$pct_ambiguous)))
.patch(viz, "NEG_BIAS = ",     paste0("NEG_BIAS = ",     .fmt(bm$neg_bias)))
.patch(viz, "SCORE_COV = ",    paste0("SCORE_COV = ",    .fmt(ps$score_coverage)))
.patch(viz, "SCORE_CLASSIF = ",paste0("SCORE_CLASSIF = ",.fmt(ps$score_classif)))
.patch(viz, "SCORE_BALANCE = ",paste0("SCORE_BALANCE = ",.fmt(ps$score_balance)))
.patch(viz, "SCORE_GLOBAL = ", paste0("SCORE_GLOBAL = ", .fmt(ps$score_global)))

message("
All ", length(.EXPECTED_CONSTANTS), " constants updated in ", viz)
message("Now run: python mixology_benchmark_viz.py")
',
  .fmt = .fmt, bm = bm, ps = ps, ct = ct
)

writeLines(update_script, "update_python_figures.R")
cat("\nPython updater written: update_python_figures.R\n")
cat("  Run: source('update_python_figures.R')\n")
cat("  Then: python mixology_benchmark_viz.py\n")

cat("\nDone. Files saved to:", getwd(), "\n")
