# ══════════════════════════════════════════════════════════════════════════════
# Mixology — Unified sentiment analysis pipeline
# Works on corpora of any size via chunk-based processing.
#
# !! Run this script top to bottom in a single session.
#    Parts 2–5 depend on objects created in Part 1.
#
# Structure:
#   Part 0  — Parameters and corpus
#   Part 1  — Tokenisation and preprocessing
#   Part 2  — Token coverage diagnostic
#   Part 3  — Sentiment scoring
#               3a. All ten lexicons (benchmark)
#               3b. Original vs fine-tuned (Mixology lexicons)
#               3c. Custom lexicon (optional)
#   Part 4  — Comparative evaluation
#               4a. Classification rate
#               4b. Negative bias
#               4c. Coverage-corrected inter-lexicon stability
#               4d. Synthetic performance score
#   Part 5  — Visualisations
#   Part 6  — Export
# ══════════════════════════════════════════════════════════════════════════════

library(mixology)
library(dplyr)
library(tidyr)
library(ggplot2)


# ══════════════════════════════════════════════════════════════════════════════
# PART 0 — Parameters and corpus
# ══════════════════════════════════════════════════════════════════════════════

CHUNK_SIZE         <- 5000   # tweets per chunk — reduce if RAM is limited
NEGATION_WINDOW    <- 3L
WEIGHTED           <- TRUE   # corpus-frequency weights for Mixology lexicons
SEED               <- 42L
MAX_CHUNK_FAILURES <- 3L     # abort after this many failed chunks

# Load your corpus — adapt as needed:
# df <- read.csv("your_corpus.csv", stringsAsFactors = FALSE)
df    <- sample_corpus_politics_en   # replace with your data frame
tweet <- df$text
N     <- length(tweet)
cat("Corpus:", format(N, big.mark = ","), "tweets\n")

# Available lexicons (ten built-in resources):
mixology_lexicon_names()


# ── Helper: safe chunk scorer ─────────────────────────────────────────────────
# Scores a character vector in fixed-size chunks to keep memory manageable.
# Accepts either a lexicon key string ("covid", "bing", etc.) or a data frame
# prepared with use_custom_lexicon().

score_chunks <- function(tweets,
                          lexicon_arg,
                          weighted        = TRUE,
                          handle_negation = TRUE,
                          chunk_size      = CHUNK_SIZE,
                          max_failures    = MAX_CHUNK_FAILURES) {

  lex_label <- if (is.character(lexicon_arg)) lexicon_arg else "custom"
  n         <- length(tweets)
  chunks    <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  total     <- length(chunks)
  results   <- vector("list", total)
  n_failed  <- 0L

  for (i in seq_along(chunks)) {
    if (i %% 10 == 0 || i == total)
      cat(sprintf("  [%s] chunk %d / %d\n", lex_label, i, total))

    idx   <- chunks[[i]]
    chunk <- tweets[idx]

    res <- tryCatch(
      mixology_sentiment(
        chunk,
        lexicon         = lexicon_arg,
        weighted        = weighted,
        handle_negation = handle_negation,
        negation_window = NEGATION_WINDOW
      ),
      error = function(e) {
        n_failed <<- n_failed + 1L
        message(sprintf(
          "  !! chunk %d of '%s' failed (%s). Filled with zeros [%d/%d allowed].",
          i, lex_label, conditionMessage(e), n_failed, max_failures
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
        "Too many chunk failures for '%s' (%d > %d allowed). ",
        lex_label, n_failed, max_failures,
        "Check your corpus for encoding issues or empty rows."
      ))

    res$doc_id   <- idx[res$doc_id]
    results[[i]] <- res
  }

  if (n_failed > 0)
    message(sprintf(
      "  [%s] completed with %d failed chunk(s) — rows show polarity = 'none'.",
      lex_label, n_failed
    ))

  dplyr::bind_rows(results)
}


# ══════════════════════════════════════════════════════════════════════════════
# PART 1 — Tokenisation and preprocessing
# ══════════════════════════════════════════════════════════════════════════════
# mixology_tokenize() is called internally by mixology_sentiment(), but can
# be used standalone to inspect tokens or apply negation handling manually.

tokens <- mixology_tokenize(
  tweet,
  remove_stopwords = TRUE,    # applies the bundled domain-specific stop list
  custom_stopwords = NULL,    # optional: character vector of extra stop words
  min_chars        = 2L       # drops single-character tokens
)

# Most frequent tokens
tokens |>
  count(token, sort = TRUE) |>
  head(20)

# Apply negation handling manually (optional — for inspection only)
tokens_negated <- mixology_negation(
  tokens,
  window           = NEGATION_WINDOW,
  custom_negations = NULL   # optional: extra negation markers
)

cat(sprintf(
  "Negated tokens: %.1f%% of all tokens\n",
  mean(tokens_negated$negated) * 100
))


# ══════════════════════════════════════════════════════════════════════════════
# PART 2 — Token coverage diagnostic
# ══════════════════════════════════════════════════════════════════════════════
# Run before scoring to identify coverage gaps across all ten lexicons.

coverage_tbl <- lexicon_coverage(tweet)
print(coverage_tbl |> arrange(desc(coverage)))


# ══════════════════════════════════════════════════════════════════════════════
# PART 3 — Sentiment scoring
# ══════════════════════════════════════════════════════════════════════════════

# ── 3a. Benchmark: all ten lexicons ───────────────────────────────────────────
# The four Mixology lexicons use corpus-frequency weighting (WEIGHTED = TRUE).
# The six general-purpose lexicons are scored without weighting to ensure a
# fair cross-lexicon comparison.

cat("\n── 3a. Scoring all ten lexicons ──\n")

mixology_keys    <- c("covid", "mixology", "covid_ft", "mixology_ft")
generalpurp_keys <- setdiff(names(mixology_lexicon_names()), mixology_keys)

# Score Mixology lexicons (weighted)
scores_mixology <- lapply(mixology_keys, function(lex) {
  cat(sprintf("Scoring: %s\n", lex))
  res <- score_chunks(tweet, lex, weighted = WEIGHTED)
  res$lexicon <- lex
  res
})
names(scores_mixology) <- mixology_keys

# Score general-purpose lexicons (unweighted)
scores_general <- lapply(generalpurp_keys, function(lex) {
  cat(sprintf("Scoring: %s\n", lex))
  res <- score_chunks(tweet, lex, weighted = FALSE)
  res$lexicon <- lex
  res
})
names(scores_general) <- generalpurp_keys

# Long-format table: one row per tweet × lexicon
all_scores_long <- bind_rows(
  bind_rows(scores_mixology),
  bind_rows(scores_general)
) |>
  mutate(lexicon_label = mixology_lexicon_names()[lexicon])

# Aggregate benchmark table
benchmark <- all_scores_long |>
  group_by(lexicon, lexicon_label) |>
  summarise(
    n_docs        = n(),
    n_matched     = sum(n_matched > 0),
    mean_coverage = mean(coverage),
    pct_none      = mean(polarity == "none") * 100,
    pct_positive  = mean(polarity == "positive" & n_matched > 0) /
                    max(mean(n_matched > 0), 1e-9) * 100,
    pct_negative  = mean(polarity == "negative" & n_matched > 0) /
                    max(mean(n_matched > 0), 1e-9) * 100,
    pct_ambiguous = mean(polarity == "ambiguous" & n_matched > 0) /
                    max(mean(n_matched > 0), 1e-9) * 100,
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

cat("\nBenchmark table (all ten lexicons):\n")
print(benchmark, n = 10)


# ── 3b. Original vs fine-tuned comparison ─────────────────────────────────────
# Direct tweet-level comparison between original and fine-tuned Mixology
# lexicons, showing the effect of gold-standard adaptation on polarity.

cat("\n── 3b. Original vs fine-tuned ──\n")

comparison_ft <- scores_mixology[["covid"]] |>
  select(doc_id, pol_covid = polarity, net_covid = score_net) |>
  left_join(
    scores_mixology[["covid_ft"]] |>
      select(doc_id, pol_covid_ft = polarity, net_covid_ft = score_net),
    by = "doc_id"
  ) |>
  left_join(
    scores_mixology[["mixology"]] |>
      select(doc_id, pol_mixo = polarity, net_mixo = score_net),
    by = "doc_id"
  ) |>
  left_join(
    scores_mixology[["mixology_ft"]] |>
      select(doc_id, pol_mixo_ft = polarity, net_mixo_ft = score_net),
    by = "doc_id"
  )

# Summary: original vs fine-tuned
.safe_bias <- function(neg, pos) {
  if (pos == 0) return(NA_real_)
  round(neg / pos, 3)
}

summary_ft <- tibble(
  lexicon = c("COVID (original)", "COVID (fine-tuned)",
              "Mixology (original)", "Mixology (fine-tuned)"),
  key     = mixology_keys
) |>
  rowwise() |>
  mutate(
    s             = list(scores_mixology[[key]]),
    pct_positive  = round(mean(s$polarity == "positive") * 100, 1),
    pct_negative  = round(mean(s$polarity == "negative") * 100, 1),
    pct_ambiguous = round(mean(s$polarity == "ambiguous") * 100, 1),
    pct_none      = round(mean(s$polarity == "none")     * 100, 1),
    mean_net      = round(mean(s$score_net), 3),
    neg_bias      = .safe_bias(sum(s$score_negative), sum(s$score_positive))
  ) |>
  select(-s, -key) |>
  ungroup()

cat("\nOriginal vs fine-tuned summary:\n")
print(summary_ft)

# Agreement between original and fine-tuned (COVID pair)
accord_ft <- comparison_ft |>
  filter(pol_covid != "none", pol_covid_ft != "none") |>
  mutate(agree = pol_covid == pol_covid_ft)

cat(sprintf(
  "\nCOVID original / fine-tuned agreement: %.1f%% (%d / %d tweets)\n",
  mean(accord_ft$agree) * 100, sum(accord_ft$agree), nrow(accord_ft)
))

# Attach scores to corpus and inspect specific cases
df_scored <- df |>
  bind_cols(scores_mixology[["covid_ft"]] |> select(-doc_id))

# Strongly negative tweets with high coverage
df_scored |>
  filter(polarity == "negative", coverage > 0.5) |>
  select(text, score_net, coverage) |>
  arrange(score_net) |>
  head(10)


# ── 3c. Custom lexicon (optional) ─────────────────────────────────────────────
# Benchmark your own lexicon in the same pipeline.
# use_custom_lexicon() prompts interactively if column names are not specified.

# my_data    <- read.csv("my_lexicon.csv")
# my_lex     <- use_custom_lexicon(my_data,
#                 word_col      = "term",
#                 sentiment_col = "polarity")
# scores_custom <- score_chunks(tweet, my_lex, weighted = FALSE)
# table(scores_custom$polarity)


# ══════════════════════════════════════════════════════════════════════════════
# PART 4 — Comparative evaluation (no ground truth required)
# ══════════════════════════════════════════════════════════════════════════════

cat("\n── Part 4: Comparative evaluation ──\n")

# ── 4a. Classification rate ───────────────────────────────────────────────────
# Tweets classified as "none" contribute nothing to the analysis.

cat("\nClassification rate (% tweets receiving a polarity label):\n")
benchmark |>
  select(lexicon_label, mean_coverage, pct_none) |>
  mutate(pct_classified = round(100 - pct_none, 1)) |>
  arrange(desc(pct_classified)) |>
  print()

# ── 4b. Negative bias ─────────────────────────────────────────────────────────
# General-purpose lexicons contain more negative than positive terms by design.
# A bias > 1.6 will systematically over-classify tweets as negative regardless
# of actual content.

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

# ── 4c. Coverage-corrected inter-lexicon stability ────────────────────────────
# Raw pairwise agreement is inflated for large lexicons because they classify
# more tweets and share larger intersections by default.
# Correction: S_AB = agreement × (C_intersection / max(C_A, C_B))
# A high corrected score means genuine coherence, not just high coverage.

set.seed(SEED)
sample_idx <- sample(seq_len(N), min(10000L, N))

all_scores_sample <- all_scores_long |>
  filter(doc_id %in% sample_idx)

coverage_indiv <- all_scores_sample |>
  group_by(lexicon) |>
  summarise(cov = mean(n_matched > 0), .groups = "drop") |>
  tibble::deframe()

sample_by_lex <- split(
  all_scores_sample |> select(lexicon, doc_id, polarity),
  all_scores_sample$lexicon
)

.stability <- function(lex_a, lex_b) {
  a <- sample_by_lex[[lex_a]] |> filter(polarity != "none") |>
    select(doc_id, pol_a = polarity)
  b <- sample_by_lex[[lex_b]] |> filter(polarity != "none") |>
    select(doc_id, pol_b = polarity)
  j <- inner_join(a, b, by = "doc_id")
  if (nrow(j) == 0) return(NA_real_)
  agreement      <- mean(j$pol_a == j$pol_b)
  c_intersection <- nrow(j) / length(sample_idx)
  round(agreement * (c_intersection / max(coverage_indiv[[lex_a]],
                                          coverage_indiv[[lex_b]])) * 100, 2)
}

all_lex   <- names(mixology_lexicon_names())
pairs     <- combn(all_lex, 2, simplify = FALSE)
n_pairs   <- length(pairs)

cat(sprintf("\nComputing stability for %d pairs (sample n = %d)...\n",
            n_pairs, length(sample_idx)))

stability_tbl <- bind_rows(lapply(seq_along(pairs), function(k) {
  p <- pairs[[k]]
  if (k %% 10 == 0 || k == n_pairs)
    cat(sprintf("  pair %d / %d\n", k, n_pairs))
  tibble(
    lex_a       = mixology_lexicon_names()[p[1]],
    lex_b       = mixology_lexicon_names()[p[2]],
    stability   = .stability(p[1], p[2])
  )
})) |>
  arrange(desc(stability))

cat("\nCoverage-corrected inter-lexicon stability:\n")
print(stability_tbl, n = n_pairs)

# ── 4d. Synthetic performance score ───────────────────────────────────────────
# Combines three internal metrics:
#   Coverage (50%) + Classification rate (30%) + Polarity balance (20%)

perf_score <- benchmark |>
  select(lexicon, lexicon_label, mean_coverage, pct_none, neg_bias) |>
  mutate(
    score_coverage = mean_coverage * 100,
    score_classif  = 100 - pct_none,
    score_balance  = if_else(
      is.na(neg_bias), 0,
      100 / (1 + abs(neg_bias - 1))
    ),
    score_global   = round(
      0.5 * score_coverage + 0.3 * score_classif + 0.2 * score_balance, 1
    )
  ) |>
  select(lexicon_label, score_coverage, score_classif,
         score_balance, score_global) |>
  mutate(across(where(is.numeric), \(x) round(x, 1))) |>
  arrange(desc(score_global))

cat("\nSynthetic performance score (coverage 50% | classification 30% | balance 20%):\n")
print(perf_score)


# ══════════════════════════════════════════════════════════════════════════════
# PART 5 — Visualisations
# ══════════════════════════════════════════════════════════════════════════════

# ── 5a. Token coverage ────────────────────────────────────────────────────────

p1 <- coverage_tbl |>
  mutate(
    lexicon_label = reorder(lexicon_label, coverage),
    is_mixo       = lexicon %in% mixology_keys
  ) |>
  ggplot(aes(x = lexicon_label, y = coverage * 100, fill = is_mixo)) +
  geom_col() +
  scale_fill_manual(
    values = c("FALSE" = "steelblue", "TRUE" = "#1D9E75"),
    labels = c("General-purpose", "Mixology"),
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

# ── 5b. Polarity distribution — all ten lexicons ──────────────────────────────

p2 <- benchmark |>
  select(lexicon_label, pct_positive, pct_negative, pct_ambiguous) |>
  pivot_longer(starts_with("pct_"),
               names_to = "polarity", values_to = "pct") |>
  mutate(
    polarity      = recode(polarity,
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
    "Ambiguous" = "#7570B3"
  )) +
  coord_flip() +
  labs(
    title    = "Polarity distribution by lexicon",
    subtitle = "% of classified tweets (matched documents only)",
    x = NULL, y = "%", fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p2)

# ── 5c. Original vs fine-tuned: polarity shift ────────────────────────────────

p3 <- benchmark |>
  filter(lexicon %in% mixology_keys) |>
  select(lexicon_label, pct_positive, pct_negative, pct_ambiguous) |>
  pivot_longer(starts_with("pct_"),
               names_to = "polarity", values_to = "pct") |>
  mutate(polarity = recode(polarity,
    pct_positive  = "Positive",
    pct_negative  = "Negative",
    pct_ambiguous = "Ambiguous"
  )) |>
  ggplot(aes(x = lexicon_label, y = pct, fill = polarity)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(
    "Positive"  = "#1D9E75",
    "Negative"  = "#D85A30",
    "Ambiguous" = "#7570B3"
  )) +
  coord_flip() +
  labs(
    title    = "Polarity distribution: original vs fine-tuned",
    subtitle = "Mixology lexicons only — % of matched tweets",
    x = NULL, y = "%", fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p3)

# ── 5d. Negative bias ─────────────────────────────────────────────────────────

p4 <- benchmark |>
  mutate(
    ratio         = pct_negative / ifelse(pct_positive == 0, 1, pct_positive),
    lexicon_label = reorder(lexicon_label, ratio)
  ) |>
  ggplot(aes(x = lexicon_label, y = ratio)) +
  geom_col(fill = "#BA7517") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  coord_flip() +
  labs(
    title    = "Negative bias by lexicon",
    subtitle = "Ratio neg% / pos% — dashed line = balanced",
    x = NULL, y = "Negative / Positive ratio"
  ) +
  theme_minimal()

print(p4)

# ── 5e. Synthetic performance score ───────────────────────────────────────────

p5 <- perf_score |>
  mutate(lexicon_label = reorder(lexicon_label, score_global)) |>
  pivot_longer(c(score_coverage, score_classif, score_balance),
               names_to = "component", values_to = "value") |>
  mutate(component = recode(component,
    score_coverage = "Coverage (x0.5)",
    score_classif  = "Classification rate (x0.3)",
    score_balance  = "Balance (x0.2)"
  )) |>
  ggplot(aes(x = lexicon_label, y = value, fill = component)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c(
    "Coverage (x0.5)"            = "#378ADD",
    "Classification rate (x0.3)" = "#1D9E75",
    "Balance (x0.2)"             = "#BA7517"
  )) +
  coord_flip() +
  labs(
    title    = "Synthetic performance score",
    subtitle = "Coverage 50% + classification rate 30% + balance 20%",
    x = NULL, y = "Score", fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p5)

# ── 5f. Inter-lexicon stability heatmap ───────────────────────────────────────

stability_matrix <- stability_tbl |>
  select(lex_a, lex_b, stability) |>
  bind_rows(stability_tbl |> select(lex_a = lex_b, lex_b = lex_a, stability)) |>
  bind_rows(tibble(
    lex_a      = unique(stability_tbl$lex_a),
    lex_b      = unique(stability_tbl$lex_a),
    stability  = 100
  ))

p6 <- stability_matrix |>
  ggplot(aes(x = lex_a, y = lex_b, fill = stability)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = round(stability, 0)), size = 2.5, colour = "white") +
  scale_fill_gradient(low = "#D85A30", high = "#1D9E75",
                      name = "Corrected\nstability") +
  labs(
    title    = "Coverage-corrected inter-lexicon stability",
    subtitle = "S = agreement x (C_intersection / max(C_A, C_B))",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

print(p6)


# ══════════════════════════════════════════════════════════════════════════════
# PART 6 — Export
# ══════════════════════════════════════════════════════════════════════════════

write.csv(benchmark,      "benchmark_all_lexicons.csv",        row.names = FALSE)
write.csv(perf_score,     "performance_scores.csv",            row.names = FALSE)
write.csv(stability_tbl,  "stability_corrected.csv",           row.names = FALSE)
write.csv(summary_ft,     "original_vs_finetuned_summary.csv", row.names = FALSE)
write.csv(comparison_ft,  "original_vs_finetuned_doclevel.csv",row.names = FALSE)
write.csv(df_scored,      "corpus_scored_covid_ft.csv",        row.names = FALSE)

# Full doc-level scores across all lexicons (large file — uncomment if needed):
# write.csv(all_scores_long, "all_scores_doclevel.csv", row.names = FALSE)

cat("\nDone. Files saved to:", getwd(), "\n")
