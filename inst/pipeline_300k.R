# ══════════════════════════════════════════════════════════════════════════════
# Mixology — Full sentiment analysis pipeline (large corpus)
# Corpus: English tweets, Western Europe, Dec. 2021
# No ground truth — internal evaluation metrics only
#
# !! IMPORTANT: run this script top to bottom in a single session.
#    Parts 2–5 depend on objects created in Part 1 (all_scores_long,
#    benchmark, etc.). Running sections out of order will produce errors.
#
# Pipeline structure:
#   Part 1 — Covid vs Mixology (pair comparison)
#   Part 2 — Benchmark across all 10 lexicons
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

CHUNK_SIZE         <- 5000   # tweets per chunk — reduce if RAM is limited
NEGATION_WINDOW    <- 3
WEIGHTED           <- TRUE   # corpus-frequency weights for Mixology lexicons
SEED               <- 42
MAX_CHUNK_FAILURES <- 3      # abort if more than this many chunks fail

# Load your corpus — adapt the line below:
# df <- read.csv("your_corpus.csv", stringsAsFactors = FALSE)
tweet <- sample_corpus_vaccin$text   # <-- replace with your object name
N     <- length(tweet)
cat("Corpus:", N, "tweets\n")


# ── Helper: safe chunk scorer ─────────────────────────────────────────────────
# Scores a character vector in fixed-size chunks to keep memory manageable.
# Accepts either a lexicon key string or a data frame prepared with
# use_custom_lexicon().
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
      cat(sprintf("  [%s] chunk %d / %d\n",
                  if (is.character(lexicon_name)) lexicon_name else "custom",
                  i, total))

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
          i,
          if (is.character(lexicon_name)) lexicon_name else "custom",
          conditionMessage(e), n_failed, max_failures
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
        if (is.character(lexicon_name)) lexicon_name else "custom",
        n_failed, max_failures,
        "Check your corpus for encoding issues or empty rows."
      ))

    res$doc_id <- idx[res$doc_id]
    results[[i]] <- res
  }

  if (n_failed > 0)
    message(sprintf(
      "  [%s] completed with %d failed chunk(s) — those rows show polarity = 'none'.",
      if (is.character(lexicon_name)) lexicon_name else "custom",
      n_failed
    ))

  dplyr::bind_rows(results)
}


# ══════════════════════════════════════════════════════════════════════════════
# PART 1 — Covid vs Mixology (original and fine-tuned)
# The four Mixology lexicons are compared using corpus-frequency weighting
# and negation handling, to assess how fine-tuning affects results.
# ══════════════════════════════════════════════════════════════════════════════

cat("\n── Part 1: Covid vs Mixology (original and fine-tuned) ──\n")

scores_covid    <- score_chunks(tweet, "covid",       weighted = WEIGHTED)
scores_mixo     <- score_chunks(tweet, "mixology",    weighted = WEIGHTED)
scores_covid_ft <- score_chunks(tweet, "covid_ft",    weighted = WEIGHTED)
scores_mixo_ft  <- score_chunks(tweet, "mixology_ft", weighted = WEIGHTED)

# Join for direct tweet-level comparison (original vs fine-tuned)
comparison_ft <- scores_covid |>
  select(doc_id, pol_covid = polarity, net_covid = score_net) |>
  left_join(
    scores_covid_ft |> select(doc_id, pol_covid_ft = polarity,
                               net_covid_ft = score_net),
    by = "doc_id"
  ) |>
  left_join(
    scores_mixo |> select(doc_id, pol_mixo = polarity,
                           net_mixo = score_net),
    by = "doc_id"
  ) |>
  left_join(
    scores_mixo_ft |> select(doc_id, pol_mixo_ft = polarity,
                              net_mixo_ft = score_net),
    by = "doc_id"
  )

# ── 1a. Descriptive summary ───────────────────────────────────────────────────

.safe_bias <- function(neg, pos) {
  if (pos == 0) {
    warning("Zero positive token matches — neg_bias is undefined; returning NA.")
    return(NA_real_)
  }
  neg / pos
}

summary_ft <- tibble(
  lexicon = c("COVID (original)", "COVID (fine-tuned)",
              "Mixology (original)", "Mixology (fine-tuned)"),
  scores  = list(scores_covid, scores_covid_ft,
                 scores_mixo,  scores_mixo_ft)
) |>
  rowwise() |>
  mutate(
    mean_coverage = mean(scores$coverage),
    pct_positive  = mean(scores$polarity == "positive") * 100,
    pct_negative  = mean(scores$polarity == "negative") * 100,
    pct_ambiguous = mean(scores$polarity == "ambiguous") * 100,
    pct_none      = mean(scores$polarity == "none") * 100,
    mean_net      = mean(scores$score_net),
    neg_bias      = .safe_bias(sum(scores$score_negative),
                               sum(scores$score_positive))
  ) |>
  select(-scores) |>
  ungroup() |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

cat("\nOriginal vs fine-tuned summary:\n")
print(summary_ft)

# ── 1b. Inter-lexicon agreement (original pair) ───────────────────────────────

accord <- comparison_ft |>
  filter(pol_covid != "none", pol_mixo != "none") |>
  mutate(agree = pol_covid == pol_mixo)

cat(sprintf(
  "\nCovid / Mixology agreement: %.1f%% (%d / %d classified tweets)\n",
  mean(accord$agree) * 100, sum(accord$agree), nrow(accord)
))

cat("\nConfusion matrix (rows = Covid, columns = Mixology):\n")
print(table(Covid = accord$pol_covid, Mixology = accord$pol_mixo))

divergent <- comparison_ft |>
  filter(pol_covid != "none", pol_mixo != "none",
         pol_covid != pol_mixo) |>
  mutate(text = tweet[doc_id])

cat(sprintf("\n%d divergent tweets (%.1f%%)\n",
    nrow(divergent), 100 * nrow(divergent) / nrow(accord)))

# ── 1c. Optional: custom lexicon ─────────────────────────────────────────────
# Uncomment and adapt to benchmark your own lexicon in the same pipeline.
# use_custom_lexicon() will prompt interactively if word_col or
# sentiment_col are not specified.

# my_data  <- read.csv("my_lexicon.csv")
# my_lex   <- use_custom_lexicon(my_data,
#               word_col      = "term",
#               sentiment_col = "polarity")
# scores_custom <- score_chunks(tweet, my_lex, weighted = FALSE)
# table(scores_custom$polarity)


# ══════════════════════════════════════════════════════════════════════════════
# PART 2 — Benchmark: all 10 lexicons
# The 6 general-purpose lexicons are scored without weighting for a fair
# cross-lexicon comparison. The Mixology lexicons from Part 1 are reused.
#
# Produces: all_scores_long — used by Parts 3, 4, and 5.
# ══════════════════════════════════════════════════════════════════════════════

cat("\n── Part 2: Benchmark — 10 lexicons ──\n")

all_lexicons   <- names(mixology_lexicon_names())
other_lexicons <- setdiff(all_lexicons,
                          c("covid", "mixology", "covid_ft", "mixology_ft"))

scores_others <- lapply(other_lexicons, function(lex) {
  cat(sprintf("  Scoring: %s\n", lex))
  res <- score_chunks(tweet, lex, weighted = FALSE, handle_negation = TRUE)
  res$lexicon <- lex
  res
})
names(scores_others) <- other_lexicons

# Long-format table: one row per tweet × lexicon
all_scores_long <- bind_rows(
  scores_covid    |> mutate(lexicon = "covid"),
  scores_mixo     |> mutate(lexicon = "mixology"),
  scores_covid_ft |> mutate(lexicon = "covid_ft"),
  scores_mixo_ft  |> mutate(lexicon = "mixology_ft"),
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
print(benchmark, n = 10)


# ══════════════════════════════════════════════════════════════════════════════
# PART 3 — Comparative evaluation (no ground truth)
# ══════════════════════════════════════════════════════════════════════════════

cat("\n── Part 3: Comparative evaluation ──\n")

# ── 3a. Token coverage ────────────────────────────────────────────────────────

cat("\nToken coverage (% corpus tokens matched):\n")
coverage_tbl <- lexicon_coverage(tweet)
print(coverage_tbl |> arrange(desc(coverage)))

# ── 3b. Classification rate ───────────────────────────────────────────────────

classif_rate <- benchmark |>
  select(lexicon_label, mean_coverage, pct_none) |>
  mutate(pct_classified = round(100 - pct_none, 1)) |>
  arrange(desc(pct_classified))

cat("\nClassification rate:\n")
print(classif_rate)

# ── 3c. Negative bias ─────────────────────────────────────────────────────────

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

set.seed(SEED)
sample_idx <- sample(seq_len(N), min(10000, N))

all_scores_sample <- all_scores_long |>
  filter(doc_id %in% sample_idx)

coverage_indiv <- all_scores_sample |>
  group_by(lexicon) |>
  summarise(cov = mean(n_matched > 0), .groups = "drop") |>
  tibble::deframe()

cat("\nIndividual tweet coverage on sample (10k):\n")
print(sort(coverage_indiv, decreasing = TRUE))

sample_by_lex <- split(
  all_scores_sample |> select(lexicon, doc_id, polarity),
  all_scores_sample$lexicon
)

.lex_matched <- function(lex_name, sample_by_lex, col_name) {
  sample_by_lex[[lex_name]] |>
    filter(polarity != "none") |>
    select(doc_id, !!col_name := polarity)
}

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

pairwise_stability_simple <- function(lex_a, lex_b, idx,
                                      sample_by_lex, coverage_indiv) {
  b <- .stability_base(lex_a, lex_b, idx, sample_by_lex, coverage_indiv)
  if (is.null(b)) return(NA_real_)
  b$agreement * (b$c_intersection / max(b$c_a, b$c_b))
}

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
    cat(sprintf("  pair %d / %d: %s vs %s\n", k, n_pairs,
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
cat("  Simple  = agreement x (C_intersection / max(C_A, C_B))\n")
cat("  Strict  = agreement x (C_intersection / sqrt(C_A x C_B))\n\n")
print(agree_tbl_cov, n = n_pairs)

# ── 3e. Synthetic performance score ───────────────────────────────────────────

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
  cat("  Note: balance score = 0 for any lexicon with NA neg_bias\n")
print(perf_score)


# ══════════════════════════════════════════════════════════════════════════════
# PART 4 — Visualisations
# ══════════════════════════════════════════════════════════════════════════════

cat("\n── Part 4: Visualisations ──\n")

# ── 4a. Token coverage ────────────────────────────────────────────────────────

p1 <- benchmark |>
  mutate(
    lexicon_label = reorder(lexicon_label, mean_coverage),
    is_mixo       = lexicon %in% c("covid", "mixology",
                                   "covid_ft", "mixology_ft")
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

# ── 4c. Original vs fine-tuned: polarity shift ────────────────────────────────

p3 <- benchmark |>
  filter(lexicon %in% c("covid", "covid_ft", "mixology", "mixology_ft")) |>
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
    subtitle = "% of matched tweets per polarity class",
    x = NULL, y = "%", fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p3)

# ── 4d. Synthetic performance score ───────────────────────────────────────────

p4 <- perf_score |>
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
    title    = "Synthetic performance score by lexicon",
    subtitle = "Coverage 50% + classification 30% + balance 20%",
    x = NULL, y = "Score", fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p4)

# ── 4e. Coverage-corrected stability heatmap ──────────────────────────────────

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

p5 <- agree_matrix_cov |>
  ggplot(aes(x = lex_a, y = lex_b, fill = agreement)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = round(agreement, 0)), size = 2.8, colour = "white") +
  scale_fill_gradient(low = "#D85A30", high = "#1D9E75",
                      name = "Corrected stability") +
  labs(
    title    = "Coverage-corrected inter-lexicon stability",
    subtitle = paste0(
      "S = agreement x (C_intersection / max(C_A, C_B)) — sample 10k tweets\n",
      "Penalises large lexicons that inflate raw agreement through coverage alone"
    ),
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

print(p5)


# ══════════════════════════════════════════════════════════════════════════════
# PART 5 — Export
# ══════════════════════════════════════════════════════════════════════════════

write.csv(benchmark,      "benchmark_all_lexicons.csv",     row.names = FALSE)
write.csv(perf_score,     "performance_scores.csv",         row.names = FALSE)
write.csv(agree_tbl_cov,  "stability_corrected.csv",        row.names = FALSE)
write.csv(comparison_ft,  "original_vs_finetuned_doclevel.csv", row.names = FALSE)
write.csv(
  divergent |> select(doc_id, text, pol_covid, pol_mixo, net_covid, net_mixo),
  "divergent_tweets.csv", row.names = FALSE
)

# Full doc-level scores (large file on 300k tweets — uncomment if needed):
# write.csv(all_scores_long, "all_scores_doclevel.csv", row.names = FALSE)

cat("\nDone. Files saved to:", getwd(), "\n")