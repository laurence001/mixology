# ══════════════════════════════════════════════════════════════════════════════
# Mixology — Full sentiment analysis pipeline
# Corpus: political measures sub-corpus (English tweets, Western Europe)
# ══════════════════════════════════════════════════════════════════════════════

library(mixology)
library(dplyr)
library(ggplot2)   # for optional visualisations


# ── 0. Load corpus ────────────────────────────────────────────────────────────

df    <- sample_corpus_politics_en   # replace with read.csv("...") if needed
tweet <- df$text


# ── 1. Available lexicons ─────────────────────────────────────────────────────

mixology_lexicon_names()
# Access a lexicon directly:
# get_lexicon("covid")
# get_lexicon("bing")


# ── 2. Token coverage — quick diagnostic ─────────────────────────────────────
# How many corpus tokens does each lexicon recognise?

coverage <- lexicon_coverage(tweet)
print(coverage)


# ── 3. Sentiment analysis — single lexicon ────────────────────────────────────
# Returns a tibble with one row per tweet:
# doc_id | n_tokens | n_matched | coverage | score_positive | score_negative
# score_ambiguous | score_net | polarity

scores_covid <- mixology_sentiment(
  tweet,
  lexicon         = "covid",   # "covid", "mixology", "bing", "nrc", etc.
  weighted        = TRUE,       # corpus-frequency weighting
  handle_negation = TRUE,       # reverse polarity after negation marker
  negation_window = 3           # window of 3 tokens after the marker
)

head(scores_covid)
table(scores_covid$polarity)


# ── 4. Attach scores back to the original data frame ─────────────────────────

df_scored <- bind_cols(df, scores_covid |> select(-doc_id))

# Negative tweets with high coverage:
df_scored |>
  filter(polarity == "negative", coverage > 0.5) |>
  select(text, score_net, coverage) |>
  arrange(score_net) |>
  head(10)


# ── 5. Benchmark — all lexicons in parallel ───────────────────────────────────
# summary = TRUE  -> one row per lexicon, aggregated metrics (benchmark table)
# summary = FALSE -> one row per tweet × lexicon (long format, for ggplot)

benchmark <- compare_lexicons(
  tweet,
  lexicons        = names(mixology_lexicon_names()),
  weighted        = FALSE,   # FALSE for a fair cross-lexicon comparison
  handle_negation = TRUE,
  negation_window = 3,
  summary         = TRUE
)

print(benchmark)


# ── 6. Doc-level results (long format) ───────────────────────────────────────

doc_results <- compare_lexicons(
  tweet,
  lexicons        = names(mixology_lexicon_names()),
  weighted        = FALSE,
  handle_negation = TRUE,
  summary         = FALSE
)

# Polarity distribution by lexicon:
doc_results |>
  filter(n_matched > 0) |>
  count(lexicon_label, polarity) |>
  group_by(lexicon_label) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  arrange(lexicon_label, polarity)


# ── 7. Cross-lexicon conflicts ────────────────────────────────────────────────
# Terms whose polarity differs across lexicons

conflicts_all <- lexicon_conflicts()
head(conflicts_all, 20)

# Conflicts between Covid and general-purpose lexicons only:
conflicts_covid <- lexicon_conflicts(
  lexicons     = c("covid", "bing", "nrc", "mixology"),
  min_conflict = 2
)
head(conflicts_covid, 20)


# ── 8. ggplot2 visualisations ─────────────────────────────────────────────────

# 8a. Token coverage by lexicon
coverage |>
  mutate(lexicon_label = reorder(lexicon_label, coverage)) |>
  ggplot(aes(x = lexicon_label, y = coverage * 100)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title    = "Token coverage by lexicon",
    subtitle = paste0("Politics corpus (n = ", length(tweet), " tweets)"),
    x        = NULL,
    y        = "Coverage (%)"
  ) +
  theme_minimal()


# 8b. Positive / negative distribution by lexicon (long format)
benchmark |>
  select(lexicon_label, pct_positive, pct_negative) |>
  tidyr::pivot_longer(
    cols      = c(pct_positive, pct_negative),
    names_to  = "polarity",
    values_to = "pct"
  ) |>
  mutate(
    polarity      = ifelse(polarity == "pct_positive", "Positive", "Negative"),
    lexicon_label = reorder(lexicon_label, ifelse(polarity == "Negative", pct, 0))
  ) |>
  ggplot(aes(x = lexicon_label, y = pct, fill = polarity)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Positive" = "#1D9E75", "Negative" = "#D85A30")) +
  coord_flip() +
  labs(
    title    = "Tweet-level polarity by lexicon",
    subtitle = "% of matched tweets classified positive or negative",
    x        = NULL,
    y        = "%",
    fill     = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")


# 8c. Negative bias (neg_tokens / pos_tokens) by lexicon
benchmark |>
  mutate(
    neg_bias      = round(mean_score_net * -1, 2),  # visual proxy
    lexicon_label = reorder(lexicon_label, -pct_negative / pct_positive)
  ) |>
  ggplot(aes(x = reorder(lexicon_label, pct_negative / ifelse(pct_positive == 0, 1, pct_positive)),
             y = pct_negative / ifelse(pct_positive == 0, 1, pct_positive))) +
  geom_col(fill = "#BA7517") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  coord_flip() +
  labs(
    title    = "Negative bias by lexicon",
    subtitle = "Ratio neg% / pos% at tweet level (dashed = balanced)",
    x        = NULL,
    y        = "Negative / Positive ratio"
  ) +
  theme_minimal()


# ── 9. Export ─────────────────────────────────────────────────────────────────

write.csv(benchmark,     "benchmark_lexicons.csv",    row.names = FALSE)
write.csv(df_scored,     "corpus_scored_covid.csv",   row.names = FALSE)
write.csv(conflicts_all, "lexicon_conflicts.csv",      row.names = FALSE)
