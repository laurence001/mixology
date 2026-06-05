# ══════════════════════════════════════════════════════════════════════════════
# Mixology — Sentiment analysis pipeline
# ══════════════════════════════════════════════════════════════════════════════

library(mixology)
library(dplyr)
library(ggplot2)


# ── 0. Load corpus ────────────────────────────────────────────────────────────

df    <- sample_corpus_politics_en   # replace with read.csv("...") if needed
tweet <- df$text


# ── 1. Available lexicons ─────────────────────────────────────────────────────
# Returns a named vector of the ten built-in lexicon keys and their labels.
# Keys: "inquirer", "subjectivity", "bing", "nrc", "afinn", "loughran",
#       "covid", "mixology", "covid_ft", "mixology_ft"

mixology_lexicon_names()

# Access any lexicon as a tibble:
# get_lexicon("covid")
# get_lexicon("covid_ft")


# ── 2. Token coverage — quick diagnostic ─────────────────────────────────────
# Proportion of corpus tokens matched by each lexicon.
# Run before scoring to identify coverage gaps.

coverage <- lexicon_coverage(tweet)
print(coverage)


# ── 3. Tokenisation and preprocessing ────────────────────────────────────────
# mixology_tokenize() is called internally by mixology_sentiment(), but can
# also be used standalone to inspect tokens, compute frequency tables, or
# pass the tokenised output to mixology_negation() manually.

tokens <- mixology_tokenize(
  tweet,
  remove_stopwords = TRUE,    # applies the bundled domain-specific stop list
  custom_stopwords = NULL,    # optional: character vector of extra stop words
  min_chars        = 2L       # drops single-character tokens
)

# Inspect the most frequent tokens in the corpus
tokens |>
  count(token, sort = TRUE) |>
  head(20)

# Apply negation handling to the tokenised table
# (mixology_sentiment() does this automatically when handle_negation = TRUE)
tokens_negated <- mixology_negation(
  tokens,
  window           = 3L,   # tokens flagged after each negation marker
  custom_negations = NULL  # optional: character vector of extra markers
)

# Proportion of tokens flagged as negated
mean(tokens_negated$negated)


# ── 4. Sentiment scoring — single lexicon ─────────────────────────────────────
# Returns one row per tweet:
#   doc_id | n_tokens | n_matched | coverage |
#   score_positive | score_negative | score_ambiguous | score_net | polarity

scores_covid <- mixology_sentiment(
  tweet,
  lexicon         = "covid",
  weighted        = TRUE,    # corpus-frequency weighting (Mixology lexicons)
  handle_negation = TRUE,    # polarity reversal within negation window
  negation_window = 3L
)

head(scores_covid)
table(scores_covid$polarity)

# Fine-tuned lexicon — compare directly
scores_covid_ft <- mixology_sentiment(
  tweet,
  lexicon         = "covid_ft",
  weighted        = TRUE,
  handle_negation = TRUE,
  negation_window = 3L
)

table(scores_covid_ft$polarity)


# ── 5. Attach scores back to the corpus ───────────────────────────────────────

df_scored <- bind_cols(df, scores_covid |> select(-doc_id))

# Inspect strongly negative tweets with high coverage
df_scored |>
  filter(polarity == "negative", coverage > 0.5) |>
  select(text, score_net, coverage) |>
  arrange(score_net) |>
  head(10)


# ── 6. Custom lexicon ─────────────────────────────────────────────────────────
# Benchmark or score with a user-supplied lexicon.
# use_custom_lexicon() validates column names and prompts interactively
# if word_col or sentiment_col are not specified.

my_data <- read.csv("my_lexicon.csv")   # must have a word and sentiment column

my_lex <- use_custom_lexicon(
  my_data,
  word_col      = "term",      # column containing the terms
  sentiment_col = "polarity"   # column containing positive/negative/ambiguous
)

scores_custom <- mixology_sentiment(tweet, lexicon = my_lex)
table(scores_custom$polarity)


# ── 7. Benchmark — all ten lexicons ───────────────────────────────────────────
# summary = TRUE  -> one row per lexicon, corpus-level aggregates
# summary = FALSE -> one row per tweet × lexicon (long format)

benchmark <- compare_lexicons(
  tweet,
  lexicons        = names(mixology_lexicon_names()),
  weighted        = FALSE,   # FALSE ensures a fair cross-lexicon comparison
  handle_negation = TRUE,
  negation_window = 3L,
  summary         = TRUE
)

print(benchmark)

# Original vs fine-tuned comparison
compare_lexicons(
  tweet,
  lexicons = c("covid", "covid_ft", "mixology", "mixology_ft"),
  weighted = FALSE,
  summary  = TRUE
)


# ── 8. Document-level results (long format) ───────────────────────────────────

doc_results <- compare_lexicons(
  tweet,
  lexicons        = names(mixology_lexicon_names()),
  weighted        = FALSE,
  handle_negation = TRUE,
  negation_window = 3L,
  summary         = FALSE
)

# Polarity distribution by lexicon
doc_results |>
  filter(n_matched > 0) |>
  count(lexicon_label, polarity) |>
  group_by(lexicon_label) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  arrange(lexicon_label, polarity)


# ── 9. Cross-lexicon conflicts ────────────────────────────────────────────────
# Terms assigned different polarities across two or more lexicons.

# All conflicts across all ten lexicons
conflicts_all <- lexicon_conflicts()
head(conflicts_all, 20)

# Conflicts between original and fine-tuned — shows what changed
conflicts_ft <- lexicon_conflicts(
  lexicons = c("covid", "covid_ft"),
  min_conflict = 2
)
head(conflicts_ft, 20)

# Conflicts between Covid and general-purpose lexicons
conflicts_covid <- lexicon_conflicts(
  lexicons     = c("covid", "bing", "nrc", "mixology"),
  min_conflict = 2
)
head(conflicts_covid, 20)


# ── 10. Visualisations ────────────────────────────────────────────────────────

# 10a. Token coverage by lexicon
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

# 10b. Positive / negative distribution by lexicon
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

# 10c. Negative bias by lexicon (ratio neg% / pos%)
benchmark |>
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
    subtitle = "Ratio neg% / pos% at tweet level (dashed = balanced)",
    x        = NULL,
    y        = "Negative / Positive ratio"
  ) +
  theme_minimal()

# 10d. Original vs fine-tuned: polarity shift
compare_lexicons(
  tweet,
  lexicons = c("covid", "covid_ft", "mixology", "mixology_ft"),
  weighted = FALSE,
  summary  = TRUE
) |>
  select(lexicon_label, pct_positive, pct_negative, pct_ambiguous) |>
  tidyr::pivot_longer(
    cols      = c(pct_positive, pct_negative, pct_ambiguous),
    names_to  = "polarity",
    values_to = "pct"
  ) |>
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
    x        = NULL, y = "%", fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")


# ── 11. Export ────────────────────────────────────────────────────────────────

write.csv(benchmark,     "benchmark_lexicons.csv",  row.names = FALSE)
write.csv(df_scored,     "corpus_scored_covid.csv", row.names = FALSE)
write.csv(conflicts_all, "lexicon_conflicts.csv",   row.names = FALSE)