# ══════════════════════════════════════════════════════════════════════════════
# Mixology — Pipeline complet d'analyse de sentiment
# Corpus : political measures sub-corpus (tweets en anglais, Europe occidentale)
# ══════════════════════════════════════════════════════════════════════════════

library(mixology)
library(dplyr)
library(ggplot2)   # pour les visualisations optionnelles


# ── 0. Chargement du corpus ───────────────────────────────────────────────────

df    <- sample_corpus_politics_en   # remplacer par read.csv("...") si besoin
tweet <- df$text


# ── 1. Lexiques disponibles ───────────────────────────────────────────────────

mixology_lexicon_names()
# Accéder à un lexique directement :
# get_lexicon("covid")
# get_lexicon("bing")


# ── 2. Couverture token — diagnostic rapide ───────────────────────────────────
# Combien de tokens du corpus chaque lexique reconnaît-il ?

coverage <- lexicon_coverage(tweet)
print(coverage)


# ── 3. Analyse de sentiment — un seul lexique ─────────────────────────────────
# Retourne un tibble avec une ligne par tweet :
# doc_id | n_tokens | n_matched | coverage | score_positive | score_negative
# score_ambiguous | score_net | polarity

scores_covid <- mixology_sentiment(
  tweet,
  lexicon         = "covid",   # "covid", "mixology", "bing", "nrc", etc.
  weighted        = TRUE,       # pondération par fréquence corpus
  handle_negation = TRUE,       # inverser la polarité après négation
  negation_window = 3           # fenêtre de 3 tokens après le marqueur
)

head(scores_covid)
table(scores_covid$polarity)


# ── 4. Réattacher les scores au dataframe original ────────────────────────────

df_scored <- bind_cols(df, scores_covid |> select(-doc_id))

# Tweets négatifs avec forte couverture :
df_scored |>
  filter(polarity == "negative", coverage > 0.5) |>
  select(text, score_net, coverage) |>
  arrange(score_net) |>
  head(10)


# ── 5. Benchmark — tous les lexiques en parallèle ─────────────────────────────
# summary = TRUE  -> une ligne par lexique, métriques agrégées (tableau benchmark)
# summary = FALSE -> une ligne par tweet × lexique (format long, pour ggplot)

benchmark <- compare_lexicons(
  tweet,
  lexicons        = names(mixology_lexicon_names()),
  weighted        = FALSE,   # FALSE pour comparaison équitable entre lexiques
  handle_negation = TRUE,
  negation_window = 3,
  summary         = TRUE
)

print(benchmark)


# ── 6. Résultats doc-level (format long) ─────────────────────────────────────

doc_results <- compare_lexicons(
  tweet,
  lexicons        = names(mixology_lexicon_names()),
  weighted        = FALSE,
  handle_negation = TRUE,
  summary         = FALSE
)

# Distribution des polarités par lexique :
doc_results |>
  filter(n_matched > 0) |>
  count(lexicon_label, polarity) |>
  group_by(lexicon_label) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  arrange(lexicon_label, polarity)


# ── 7. Conflits inter-lexiques ────────────────────────────────────────────────
# Termes dont la polarité diffère selon les lexiques

conflicts_all <- lexicon_conflicts()
head(conflicts_all, 20)

# Conflits entre Covid et les lexiques généraux seulement :
conflicts_covid <- lexicon_conflicts(
  lexicons     = c("covid", "bing", "nrc", "mixology"),
  min_conflict = 2
)
head(conflicts_covid, 20)


# ── 8. Visualisations ggplot2 ─────────────────────────────────────────────────

# 8a. Couverture token par lexique
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


# 8b. Distribution positive / négative par lexique (format long)
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


# 8c. Negative bias (neg_tokens / pos_tokens) par lexique
benchmark |>
  mutate(
    neg_bias      = round(mean_score_net * -1, 2),  # proxy visuel
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
