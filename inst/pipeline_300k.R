# ══════════════════════════════════════════════════════════════════════════════
# Mixology — Analyse de sentiment sur grand corpus (300k+ tweets)
# Sans ground truth — évaluation par métriques internes
# ══════════════════════════════════════════════════════════════════════════════

library(mixology)
library(dplyr)
library(tidyr)
library(ggplot2)


# ── 0. Paramètres globaux ─────────────────────────────────────────────────────

CHUNK_SIZE      <- 5000    # tweets par chunk (ajuster selon RAM disponible)
NEGATION_WINDOW <- 3
WEIGHTED        <- TRUE    # pondération corpus pour covid et mixology
SEED            <- 42

# Chemin vers votre fichier (adapter)
# df <- read.csv("votre_corpus.csv", stringsAsFactors = FALSE)
# Ici on suppose que vous avez déjà df en mémoire :
tweet <- df$text
N     <- length(tweet)
cat("Corpus :", N, "tweets\n")


# ══════════════════════════════════════════════════════════════════════════════
# PARTIE 1 — Covid vs Mixology (comparaison des deux lexiques maison)
# ══════════════════════════════════════════════════════════════════════════════

cat("\n── Partie 1 : Covid vs Mixology ──\n")

score_chunks <- function(tweets, lexicon_name, weighted = TRUE,
                          handle_negation = TRUE, chunk_size = CHUNK_SIZE) {
  n      <- length(tweets)
  chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  total  <- length(chunks)

  results <- vector("list", total)
  for (i in seq_along(chunks)) {
    if (i %% 10 == 0 || i == total)
      cat(sprintf("  [%s] chunk %d / %d\n", lexicon_name, i, total))
    idx <- chunks[[i]]
    results[[i]] <- mixology_sentiment(
      tweets[idx],
      lexicon         = lexicon_name,
      weighted        = weighted,
      handle_negation = handle_negation,
      negation_window = NEGATION_WINDOW
    ) |>
      mutate(doc_id = idx[doc_id])   # rétablir les vrais indices
  }
  bind_rows(results)
}

# Scorer les deux lexiques maison
scores_covid   <- score_chunks(tweet, "covid",    weighted = WEIGHTED)
scores_mixo    <- score_chunks(tweet, "mixology", weighted = WEIGHTED)

# Joindre pour comparaison directe
comparison_12 <- scores_covid |>
  select(doc_id, n_tokens,
         cov_covid   = coverage,
         net_covid   = score_net,
         pol_covid   = polarity) |>
  left_join(
    scores_mixo |>
      select(doc_id,
             cov_mixo  = coverage,
             net_mixo  = score_net,
             pol_mixo  = polarity),
    by = "doc_id"
  )

# ── 1a. Statistiques descriptives ─────────────────────────────────────────────

summary_12 <- tibble(
  lexicon       = c("Mixology Covid", "Mixology"),
  n_tweets      = N,
  mean_coverage = c(mean(comparison_12$cov_covid), mean(comparison_12$cov_mixo)),
  pct_positive  = c(
    mean(comparison_12$pol_covid == "positive") * 100,
    mean(comparison_12$pol_mixo  == "positive") * 100
  ),
  pct_negative  = c(
    mean(comparison_12$pol_covid == "negative") * 100,
    mean(comparison_12$pol_mixo  == "negative") * 100
  ),
  pct_ambiguous = c(
    mean(comparison_12$pol_covid == "ambiguous") * 100,
    mean(comparison_12$pol_mixo  == "ambiguous") * 100
  ),
  pct_none      = c(
    mean(comparison_12$pol_covid == "none") * 100,
    mean(comparison_12$pol_mixo  == "none") * 100
  ),
  mean_net      = c(mean(comparison_12$net_covid), mean(comparison_12$net_mixo)),
  neg_bias      = c(
    sum(scores_covid$score_negative) / sum(scores_covid$score_positive),
    sum(scores_mixo$score_negative)  / sum(scores_mixo$score_positive)
  )
) |> mutate(across(where(is.numeric), \(x) round(x, 3)))

cat("\nRésumé Covid vs Mixology :\n")
print(summary_12)

# ── 1b. Accord inter-lexiques : dans combien de cas les deux lexiques ──────────
#         donnent-ils la même polarité dominante ?

accord <- comparison_12 |>
  filter(pol_covid != "none", pol_mixo != "none") |>
  mutate(agree = pol_covid == pol_mixo)

cat(sprintf("\nAccord Covid / Mixology : %.1f%% (%d / %d tweets classifiés)\n",
    mean(accord$agree) * 100, sum(accord$agree), nrow(accord)))

# Matrice de confusion inter-lexiques
cat("\nMatrice d'accord (lignes = Covid, colonnes = Mixology) :\n")
print(table(Covid = accord$pol_covid, Mixology = accord$pol_mixo))

# Tweets où les deux lexiques divergent
divergent <- comparison_12 |>
  filter(pol_covid != "none", pol_mixo != "none",
         pol_covid != pol_mixo) |>
  mutate(text = tweet[doc_id])

cat(sprintf("\n%d tweets avec polarités divergentes (%.1f%%)\n",
    nrow(divergent), 100 * nrow(divergent) / nrow(accord)))


# ══════════════════════════════════════════════════════════════════════════════
# PARTIE 2 — Benchmark sur tous les lexiques
# ══════════════════════════════════════════════════════════════════════════════

cat("\n── Partie 2 : Benchmark 8 lexiques ──\n")

all_lexicons <- names(mixology_lexicon_names())

# Scorer les 6 lexiques restants (covid et mixology déjà faits)
other_lexicons <- setdiff(all_lexicons, c("covid", "mixology"))

scores_others <- lapply(other_lexicons, function(lex) {
  cat(sprintf("  Scoring : %s\n", lex))
  res <- score_chunks(tweet, lex, weighted = FALSE, handle_negation = TRUE)
  res$lexicon <- lex
  res
})
names(scores_others) <- other_lexicons

# Assembler tous les résultats en format long
all_scores_long <- bind_rows(
  scores_covid |> mutate(lexicon = "covid"),
  scores_mixo  |> mutate(lexicon = "mixology"),
  bind_rows(scores_others)
) |>
  mutate(lexicon_label = mixology_lexicon_names()[lexicon])

# ── 2a. Tableau benchmark agrégé ─────────────────────────────────────────────

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
    neg_bias      = sum(score_negative) / pmax(sum(score_positive), 1),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  arrange(desc(mean_coverage))

cat("\nTableau benchmark :\n")
print(benchmark, n = 8)


# ══════════════════════════════════════════════════════════════════════════════
# PARTIE 3 — Évaluation comparative (sans ground truth)
# Métriques internes : couverture, stabilité, cohérence, biais négatif
# ══════════════════════════════════════════════════════════════════════════════

cat("\n── Partie 3 : Évaluation comparative ──\n")

# ── 3a. Couverture token ──────────────────────────────────────────────────────
# Un bon lexique couvre un maximum de tokens pertinents du corpus.

cat("\nCouverture token (% tokens du corpus reconnus) :\n")
coverage_tbl <- lexicon_coverage(tweet)
print(coverage_tbl |> arrange(desc(coverage)))

# ── 3b. Taux de classification (tweets classifiés vs 'none') ─────────────────
# Un lexique qui laisse trop de tweets sans polarité est peu utile.

classif_rate <- benchmark |>
  select(lexicon_label, mean_coverage, pct_none) |>
  mutate(pct_classified = round(100 - pct_none, 1)) |>
  arrange(desc(pct_classified))

cat("\nTaux de classification :\n")
print(classif_rate)

# ── 3c. Biais négatif ─────────────────────────────────────────────────────────
# Ratio neg / pos : < 1.2 = équilibré, > 2 = fortement biaisé vers le négatif.
# Un lexique très biaisé surestime le sentiment négatif systématiquement.

cat("\nBiais négatif (neg_tokens / pos_tokens) :\n")
benchmark |>
  select(lexicon_label, neg_bias) |>
  mutate(evaluation = case_when(
    neg_bias < 1.2 ~ "équilibré",
    neg_bias < 1.6 ~ "légèrement biaisé",
    neg_bias < 2.0 ~ "biaisé",
    TRUE           ~ "très biaisé"
  )) |>
  arrange(neg_bias) |>
  print()

# ── 3d. Stabilité inter-lexiques (accord par paires sur échantillon) ──────────
# Calculé sur un échantillon de 10k tweets pour limiter le temps de calcul.

set.seed(SEED)
sample_idx <- sample(seq_len(N), min(10000, N))

pairwise_agreement <- function(lex_a, lex_b, idx, all_long) {
  a <- all_long |> filter(lexicon == lex_a, doc_id %in% idx,
                           polarity != "none") |>
    select(doc_id, pol_a = polarity)
  b <- all_long |> filter(lexicon == lex_b, doc_id %in% idx,
                           polarity != "none") |>
    select(doc_id, pol_b = polarity)
  joined <- inner_join(a, b, by = "doc_id")
  if (nrow(joined) == 0) return(NA_real_)
  mean(joined$pol_a == joined$pol_b)
}

pairs <- combn(all_lexicons, 2, simplify = FALSE)
cat("\nAccord par paires (échantillon 10k tweets) :\n")
agree_tbl <- bind_rows(lapply(pairs, function(p) {
  ag <- pairwise_agreement(p[1], p[2], sample_idx, all_scores_long)
  tibble(
    lex_a = mixology_lexicon_names()[p[1]],
    lex_b = mixology_lexicon_names()[p[2]],
    agreement = round(ag * 100, 1)
  )
})) |> arrange(desc(agreement))

print(agree_tbl, n = 28)

# ── 3e. Score de performance synthétique (sans ground truth) ──────────────────
# Combinaison de : couverture, taux de classification, biais équilibré
# Plus le score est élevé, plus le lexique est adapté à ce corpus.

perf_score <- benchmark |>
  select(lexicon, lexicon_label, mean_coverage, pct_none, neg_bias) |>
  mutate(
    score_coverage   = mean_coverage * 100,           # max 100
    score_classif    = 100 - pct_none,                # max 100
    score_balance    = 100 / (1 + abs(neg_bias - 1)), # max 100 si neg_bias = 1
    score_global     = round(
      0.5 * score_coverage + 0.3 * score_classif + 0.2 * score_balance, 1
    )
  ) |>
  select(lexicon_label, score_coverage, score_classif,
         score_balance, score_global) |>
  mutate(across(where(is.numeric), \(x) round(x, 1))) |>
  arrange(desc(score_global))

cat("\nScore de performance synthétique :\n")
cat("  Poids : couverture token 50% | taux classification 30% | équilibre 20%\n\n")
print(perf_score)


# ══════════════════════════════════════════════════════════════════════════════
# PARTIE 4 — Visualisations
# ══════════════════════════════════════════════════════════════════════════════

cat("\n── Partie 4 : Visualisations ──\n")

# ── 4a. Couverture token ──────────────────────────────────────────────────────

p1 <- benchmark |>
  mutate(lexicon_label = reorder(lexicon_label, mean_coverage),
         is_mixo = lexicon %in% c("covid", "mixology")) |>
  ggplot(aes(x = lexicon_label, y = mean_coverage * 100,
             fill = is_mixo)) +
  geom_col() +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "#1D9E75"),
                    labels = c("General lexicons", "Mixology lexicons"),
                    name   = NULL) +
  coord_flip() +
  labs(title    = "Token coverage by lexicon",
       subtitle = paste0("Politics corpus, n = ", format(N, big.mark = ","), " tweets"),
       x = NULL, y = "Coverage (%)") +
  theme_minimal() +
  theme(legend.position = "top")

print(p1)


# ── 4b. Distribution des polarités ───────────────────────────────────────────

p2 <- benchmark |>
  select(lexicon_label, pct_positive, pct_negative, pct_ambiguous) |>
  pivot_longer(cols = starts_with("pct_"),
               names_to = "polarity", values_to = "pct") |>
  mutate(
    polarity      = recode(polarity,
                           pct_positive  = "Positive",
                           pct_negative  = "Negative",
                           pct_ambiguous = "Ambiguous"),
    lexicon_label = reorder(lexicon_label, ifelse(polarity == "Negative", pct, 0))
  ) |>
  ggplot(aes(x = lexicon_label, y = pct, fill = polarity)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(
    "Positive"  = "#1D9E75",
    "Negative"  = "#D85A30",
    "Ambiguous" = "#888780"
  )) +
  coord_flip() +
  labs(title    = "Polarity distribution by lexicon",
       subtitle = "% of classified tweets (matched docs only)",
       x = NULL, y = "%", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

print(p2)


# ── 4c. Score de performance synthétique ─────────────────────────────────────

p3 <- perf_score |>
  mutate(lexicon_label = reorder(lexicon_label, score_global)) |>
  pivot_longer(cols = c(score_coverage, score_classif, score_balance),
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
  labs(title    = "Synthetic performance score by lexicon",
       subtitle = "Weighted: coverage 50% + classification 30% + balance 20%",
       x = NULL, y = "Score", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

print(p3)


# ── 4d. Accord inter-lexiques (heatmap) ───────────────────────────────────────

agree_matrix <- agree_tbl |>
  bind_rows(agree_tbl |> rename(lex_a = lex_b, lex_b = lex_a)) |>
  bind_rows(tibble(lex_a = unique(agree_tbl$lex_a),
                   lex_b = unique(agree_tbl$lex_a),
                   agreement = 100))

p4 <- agree_matrix |>
  ggplot(aes(x = lex_a, y = lex_b, fill = agreement)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(agreement, 0), "%")),
            size = 2.8, colour = "white") +
  scale_fill_gradient(low = "#D85A30", high = "#1D9E75",
                      name = "Agreement (%)") +
  labs(title    = "Pairwise agreement between lexicons",
       subtitle = "% tweets classified with the same dominant polarity (sample 10k)",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

print(p4)


# ══════════════════════════════════════════════════════════════════════════════
# PARTIE 5 — Export
# ══════════════════════════════════════════════════════════════════════════════

write.csv(benchmark,      "benchmark_all_lexicons.csv",     row.names = FALSE)
write.csv(perf_score,     "performance_scores.csv",         row.names = FALSE)
write.csv(agree_tbl,      "pairwise_agreement.csv",         row.names = FALSE)
write.csv(comparison_12,  "covid_vs_mixology_doclevel.csv", row.names = FALSE)
write.csv(divergent |> select(doc_id, text, pol_covid, pol_mixo, net_covid, net_mixo),
                          "divergent_tweets.csv",           row.names = FALSE)

# Scores complets doc-level (attention : fichier lourd sur 300k lignes)
# write.csv(all_scores_long, "all_scores_docLevel.csv", row.names = FALSE)

cat("\nDone. Fichiers exportés dans le répertoire de travail.\n")
cat("getwd() :", getwd(), "\n")
