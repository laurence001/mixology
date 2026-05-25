# data-raw/prepare_data.R
# Run this script once to convert CSV files into .rda datasets for the package.
# Execute with: source("data-raw/prepare_data.R")

library(readr)
library(usethis)

# ── Mixology Covid Lexicon ────────────────────────────────────────────────────
mixology_covid_lexicon <- readr::read_csv(
  "data-raw/mixology_covid_lexicon_v3.csv",
  col_types = readr::cols(
    word        = readr::col_character(),
    sentiment   = readr::col_character(),
    weight      = readr::col_double(),
    freq_corpus = readr::col_integer()
  )
)
usethis::use_data(mixology_covid_lexicon, overwrite = TRUE)

# ── Mixology Lexicon (merged) ─────────────────────────────────────────────────
mixology_lexicon <- readr::read_csv(
  "data-raw/mixology_lexicon_v3.csv",
  col_types = readr::cols(
    word        = readr::col_character(),
    sentiment   = readr::col_character(),
    weight      = readr::col_double(),
    freq_corpus = readr::col_integer()
  )
)
usethis::use_data(mixology_lexicon, overwrite = TRUE)

# ── Stop words ────────────────────────────────────────────────────────────────
stop_words_en <- readr::read_csv(
  "data-raw/stop_words_en_v3.csv",
  col_types = readr::cols(word = readr::col_character())
)
usethis::use_data(stop_words_en, overwrite = TRUE)

# ── Negation markers ─────────────────────────────────────────────────────────
negations_en <- readr::read_csv(
  "data-raw/negative_en.csv",
  col_types = readr::cols(word = readr::col_character())
)
usethis::use_data(negations_en, overwrite = TRUE)

message("All datasets saved to data/")
