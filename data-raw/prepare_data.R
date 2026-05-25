# data-raw/prepare_data.R
# Run this script ONCE from the package root to generate all .rda datasets.
#
# Usage (from RStudio with the package open, or from terminal):
#   source("data-raw/prepare_data.R")
#   # or:
#   Rscript data-raw/prepare_data.R
#
# Required packages: readr, usethis, dplyr

if (!requireNamespace("readr",   quietly = TRUE)) install.packages("readr")
if (!requireNamespace("usethis", quietly = TRUE)) install.packages("usethis")
if (!requireNamespace("dplyr",   quietly = TRUE)) install.packages("dplyr")

library(readr)
library(usethis)
library(dplyr)

message("Building mixology datasets...")

# ── Helper: read a simple word/sentiment CSV ──────────────────────────────────
read_lex <- function(path) {
  readr::read_csv(path, col_types = readr::cols(
    word      = readr::col_character(),
    sentiment = readr::col_character()
  ))
}

# ── 1. General Inquirer ───────────────────────────────────────────────────────
lexicon_inquirer <- read_lex("data-raw/inquirer.csv")
usethis::use_data(lexicon_inquirer, overwrite = TRUE)
message("  lexicon_inquirer: ", nrow(lexicon_inquirer), " terms")

# ── 2. MPQA Subjectivity Lexicon ──────────────────────────────────────────────
lexicon_subjectivity <- read_lex("data-raw/subjectivity.csv")
usethis::use_data(lexicon_subjectivity, overwrite = TRUE)
message("  lexicon_subjectivity: ", nrow(lexicon_subjectivity), " terms")

# ── 3. Bing Liu ───────────────────────────────────────────────────────────────
lexicon_bing <- read_lex("data-raw/bing.csv")
usethis::use_data(lexicon_bing, overwrite = TRUE)
message("  lexicon_bing: ", nrow(lexicon_bing), " terms")

# ── 4. NRC ────────────────────────────────────────────────────────────────────
lexicon_nrc <- read_lex("data-raw/nrc.csv")
usethis::use_data(lexicon_nrc, overwrite = TRUE)
message("  lexicon_nrc: ", nrow(lexicon_nrc), " terms")

# ── 5. AFINN ──────────────────────────────────────────────────────────────────
lexicon_afinn <- read_lex("data-raw/afinn.csv")
usethis::use_data(lexicon_afinn, overwrite = TRUE)
message("  lexicon_afinn: ", nrow(lexicon_afinn), " terms")

# ── 6. Loughran-McDonald ─────────────────────────────────────────────────────
lexicon_loughran <- read_lex("data-raw/loughran.csv")
usethis::use_data(lexicon_loughran, overwrite = TRUE)
message("  lexicon_loughran: ", nrow(lexicon_loughran), " terms")

# ── 7. Mixology Covid Lexicon ─────────────────────────────────────────────────
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
message("  mixology_covid_lexicon: ", nrow(mixology_covid_lexicon), " terms")

# ── 8. Mixology Lexicon (merged) ──────────────────────────────────────────────
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
message("  mixology_lexicon: ", nrow(mixology_lexicon), " terms")

# ── 9. Stop words ─────────────────────────────────────────────────────────────
stop_words_en <- readr::read_csv(
  "data-raw/stop_words_en_v3.csv",
  col_types = readr::cols(word = readr::col_character())
)
usethis::use_data(stop_words_en, overwrite = TRUE)
message("  stop_words_en: ", nrow(stop_words_en), " terms")

# ── 10. Negation markers ──────────────────────────────────────────────────────
negations_en <- readr::read_csv(
  "data-raw/negative_en.csv",
  col_types = readr::cols(word = readr::col_character())
)
usethis::use_data(negations_en, overwrite = TRUE)
message("  negations_en: ", nrow(negations_en), " terms")

message("\nAll datasets saved to data/  -- now run devtools::document() then devtools::install()")
