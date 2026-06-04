# data-raw/prepare_data.R
# Regenerates the .rds files in inst/data/ from the source CSVs in data-raw/.
# Run this if you update any CSV source file.
#
# Usage:
#   source("data-raw/prepare_data.R")
#   # or from terminal:
#   Rscript data-raw/prepare_data.R

if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
library(readr)

out_dir <- file.path("inst", "data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

save_rds <- function(df, name) {
  path <- file.path(out_dir, paste0(name, ".rds"))
  saveRDS(df, path)
  message("  ", name, ".rds  (", nrow(df), " rows)")
}

message("Building mixology datasets...")

save_rds(read_csv("data-raw/inquirer.csv",   col_types = "cc"), "lexicon_inquirer")
save_rds(read_csv("data-raw/subjectivity.csv",
                  locale = locale(encoding = "latin1"),
                  col_types = "cc"),                             "lexicon_subjectivity")
save_rds(read_csv("data-raw/bing.csv",       col_types = "cc"), "lexicon_bing")
save_rds(read_csv("data-raw/nrc.csv",        col_types = "cc"), "lexicon_nrc")
save_rds(read_csv("data-raw/afinn.csv",      col_types = "cc"), "lexicon_afinn")
save_rds(read_csv("data-raw/loughran.csv",   col_types = "cc"), "lexicon_loughran")

save_rds(read_csv("data-raw/mixology_covid_lexicon_v3.csv",
                  col_types = "ccdi"),                           "mixology_covid_lexicon")
save_rds(read_csv("data-raw/mixology_lexicon_v3.csv",
                  col_types = "ccdi"),                           "mixology_lexicon")

save_rds(read_csv("data-raw/stop_words_en_v3.csv", col_types = "c"), "stop_words_en")
save_rds(read_csv("data-raw/negative_en.csv",      col_types = "c"), "negations_en")

message("Done. Files written to inst/data/")
message("Now run: devtools::install() to reinstall the package.")

save_rds(read_csv("data-raw/mixology_lexicon_ft.csv",
                  col_types = "ccdi"),                            "mixology_lexicon_ft")
save_rds(read_csv("data-raw/covid_lexicon_ft.csv",
                  col_types = "ccdi"),                            "mixology_covid_lexicon_ft")

message("Fine-tuned lexicons added.")
