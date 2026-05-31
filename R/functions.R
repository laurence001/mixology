# ── Internal constants ─────────────────────────────────────────────────────────

.LEXICON_NAMES <- c(
  "inquirer", "subjectivity", "bing", "nrc", "afinn",
  "loughran", "covid", "mixology"
)

.LEXICON_LABELS <- c(
  inquirer     = "General Inquirer",
  subjectivity = "MPQA Subjectivity",
  bing         = "Bing Liu",
  nrc          = "NRC",
  afinn        = "AFINN",
  loughran     = "Loughran-McDonald",
  covid        = "Mixology Covid",
  mixology     = "Mixology"
)

.LEXICON_FILES <- c(
  inquirer     = "lexicon_inquirer.rds",
  subjectivity = "lexicon_subjectivity.rds",
  bing         = "lexicon_bing.rds",
  nrc          = "lexicon_nrc.rds",
  afinn        = "lexicon_afinn.rds",
  loughran     = "lexicon_loughran.rds",
  covid        = "mixology_covid_lexicon.rds",
  mixology     = "mixology_lexicon.rds"
)

.EXTRA_FILES <- c(
  stop_words_en = "stop_words_en.rds",
  negations_en  = "negations_en.rds"
)

# Internal cache so each file is read only once per session
.cache <- new.env(parent = emptyenv())

.load_rds <- function(filename) {
  if (!exists(filename, envir = .cache)) {
    path <- system.file("data", filename, package = "mixology")
    if (!nzchar(path)) {
      stop("Cannot find '", filename, "' in the mixology package. ",
           "Please run source('data-raw/prepare_data.R') from the package root ",
           "to generate the data files, then reinstall.", call. = FALSE)
    }
    assign(filename, readRDS(path), envir = .cache)
  }
  tibble::as_tibble(get(filename, envir = .cache))
}


# ── get_lexicon() ──────────────────────────────────────────────────────────────

#' Retrieve a sentiment lexicon by name
#'
#' Convenience accessor for all eight sentiment lexicons bundled in the
#' Mixology package. Returns a tibble with polarity assignments for each term.
#' The two Mixology lexicons additionally include corpus frequency weights.
#'
#' @param lexicon Character. One of the eight lexicon keys returned by
#'   \code{\link{mixology_lexicon_names}}: \code{"inquirer"},
#'   \code{"subjectivity"}, \code{"bing"}, \code{"nrc"}, \code{"afinn"},
#'   \code{"loughran"}, \code{"covid"} (Mixology Covid Lexicon), or
#'   \code{"mixology"} (merged Mixology Lexicon).
#'
#' @return A tibble. All lexicons include \code{word} (character) and
#'   \code{sentiment} (one of \code{"positive"}, \code{"negative"}, or
#'   \code{"ambiguous"}). The \code{"covid"} and \code{"mixology"} lexicons
#'   additionally include:
#'   \describe{
#'     \item{weight}{Numeric. Log-normalised corpus frequency weight in the
#'       range 0.5--3.0. Used in weighted scoring via
#'       \code{\link{mixology_sentiment}}.}
#'     \item{freq_corpus}{Integer. Raw token frequency in the reference
#'       corpus.}
#'   }
#'
#' @examples
#' # Retrieve the Mixology Covid lexicon
#' get_lexicon("covid")
#'
#' # Retrieve the Bing Liu lexicon
#' get_lexicon("bing")
#'
#' # Inspect positive terms sorted by corpus frequency
#' library(dplyr)
#' get_lexicon("covid") |>
#'   filter(sentiment == "positive") |>
#'   arrange(desc(freq_corpus)) |>
#'   head(20)
#'
#' @seealso \code{\link{mixology_lexicon_names}}, \code{\link{mixology_sentiment}},
#'   \code{\link{lexicon_conflicts}}
#' @export
get_lexicon <- function(lexicon) {
  lexicon <- match.arg(lexicon, .LEXICON_NAMES)
  .load_rds(.LEXICON_FILES[lexicon])
}


# ── mixology_lexicon_names() ───────────────────────────────────────────────────

#' List available lexicon identifiers
#'
#' Returns a named character vector of the eight sentiment lexicons bundled in
#' the Mixology package. The names are the short key strings used as the
#' \code{lexicon} argument in other package functions; the values are the
#' corresponding human-readable labels.
#'
#' @return A named character vector with eight elements.
#'
#' @examples
#' mixology_lexicon_names()
#'
#' # Extract the keys for use in other functions
#' names(mixology_lexicon_names())
#'
#' @seealso \code{\link{get_lexicon}}, \code{\link{mixology_sentiment}},
#'   \code{\link{compare_lexicons}}
#' @export
mixology_lexicon_names <- function() {
  .LEXICON_LABELS
}

# Internal helpers for stop words and negations
.stop_words_en <- function() .load_rds("stop_words_en.rds")
.negations_en  <- function() .load_rds("negations_en.rds")


# ── mixology_tokenize() ────────────────────────────────────────────────────────

#' Tokenise text for sentiment analysis
#'
#' Preprocesses and tokenises a character vector of texts into a long-format
#' tibble of unigram tokens. The pipeline lowercases text, removes punctuation
#' while preserving apostrophes for contractions, splits on whitespace, and
#' optionally removes stop words. The output is compatible with
#' \code{\link{mixology_negation}} and serves as the input to
#' \code{\link{mixology_sentiment}}.
#'
#' @param text Character vector. One element per document or tweet.
#' @param remove_stopwords Logical. If \code{TRUE} (default), tokens found in
#'   the bundled domain-specific stop word list are removed before scoring.
#' @param custom_stopwords Character vector of additional stop words to remove,
#'   appended to the default list. Default \code{NULL}.
#' @param min_chars Integer. Minimum token length in characters. Tokens shorter
#'   than this value are excluded. Default \code{2L}.
#'
#' @return A tibble with two columns:
#'   \describe{
#'     \item{doc_id}{Integer. Document identifier corresponding to the position
#'       of the source text in the input vector.}
#'     \item{token}{Character. The preprocessed token string.}
#'   }
#'
#' @examples
#' tweets <- c(
#'   "Lockdown is not justified",
#'   "The vaccine rollout has been excellent!"
#' )
#'
#' # Default: stopwords removed
#' mixology_tokenize(tweets)
#'
#' # Retain all tokens including stopwords
#' mixology_tokenize(tweets, remove_stopwords = FALSE)
#'
#' # Apply additional custom stopwords
#' mixology_tokenize(tweets, custom_stopwords = c("vaccine", "lockdown"))
#'
#' @importFrom stringr str_to_lower str_replace_all str_split
#' @importFrom tibble tibble
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @seealso \code{\link{mixology_negation}}, \code{\link{mixology_sentiment}}
#' @export
mixology_tokenize <- function(text,
                               remove_stopwords = TRUE,
                               custom_stopwords = NULL,
                               min_chars = 2L) {
  stopifnot(is.character(text))
  cleaned <- stringr::str_to_lower(text)
  cleaned <- stringr::str_replace_all(cleaned, "[^a-z']", " ")
  cleaned <- stringr::str_replace_all(cleaned, "\\s+", " ")
  cleaned <- trimws(cleaned)

  tokens_list <- stringr::str_split(cleaned, " ")
  result <- tibble::tibble(
    doc_id = rep(seq_along(tokens_list), lengths(tokens_list)),
    token  = unlist(tokens_list, use.names = FALSE)
  )
  result <- dplyr::filter(result, nchar(.data$token) >= min_chars)

  if (remove_stopwords) {
    sw <- .stop_words_en()$word
    if (!is.null(custom_stopwords))
      sw <- unique(c(sw, stringr::str_to_lower(custom_stopwords)))
    result <- dplyr::filter(result, !(.data$token %in% sw))
  }
  result
}


# ── mixology_negation() ────────────────────────────────────────────────────────

#' Apply negation handling to a tokenised table
#'
#' Scans each document in a tokenised tibble for negation markers and flags
#' the next \code{window} tokens for polarity reversal. The output adds a
#' logical \code{negated} column which is used by \code{\link{mixology_sentiment}}
#' to invert matched token polarities during scoring.
#'
#' Negation is applied at the token level: flagged tokens that match a lexicon
#' entry have their polarity reversed from positive to negative or vice versa.
#' Ambiguous tokens are not affected by negation.
#'
#' @param tokens_tbl A tibble as returned by \code{\link{mixology_tokenize}},
#'   with columns \code{doc_id} (integer) and \code{token} (character).
#' @param window Integer. Number of tokens to flag after each negation marker.
#'   Default \code{3L}.
#' @param custom_negations Character vector of additional negation markers to
#'   use alongside the default list of 22 bundled markers (e.g. \code{"not"},
#'   \code{"never"}, \code{"can't"}). Default \code{NULL}.
#'
#' @return The input tibble with an added logical column \code{negated}.
#'   Tokens within the negation window are \code{TRUE}; all others are
#'   \code{FALSE}.
#'
#' @examples
#' tweets <- c(
#'   "The vaccine is not effective.",
#'   "Lockdown measures are never justified."
#' )
#'
#' # Tokenise first, then apply negation handling
#' toks <- mixology_tokenize(tweets)
#' mixology_negation(toks)
#'
#' # Wider negation window
#' mixology_negation(toks, window = 5L)
#'
#' # Add custom negation markers
#' mixology_negation(toks, custom_negations = c("hardly", "barely", "scarcely"))
#'
#' @seealso \code{\link{mixology_tokenize}}, \code{\link{mixology_sentiment}}
#' @export
mixology_negation <- function(tokens_tbl, window = 3L,
                               custom_negations = NULL) {
  neg_markers <- .negations_en()$word
  if (!is.null(custom_negations))
    neg_markers <- unique(c(neg_markers,
                            stringr::str_to_lower(custom_negations)))

  docs <- split(tokens_tbl, tokens_tbl$doc_id)
  result <- lapply(docs, function(d) {
    toks    <- d$token
    n       <- length(toks)
    negated <- logical(n)
    for (i in seq_along(toks)) {
      if (toks[i] %in% neg_markers && i < n) {
        idx <- seq(i + 1L, min(i + window, n))
        negated[idx] <- TRUE
      }
    }
    d$negated <- negated
    d
  })
  do.call(rbind, result)
}


# ── Internal scorer ────────────────────────────────────────────────────────────

.score_one_lexicon <- function(tokens_tbl, lex, weighted) {
  has_weight <- "weight" %in% names(lex)
  matched <- dplyr::left_join(tokens_tbl, lex, by = c("token" = "word"))
  matched <- dplyr::filter(matched, !is.na(.data$sentiment))
  matched <- dplyr::mutate(matched,
    sentiment_eff = dplyr::if_else(
      .data$negated & .data$sentiment != "ambiguous",
      dplyr::if_else(.data$sentiment == "positive", "negative", "positive"),
      .data$sentiment
    ),
    score_val = if (weighted && has_weight) .data$weight else 1
  )
  matched |>
    dplyr::group_by(.data$doc_id) |>
    dplyr::summarise(
      n_matched       = dplyr::n(),
      score_positive  = sum(.data$score_val[.data$sentiment_eff == "positive"],  na.rm = TRUE),
      score_negative  = sum(.data$score_val[.data$sentiment_eff == "negative"],  na.rm = TRUE),
      score_ambiguous = sum(.data$score_val[.data$sentiment_eff == "ambiguous"], na.rm = TRUE),
      .groups = "drop"
    )
}


# ── mixology_sentiment() ───────────────────────────────────────────────────────

#' Compute sentiment scores for a text corpus
#'
#' Implements the full Mixology sentiment analysis pipeline: tokenise text,
#' optionally apply negation detection, match tokens against a specified
#' lexicon, and aggregate positive, negative, and ambiguous scores at the
#' document level.
#'
#' When \code{weighted = TRUE} and the selected lexicon is \code{"covid"} or
#' \code{"mixology"}, each matched token's contribution is scaled by its
#' log-normalised corpus frequency weight (range 0.5--3.0). For general-purpose
#' lexicons, which do not include frequency weights, this argument has no
#' effect and scoring is always unweighted.
#'
#' When \code{handle_negation = TRUE}, \code{\link{mixology_negation}} is
#' called internally before scoring. Tokens within the negation window have
#' their polarity reversed; ambiguous tokens are not affected.
#'
#' Documents with no matched tokens receive \code{polarity = "none"} and
#' zero scores across all sentiment columns.
#'
#' @param text Character vector. One element per document or tweet.
#' @param lexicon Character. Lexicon key. One of \code{"inquirer"},
#'   \code{"subjectivity"}, \code{"bing"}, \code{"nrc"}, \code{"afinn"},
#'   \code{"loughran"}, \code{"covid"} (default), or \code{"mixology"}.
#'   Use \code{\link{mixology_lexicon_names}} to list available keys.
#' @param weighted Logical. If \code{TRUE} (default), corpus frequency weights
#'   are applied for the \code{"covid"} and \code{"mixology"} lexicons.
#'   Has no effect for general-purpose lexicons, which lack frequency weights.
#' @param handle_negation Logical. If \code{TRUE}, polarity reversal is applied
#'   within a sliding window following negation markers. Default \code{FALSE}.
#' @param negation_window Integer. Width of the negation sliding window in
#'   tokens. Default \code{3L}. Only used when \code{handle_negation = TRUE}.
#' @param remove_stopwords Logical. If \code{TRUE} (default), the bundled
#'   domain-specific stop word list is applied before tokenisation.
#' @param custom_stopwords Character vector of additional stop words.
#'   Default \code{NULL}.
#' @param custom_negations Character vector of additional negation markers.
#'   Default \code{NULL}.
#'
#' @return A tibble with one row per input document and nine columns:
#'   \describe{
#'     \item{doc_id}{Integer. Document position in the input vector.}
#'     \item{n_tokens}{Integer. Number of tokens after preprocessing.}
#'     \item{n_matched}{Numeric. Number of tokens matched to the lexicon.}
#'     \item{coverage}{Numeric. Proportion of tokens matched (0--1).}
#'     \item{score_positive}{Numeric. Aggregated positive sentiment score.}
#'     \item{score_negative}{Numeric. Aggregated negative sentiment score.}
#'     \item{score_ambiguous}{Numeric. Aggregated ambiguous sentiment score.}
#'     \item{score_net}{Numeric. Net sentiment score
#'       (\code{score_positive - score_negative}).}
#'     \item{polarity}{Character. Dominant polarity classification:
#'       \code{"positive"}, \code{"negative"}, \code{"ambiguous"}, or
#'       \code{"none"} (no tokens matched).}
#'   }
#'
#' @examples
#' tweets <- c(
#'   "The lockdown is terrible and unjustified",
#'   "The vaccine rollout has been excellent",
#'   "I am not sure about the booster"
#' )
#'
#' # Default: Mixology Covid lexicon with frequency weighting
#' mixology_sentiment(tweets)
#'
#' # General-purpose lexicon, no weighting
#' mixology_sentiment(tweets, lexicon = "bing", weighted = FALSE)
#'
#' # Enable negation handling
#' mixology_sentiment(tweets, handle_negation = TRUE)
#'
#' # Score all tweets and inspect unclassified documents
#' library(dplyr)
#' mixology_sentiment(tweets, lexicon = "inquirer") |>
#'   filter(polarity == "none")
#'
#' @importFrom dplyr left_join mutate group_by summarise filter if_else case_when
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @seealso \code{\link{compare_lexicons}}, \code{\link{mixology_lexicon_names}},
#'   \code{\link{get_lexicon}}, \code{\link{mixology_negation}}
#' @export
mixology_sentiment <- function(text,
                                lexicon          = "covid",
                                weighted         = TRUE,
                                handle_negation  = FALSE,
                                negation_window  = 3L,
                                remove_stopwords = TRUE,
                                custom_stopwords = NULL,
                                custom_negations = NULL) {
  lexicon <- match.arg(lexicon, .LEXICON_NAMES)
  lex     <- get_lexicon(lexicon)

  toks <- mixology_tokenize(text,
                             remove_stopwords = remove_stopwords,
                             custom_stopwords = custom_stopwords)
  tok_counts <- toks |>
    dplyr::group_by(.data$doc_id) |>
    dplyr::summarise(n_tokens = dplyr::n(), .groups = "drop")

  if (handle_negation) {
    toks <- mixology_negation(toks, window = negation_window,
                               custom_negations = custom_negations)
  } else {
    toks$negated <- FALSE
  }

  scores   <- .score_one_lexicon(toks, lex, weighted)
  all_docs <- tibble::tibble(doc_id = seq_along(text))
  result   <- dplyr::left_join(all_docs, tok_counts, by = "doc_id")
  result   <- dplyr::left_join(result,   scores,     by = "doc_id")

  for (col in c("n_matched", "score_positive", "score_negative", "score_ambiguous"))
    result[[col]][is.na(result[[col]])] <- 0
  result$n_tokens[is.na(result$n_tokens)] <- 0L

  result <- dplyr::mutate(result,
    coverage  = dplyr::if_else(.data$n_tokens > 0,
                                .data$n_matched / .data$n_tokens, 0),
    score_net = .data$score_positive - .data$score_negative,
    polarity  = dplyr::case_when(
      .data$n_matched == 0                                       ~ "none",
      .data$score_positive > .data$score_negative &
        .data$score_positive >= .data$score_ambiguous            ~ "positive",
      .data$score_negative > .data$score_positive &
        .data$score_negative >= .data$score_ambiguous            ~ "negative",
      .data$score_ambiguous > 0                                  ~ "ambiguous",
      TRUE                                                       ~ "none"
    )
  )

  result[, c("doc_id", "n_tokens", "n_matched", "coverage",
             "score_positive", "score_negative", "score_ambiguous",
             "score_net", "polarity")]
}


# ── compare_lexicons() ─────────────────────────────────────────────────────────

#' Compare sentiment results across multiple lexicons
#'
#' Runs \code{\link{mixology_sentiment}} for each requested lexicon and returns
#' a corpus-level summary table or a long-format document-level tibble, making
#' cross-lexicon benchmarking straightforward. This is the primary function for
#' replicating the benchmarking results reported in Dierickx (2026).
#'
#' By default, \code{weighted = FALSE} so that all eight lexicons are compared
#' on equal terms. The two Mixology lexicons support frequency weighting; the
#' six general-purpose lexicons do not.
#'
#' @param text Character vector. One element per document or tweet.
#' @param lexicons Character vector. Subset of the eight available lexicon keys.
#'   Default: all eight, as returned by \code{\link{mixology_lexicon_names}}.
#' @param weighted Logical. Default \code{FALSE} for fair cross-lexicon
#'   comparison. Set to \code{TRUE} to apply frequency weights (effective only
#'   for \code{"covid"} and \code{"mixology"}).
#' @param handle_negation Logical. Applied uniformly across all lexicons.
#'   Default \code{FALSE}.
#' @param negation_window Integer. Negation sliding window width. Default
#'   \code{3L}.
#' @param remove_stopwords Logical. Default \code{TRUE}.
#' @param custom_stopwords Character vector. Default \code{NULL}.
#' @param summary Logical. If \code{TRUE} (default), returns one row per
#'   lexicon with corpus-level aggregates. If \code{FALSE}, returns one row
#'   per document per lexicon in long format.
#'
#' @return
#' When \code{summary = TRUE}: a tibble with one row per lexicon and ten
#' columns:
#'   \describe{
#'     \item{lexicon}{Character. Lexicon key string.}
#'     \item{lexicon_label}{Character. Human-readable lexicon name.}
#'     \item{n_terms}{Integer. Number of terms in the lexicon.}
#'     \item{n_docs}{Integer. Total number of input documents.}
#'     \item{n_matched_docs}{Integer. Documents with at least one matched
#'       token.}
#'     \item{mean_coverage}{Numeric. Mean token coverage across all
#'       documents.}
#'     \item{pct_positive}{Numeric. Percentage of classified documents
#'       assigned positive polarity.}
#'     \item{pct_negative}{Numeric. Percentage of classified documents
#'       assigned negative polarity.}
#'     \item{pct_ambiguous}{Numeric. Percentage of classified documents
#'       assigned ambiguous polarity.}
#'     \item{mean_score_net}{Numeric. Mean net sentiment score across all
#'       documents.}
#'   }
#'
#' When \code{summary = FALSE}: all columns from
#' \code{\link{mixology_sentiment}} plus \code{lexicon} and
#' \code{lexicon_label}.
#'
#' @examples
#' tweets <- c(
#'   "The lockdown is terrible",
#'   "The vaccine rollout has been excellent",
#'   "I am not sure about the restrictions"
#' )
#'
#' # Full benchmark across all eight lexicons
#' compare_lexicons(tweets)
#'
#' # Subset of lexicons
#' compare_lexicons(tweets, lexicons = c("bing", "nrc", "covid"))
#'
#' # Per-document long format
#' compare_lexicons(tweets, lexicons = c("covid", "bing"), summary = FALSE)
#'
#' @importFrom dplyr bind_rows mutate group_by summarise select
#' @importFrom rlang .data
#' @seealso \code{\link{mixology_sentiment}}, \code{\link{lexicon_coverage}},
#'   \code{\link{mixology_lexicon_names}}
#' @export
compare_lexicons <- function(text,
                              lexicons         = .LEXICON_NAMES,
                              weighted         = FALSE,
                              handle_negation  = FALSE,
                              negation_window  = 3L,
                              remove_stopwords = TRUE,
                              custom_stopwords = NULL,
                              summary          = TRUE) {
  lexicons <- match.arg(lexicons, .LEXICON_NAMES, several.ok = TRUE)

  results <- lapply(lexicons, function(lex_name) {
    res <- mixology_sentiment(text,
      lexicon          = lex_name,
      weighted         = weighted,
      handle_negation  = handle_negation,
      negation_window  = negation_window,
      remove_stopwords = remove_stopwords,
      custom_stopwords = custom_stopwords)
    res$lexicon       <- lex_name
    res$lexicon_label <- .LEXICON_LABELS[lex_name]
    res
  })

  long <- dplyr::bind_rows(results)
  if (!summary) return(long)

  lex_sizes <- vapply(lexicons, function(l) nrow(get_lexicon(l)), integer(1))

  long |>
    dplyr::group_by(.data$lexicon, .data$lexicon_label) |>
    dplyr::summarise(
      n_docs         = dplyr::n(),
      n_matched_docs = sum(.data$n_matched > 0),
      mean_coverage  = round(mean(.data$coverage, na.rm = TRUE), 3),
      pct_positive   = round(100 * sum(.data$polarity == "positive" &
                               .data$n_matched > 0) / sum(.data$n_matched > 0), 1),
      pct_negative   = round(100 * sum(.data$polarity == "negative" &
                               .data$n_matched > 0) / sum(.data$n_matched > 0), 1),
      pct_ambiguous  = round(100 * sum(.data$polarity == "ambiguous" &
                               .data$n_matched > 0) / sum(.data$n_matched > 0), 1),
      mean_score_net = round(mean(.data$score_net, na.rm = TRUE), 3),
      .groups = "drop"
    ) |>
    dplyr::mutate(n_terms = lex_sizes[.data$lexicon]) |>
    dplyr::select(.data$lexicon, .data$lexicon_label, .data$n_terms,
                  .data$n_docs, .data$n_matched_docs, .data$mean_coverage,
                  .data$pct_positive, .data$pct_negative, .data$pct_ambiguous,
                  .data$mean_score_net)
}


# ── lexicon_coverage() ─────────────────────────────────────────────────────────

#' Compute token coverage for one or more lexicons
#'
#' A lightweight diagnostic that returns the proportion of corpus tokens
#' matched by each specified lexicon, without full sentiment scoring. Useful
#' for quickly assessing lexical observability before running a full analysis.
#'
#' @param text Character vector. One element per document or tweet.
#' @param lexicons Character vector of lexicon keys. Accepts one or more keys
#'   from \code{\link{mixology_lexicon_names}}. Default: all eight lexicons.
#' @param remove_stopwords Logical. If \code{TRUE} (default), stop words are
#'   removed before computing coverage, matching the behaviour of
#'   \code{\link{mixology_sentiment}}.
#' @param custom_stopwords Character vector of additional stop words.
#'   Default \code{NULL}.
#'
#' @return A tibble with one row per lexicon and six columns:
#'   \describe{
#'     \item{lexicon}{Character. Lexicon key string.}
#'     \item{lexicon_label}{Character. Human-readable lexicon name.}
#'     \item{n_terms}{Integer. Number of terms in the lexicon.}
#'     \item{n_tokens}{Integer. Total number of tokens across all documents
#'       after preprocessing.}
#'     \item{n_matched}{Integer. Number of tokens found in the lexicon.}
#'     \item{coverage}{Numeric. Proportion of tokens matched (0--1).}
#'   }
#'
#' @examples
#' tweets <- c(
#'   "The lockdown is terrible and unjustified",
#'   "The vaccine rollout has been excellent"
#' )
#'
#' # Coverage for a single lexicon
#' lexicon_coverage(tweets, lexicons = "covid")
#'
#' # Coverage across multiple lexicons
#' lexicon_coverage(tweets, lexicons = c("covid", "mixology", "bing", "nrc"))
#'
#' # Coverage across all eight lexicons
#' lexicon_coverage(tweets)
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @seealso \code{\link{compare_lexicons}}, \code{\link{mixology_lexicon_names}}
#' @export
lexicon_coverage <- function(text,
                              lexicons         = .LEXICON_NAMES,
                              remove_stopwords = TRUE,
                              custom_stopwords = NULL) {
  lexicons <- match.arg(lexicons, .LEXICON_NAMES, several.ok = TRUE)
  toks     <- mixology_tokenize(text, remove_stopwords = remove_stopwords,
                                 custom_stopwords = custom_stopwords)
  n_tokens <- nrow(toks)

  rows <- lapply(lexicons, function(lex_name) {
    lex       <- get_lexicon(lex_name)
    n_matched <- sum(toks$token %in% lex$word)
    tibble::tibble(
      lexicon       = lex_name,
      lexicon_label = .LEXICON_LABELS[lex_name],
      n_terms       = nrow(lex),
      n_tokens      = n_tokens,
      n_matched     = n_matched,
      coverage      = round(n_matched / n_tokens, 3)
    )
  })
  dplyr::bind_rows(rows)
}


# ── lexicon_conflicts() ────────────────────────────────────────────────────────

#' Identify terms with conflicting polarities across lexicons
#'
#' Returns a table of terms that are assigned different sentiment polarities
#' across two or more of the specified lexicons. This function was used during
#' the construction of the Mixology lexicons to identify conflicting entries:
#' terms with conflicts between positive and negative assignments were
#' recoded to ambiguous following a conservative resolution rule.
#'
#' @param lexicons Character vector of lexicon keys to compare. Default: all
#'   eight lexicons returned by \code{\link{mixology_lexicon_names}}.
#' @param min_conflict Integer. Minimum number of distinct polarity values a
#'   term must have across the specified lexicons to be included in the output.
#'   Default \code{2L} (any conflict). Set to \code{3L} to return only terms
#'   with three or more distinct assignments.
#'
#' @return A tibble with one row per conflicting term. Columns include:
#'   \describe{
#'     \item{word}{Character. The conflicting term.}
#'     \item{(lexicon keys)}{One column per specified lexicon, showing the
#'       polarity assigned by that resource (\code{NA} if the term is absent).}
#'     \item{n_distinct}{Integer. The number of distinct polarity values
#'       assigned to this term across the specified lexicons.}
#'   }
#'   Results are sorted by descending \code{n_distinct}, then alphabetically
#'   by \code{word}.
#'
#' @examples
#' # All conflicts across all eight lexicons
#' lexicon_conflicts()
#'
#' # Conflicts between a subset of lexicons
#' lexicon_conflicts(lexicons = c("covid", "bing", "nrc"))
#'
#' # Only terms with three or more distinct polarity assignments
#' lexicon_conflicts(min_conflict = 3L)
#'
#' # Inspect conflicts for a specific term
#' library(dplyr)
#' lexicon_conflicts() |>
#'   filter(word == "positive")
#'
#' @importFrom dplyr full_join filter
#' @importFrom tibble as_tibble
#' @seealso \code{\link{get_lexicon}}, \code{\link{mixology_lexicon_names}}
#' @export
lexicon_conflicts <- function(lexicons     = .LEXICON_NAMES,
                               min_conflict = 2L) {
  lexicons <- match.arg(lexicons, .LEXICON_NAMES, several.ok = TRUE)

  frames <- lapply(lexicons, function(lex_name) {
    lex <- get_lexicon(lex_name)[, c("word", "sentiment")]
    names(lex)[2] <- lex_name
    lex
  })

  wide <- Reduce(function(a, b) dplyr::full_join(a, b, by = "word"), frames)

  wide$n_distinct <- apply(wide[, lexicons, drop = FALSE], 1, function(row) {
    length(unique(na.omit(row)))
  })

  wide <- dplyr::filter(wide, .data$n_distinct >= min_conflict)
  wide <- wide[order(-wide$n_distinct, wide$word), ]
  tibble::as_tibble(wide)
}
