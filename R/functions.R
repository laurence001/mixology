# ── Internal constants ─────────────────────────────────────────────────────────

.LEXICON_NAMES <- c(
  "inquirer", "subjectivity", "bing", "nrc", "afinn",
  "loughran", "covid", "mixology"
)

.LEXICON_LABELS <- c(
  inquirer    = "General Inquirer",
  subjectivity = "MPQA Subjectivity",
  bing        = "Bing Liu",
  nrc         = "NRC",
  afinn       = "AFINN",
  loughran    = "Loughran-McDonald",
  covid       = "Mixology Covid",
  mixology    = "Mixology"
)


# ── get_lexicon() ──────────────────────────────────────────────────────────────

#' Retrieve a sentiment lexicon by name
#'
#' Convenience accessor for all lexicons bundled in the package. Returns a
#' tibble with at least two columns: \code{word} and \code{sentiment}.
#' The Mixology lexicons also include \code{weight} and \code{freq_corpus}.
#'
#' @param lexicon Character. One of:
#'   \code{"inquirer"}, \code{"subjectivity"}, \code{"bing"},
#'   \code{"nrc"}, \code{"afinn"}, \code{"loughran"},
#'   \code{"covid"} (Mixology Covid Lexicon),
#'   \code{"mixology"} (merged Mixology Lexicon).
#'
#' @return A tibble.
#'
#' @examples
#' get_lexicon("bing")
#' get_lexicon("covid")
#'
#' # List available lexicons
#' mixology_lexicon_names()
#'
#' @export
get_lexicon <- function(lexicon) {
  lexicon <- match.arg(lexicon, .LEXICON_NAMES)
  switch(lexicon,
    inquirer     = mixology::lexicon_inquirer,
    subjectivity = mixology::lexicon_subjectivity,
    bing         = mixology::lexicon_bing,
    nrc          = mixology::lexicon_nrc,
    afinn        = mixology::lexicon_afinn,
    loughran     = mixology::lexicon_loughran,
    covid        = mixology::mixology_covid_lexicon,
    mixology     = mixology::mixology_lexicon
  )
}


#' List available lexicon names
#'
#' @return A named character vector mapping short names to full labels.
#' @examples
#' mixology_lexicon_names()
#' @export
mixology_lexicon_names <- function() {
  .LEXICON_LABELS
}


# ── mixology_tokenize() ────────────────────────────────────────────────────────

#' Tokenise text for sentiment analysis
#'
#' Lowercases text, removes punctuation (preserving apostrophes for
#' contractions), splits on whitespace, and optionally removes stop words.
#'
#' @param text Character vector. One element per document or tweet.
#' @param remove_stopwords Logical. Remove tokens found in
#'   \code{\link{stop_words_en}}. Default \code{TRUE}.
#' @param custom_stopwords Character vector of additional stop words.
#'   Default \code{NULL}.
#' @param min_chars Integer. Minimum token length to retain. Default \code{2}.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{doc_id}{Integer. Index of the source element in \code{text}.}
#'   \item{token}{Character. The cleaned token.}
#' }
#'
#' @examples
#' tweets <- c("Lockdown is not justified", "The vaccine works!")
#' mixology_tokenize(tweets)
#'
#' @importFrom stringr str_to_lower str_replace_all str_split
#' @importFrom tibble tibble
#' @importFrom dplyr filter
#' @importFrom rlang .data
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
    sw <- mixology::stop_words_en$word
    if (!is.null(custom_stopwords)) {
      sw <- unique(c(sw, stringr::str_to_lower(custom_stopwords)))
    }
    result <- dplyr::filter(result, !(.data$token %in% sw))
  }

  result
}


# ── mixology_negation() ────────────────────────────────────────────────────────

#' Mark tokens within a negation window
#'
#' Scans each document for negation markers and flags tokens within a
#' sliding window following each marker. Flagged tokens have their polarity
#' reversed during scoring in \code{\link{mixology_sentiment}}.
#'
#' @param tokens_tbl A tibble as returned by \code{\link{mixology_tokenize}},
#'   with columns \code{doc_id} and \code{token}.
#' @param window Integer. Number of tokens after a negation marker to mark
#'   as negated. Default \code{3}.
#' @param custom_negations Character vector of additional negation markers.
#'   Default \code{NULL}.
#'
#' @return The input tibble with an added logical column \code{negated}.
#'
#' @examples
#' tweets <- c("The vaccine is not effective", "I never felt better")
#' toks <- mixology_tokenize(tweets)
#' mixology_negation(toks)
#'
#' @export
mixology_negation <- function(tokens_tbl, window = 3L,
                               custom_negations = NULL) {
  neg_markers <- mixology::negations_en$word
  if (!is.null(custom_negations)) {
    neg_markers <- unique(c(neg_markers,
                            stringr::str_to_lower(custom_negations)))
  }

  docs <- split(tokens_tbl, tokens_tbl$doc_id)
  result <- lapply(docs, function(d) {
    toks    <- d$token
    negated <- logical(length(toks))
    for (i in seq_along(toks)) {
      if (toks[i] %in% neg_markers) {
        idx <- seq(i + 1L, min(i + window, length(toks)))
        negated[idx] <- TRUE
      }
    }
    d$negated <- negated
    d
  })

  do.call(rbind, result)
}


# ── .score_one_lexicon() — internal ───────────────────────────────────────────

.score_one_lexicon <- function(tokens_tbl, lex, weighted) {
  # tokens_tbl must already have $negated column
  has_weight <- "weight" %in% names(lex)

  matched <- dplyr::left_join(tokens_tbl, lex,
                               by = c("token" = "word"))
  matched <- dplyr::filter(matched, !is.na(.data$sentiment))

  # Flip polarity for negated tokens
  matched <- dplyr::mutate(matched,
    sentiment_eff = dplyr::if_else(
      .data$negated & .data$sentiment != "ambiguous",
      dplyr::if_else(.data$sentiment == "positive",
                     "negative", "positive"),
      .data$sentiment
    )
  )

  # Weight column
  if (weighted && has_weight) {
    matched <- dplyr::mutate(matched, score_val = .data$weight)
  } else {
    matched <- dplyr::mutate(matched, score_val = 1)
  }

  # Aggregate per doc
  matched |>
    dplyr::group_by(.data$doc_id) |>
    dplyr::summarise(
      n_matched       = dplyr::n(),
      score_positive  = sum(.data$score_val[.data$sentiment_eff == "positive"],
                            na.rm = TRUE),
      score_negative  = sum(.data$score_val[.data$sentiment_eff == "negative"],
                            na.rm = TRUE),
      score_ambiguous = sum(.data$score_val[.data$sentiment_eff == "ambiguous"],
                            na.rm = TRUE),
      .groups = "drop"
    )
}


# ── mixology_sentiment() ───────────────────────────────────────────────────────

#' Compute sentiment scores for a corpus
#'
#' Full pipeline: tokenise → (optionally) detect negation → match against
#' a lexicon → aggregate per document.
#'
#' @param text Character vector. One element per document or tweet.
#' @param lexicon Character. Lexicon to use. One of
#'   \code{"inquirer"}, \code{"subjectivity"}, \code{"bing"},
#'   \code{"nrc"}, \code{"afinn"}, \code{"loughran"},
#'   \code{"covid"} (default), \code{"mixology"}.
#' @param weighted Logical. Multiply match counts by the \code{weight} column
#'   (available for \code{"covid"} and \code{"mixology"} only). For other
#'   lexicons, unweighted counts are always used. Default \code{TRUE}.
#' @param handle_negation Logical. Reverse polarity of tokens within a
#'   negation window. Default \code{FALSE}.
#' @param negation_window Integer. Window size for negation. Default \code{3}.
#' @param remove_stopwords Logical. Default \code{TRUE}.
#' @param custom_stopwords Character vector. Additional stop words.
#' @param custom_negations Character vector. Additional negation markers.
#'
#' @return A tibble with one row per document:
#' \describe{
#'   \item{doc_id}{Integer.}
#'   \item{n_tokens}{Integer. Tokens after stop word removal.}
#'   \item{n_matched}{Integer. Tokens matched in the lexicon.}
#'   \item{coverage}{Numeric. \code{n_matched / n_tokens}.}
#'   \item{score_positive}{Numeric. Sum of (weighted) positive matches.}
#'   \item{score_negative}{Numeric. Sum of (weighted) negative matches.}
#'   \item{score_ambiguous}{Numeric. Sum of (weighted) ambiguous matches.}
#'   \item{score_net}{Numeric. \code{score_positive - score_negative}.}
#'   \item{polarity}{Character. Dominant polarity: \code{"positive"},
#'     \code{"negative"}, \code{"ambiguous"}, or \code{"none"}.}
#' }
#'
#' @examples
#' tweets <- c(
#'   "The lockdown is terrible and unjustified",
#'   "The vaccine rollout has been excellent",
#'   "I am not sure about the booster"
#' )
#'
#' # Basic
#' mixology_sentiment(tweets)
#'
#' # With negation handling
#' mixology_sentiment(tweets, handle_negation = TRUE)
#'
#' # Using Bing lexicon
#' mixology_sentiment(tweets, lexicon = "bing", weighted = FALSE)
#'
#' @importFrom dplyr left_join mutate group_by summarise filter if_else
#'   case_when bind_rows select
#' @importFrom tibble tibble
#' @importFrom rlang .data
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
    toks <- mixology_negation(toks,
                               window           = negation_window,
                               custom_negations = custom_negations)
  } else {
    toks$negated <- FALSE
  }

  scores <- .score_one_lexicon(toks, lex, weighted)

  all_docs <- tibble::tibble(doc_id = seq_along(text))
  result   <- dplyr::left_join(all_docs, tok_counts, by = "doc_id")
  result   <- dplyr::left_join(result,   scores,     by = "doc_id")

  # Fill NAs for docs with no matches
  num_cols <- c("n_matched", "score_positive", "score_negative",
                "score_ambiguous")
  for (col in num_cols) {
    result[[col]][is.na(result[[col]])] <- 0
  }
  result$n_tokens[is.na(result$n_tokens)] <- 0L

  result <- dplyr::mutate(result,
    coverage  = dplyr::if_else(.data$n_tokens > 0,
                                .data$n_matched / .data$n_tokens, 0),
    score_net = .data$score_positive - .data$score_negative,
    polarity  = dplyr::case_when(
      .data$n_matched == 0                                          ~ "none",
      .data$score_positive  > .data$score_negative  &
        .data$score_positive  >= .data$score_ambiguous              ~ "positive",
      .data$score_negative  > .data$score_positive  &
        .data$score_negative  >= .data$score_ambiguous              ~ "negative",
      .data$score_ambiguous > 0                                     ~ "ambiguous",
      TRUE                                                          ~ "none"
    )
  )

  result[, c("doc_id", "n_tokens", "n_matched", "coverage",
             "score_positive", "score_negative", "score_ambiguous",
             "score_net", "polarity")]
}


# ── compare_lexicons() ─────────────────────────────────────────────────────────

#' Compare sentiment results across multiple lexicons
#'
#' Runs \code{\link{mixology_sentiment}} for each requested lexicon and
#' returns a long-format tibble for easy comparison and visualisation.
#'
#' @param text Character vector. One element per document or tweet.
#' @param lexicons Character vector. Lexicons to compare. Any subset of
#'   \code{c("inquirer", "subjectivity", "bing", "nrc", "afinn",
#'   "loughran", "covid", "mixology")}. Default: all eight.
#' @param weighted Logical. Use corpus weights where available. Default
#'   \code{FALSE} so that lexicons are compared on equal (unweighted) terms.
#' @param handle_negation Logical. Default \code{FALSE}.
#' @param negation_window Integer. Default \code{3}.
#' @param remove_stopwords Logical. Default \code{TRUE}.
#' @param custom_stopwords Character vector. Default \code{NULL}.
#' @param summary Logical. If \code{TRUE} (default), returns corpus-level
#'   aggregates per lexicon. If \code{FALSE}, returns document-level results.
#'
#' @return
#' When \code{summary = TRUE}, a tibble with one row per lexicon and columns:
#' \describe{
#'   \item{lexicon}{Character. Short name.}
#'   \item{lexicon_label}{Character. Full name.}
#'   \item{n_terms}{Integer. Number of terms in the lexicon.}
#'   \item{n_docs}{Integer. Total documents.}
#'   \item{n_matched_docs}{Integer. Documents with at least one match.}
#'   \item{mean_coverage}{Numeric. Mean token coverage across documents.}
#'   \item{pct_positive}{Numeric. Percentage of matched docs classified positive.}
#'   \item{pct_negative}{Numeric. Percentage of matched docs classified negative.}
#'   \item{pct_ambiguous}{Numeric. Percentage of matched docs classified ambiguous.}
#'   \item{mean_score_net}{Numeric. Mean net score (\code{positive - negative}).}
#' }
#'
#' When \code{summary = FALSE}, a tibble with one row per document × lexicon,
#' with all columns from \code{\link{mixology_sentiment}} plus \code{lexicon}
#' and \code{lexicon_label}.
#'
#' @examples
#' tweets <- c(
#'   "The lockdown is terrible and completely unjustified",
#'   "The vaccine rollout has been excellent, great progress",
#'   "I am not sure about the restrictions"
#' )
#'
#' # Summary table across all lexicons
#' compare_lexicons(tweets)
#'
#' # Document-level, selected lexicons
#' compare_lexicons(tweets,
#'   lexicons = c("bing", "nrc", "covid"),
#'   summary  = FALSE
#' )
#'
#' @importFrom dplyr bind_rows mutate group_by summarise filter n left_join
#'   select
#' @importFrom tibble tibble
#' @importFrom rlang .data
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
    res <- mixology_sentiment(
      text,
      lexicon          = lex_name,
      weighted         = weighted,
      handle_negation  = handle_negation,
      negation_window  = negation_window,
      remove_stopwords = remove_stopwords,
      custom_stopwords = custom_stopwords
    )
    res$lexicon       <- lex_name
    res$lexicon_label <- .LEXICON_LABELS[lex_name]
    res
  })

  long <- dplyr::bind_rows(results)

  if (!summary) {
    return(long)
  }

  # Corpus-level summary
  lex_sizes <- sapply(lexicons, function(l) nrow(get_lexicon(l)))

  long |>
    dplyr::group_by(.data$lexicon, .data$lexicon_label) |>
    dplyr::summarise(
      n_docs         = dplyr::n(),
      n_matched_docs = sum(.data$n_matched > 0),
      mean_coverage  = round(mean(.data$coverage, na.rm = TRUE), 3),
      pct_positive   = round(
        100 * sum(.data$polarity == "positive" & .data$n_matched > 0) /
          sum(.data$n_matched > 0), 1),
      pct_negative   = round(
        100 * sum(.data$polarity == "negative" & .data$n_matched > 0) /
          sum(.data$n_matched > 0), 1),
      pct_ambiguous  = round(
        100 * sum(.data$polarity == "ambiguous" & .data$n_matched > 0) /
          sum(.data$n_matched > 0), 1),
      mean_score_net = round(mean(.data$score_net, na.rm = TRUE), 3),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      n_terms = lex_sizes[.data$lexicon]
    ) |>
    dplyr::select(
      .data$lexicon, .data$lexicon_label, .data$n_terms,
      .data$n_docs, .data$n_matched_docs, .data$mean_coverage,
      .data$pct_positive, .data$pct_negative, .data$pct_ambiguous,
      .data$mean_score_net
    )
}


# ── lexicon_coverage() ─────────────────────────────────────────────────────────

#' Compute per-lexicon coverage for a corpus
#'
#' Returns the proportion of corpus tokens (after stop word removal) that are
#' found in each lexicon, without performing full sentiment scoring.
#' Useful for a quick diagnostic before running \code{\link{compare_lexicons}}.
#'
#' @param text Character vector.
#' @param lexicons Character vector. Default: all eight lexicons.
#' @param remove_stopwords Logical. Default \code{TRUE}.
#' @param custom_stopwords Character vector. Default \code{NULL}.
#'
#' @return A tibble with columns \code{lexicon}, \code{lexicon_label},
#'   \code{n_terms}, \code{n_tokens}, \code{n_matched}, \code{coverage}.
#'
#' @examples
#' tweets <- c("Lockdown is terrible", "The vaccine is great")
#' lexicon_coverage(tweets)
#'
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @export
lexicon_coverage <- function(text,
                              lexicons         = .LEXICON_NAMES,
                              remove_stopwords = TRUE,
                              custom_stopwords = NULL) {
  lexicons <- match.arg(lexicons, .LEXICON_NAMES, several.ok = TRUE)

  toks <- mixology_tokenize(text,
                             remove_stopwords = remove_stopwords,
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
#' Returns terms that appear in at least two lexicons with different
#' sentiment labels. Useful for quality control and understanding disagreements
#' between resources.
#'
#' @param lexicons Character vector. Lexicons to compare. Default: all eight.
#' @param min_conflict Integer. Minimum number of lexicons a term must appear
#'   in (with conflicting labels) to be included. Default \code{2}.
#'
#' @return A tibble with one row per term, with columns for each lexicon's
#'   label (or \code{NA} if absent) and a \code{n_conflict} column counting
#'   how many distinct polarities that term has across the selected lexicons.
#'
#' @examples
#' # Conflicts between Mixology Covid and Bing
#' lexicon_conflicts(c("covid", "bing"))
#'
#' # All eight lexicons
#' lexicon_conflicts()
#'
#' @importFrom dplyr full_join select mutate filter
#' @importFrom tibble tibble
#' @export
lexicon_conflicts <- function(lexicons    = .LEXICON_NAMES,
                               min_conflict = 2L) {
  lexicons <- match.arg(lexicons, .LEXICON_NAMES, several.ok = TRUE)

  # Build one wide data frame: word | lex1 | lex2 | ...
  frames <- lapply(lexicons, function(lex_name) {
    lex <- get_lexicon(lex_name)[, c("word", "sentiment")]
    names(lex)[2] <- lex_name
    lex
  })

  wide <- Reduce(function(a, b) {
    dplyr::full_join(a, b, by = "word")
  }, frames)

  # Count distinct non-NA sentiments per row
  sent_cols <- lexicons
  wide$n_distinct <- apply(wide[, sent_cols, drop = FALSE], 1, function(row) {
    vals <- unique(na.omit(row))
    length(vals)
  })

  wide <- dplyr::filter(wide, .data$n_distinct >= min_conflict)
  wide <- wide[order(-wide$n_distinct, wide$word), ]

  tibble::as_tibble(wide)
}
