#' Retrieve a Mixology lexicon
#'
#' Convenience function to access the package lexicons by name.
#'
#' @param lexicon Character. One of \code{"covid"} (default) or \code{"full"}.
#'   \code{"covid"} returns the Mixology Covid Lexicon (4,166 terms);
#'   \code{"full"} returns the merged Mixology Lexicon (16,528 terms).
#'
#' @return A tibble with columns \code{word}, \code{sentiment}, \code{weight},
#'   \code{freq_corpus}.
#'
#' @examples
#' lex <- get_lexicon("covid")
#' head(lex)
#'
#' @export
get_lexicon <- function(lexicon = c("covid", "full")) {
  lexicon <- match.arg(lexicon)
  if (lexicon == "covid") {
    mixology::mixology_covid_lexicon
  } else {
    mixology::mixology_lexicon
  }
}


#' Tokenise a character vector for sentiment analysis
#'
#' Lowercases text, removes punctuation (keeping apostrophes for contractions),
#' splits into tokens, and optionally removes stop words.
#'
#' @param text Character vector. One element per document/tweet.
#' @param remove_stopwords Logical. If \code{TRUE} (default), removes tokens
#'   found in \code{\link{stop_words_en}}.
#' @param custom_stopwords Character vector of additional stop words to remove.
#'   Default \code{NULL}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{doc_id}{Integer. Index of the source document in \code{text}.}
#'     \item{token}{Character. The cleaned token.}
#'   }
#'
#' @examples
#' tweets <- c("Lockdown is terrible", "The vaccine is great!")
#' mixology_tokenize(tweets)
#'
#' @importFrom tibble tibble
#' @importFrom dplyr filter
#' @importFrom stringr str_to_lower str_replace_all str_split
#' @importFrom rlang .data
#' @export
mixology_tokenize <- function(text, remove_stopwords = TRUE,
                               custom_stopwords = NULL) {
  stopifnot(is.character(text))

  # Lowercase and keep only letters + apostrophes
  cleaned <- stringr::str_to_lower(text)
  cleaned <- stringr::str_replace_all(cleaned, "[^a-z']", " ")
  cleaned <- stringr::str_replace_all(cleaned, "\\s+", " ")

  # Split and build long tibble
  tokens_list <- stringr::str_split(cleaned, " ")
  result <- tibble::tibble(
    doc_id = rep(seq_along(tokens_list),
                 lengths(tokens_list)),
    token  = unlist(tokens_list, use.names = FALSE)
  )

  # Remove empty strings
  result <- dplyr::filter(result, nchar(.data$token) > 1)

  # Remove stop words
  if (remove_stopwords) {
    sw <- mixology::stop_words_en$word
    if (!is.null(custom_stopwords)) {
      sw <- unique(c(sw, stringr::str_to_lower(custom_stopwords)))
    }
    result <- dplyr::filter(result, !(.data$token %in% sw))
  }

  result
}


#' Apply negation window to tokens
#'
#' Marks tokens that follow a negation marker within a sliding window.
#' Negated tokens have their sentiment reversed during scoring.
#'
#' @param tokens_tbl A tibble as returned by \code{\link{mixology_tokenize}},
#'   with columns \code{doc_id} and \code{token}.
#' @param window Integer. Number of tokens after a negation marker within which
#'   a token is considered negated. Default \code{3}.
#' @param custom_negations Character vector of additional negation markers.
#'   Default \code{NULL}.
#'
#' @return The input tibble with an added logical column \code{negated}.
#'
#' @examples
#' tweets <- c("The vaccine is not great", "I never felt so free")
#' toks <- mixology_tokenize(tweets)
#' mixology_negation(toks)
#'
#' @importFrom dplyr mutate group_by
#' @importFrom rlang .data
#' @export
mixology_negation <- function(tokens_tbl, window = 3,
                               custom_negations = NULL) {
  neg_markers <- mixology::negations_en$word
  if (!is.null(custom_negations)) {
    neg_markers <- unique(c(neg_markers,
                            stringr::str_to_lower(custom_negations)))
  }

  # Per document: slide window
  docs <- split(tokens_tbl, tokens_tbl$doc_id)
  result <- lapply(docs, function(d) {
    toks <- d$token
    negated <- logical(length(toks))
    for (i in seq_along(toks)) {
      if (toks[i] %in% neg_markers) {
        # Mark the next `window` tokens
        idx <- seq(i + 1, min(i + window, length(toks)))
        negated[idx] <- TRUE
      }
    }
    d$negated <- negated
    d
  })

  do.call(rbind, result)
}


#' Compute sentiment scores for a corpus
#'
#' Matches tokens against a Mixology lexicon and returns per-document
#' sentiment scores. Supports optional weighting and negation handling.
#'
#' @param text Character vector. One element per document/tweet.
#' @param lexicon Character. \code{"covid"} (default) or \code{"full"}.
#'   See \code{\link{get_lexicon}}.
#' @param weighted Logical. If \code{TRUE} (default), multiplies match
#'   counts by the \code{weight} column of the lexicon. If \code{FALSE},
#'   uses unweighted counts (replicates classic tidytext behaviour).
#' @param handle_negation Logical. If \code{TRUE}, reverses the polarity of
#'   sentiment-bearing tokens that fall within a negation window. Default
#'   \code{FALSE} (unigram baseline).
#' @param negation_window Integer. Window size for negation detection.
#'   Only used when \code{handle_negation = TRUE}. Default \code{3}.
#' @param remove_stopwords Logical. Default \code{TRUE}.
#'
#' @return A tibble with one row per document and columns:
#'   \describe{
#'     \item{doc_id}{Integer.}
#'     \item{n_tokens}{Integer. Number of tokens after stop word removal.}
#'     \item{n_matched}{Integer. Tokens matched in the lexicon.}
#'     \item{coverage}{Numeric. \code{n_matched / n_tokens}.}
#'     \item{score_positive}{Numeric. Sum of (weighted) positive matches.}
#'     \item{score_negative}{Numeric. Sum of (weighted) negative matches.}
#'     \item{score_ambiguous}{Numeric. Sum of (weighted) ambiguous matches.}
#'     \item{score_net}{Numeric. \code{score_positive - score_negative}.}
#'     \item{polarity}{Character. Dominant polarity:
#'       \code{"positive"}, \code{"negative"}, \code{"ambiguous"},
#'       or \code{"none"} (no matches).}
#'   }
#'
#' @examples
#' tweets <- c(
#'   "The lockdown is terrible and unfair",
#'   "The vaccine rollout is excellent, great progress",
#'   "I am not sure about the booster"
#' )
#' mixology_sentiment(tweets)
#' mixology_sentiment(tweets, weighted = TRUE, handle_negation = TRUE)
#'
#' @importFrom dplyr left_join mutate group_by summarise filter select if_else n
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @export
mixology_sentiment <- function(text,
                                lexicon = c("covid", "full"),
                                weighted = TRUE,
                                handle_negation = FALSE,
                                negation_window = 3,
                                remove_stopwords = TRUE) {
  lexicon <- match.arg(lexicon)
  lex <- get_lexicon(lexicon)

  # Tokenise
  toks <- mixology_tokenize(text,
                             remove_stopwords = remove_stopwords)

  # Negation
  if (handle_negation) {
    toks <- mixology_negation(toks, window = negation_window)
  } else {
    toks$negated <- FALSE
  }

  # Count tokens per doc
  tok_counts <- toks |>
    dplyr::group_by(.data$doc_id) |>
    dplyr::summarise(n_tokens = dplyr::n(), .groups = "drop")

  # Join with lexicon
  matched <- dplyr::left_join(toks, lex,
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

  # Score: weighted or unweighted
  if (weighted) {
    matched <- dplyr::mutate(matched,
      score_val = .data$weight)
  } else {
    matched <- dplyr::mutate(matched, score_val = 1)
  }

  # Aggregate per doc
  scores <- matched |>
    dplyr::group_by(.data$doc_id) |>
    dplyr::summarise(
      n_matched      = dplyr::n(),
      score_positive = sum(.data$score_val[.data$sentiment_eff == "positive"],
                           na.rm = TRUE),
      score_negative = sum(.data$score_val[.data$sentiment_eff == "negative"],
                           na.rm = TRUE),
      score_ambiguous = sum(.data$score_val[.data$sentiment_eff == "ambiguous"],
                            na.rm = TRUE),
      .groups = "drop"
    )

  # All doc_ids (including those with no matches)
  all_docs <- tibble::tibble(doc_id = seq_along(text))
  result <- dplyr::left_join(all_docs, tok_counts, by = "doc_id")
  result <- dplyr::left_join(result, scores, by = "doc_id")

  # Fill NAs for docs with no matches
  result[is.na(result)] <- 0

  result <- dplyr::mutate(result,
    coverage = dplyr::if_else(.data$n_tokens > 0,
                               .data$n_matched / .data$n_tokens, 0),
    score_net = .data$score_positive - .data$score_negative,
    polarity = dplyr::case_when(
      .data$n_matched == 0                               ~ "none",
      .data$score_positive > .data$score_negative &
        .data$score_positive >= .data$score_ambiguous    ~ "positive",
      .data$score_negative > .data$score_positive &
        .data$score_negative >= .data$score_ambiguous    ~ "negative",
      .data$score_ambiguous > 0                         ~ "ambiguous",
      TRUE                                               ~ "none"
    )
  )

  result[, c("doc_id", "n_tokens", "n_matched", "coverage",
             "score_positive", "score_negative", "score_ambiguous",
             "score_net", "polarity")]
}
