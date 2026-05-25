#' Mixology Covid Lexicon
#'
#' A manually annotated sentiment lexicon of 4,166 terms developed specifically
#' for opinion mining of Twitter data about Covid-19 (vaccination, political
#' measures, protests). Terms were extracted from the 4,000 most frequent tokens
#' of a corpus of 311,882 English tweets collected in Western Europe between
#' December 12 and 31, 2021. Each term was reviewed manually against bigram and
#' trigram context and cross-referenced with six general-purpose dictionaries.
#'
#' @format A data frame with 4,166 rows and 4 variables:
#' \describe{
#'   \item{word}{Character. The term.}
#'   \item{sentiment}{Character. Polarity: \code{"positive"}, \code{"negative"},
#'     or \code{"ambiguous"}. Ambiguous terms can be positive or negative
#'     depending on context.}
#'   \item{weight}{Numeric. Log-normalised frequency weight computed from the
#'     political measures sub-corpus (n=4,371 tweets). Range: 0.5 (unseen in
#'     corpus) to 3.0 (most frequent term). Formula:
#'     \code{0.5 + (log(freq + 1) / log(max_freq + 1)) * 2.5}.}
#'   \item{freq_corpus}{Integer. Raw token frequency in the political measures
#'     sub-corpus. 0 = term not observed in this corpus.}
#' }
#' @source Mixology open research project by Laurence Dierickx.
#'   \url{https://ohmybox.info}
#' @seealso \code{\link{mixology_lexicon}}, \code{\link{stop_words_en}},
#'   \code{\link{negations_en}}
"mixology_covid_lexicon"


#' Mixology Lexicon
#'
#' A merged sentiment lexicon of 16,528 terms combining the Mixology Covid
#' Lexicon with six general-purpose English sentiment dictionaries: General
#' Inquirer, MPQA Subjectivity Lexicon, Bing, NRC, Afinn, and Loughran.
#' After merging, all terms were reviewed manually (14,446 terms reviewed)
#' to resolve cross-dictionary polarity conflicts.
#'
#' @format A data frame with 16,528 rows and 4 variables:
#' \describe{
#'   \item{word}{Character. The term.}
#'   \item{sentiment}{Character. Polarity: \code{"positive"}, \code{"negative"},
#'     or \code{"ambiguous"}.}
#'   \item{weight}{Numeric. Log-normalised frequency weight (see
#'     \code{\link{mixology_covid_lexicon}} for formula). 0.5 for terms
#'     not observed in the reference corpus.}
#'   \item{freq_corpus}{Integer. Raw token frequency in the political measures
#'     sub-corpus.}
#' }
#' @source Mixology open research project by Laurence Dierickx.
#'   \url{https://ohmybox.info}
#' @seealso \code{\link{mixology_covid_lexicon}}
"mixology_lexicon"


#' English Stop Words (Mixology)
#'
#' A custom list of 350 English stop words compiled for the Mixology project.
#' Adapted from standard stop word lists and extended with corpus-specific
#' noise terms (Twitter usernames patterns, common abbreviations, etc.).
#'
#' @format A data frame with 350 rows and 1 variable:
#' \describe{
#'   \item{word}{Character. The stop word.}
#' }
#' @source Mixology open research project. See Blog 13.
#'   \url{https://ohmybox.info}
"stop_words_en"


#' English Negation Markers
#'
#' A list of 22 English negation markers used to detect polarity reversal
#' in sentiment analysis. When a negation marker precedes a sentiment-bearing
#' term within a defined window, the polarity of that term is reversed.
#'
#' @format A data frame with 22 rows and 1 variable:
#' \describe{
#'   \item{word}{Character. The negation marker (e.g. \code{"not"},
#'     \code{"never"}, \code{"can't"}, \code{"ain't"}).}
#' }
#' @source Mixology open research project.
"negations_en"
