#' General Inquirer Lexicon (harmonised)
#'
#' The General Inquirer lexicon, originally developed in 1962 for content
#' analysis in the social sciences. Based on the Harvard IV Dictionary and
#' the Lasswell Dictionary. Polarity has been harmonised to three categories.
#'
#' @format A tibble with 4,206 rows and 2 variables:
#' \describe{
#'   \item{word}{Character. The term (lowercased).}
#'   \item{sentiment}{Character. \code{"positive"} or \code{"negative"}.}
#' }
#' @source Stone, P. J., Dunphy, D. C., Smith, M. S., & Ogilvie, D. M. (1966).
#'   \emph{The General Inquirer}. MIT Press.
#' @seealso \code{\link{compare_lexicons}}
"lexicon_inquirer"


#' MPQA Subjectivity Lexicon (harmonised)
#'
#' The MPQA Subjectivity Lexicon, aggregated from manually developed and
#' automatically constructed sources. Originally contains over 8,000 words
#' across positive, negative, and neutral categories. The \code{"neutral"}
#' and \code{"both"} original categories have been mapped to
#' \code{"ambiguous"}.
#'
#' @format A tibble with 6,884 rows and 2 variables:
#' \describe{
#'   \item{word}{Character. The term (lowercased). Note: POS suffixes
#'     (e.g. \code{pos1}, \code{pos2}) have been removed.}
#'   \item{sentiment}{Character. \code{"positive"}, \code{"negative"},
#'     or \code{"ambiguous"}.}
#' }
#' @source Wilson, T., Wiebe, J., & Hoffmann, P. (2005). Recognizing
#'   contextual polarity in phrase-level sentiment analysis. Proceedings of
#'   HLT-EMNLP, 347–354.
#' @seealso \code{\link{compare_lexicons}}
"lexicon_subjectivity"


#' Bing Liu Opinion Lexicon (harmonised)
#'
#' One of the most widely used sentiment dictionaries, compiled by Bing Liu
#' and Minqing Hu. Contains approximately 6,800 terms with positive and
#' negative polarities, regularly updated since 2004.
#'
#' @format A tibble with 6,783 rows and 2 variables:
#' \describe{
#'   \item{word}{Character. The term (lowercased).}
#'   \item{sentiment}{Character. \code{"positive"} or \code{"negative"}.}
#' }
#' @source Hu, M., & Liu, B. (2004). Mining and summarizing customer reviews.
#'   \emph{Proceedings of KDD 2004}, 168–177.
#' @seealso \code{\link{compare_lexicons}}
"lexicon_bing"


#' NRC Emotion Lexicon (harmonised)
#'
#' The NRC Word-Emotion Association Lexicon. Originally classifies terms
#' across eight emotions (anger, fear, anticipation, trust, surprise,
#' sadness, joy, disgust) and two polarities (positive, negative).
#' Emotions have been mapped to the three-category polarity scheme:
#' anger, fear, sadness, disgust, negative → \code{"negative"};
#' anticipation, trust, joy, positive → \code{"positive"};
#' surprise → \code{"ambiguous"}.
#' Duplicate entries (a word appearing under multiple emotions) have been
#' resolved by retaining one polarity per word.
#'
#' @format A tibble with 6,456 rows and 2 variables:
#' \describe{
#'   \item{word}{Character. The term (lowercased).}
#'   \item{sentiment}{Character. \code{"positive"}, \code{"negative"},
#'     or \code{"ambiguous"}.}
#' }
#' @source Mohammad, S. M., & Turney, P. D. (2013). Crowdsourcing a
#'   word–emotion association lexicon. \emph{Computational Intelligence},
#'   29(3), 436–465.
#' @seealso \code{\link{compare_lexicons}}
"lexicon_nrc"


#' AFINN Lexicon (harmonised)
#'
#' The AFINN lexicon, developed by Finn Årup Nielsen between 2009 and 2011.
#' Originally scores terms on a Likert scale from −5 to +5. Scores have
#' been binarised: positive scores → \code{"positive"}, negative scores →
#' \code{"negative"}, zero → \code{"ambiguous"}.
#'
#' @format A tibble with 2,477 rows and 2 variables:
#' \describe{
#'   \item{word}{Character. The term (lowercased).}
#'   \item{sentiment}{Character. \code{"positive"}, \code{"negative"},
#'     or \code{"ambiguous"}.}
#' }
#' @source Nielsen, F. Å. (2011). A new ANEW: Evaluation of a word list
#'   for sentiment analysis in microblogs. \emph{arXiv:1103.2903}.
#' @seealso \code{\link{compare_lexicons}}
"lexicon_afinn"


#' Loughran-McDonald Lexicon (harmonised)
#'
#' A lexicon of financial terms developed by Loughran and McDonald. Contains
#' six original categories: negative, positive, uncertainty, litigious,
#' constraining, superfluous. These have been mapped as follows:
#' negative, litigious, constraining → \code{"negative"};
#' positive → \code{"positive"};
#' uncertainty, superfluous → \code{"ambiguous"}.
#'
#' @format A tibble with 3,917 rows and 2 variables:
#' \describe{
#'   \item{word}{Character. The term (lowercased).}
#'   \item{sentiment}{Character. \code{"positive"}, \code{"negative"},
#'     or \code{"ambiguous"}.}
#' }
#' @source Loughran, T., & McDonald, B. (2011). When is a liability not a
#'   liability? Textual analysis, dictionaries, and 10-Ks.
#'   \emph{Journal of Finance}, 66(1), 35–65.
#' @seealso \code{\link{compare_lexicons}}
"lexicon_loughran"


#' Mixology Covid Lexicon
#'
#' A manually annotated sentiment lexicon of 4,166 terms developed
#' specifically for opinion mining of Twitter data about the Covid-19 crisis.
#' Terms were extracted from the 4,000 most frequent tokens of a corpus of
#' 311,882 English tweets collected in Western Europe between December 12 and
#' 31, 2021. Each term was reviewed manually with bigram and trigram context
#' and cross-referenced with six general-purpose dictionaries.
#'
#' Unlike general-purpose dictionaries, this lexicon has a near-balanced
#' distribution between positive and negative terms, reflecting a deliberate
#' design choice. The notion of ambiguity is favoured over neutrality for
#' terms whose polarity depends on context (e.g. \emph{tested positive},
#' \emph{government}).
#'
#' @format A tibble with 4,166 rows and 4 variables:
#' \describe{
#'   \item{word}{Character. The term (lowercased).}
#'   \item{sentiment}{Character. \code{"positive"}, \code{"negative"},
#'     or \code{"ambiguous"}.}
#'   \item{weight}{Numeric. Log-normalised corpus frequency weight,
#'     range 0.5–3.0. Formula:
#'     \code{0.5 + (log(freq + 1) / log(max_freq + 1)) * 2.5}.
#'     Terms unseen in the reference corpus receive 0.5.}
#'   \item{freq_corpus}{Integer. Raw token frequency in the political
#'     measures sub-corpus (n = 4,371 tweets).}
#' }
#' @source Dierickx, L. (2022). Mixology open research project.
#'   \url{https://ohmybox.info}
#' @seealso \code{\link{mixology_lexicon}}, \code{\link{compare_lexicons}}
"mixology_covid_lexicon"


#' Mixology Lexicon
#'
#' A merged sentiment lexicon combining the Mixology Covid Lexicon with six
#' harmonised general-purpose English sentiment dictionaries (General
#' Inquirer, MPQA, Bing, NRC, AFINN, Loughran-McDonald). After merging,
#' all 14,446 terms were reviewed manually to resolve cross-dictionary
#' polarity conflicts.
#'
#' @format A tibble with 16,528 rows and 4 variables:
#' \describe{
#'   \item{word}{Character. The term (lowercased).}
#'   \item{sentiment}{Character. \code{"positive"}, \code{"negative"},
#'     or \code{"ambiguous"}.}
#'   \item{weight}{Numeric. Log-normalised corpus frequency weight
#'     (see \code{\link{mixology_covid_lexicon}}).}
#'   \item{freq_corpus}{Integer. Raw token frequency in the reference
#'     corpus.}
#' }
#' @source Dierickx, L. (2022). Mixology open research project.
#'   \url{https://ohmybox.info}
#' @seealso \code{\link{mixology_covid_lexicon}}, \code{\link{compare_lexicons}}
"mixology_lexicon"


#' English Stop Words (Mixology)
#'
#' A custom list of 350 English stop words compiled for the Mixology project,
#' adapted from standard lists and extended with corpus-specific noise terms
#' (Twitter patterns, common abbreviations, etc.).
#'
#' @format A tibble with 350 rows and 1 variable:
#' \describe{
#'   \item{word}{Character. The stop word.}
#' }
#' @source Dierickx, L. (2022). Mixology open research project.
#'   \url{https://ohmybox.info}
"stop_words_en"


#' English Negation Markers
#'
#' A list of 22 English negation markers for polarity reversal in sentiment
#' analysis (e.g. \code{"not"}, \code{"never"}, \code{"can't"},
#' \code{"ain't"}).
#'
#' @format A tibble with 22 rows and 1 variable:
#' \describe{
#'   \item{word}{Character. The negation marker.}
#' }
#' @source Dierickx, L. (2022). Mixology open research project.
#'   \url{https://ohmybox.info}
"negations_en"


#' Mixology Covid Lexicon (fine-tuned)
#'
#' A fine-tuned version of the Mixology Covid Lexicon, adapted using a
#' gold-standard annotation of 1,000 tweets. Fine-tuning was carried out in
#' three stages: candidate term extraction via TF-IDF and PMI on misclassified
#' tweets, manual review of candidates, and polarity correction or addition
#' of missing terms. Compared to the original, this lexicon includes 186 new
#' terms and 172 corrected polarity assignments, improving macro-F1 from 0.335
#' to 0.435 against the gold standard.
#'
#' @format A tibble with 4,355 rows and 4 variables:
#' \describe{
#'   \item{word}{Character. The term (lowercased).}
#'   \item{sentiment}{Character. \code{"positive"}, \code{"negative"},
#'     or \code{"ambiguous"}.}
#'   \item{weight}{Numeric. Log-normalised corpus frequency weight,
#'     range 0.5--3.0. New terms added during fine-tuning receive 1.0.}
#'   \item{freq_corpus}{Integer. Raw token frequency in the reference corpus.
#'     New terms added during fine-tuning receive 0.}
#' }
#' @source Dierickx, L. (2026). Wrong dictionary, wrong answer? A
#'   domain-adapted lexicon framework for crisis sentiment analysis.
#' @seealso \code{\link{mixology_covid_lexicon}}, \code{\link{compare_lexicons}}
"mixology_covid_lexicon_ft"


#' Mixology Lexicon (fine-tuned)
#'
#' A fine-tuned version of the Mixology Lexicon, adapted using a gold-standard
#' annotation of 1,000 tweets. Compared to the original, this lexicon includes
#' 197 new terms and 210 corrected polarity assignments, improving macro-F1
#' from 0.351 to 0.453 against the gold standard. The most substantial gains
#' were in the Ambiguous class (F1: 0.043 to 0.276) and Positive class
#' (F1: 0.353 to 0.434).
#'
#' @format A tibble with 16,727 rows and 4 variables:
#' \describe{
#'   \item{word}{Character. The term (lowercased).}
#'   \item{sentiment}{Character. \code{"positive"}, \code{"negative"},
#'     or \code{"ambiguous"}.}
#'   \item{weight}{Numeric. Log-normalised corpus frequency weight,
#'     range 0.5--3.0. New terms added during fine-tuning receive 1.0.}
#'   \item{freq_corpus}{Integer. Raw token frequency in the reference corpus.
#'     New terms added during fine-tuning receive 0.}
#' }
#' @source Dierickx, L. (2026). Wrong dictionary, wrong answer? A
#'   domain-adapted lexicon framework for crisis sentiment analysis.
#' @seealso \code{\link{mixology_lexicon}}, \code{\link{compare_lexicons}}
"mixology_lexicon_ft"
