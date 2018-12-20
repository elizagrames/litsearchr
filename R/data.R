#' Example BBWO document-feature matrix
#'
#' A document-feature matrix containing two example terms
#' from the vignette dictionary and imported naive search.
#'
#' @format A document-feature matrix with 775 documents and 2 features
#'


#' Grouped keywords for the BBWO vignette
#'
#' A dataset containing the keyword groupings used in the
#' vignette about Black-backed Woodpecker occupancy of
#' post-fire forest systems.
#'
#' @format A data frame with 429 rows of 2 variables:
#' \describe{
#'   \item{group}{concept group to which a term belongs}
#'   \item{term}{the term or phrase output by litsearchr}
#' }
"BBWO_grouped_keywords"


#' Color transparency conversion numbers
#'
#' A dataset containing conversion codes to go from a transparency
#' setting to an alpha code appended to a hex code.
#'
#' @format A data frame with 256 rows of 2 variables:
#' \describe{
#'   \item{code}{the alpha code}
#'   \item{intensity}{the percent color transparency to which a code corresponds}
#' }
"color_alphas"


#' Stopwords to remove from keywords
#'
#' A dataset of common terms to use as stopwords when tokenizing texts.
#'
#' @format A character vector with 593 entries
"custom_stopwords"


#' Databases that can be imported with litsearchr
#'
#' A dataset containing the platforms, databases, and download methods
#' that litsearchr recognizes and can import.
#'
#' @format A database with 25 rows of 3 variables
#' \describe{
#'   \item{Platform}{the platform used to access a database}
#'   \item{Database}{the database accessed}
#'   \item{Download_method}{the way results must be downloaded for litsearchr to recognize the format}
#' }
"database_list"


#' Languages litsearchr can write searches in
#'
#' A dataset of the languages that searches can be translated
#' into along with their short form, character encoding, and
#' whether a scientific journal indexed in ulrich uses them.
#'
#' @format A database with 53 rows of 4 variables:
#' \describe{
#'   \item{Short}{the short form language code}
#'   \item{Language}{the name of the language}
#'   \item{Encoding}{which character encoding to use for a language}
#'   \item{Used}{whether or not the language is used by a scientific journal}
#' }
"possible_langs"


#' Non-english scientific journal list
#'
#' A dataset of non-english scientific journals indexed in
#' Ulrich's Global Serials Directory.
#'
#' @format A database with 5100 rows of 6 variables:
#' \describe{
#'   \item{Title}{the journal title}
#'   \item{ISSN}{the journal ISSN}
#'   \item{SubjectCodes}{disciplines covered by the journal, as tagged by Ulrich's}
#'   \item{Country}{the country of publication for the journal}
#'   \item{Start.Year}{the first year of journal publication}
#'   \item{Language}{the primary language of articles published in the journal}
#' }
"ulrich"

