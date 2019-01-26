#' Example document-feature matrix
#'
#' A document-feature matrix containing a subset of example
#' Black-backed Woodpecker (Picoides arcticus) articles
#' and keywords.
#'
#' @format A document-feature matrix with 36 documents and 79 features
#'
"BBWO_dfm"

#' Example deduplicated articles
#'
#' A data frame containing a subset of example imported
#' Black-backed Woodpecker (Picoides arcticus) articles
#' after deduplication.
#'
#' @format A data frame with 36 rows of 18 variables for each article:
#' \describe{
#'  \item{id}{A unique ID}
#'  \item{text}{The title, abstract, and/or keywords combined}
#'  \item{title}{The title}
#'  \item{abstract}{The abstract}
#'  \item{keywords}{The author- or database-tagged keywords}
#'  \item{methods}{Database-tagged methods}
#'  \item{type}{Type of article (e.g. thesis, journal, book chapter)}
#'  \item{authors}{Authors of the article}
#'  \item{affiliation}{Author institutional affiliation}
#'  \item{source}{Journal or source of publication}
#'  \item{year}{Year of publication}
#'  \item{volume}{Volume of publication, if applicable}
#'  \item{issue}{Issue of publication, if applicable}
#'  \item{startpage}{The first page of the article}
#'  \item{endpage}{The last page of the article}
#'  \item{doi}{The DOI of the article}
#'  \item{language}{Original language of publication}
#'  \item{database}{The database from which the article was exported}
#'  }
#'
"BBWO_data"

#' Example imported articles
#'
#' A data frame containing a subset of example imported
#' Black-backed Woodpecker (Picoides arcticus) articles
#' prior to deduplication.
#'
#' @format A data frame with 36 rows of 18 variables for each article:
#' \describe{
#'  \item{id}{A unique ID}
#'  \item{text}{The title, abstract, and/or keywords combined}
#'  \item{title}{The title}
#'  \item{abstract}{The abstract}
#'  \item{keywords}{The author- or database-tagged keywords}
#'  \item{methods}{Database-tagged methods}
#'  \item{type}{Type of article (e.g. thesis, journal, book chapter)}
#'  \item{authors}{Authors of the article}
#'  \item{affiliation}{Author institutional affiliation}
#'  \item{source}{Journal or source of publication}
#'  \item{year}{Year of publication}
#'  \item{volume}{Volume of publication, if applicable}
#'  \item{issue}{Issue of publication, if applicable}
#'  \item{startpage}{The first page of the article}
#'  \item{endpage}{The last page of the article}
#'  \item{doi}{The DOI of the article}
#'  \item{language}{Original language of publication}
#'  \item{database}{The database from which the article was exported}
#'  }
#'
"BBWO_import"

#' Example keyword co-occurrence network
#'
#' An example of a keyword co-occurrence network for a subset of
#' articles about Black-backed Woodpeckers (Picoides arcticus).
#'
#'@format An igraph object
#'
"BBWO_graph"

#' An example of exported naive search hits
#'
#' A subset of the results of a naive search in Scopus
#' about Black-backed Woodpeckers (Picoides arcticus).
#'
#' @format A dataframe with 10 rows of 22 variables:
#' \describe{
#' \item{Authors}{Article authors}
#' \item{Author.Ids}{Author identifications}
#' \item{Title}{Article title}
#' \item{Year}{Year of article publication}
#' \item{Source.title}{Journal or source title}
#' \item{Volume}{Volume of publication}
#' \item{Issue}{Issue of publication}
#' \item{Art..No.}{Article number in journal, if applicable}
#' \item{Page.start}{First page of article}
#' \item{Page.end}{Last page of article}
#' \item{Page.count}{Total number of pages in article}
#' \item{Cited.by}{Number of times article has been cited}
#' \item{DOI}{Article DOI}
#' \item{Link}{Link to article}
#' \item{Affiliations}{Author affiliations}
#' \item{Authors.with.affiliations}{Author affiliations associated with each author}
#' \item{Abstract}{Article abstract}
#' \item{Author.keywords}{Author-tagged keywords}
#' \item{Document.Type}{Type of article (e.g. journal article, book chapter)}
#' \item{Access.Type}{Whether or not the article is open access}
#' \item{Source}{The database source}
#' \item{EID}{The identifier within the source}
#' }
#'
"scopus_example"

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



#' Similar terms from example
#'
#' A dataset of terms marked as similar when we were
#' building an example search strategy.
#'
#' @format A database with 922 rows of 2 variables
#' \describe{
#' \item{group} the group we assigned
#' \item{term} the similar search term}
#' }
"BBWO_similar_grouped"
