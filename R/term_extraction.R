#' Create a corpus from a data frame
#' @description Calls the corpus function from quanteda to create a corpus from the data frame with search hits.
#' @param df a data frame where at least one column is called 'text'
#' @return a corpus object
make_corpus <- function(df){
  search_corpus <- quanteda::corpus(df)
  return(search_corpus)
}

#' Add new stopwords to ignore
#' @description Allows the user to add additional stopwords to the built-in English stopwords list.
#' @param new_stopwords a character vector of new words to add
#' @return an updated vector of custom stopwords to remove from text
add_stopwords <- function(new_stopwords){
  custom_stopwords <- sort(unique(append(litsearchr::custom_stopwords, new_stopwords)))
  return(custom_stopwords)
}

#' Extract potential keywords from abstracts and titles
#' @description Uses the RAKE (Rapid Automatic Keyword Extractor) from rapidraker to extract potential keyword terms from titles and abstracts.
#' @param df a dataframe created with import_scope
#' @param new_stopwords a character vector of stopwords to ignore
#' @param min_freq a number, the minimum occurrences of a potential term
#' @param title include titles if TRUE
#' @param abstract include abstracts if TRUE
#' @param ngrams if TRUE, only extracts phrases with word count greater than a specified n
#' @param n the minimum word count for ngrams
#' @return a character vector of potential keyword terms
extract_terms <- function(df, new_stopwords=NULL, min_freq=2, title=TRUE, abstract=TRUE, ngrams=TRUE, n=2){
  if (title == TRUE){
    if (abstract == TRUE){
      article_subjects <- paste(df$title, df$abstract, collapse=". ")
    }
    if (abstract == FALSE){
      article_subjects <- paste(df$title, collapse=". ")
    }
  }
  if (title == FALSE){
    if (abstract == TRUE){
      article_subjects <- paste(df$abstract, collapse=". ")
    }
    if (abstract == FALSE){print("You aren't selecting any text to pass to RAKE!")}
  }

  possible_terms <- rapidraker::rapidrake(tolower(article_subjects),
                                          stop_words = add_stopwords(new_stopwords),
                                          stem=FALSE)
  likely_terms <- possible_terms[[1]]$keyword[which(possible_terms[[1]]$freq >= min_freq)]
  if (ngrams==TRUE){
    likely_terms <- likely_terms[which(sapply(strsplit(as.character(likely_terms), " "), length) >= n)]
  }

  return(likely_terms)
}

#' Extract actual article keywords
#' @description Extracts actual author-and-database tagged keywords.
#' @param df a data frame of search hits from import_scope
#' @param min_freq a number, the minimum occurrences to be included
#' @param ngrams if TRUE, only extracts phrases with word count greater than a specified n
#' @param n the minimum word count for ngrams
#' @return a character vector of keywords actually occurring in the search dataset
select_actual_terms <- function(df, min_freq=2, ngrams=TRUE, n=2){
  cleaned_keywords <- clean_keywords(df)$keyword
  possible_terms <- paste(df$keywords, collapse=";")
  possible_terms <- strsplit(possible_terms, ";")[[1]]
  possible_terms <- stringr::str_trim(tm::removePunctuation(possible_terms))

  term_freq_table <- table(possible_terms)

  actual_terms <- tolower(names(term_freq_table)[which(term_freq_table >= min_freq)])
  if(length(which(actual_terms=="")>0)){actual_terms <- actual_terms[-which(actual_terms=="")]}
  if (ngrams==TRUE){
    actual_terms <- actual_terms[which(sapply(strsplit(as.character(actual_terms), " "), length) >= n)]
  }


  return(actual_terms)

}

#' Make a dictionary from keywords
#' @description Combines actual keywords and likely keywords into a dictionary object using the as.dictionary function from quanteda.
#' @param actual_terms a character vector of search terms
#' @param likely_terms a character vector of search terms
#' @return a quanteda dictionary object
make_dictionary <- function(actual_terms=NULL, likely_terms=NULL){

  complete_keywords <- unique(tolower(append(actual_terms, likely_terms)))

  my_dic <- as.data.frame(cbind(complete_keywords, complete_keywords))
  colnames(my_dic) <- c("word", "sentiment")

  my_dic <- quanteda::as.dictionary(my_dic)

  return(my_dic)
}

#' Create document-term matrix
#' @description Calls the dfm function from quanteda to create a document-term matrix of terms included in a custom dictionary of potential terms.
#' @param corpus a corpus object
#' @param my_dic a dictionary object
#' @param custom_stopwords a character vector of words to ignore
#' @return a quanteda dfm object
create_dfm <- function(corpus=make_corpus(df), my_dic=make_dictionary(), custom_stopwords=add_stopwords(NULL)){

  search_dfm <- quanteda::dfm(corpus,
                              stem = FALSE,
                              remove=litsearchr::custom_stopwords,
                              remove_numbers=TRUE,
                              remove_punct=TRUE,
                              remove_symbols=TRUE,
                              remove_separators=TRUE,
                              remove_twitter=TRUE,
                              remove_hyphens=TRUE,
                              remove_url=TRUE,
                              dictionary=my_dic,
                              tolower=TRUE)

  return(search_dfm)

}
