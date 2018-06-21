#' Remove duplicate studies and punctuation
#' @description This function removes entries from the database that are duplicated articles (e.g. articles that were returned by two different databases) based on the first 40 characters of the title. It also replaces all miscellaneous punctuation marks used to separate keywords and replaces them with a semicolon so that keywords properly separate in later steps.
#' @param df The dataset you want to deduplicate.
#' @param chars How many characters you want to include for deduplication (default is 40).
#' @return The cleaned dataset.
clean_dataset <- function(df, chars=40){
  # deduplicate
  df$dedupe <- stringr::str_trim(substring(tolower(df$title), 1, chars))
  df <- df[!duplicated(df$dedupe), ]

  # remove_punctuation
  df$key <- tolower(as.character(df$key))
  removals <- c(", ", ",", "/", ", ", "\\[", "\\]", "\\(", "\\)", ";;")
  for (i in 1:length(removals)){
    df$key <- gsub(removals[i], df$key, replacement=";")
  }

  return(df)
}


#' Find unique keywords
#' @description Many keywords are repeated across studies; only searching for unique keywords reduces computation time in later steps.
#' @param df The dataset of titles, abstracts, and keywords.
#' @param singular If \code{TRUE}, plural keywords are made singular (e.g. animals and animal count as the same word).
#' @return A vector of unique keyword from all studies in dataset.
get_unique <- function(df, singular=TRUE){
  all_keywords <- c()
  for (i in 1:length(df$key)){
    all_keywords <- paste(all_keywords, ";", as.character(df$key[i]), sep="")
  }

  all_keywords <- strsplit(all_keywords, ";")

  unique_keywords <- unique(all_keywords[[1]])
  unique_keywords <- unique_keywords[-1]
  unique_keywords <- stringr::str_trim(unique_keywords)

  if (length(which(unique_keywords=="")) > 0) {
    unique_keywords <- unique_keywords[-which(unique_keywords=="")]
  }

  if (singular==TRUE) {
    require(pluralize, quietly=TRUE)
    unique_keywords <- unique(pluralize::singularize(unique_keywords))}

  return(unique_keywords)
}


#' Create searchable study subjects
#' @description Combines titles, abstracts, keywords, and/or other aspects of the study (e.g. modeling approach) from metadata to search for untagged keywords.
#' @return The same dataset with an additional column vector of the concatenated subject.
create_subjects <- function(df, keys=TRUE, titles=TRUE, abstracts=TRUE, models=FALSE){
  df$subj <- rep("", length(df$id))

  if (keys==TRUE){df$subj <- paste(df$subj, df$key)}
  if (titles==TRUE){df$subj <- paste(df$subj, df$title)}
  if (abstracts==TRUE){df$subj <- paste(df$subj, df$abst)}
  if (models==TRUE){df$subj <- paste(df$subj, df$mods)}

  df$subj <- stringr::str_trim(df$subj)

  return(df)
}

#' Search study subjects for keywords
#' @description Searches the study subjects, specified in \code{create_subjects}, to add additional keywords that were not tagged by the authors but appear in the title or abstract.
#' @param df The dataset.
#' @param keywords A vector of unique keywords in the sample.
#' @param stemming Defaults to false, but can search for stemmed versions of keywords.
#' @param minlength The minimum length for a word stem to be allowed to stem. Do not set to less than 3. For example, stemming "relative" results in "rel" which will also pick up unrelated words like relationship, religion, relict, etc...
#' @param exact Only searches for complete phrases and not partial words. If set to \code{TRUE}, the keyword "fire" will only pick up that exact phrase, but will not pick up "wildfire".
#' @return The dataset with an additional column vector of "descriptors" (keywords and newly generated keywords from titles and abstracts).
get_descriptors <- function(df, keywords, stemming=FALSE, minlength=5, exact=FALSE){

  df$desc <- rep("", length(df$id))

  if (stemming==TRUE){
    truncated_keys <- SnowballC::wordStem(keywords)
    truncated_keys <- stringr::str_trim(truncated_keys)
    truncated_keys <- gsub("\\)", truncated_keys, replacement="")
    truncated_keys <- gsub("\\(", truncated_keys, replacement="")
    truncated_chars <- nchar(truncated_keys)
    truncated_keys <- truncated_keys[which(truncated_chars >= min.length)]
    returned_keys <- keywords[which(trunc.chars >= min.length)]

    for (i in 1:length(df$id)){
      detect <- stringr::str_detect(df$subj[i], truncated_keys)
      descriptors <- returned_keys[which(detect==TRUE)]
      descriptors <- stringr::str_trim(descriptors)
      descriptors <- unique(descriptors)
      new_keys <- descriptors[1]
      for (i in 2:length(descriptors)){
        new_keys <- paste(new_keys, descriptors[i], sep=";")
      }
      df$desc[i] <- stringr::str_trim(new_keys)
    }
  }
  if (stemming==FALSE) {
    if (exact==TRUE) {keywords <- paste("\\b", keywords, "\\b", sep="")}
    for (i in 1:length(df$id)){
      detect <- stringr::str_detect(df$subj[i], as.character(keywords))
      descriptors <- keywords[which(detect==TRUE)]
      descriptors <- stringr::str_trim(descriptors)
      descriptors <- unique(descriptors)
      new_keys <- paste(df$key[i], descriptors[1])
      for (i in 2:length(descriptors)){
        new_keys <- paste(new_keys, descriptors[i], sep=";")
      }
      df$desc[i] <- stringr::str_trim(new_keys)
    }
  }

  return(df)
}


#' Generate co-occurrence dataset
#' @description Takes the study keyword dataset and converts it to a matrix of 1s and 0s indicating presence (or absence) of a keyword in that study.
#' @param df The study keyword dataset.
#' @param keys The unique keywords from the sample.
#' @param onlykeywords If set to \code{TRUE}, the search is restricted to author-tagged keywords rather than the descriptors generated with \code{get_descriptors}.
#' @return A presence-absence dataset of keywords for each study.
make_dataset <- function(df, keys, onlykeywords=FALSE){
  dataset <- as.data.frame(matrix(c(rep(NA, length(keys))), nrow=1, byrow=TRUE))
  colnames(dataset) <- keys

  if (onlykeywords==FALSE){

    for (i in 1:nrow(df)){
      study <- as.character(df$desc[i])
      split_study <- strsplit(study, ";")
      split_study <- unique(split_study[[1]])
      split_study <- unique(split_study)
      blank_row <- as.data.frame(matrix(c(rep(NA, length(keys))), nrow=1, byrow=TRUE))
      colnames(blank_row) <- keys

      for (j in 1:length(keys)){
        blank_row[j] <- as.numeric((keys[j] %in% split_study))
      }

      dataset <- rbind(dataset, blank_row)

    }
    dataset <- dataset[-1,]}

  if (onlykeywords==TRUE){

    for (i in 1:nrow(df)){
      study <- as.character(df$key[i])
      split_study <- strsplit(study, ";")
      split_study <- unique(split_study[[1]])
      split_study <- unique(split_study)
      blank_row <- as.data.frame(matrix(c(rep(NA, length(keys))), nrow=1, byrow=TRUE))
      colnames(blank_row) <- keys

      for (j in 1:length(keys)){
        blank_row[j] <- as.numeric((keys[j] %in% split_study))
      }

      dataset <- rbind(dataset, blank_row)

    }
    dataset <- dataset[-1, ]}

  return(dataset)
}


#' Remove rare keywords
#' @description Trims the dataset down to remove keyword column vectors that are rarely used. The primary purpose for this function is to reduce the computation time for the co-occurrence matrix and associated graph.
#' @param dataset The presence-absence dataset of keywords.
#' @param studies The minimum number of studies a keyword must appear in to stay in the dataset.
#' @return A reduced dataset that does not contain entries for rare keywords.

trim_dataset <- function(dataset, studies=2){
  total_occurrences <- colSums(dataset)
  keyword_data <- rbind(dataset, total_occurrences)
  minstud <- studies
  multiples <- which(keyword_data[nrow(keyword_data), ] >= minstud)
  trimmed_data <- keyword_data[ , multiples]
  trimmed_data <- trimmed_data[-nrow(keyword_data), ]
  return(trimmed_data)
}

