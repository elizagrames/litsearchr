detect_database <- function(df){
  database <- c()
  firstline <- paste(df[1,], collapse=" ")

  if (stringr::str_detect(firstline, "ZOOR")==TRUE){
    database <- "ZooRec"}

  if (stringr::str_detect(firstline, "BCI:")==TRUE){database <- "BIOSIS"}

  if (stringr::str_detect(firstline, "Scopus")==TRUE){database <- "Scopus"}

  if (stringr::str_detect(firstline, "search.proquest")==TRUE){database <- "ProQuest"}

  if (database==""){print("Database format not recognized.")}

  return(database)
}

import_scope <- function(directory, remove_duplicates=TRUE, clean_dataset=TRUE, save_full_dataset=FALSE){
  import.files <- paste(directory, list.files(path=directory), sep="")
  df <- c()

  for (i in 1:length(import.files)){
    if (stringr::str_detect(import.files[i], ".csv")==TRUE){
      df <- read.csv(import.files[i], header=TRUE, stringsAsFactors = FALSE)
      }
    if (stringr::str_detect(import.files[i], ".txt")==TRUE){
      df <- read.delim(import.files[i], header=TRUE, stringsAsFactors = FALSE)
      }
    if (stringr::str_detect(import.files[i], ".xls")==TRUE){
      requireNamespace("gdata", quietly = TRUE)
      df <- gdata::read.xls(import.files[i])
    }
  database <- detect_database(df)

  if (database=="Scopus"){
    df <- dplyr::select(df,
                        id=EID,
                        title=Title,
                        abstract=Abstract,
                        keywords=Author.Keywords,
                        type=Document.Type,
                        authors=X...Authors,
                        affiliation=Affiliations,
                        source=Source.title,
                        year=Year,
                        volume=Volume,
                        issue=Issue,
                        startpage=Page.start,
                        endpage=Page.end,
                        doi=DOI
    )
    df$methods <- rep("", length(df$id))
    df$language <- rep("", length(df$id))
    df$text <- paste(df$abstract, df$keywords, sep=" ")
  }
  if (database=="ZooRec"){
    df <- dplyr::select(df,
                        id=AN,
                        title=TI,
                        abstract=AB,
                        keywords=DE,
                        type=DT,
                        authors=AU,
                        affiliation=C1,
                        source=SO,
                        year=PY,
                        volume=VL,
                        issue=IS,
                        startpage=PS,
                        doi=DI,
                        language=LA)
    temp <- strsplit(as.character(df$startpage), "-")
    if (length(temp) >0){
      for (i in 1:length(temp)){
        df$startpage[i] <- temp[[i]][1]
        if (length(temp[[i]]) > 1){df$endpage[i] <- temp[[i]][2]}
      }
    }
    df$methods <- rep("", length(df$id))
    df$text <- paste(df$abstract, df$keywords, sep=" ")
  }
  if (database=="BIOSIS"){
    df <- dplyr::select(df,
                        id=UT,
                        title=TI,
                        abstract=AB,
                        keywords=MI,
                        methods=MQ,
                        type=DT,
                        authors=AU,
                        affiliation=C1,
                        source=SO,
                        year=PY,
                        volume=VL,
                        issue=IS,
                        startpage=BP,
                        endpage=EP,
                        doi=DI,
                        language=LA)
    df$text <- paste(df$abstract, df$keywords, sep=" ")
  }
  if (database=="ProQuest"){
    df <- dplyr::select(df,
                        id=StoreId,
                        title=Title,
                        abstract=Abstract,
                        keywords=subjectTerms,
                        altkeys=subjects,
                        type=documentType,
                        authors=Authors,
                        affiliation=AuthorAffiliation,
                        source=pubtitle,
                        year=year,
                        volume=volume,
                        issue=issue,
                        startpage=pages,
                        doi=digitalObjectIdentifier,
                        language=language)
    temp <- strsplit(as.character(df$startpage), "-")
    if (length(temp) >0){
      for (i in 1:length(temp)){
        df$startpage[i] <- temp[[i]][1]
        if (length(temp[[i]]) > 1){df$endpage[i] <- temp[[i]][2]}
      }
    }
    df$methods <- rep("", length(df$id))
    df$keywords <- paste(df$keywords, df$altkeys, sep=";")
    df$text <- paste(df$abstract, df$keywords, sep=" ")
  }

  df$database <- rep(database, nrow(df))

  df <- dplyr::select(df, id, text, title, abstract, keywords,
                      methods, type, authors, affiliation, source,
                      year, volume, issue, startpage, endpage, doi, language,
                      database)

  if (i == 1){search_hits <- df}
  if (i > 1){search_hits <- rbind(search_hits, df)}

  }

  if (save_full_dataset==TRUE){write.csv(search_hits, "./full_dataset.csv")}
  if (remove_duplicates==TRUE){search_hits <- deduplicate(search_hits)}
  if (clean_dataset==TRUE){search_hits <- clean_keywords(search_hits)}

  return(search_hits)

}


#' Remove duplicate articles
#' @description Uses similarity of tokenized abstracts and titles to detect duplicates and remove them from the dataset.
#' @param df a data frame created with import_scope()
#' @param doc_sim the minimum similarity between two abstracts to be marked as duplicated
#' @param title_sim the minimum similarity between two titles to be marked as duplicated
#' @param mean_sim the minimum mean similarity of abstract and title similarity to be marked as duplicated
#' @return a data frame with duplicates removed
deduplicate <- function(df, doc_sim=.85, title_sim=.95, mean_sim=.8){
  require(quanteda, quietly=TRUE)
  full_dfm <- quanteda::dfm(make_corpus(df),
                            remove = stopwords("english"),
                            remove_numbers=TRUE,
                            remove_punct=TRUE,
                            remove_symbols=TRUE,
                            remove_separators=TRUE,
                            remove_twitter=TRUE,
                            remove_hyphens=TRUE,
                            remove_url=TRUE)
  dfm_similarity <- quanteda::textstat_simil(full_dfm, margin = "documents")

  sim_mat <- as.matrix(dfm_similarity)
  sim_mat[lower.tri(sim_mat, diag=TRUE)] <- NA
  sim_mat <- as.data.frame(sim_mat)

  indices <- data.frame(ind = which(sim_mat > 0.5, arr.ind=TRUE))
  indices$doc1 <- rownames(sim_mat)[indices$ind.row]
  indices$doc2 <- colnames(sim_mat)[indices$ind.col]
  indices$sim_score <- sim_mat[which(sim_mat > 0.5, arr.ind=TRUE)]
  indices$title1 <- df$title[indices$ind.row]
  indices$title2 <- df$title[indices$ind.col]
  indices$title_sim <- rep(NA, nrow(indices))

  for (i in 1:nrow(indices)){
    check_corpus <- quanteda::corpus(c(indices$title1[i], indices$title2[i]))
    check_dfm <- quanteda::dfm(check_corpus,
                               remove_numbers=TRUE,
                               remove_punct=TRUE,
                               remove_symbols=TRUE,
                               remove_separators=TRUE,
                               remove_twitter=TRUE,
                               remove_hyphens=TRUE,
                               remove_url=TRUE)
    check_sim <- quanteda::textstat_simil(check_dfm, method="cosine")
    indices$title_sim[i] <- as.numeric(check_sim)

  }

  indices$mean_similarity <- (indices$sim_score + indices$title_sim)/2

  remove_by_title <- which(indices$title_sim > title_sim)
  remove_by_doc <- which(indices$sim_score > doc_sim)
  remove_by_mean <- which(indices$mean_similarity > mean_sim)
  remove_these <- append(remove_by_doc, c(remove_by_title, remove_by_mean))
  remove_docs <- sort(unique(as.numeric(gsub("text", "", indices$doc2[remove_these]))))

  new_data <- df[-c(remove_docs),]

  return(new_data)
}

#' Remove duplicate studies and punctuation
#' @description Replaces all miscellaneous punctuation marks used to separate keywords and replaces them with a semicolon so that keywords properly separate in later steps.
#' @param df a data frame from import_scope() to deduplicate
#' @return a data frame with keyword punctuation standardized
clean_keywords <- function(df){
  df$keywords <- tolower(as.character(df$keywords))
  removals <- c("\\(",
                "\\)",
                ":",
                "=",
                "%",
                "\\+",
                "<",
                ">",
                "\\?",
                "\\\\",
                "&",
                "!",
                "\\$",
                "\\*"
  )
  for (i in 1:length(removals)){
    df$keywords <- gsub(removals[i], df$keywords, replacement="")
  }

  # replace keyword separators with standardized semicolon
  replacements <- c(", ",
                    ",",
                    "/",
                    ";;",
                    ", ",
                    "\\[",
                    "\\]"
  )
  for (i in 1:length(replacements)){
    df$keywords <- gsub(replacements[i], df$keywords, replacement=";")
  }

  return(df)
}


