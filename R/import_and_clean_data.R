#' Detects which database a search is from
#' @description Uses the column names from databases to identify which database a search is from. This function can detect searches done in BIOSIS, Zoological Record, Web of Science with "All Databases" selected, Scopus, and any EBSCO-indexed database.
#' @param df an exported dataset from any supported database
#' @return a character vector with the name of the database or an error that the database was not identified
detect_database <- function(df){
  database <- ""
  database_signature <- paste(colnames(df)[-1], collapse=" ")
  database <- names(litsearchr::importable_databases)[which(stringr::str_detect(litsearchr::importable_databases, database_signature)==TRUE)]

  if (length(database)==0){print("Database format not recognized.")}

  if (length(database)>0){return(database)}
}

#' Import results of a scoping search
#' @description Imports the results of a scoping search, combines them into a single dataset, and (optionally) removes duplicate hits based on document similarity. Duplicates can be removed subsequently with custom similarity cutoffs using deduplicate() on the full dataset.
#' @param directory the full path to the directory in which the searches are saved
#' @param remove_duplicates if TRUE, removes duplicates based on document similarity
#' @param clean_dataset if TRUE, removes excess punctuation and standardizes keywords
#' @param save_full_dataset if TRUE, saves a .csv of the full dataset in the working directory
#' @return a data frame of all the search results combined
import_scope <- function (directory, remove_duplicates = TRUE, clean_dataset = TRUE, save_full_dataset = FALSE) {
  import_files <- paste(directory, list.files(path = directory),
                        sep = "")
  for (i in 1:length(import_files)) {
    df <- c()
    if (stringr::str_detect(import_files[i], ".csv") == TRUE) {
      df <- read.csv(import_files[i], header = TRUE, stringsAsFactors = FALSE)
    }
    if (stringr::str_detect(import_files[i], ".txt") == TRUE) {
      df <- read.table(import_files[i], sep = "\t", header = TRUE,
                       comment.char = "#", na.strings = ".", stringsAsFactors = FALSE,
                       quote = "", fill = TRUE)
    }

    colnames(df) <- gsub("\\.\\.\\.", "\\.\\.", colnames(df))

    temp_cn <- strsplit(colnames(df)[1], "\\.\\.")[[1]]
    if (length(temp_cn)>1){
      colnames(df)[1] <- temp_cn[2]
    }

    if (colnames(df)[length(colnames(df))] == "X") {
      df <- df[, -length(colnames(df))]
    }
    database <- c()
    database <- detect_database(df)
    if (database == "Scopus") {
      df <- as.data.frame(cbind(id = df$EID,
                                title = df$Title,
                                abstract = df$Abstract,
                                keywords = df$Author.Keywords,
                                type = df$Document.Type,
                                authors = df$Authors,
                                affiliation = df$Affiliations,
                                source = df$Source.title,
                                year = df$Year,
                                volume = df$Volume,
                                issue = df$Issue,
                                startpage = df$Page.start,
                                endpage = df$Page.end,
                                doi = df$DOI))
      df$methods <- rep("", length(df$id))
      df$language <- rep("", length(df$id))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if (database == "ZooRec") {
      df <- as.data.frame(cbind(id = df$AN,
                                title = df$TI,
                                abstract = df$AB,
                                keywords = df$DE,
                                type = df$DT,
                                authors = df$AU,
                                affiliation = df$C1,
                                source = df$SO,
                                year = df$PY,
                                volume = df$VL,
                                issue = df$IS,
                                startpage = df$PS,
                                doi = df$DI,
                                language = df$LA))
      df$startpage <- as.character(df$startpage)
      temp <- strsplit(as.character(df$startpage), "-")
      if (length(temp) > 0) {
        for (j in 1:length(temp)) {
          df$startpage[j] <- temp[[j]][1]
          if (length(temp[[j]]) > 1) {
            df$endpage[j] <- temp[[j]][2]
          }
        }
      }
      df$methods <- rep("", length(df$id))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if (database == "BIOSIS") {
      df <- as.data.frame(cbind(id = df$UT,
                                title = df$TI,
                                abstract = df$AB,
                                methods = df$MQ,
                                keywords = df$MI,
                                type = df$DT,
                                authors = df$AU,
                                affiliation = df$C1,
                                source = df$SO,
                                year = df$PY,
                                volume = df$VL,
                                issue = df$IS,
                                startpage = df$BP,
                                endpage = df$EP,
                                doi = df$DI,
                                language = df$LA))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if (database == "MEDLINE") {
      df <- as.data.frame(cbind(id = df$AN,
                                title = df$TI,
                                abstract = df$AB,
                                keywords = df$ID,
                                type = df$DT,
                                authors = df$AU,
                                affiliation = df$C1,
                                source = df$SO,
                                year = df$Y,
                                volume = df$VL,
                                issue = df$IS,
                                startpage = df$PS,
                                doi = df$DI,
                                language = df$LA))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
      df$methods <- rep("", length(df$id))
      temp <- strsplit(as.character(df$startpage), "-")
      if (length(temp) > 0) {
        for (j in 1:length(temp)) {
          df$startpage[j] <- temp[[j]][1]
          if (length(temp[[j]]) > 1) {
            df$endpage[j] <- temp[[j]][2]
          }
        }
      }
    }
    if (database == "WoS") {
      df <- as.data.frame(cbind(id = df$UT,
                                title = df$TI,
                                abstract = df$AB,
                                authors = df$AU,
                                source = df$SO,
                                year = df$PY,
                                volume = df$VL,
                                issue = df$IS,
                                startpage = df$BP,
                                endpage = df$EP,
                                doi = df$DI))
      df$keywords <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$type <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if (database == "EBSCO") {
      df <- as.data.frame(cbind(id = df$Accession.Number,
                                title = df$Article.Title,
                                abstract = df$Abstract,
                                authors = df$Author,
                                source = df$Journal.Title,
                                year = df$Publication.Date,
                                volume = df$Volume,
                                issue = df$Issue,
                                startpage = df$First.Page,
                                endpage = df$Page.Count,
                                doi = df$DOI,
                                keywords = df$Keywords,
                                type = df$Doctype))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if (length(database)>0){

      df$database <- rep(database, nrow(df))
      df <- as.data.frame(
        cbind(id=df$id, text=df$text, title=df$title, abstract=df$abstract, keywords=df$keywords,
              methods=df$methods, type=df$type, authors=df$authors, affiliation=df$affiliation, source=df$source, year=df$year,
              volume=df$volume, issue=df$issue, startpage=df$startpage, endpage=df$endpage, doi=df$doi, language=df$language,
              database=df$database))
      df[] <- lapply(df, as.character)
      if (i == 1) {
        search_hits <- df
      }
      if (i > 1) {
        search_hits <- rbind(search_hits, df)
      }
    }
  }
  if (save_full_dataset == TRUE) {
    write.csv(search_hits, "./full_dataset.csv")
  }
  if (remove_duplicates == TRUE) {
    search_hits <- deduplicate(search_hits)
  }
  if (clean_dataset == TRUE) {
    search_hits <- clean_keywords(search_hits)
  }
  return(search_hits)
}


#' Remove duplicate articles
#' @description Uses similarity of tokenized abstracts and titles to detect duplicates and remove them from the dataset.
#' @param df a data frame created with import_scope()
#' @param use_abstracts if TRUE, tokenizes and computes similarity scores for all abstracts
#' @param use_titles if TRUE, computes similarity based on titles
#' @param title_method if "tokens", tokenizes titles to compute similarity; if "quick", removes punctuation and capitalization then removes exact duplicates
#' @param doc_sim the minimum similarity between two abstracts to be marked as duplicated
#' @param title_sim the minimum similarity between two titles to be marked as duplicated
#' @return a data frame with duplicates removed
deduplicate <- function(df, use_abstracts=TRUE, use_titles=TRUE, title_method="tokens", doc_sim=.85, title_sim=.95){

  remove_abstracts <- c()
  remove_titles <- c()

  if (use_abstracts==TRUE){
    dfA <- as.data.frame(cbind(id=as.character(df$id), text=as.character(df$text)))
    dfA$text <- as.character(dfA$text)
    full_dfm <- quanteda::dfm(make_corpus(dfA),
                              remove = quanteda::stopwords("english"),
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

    indices <- data.frame(ind = which(sim_mat > doc_sim, arr.ind=TRUE))
    indices$doc1 <- rownames(sim_mat)[indices$ind.row]
    indices$doc2 <- colnames(sim_mat)[indices$ind.col]
    remove_abstracts <- sort(unique(as.numeric(gsub("text", "", indices$doc2))))
  }

  if (use_titles==TRUE){
    if (title_method=="quick"){
      remove_titles <- which(duplicated(tolower(tm::removePunctuation(df$title)))==TRUE)
    }

    if (title_method=="tokens"){

      dfT <- as.data.frame(cbind(id=as.character(df$id), text=as.character(df$title)))
      dfT$text <- as.character(dfT$text)
      full_dfm <- quanteda::dfm(make_corpus(dfT),
                                remove = quanteda::stopwords("english"),
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

      indices <- data.frame(ind = which(sim_mat > title_sim, arr.ind=TRUE))
      indices$doc1 <- rownames(sim_mat)[indices$ind.row]
      indices$doc2 <- colnames(sim_mat)[indices$ind.col]
      remove_titles <- sort(unique(as.numeric(gsub("text", "", indices$doc2))))

    }
  }

  remove_documents <- unique(append(remove_abstracts, remove_titles))

  if (length(remove_documents) > 0){new_data <- df[-c(remove_documents),]}
  if (length(remove_documents) == 0){new_data <- df}

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


