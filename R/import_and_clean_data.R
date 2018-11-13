
#' Detects which database a search is from
#' @description Uses the column names from databases to identify which database a search is from. This function can detect searches done in Web of Science databases (BIOSIS, Zoological Record, or MEDLINE), Scopus, and any EBSCO-indexed database.
#' @param df an exported dataset from any supported database
#' @return a character vector with the name of the database or an error that the database was not identified
detect_database <- function(df){
  database <- ""
  database_signature <- paste(df[1,], collapse=" ")
  if(stringr::str_detect(database_signature, "ZOOREC:ZOOR")){database <- "ZooRec"}
  if(stringr::str_detect(database_signature, "BCI:BCI")){database <- "BIOSIS"}
  if(stringr::str_detect(database_signature, "www.scopus.com")){database <- "Scopus"}
  if(stringr::str_detect(database_signature, "search.proquest.com")){database <- "ProQuest"}
  if(stringr::str_detect(database_signature, "ebscohost.com")){database <- "EBSCO"}
  if(stringr::str_detect(database_signature, "NDLTD_import")){database <- "NDLTD"}
  if(stringr::str_detect(database_signature, "OATD_import")){database <- "OATD"}
  if(stringr::str_detect(database_signature, "OpenThesis_import")){database <- "OpenThesis"}

  if (length(database)>0){return(database)}

  if (length(database)==0){
    database <- "Unknown"
    print("Database format not recognized.")
  }

}

#' Import results of a scoping search
#' @description Imports the results of a scoping search, combines them into a single dataset, and (optionally) removes duplicate hits based on document similarity. Duplicates can be removed subsequently with custom similarity cutoffs using deduplicate() on the full dataset.
#' @param directory the full path to the directory in which the searches are saved
#' @param remove_duplicates if TRUE, removes duplicates based on document similarity
#' @param clean_dataset if TRUE, removes excess punctuation and standardizes keywords
#' @param save_full_dataset if TRUE, saves a .csv of the full dataset in the working directory
#' @param verbose if TRUE, prints which file is currently being imported
#' @return a data frame of all the search results combined
import_naive <- function(directory, remove_duplicates = TRUE, clean_dataset = TRUE,
                         save_full_dataset = FALSE, verbose = TRUE){
  if(save_full_dataset==TRUE){
    if(utils::menu(c("yes", "no"),
            title="This will save the full dataset to a .csv file in your working directory. Do you want litsearchr to save the full dataset?")==2){
      save_full_dataset <- FALSE
    }
  }
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

    if (stringr::str_detect(paste(colnames(df), collapse=" "), "\\.\\.")){
      temp_cn <- strsplit(as.character(colnames(df)[1]), "\\.\\.")
      if (length(temp_cn[[1]]) > 1) {
        colnames(df)[1] <- temp_cn[[1]][2]
      }
    }
    if(length(which(colnames(df)=="X"))>0){df <- df[, -which(colnames(df)=="X")]}

    if(verbose==TRUE){print(paste("Importing file", import_files[i]))}
    database <- c()
    database <- detect_database(df)
    if(database == "Scopus"){
      df <- as.data.frame(cbind(id = df$EID, title = df$Title,
                                abstract = df$Abstract, keywords = df$Author.Keywords,
                                type = df$Document.Type, authors = df$Authors,
                                affiliation = df$Affiliations, source = df$Source.title,
                                year = df$Year, volume = df$Volume, issue = df$Issue,
                                startpage = df$Page.start, endpage = df$Page.end,
                                doi = df$DOI))
      df$methods <- rep("", length(df$id))
      df$language <- rep("", length(df$id))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "ZooRec"){
      df <- as.data.frame(cbind(id = df$AN, title = df$TI,
                                abstract = df$AB, keywords = df$DE, type = df$DT,
                                authors = df$AU, affiliation = df$C1, source = df$SO,
                                year = df$PY, volume = df$VL, issue = df$IS,
                                startpage = df$PS, doi = df$DI, language = df$LA))
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
    if(database == "BIOSIS"){
      df <- as.data.frame(cbind(id = df$UT, title = df$TI,
                                abstract = df$AB, methods = df$MQ, keywords = df$MI,
                                type = df$DT, authors = df$AU, affiliation = df$C1,
                                source = df$SO, year = df$PY, volume = df$VL,
                                issue = df$IS, startpage = df$BP, endpage = df$EP,
                                doi = df$DI, language = df$LA))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "MEDLINE"){
      df <- as.data.frame(cbind(id = df$AN, title = df$TI,
                                abstract = df$AB, keywords = df$ID, type = df$DT,
                                authors = df$AU, affiliation = df$C1, source = df$SO,
                                year = df$Y, volume = df$VL, issue = df$IS, startpage = df$PS,
                                doi = df$DI, language = df$LA))
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
    if(database == "EBSCO"){
      df <- as.data.frame(cbind(id = df$Accession.Number,
                                title = df$Article.Title, abstract = df$Abstract,
                                authors = df$Author, source = df$Journal.Title,
                                year = df$Publication.Date, volume = df$Volume,
                                issue = df$Issue, startpage = df$First.Page,
                                endpage = df$Page.Count, doi = df$DOI, keywords = df$Keywords,
                                type = df$Doctype))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "NDLTD"){
      df <- as.data.frame(cbind(title=df$title, authors=df$author,
                                year=df$date, abstract=df$abstract))
      df$id <- rep("", nrow(df))
      df$source <- rep("", nrow(df))
      df$volume <- rep("", nrow(df))
      df$issue <- rep("", nrow(df))
      df$startpage <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$doi <- rep("", nrow(df))
      df$keywords <- rep("", nrow(df))
      df$type <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if (database == "OATD"){
      df <- as.data.frame(cbind(title=df$title, authors=df$author,
                                abstract=df$abstract))
      df$id <- rep("", nrow(df))
      df$source <- rep("", nrow(df))
      df$volume <- rep("", nrow(df))
      df$issue <- rep("", nrow(df))
      df$startpage <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$doi <- rep("", nrow(df))
      df$keywords <- rep("", nrow(df))
      df$type <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$year <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if (database == "OpenThesis"){
      df <- as.data.frame(cbind(title=df$title, authors=df$author,
                                year=df$date))
      df$id <- rep("", nrow(df))
      df$abstract <- rep("", nrow(df))
      df$source <- rep("", nrow(df))
      df$volume <- rep("", nrow(df))
      df$issue <- rep("", nrow(df))
      df$startpage <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$doi <- rep("", nrow(df))
      df$keywords <- rep("", nrow(df))
      df$type <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if (database=="ProQuest"){
      df <- as.data.frame(cbind(id=df$StoreId, title=df$Title, abstract=df$Abstract, keywords=df$subjectTerms, type=df$documentType,
                                authors=df$Authors, source=df$pubtitle, year=df$year, volume=df$volume, issue=df$issue,
                                startpage=df$pages, doi=df$digitalObjectIdentifier, language=df$language))
      df$affiliation <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))

      df$startpage <- as.character(df$startpage)
      temp <- strsplit(as.character(df$startpage), "-")
      if (length(temp) > 0) {
        for (j in 1:length(temp)) {
          if(length(temp[[j]])>0){
            df$startpage[j] <- temp[[j]][1]
          }
          if (length(temp[[j]]) > 1) {
            df$endpage[j] <- temp[[j]][2]
          }
        }
      }

      df$text <- paste(df$abstract, df$keywords, sep=" ")
    }


    if (database != "Unknown") {
      df$database <- rep(database, nrow(df))
      df[] <- lapply(df, as.character)
      df <- as.data.frame(cbind(id = df$id, text = df$text,
                                title = df$title, abstract = df$abstract, keywords = df$keywords,
                                methods = df$methods, type = df$type, authors = df$authors,
                                affiliation = df$affiliation, source = df$source,
                                year = df$year, volume = df$volume, issue = df$issue,
                                startpage = df$startpage, endpage = df$endpage,
                                doi = df$doi, language = df$language, database = df$database))
      df[] <- lapply(df, as.character)
      if (i == 1) {
        search_hits <- df
      }
      if (i > 1) {
        search_hits <- rbind(search_hits, df)
      }
    }
    if(database=="Unknown"){
      print(paste("Warning: Unable to recognize format for", import_files[i]))
    }
  }


  if (save_full_dataset == TRUE) {
    write.csv(search_hits, "./full_dataset.csv")
    print("Complete dataset written to .csv file.")
  }
  if (remove_duplicates == TRUE) {
    print("Removing duplicates.")
    search_hits <- deduplicate(search_hits)
  }
  if (clean_dataset == TRUE) {
    print("Cleaning dataset.")
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


