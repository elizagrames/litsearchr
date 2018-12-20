
#' Detects which database a search is from
#' @description Uses the column names from databases to identify which database a search is from. This function can detect searches done in Web of Science databases (BIOSIS, Zoological Record, or MEDLINE), Scopus, and any EBSCO-indexed database.
#' @param df an exported dataset from any supported database
#' @return a character vector with the name of the database or an error that the database was not identified
#' @examples detect_database(df=scopus_example)
detect_database <- function(df){
  database <- ""
  database_signature <- paste(df[1,], collapse=" ")
  if(stringr::str_detect(database_signature, "ZOOREC:ZOOR")){database <- "ZooRec"}
  if(stringr::str_detect(database_signature, "BCI:BCI")){database <- "BIOSIS"}
  if(stringr::str_detect(database_signature, "www.scopus.com")){database <- "Scopus"}
  if(stringr::str_detect(database_signature, "search.proquest.com")){database <- "ProQuest"}
  if(stringr::str_detect(database_signature, "ebscohost.com")){database <- "EBSCO"}

  if(stringr::str_detect(database_signature, "ndltd_scrape")){database <- "NDLTD"}
  if(stringr::str_detect(database_signature, "oatd_scrape")){database <- "OATD"}
  if(stringr::str_detect(database_signature, "openthesis_scrape")){database <- "OpenThesis"}

  if(stringr::str_detect(database_signature, "googlescholar_scrape")){database <- "googlescholar_scrape"}
  if(stringr::str_detect(database_signature, "jstor_scrape")){database <- "jstor_scrape"}
  if(stringr::str_detect(database_signature, "scopus_scrape")){database <- "scopus_scrape"}
  if(stringr::str_detect(database_signature, "cabdirect_scrape")){database <- "cabdirect_scrape"}
  if(stringr::str_detect(database_signature, "ingenta_scrape")){database <- "ingenta_scrape"}
  if(stringr::str_detect(database_signature, "pubmed_scrape")){database <- "pubmed_scrape"}
  if(stringr::str_detect(database_signature, "worldcat_scrape")){database <- "worldcat_scrape"}
  if(stringr::str_detect(database_signature, "wos_scrape")){database <- "wos_scrape"}

  if (length(database)>0){return(database)}

  if (length(database)==0){
    database <- "Unknown"
    print("Database format not recognized.")
  }

}

#' Print databases/platform exports that litsearchr can import or search in
#' @description Prints a data frame of platforms, databases, and download methods that litsearchr recognizes
#' @examples usable_databases()
usable_databases <- function(){
  print(litsearchr::database_list)
}

#' Import results of a search
#' @description Imports the results of a search, combines them into a single dataset, and (optionally) removes duplicate hits based on document similarity. Duplicates can be removed subsequently with custom similarity cutoffs using deduplicate() on the full dataset.
#' @param directory the full path to the directory in which the searches are saved
#' @param remove_duplicates if TRUE, removes duplicates based on document similarity
#' @param clean_dataset if TRUE, removes excess punctuation and standardizes keywords
#' @param save_full_dataset if TRUE, saves a .csv of the full dataset in the working directory
#' @param verbose if TRUE, prints which file is currently being imported
#' @param save_directory the directory to save results to if save_full_dataset=TRUE
#' @param duplicate_methods the method to pass to deduplicate() if remove_duplicates is TRUE
#' @return a data frame of all the search results combined
#' @example inst/examples/import_results.R
import_results <- function(directory, remove_duplicates = FALSE, duplicate_methods=c("tokens", "quick", "levenshtein"), clean_dataset = TRUE,
                           save_full_dataset = FALSE, verbose = TRUE, save_directory="./"){
  if(save_full_dataset==TRUE){
    if(utils::menu(c("yes", "no"),
                   title="This will save the full dataset to a .csv file in your working directory. Do you want litsearchr to save the full dataset?")==2){
      save_full_dataset <- FALSE
    }
  }

  import_files <- paste(directory, list.files(path = directory),
                        sep = "")

  for(i in 1:length(import_files)){
    if(i==1){removals <- c()}
    if(stringr::str_detect(import_files[i], ".csv")){}else{
      if(stringr::str_detect(import_files[i], ".txt")){}else{
        if(stringr::str_detect(import_files[i], ".xls")==FALSE){
          print(paste("File format is not recognized. Skipping", import_files[i]))
          removals <- append(removals, i)}}
    }
    if(i==length(import_files)){
      if(length(removals) > 0){
      import_files <- import_files[-removals]
      }
    }
  }

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
    if (stringr::str_detect(import_files[i], ".xls") == TRUE) {
      df <- xlsx::read.xlsx(import_files[i], 1)
      df[] <- lapply(df, function(x) if(is.factor(x)) as.character(x) else x)
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
    database <- litsearchr::detect_database(df)
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
      df$endpage <- rep("", nrow(df))
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

    if(database == "googlescholar_scrape"){
      df <- as.data.frame(cbind(title=df$title, authors=df$authors,
                                year=df$year, source=df$journal))
      df$id <- rep("", nrow(df))
      df$abstract <- rep("", nrow(df))
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
    if(database == "jstor_scrape"){
      df <- as.data.frame(cbind(title=df$title, authors=df$authors,
                                source=df$journal, keywords=df$keyword,
                                type=df$type, doi=df$doi, volume=df$pubinfo))
      df$id <- rep("", nrow(df))
      df$issue <- rep("", nrow(df))
      df$startpage <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$year <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$abstract <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "scopus_scrape"){
      df <- as.data.frame(cbind(id=df$id,
                                title=df$title,
                                authors=df$authors,
                                year=df$year,
                                source=df$publication,
                                volume=df$volume,
                                issue=df$issue,
                                startpage=df$pages,
                                doi=df$doi))
      df$abstract <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$keywords <- rep("", nrow(df))
      df$type <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "cabdirect_scrape"){
      df <- as.data.frame(cbind(title=df$title, authors=df$authors,
                                source=df$source))
      df$id <- rep("", nrow(df))
      df$year <- rep("", nrow(df))
      df$issue <- rep("", nrow(df))
      df$abstract <- rep("", nrow(df))
      df$startpage <- rep("", nrow(df))
      df$volume <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$doi <- rep("", nrow(df))
      df$keywords <- rep("", nrow(df))
      df$type <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "ingenta_scrape"){
      df <- as.data.frame(cbind(title=df$title, authors=df$authors,
                                year=df$date, abstract=df$abstract, doi=df$doi, volume=df$pubinfo))
      df$id <- rep("", nrow(df))
      df$source <- rep("", nrow(df))
      df$issue <- rep("", nrow(df))
      df$startpage <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$keywords <- rep("", nrow(df))
      df$type <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "pubmed_scrape"){
      df <- as.data.frame(cbind(title=df$title, authors=df$authors,
                                year=df$year, abstract=df$abstract,
                                id=df$id, keywords=df$keywords, source=df$source, volume=df$volume, doi=df$doi))
      df$issue <- rep("", nrow(df))
      df$startpage <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$type <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$language <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "worldcat_scrape"){
      df <- as.data.frame(cbind(title=df$title, authors=df$authors,
                                year=df$date, abstract=df$abstract,
                                type=df$type, language=df$language,
                                source=df$publication, volume=df$pubinfo))
      df$id <- rep("", nrow(df))
      df$issue <- rep("", nrow(df))
      df$startpage <- rep("", nrow(df))
      df$endpage <- rep("", nrow(df))
      df$doi <- rep("", nrow(df))
      df$keywords <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$affiliation <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }
    if(database == "wos_scrape"){
      df <- as.data.frame(cbind(id = df$id,
                                title = df$title,
                                abstract = df$abstract,
                                keywords = df$keywords,
                                type = df$type,
                                authors = df$authors,
                                affiliation = df$address,
                                source = df$publication,
                                year = df$date,
                                volume = df$volume,
                                issue = df$issue,
                                startpage = df$pgs,
                                doi = df$doi,
                                language = df$language))
      df$endpage <- rep("", nrow(df))
      df$methods <- rep("", nrow(df))
      df$text <- paste(df$abstract, df$keywords, sep = " ")
    }

    if (database != "Unknown") {
      df$database <- rep(database, nrow(df))
      df[] <- lapply(df, as.character)
      df <- as.data.frame(cbind(id = df$id,
                                text = df$text,
                                title = df$title,
                                abstract = df$abstract,
                                keywords = df$keywords,
                                methods = df$methods,
                                type = df$type,
                                authors = df$authors,
                                affiliation = df$affiliation,
                                source = df$source,
                                year = df$year,
                                volume = df$volume,
                                issue = df$issue,
                                startpage = df$startpage,
                                endpage = df$endpage,
                                doi = df$doi,
                                language = df$language,
                                database = df$database))
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
    write.csv(search_hits, paste(save_directory, "full_dataset.csv", sep=""))
    print("Complete dataset written to .csv file.")
  }
  if (remove_duplicates == TRUE) {
    print("Removing duplicates.")
    search_hits <- deduplicate(search_hits, method = duplicate_methods)
  }
  if (clean_dataset == TRUE) {
    print("Cleaning dataset.")
    search_hits <- clean_keywords(search_hits)
  }
  return(search_hits)
}

#' Remove duplicate articles
#' @description Uses similarity of abstracts and titles to detect duplicates and remove them from the dataset.
#' @param df a data frame created with import_scope()
#' @param use_abstracts if TRUE, computes similarity scores for all abstracts
#' @param use_titles if TRUE, computes similarity based on titles
#' @param method if "tokens", tokenizes titles to compute similarity; if "quick", removes punctuation and capitalization then removes exact duplicates; if "levenshtein", uses levenshtein distances to compute similarity
#' @param doc_sim the minimum similarity between two abstracts to be marked as duplicated
#' @param title_sim the minimum similarity between two titles to be marked as duplicated
#' @return a data frame with duplicates removed
#' @examples deduplicate(BBWO_import, use_abstracts=FALSE, use_titles=TRUE, method="tokens", title_sim=.95)
deduplicate <- function(df, use_abstracts=TRUE, use_titles=TRUE, method=c("quick", "levenshtein", "tokens"), doc_sim=.85, title_sim=.95){

  remove_abstracts <- c()
  remove_titles <- c()

  if (use_abstracts==TRUE){

    if(method=="tokens"){
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

    if(method=="quick"){
      remove_abstracts <- which(duplicated(tolower(tm::removePunctuation(df$text)))==TRUE)
    }

    if(method=="levenshtein"){
      lev_sim <- utils::adist(df$text)
      x <- df$text
      y <- df$text

      for(i in 1:length(x)){
        x[i] <- paste(quanteda::tokens_remove(quanteda::tokens(x[i]), litsearchr::custom_stopwords), collapse=" ")
        for(j in 1:length(y)){
          y[j] <- paste(quanteda::tokens_remove(quanteda::tokens(y[j]), litsearchr::custom_stopwords), collapse=" ")
          lev_sim[i,j] <- 1 - utils::adist(x[i], y[j])/max(nchar(x[i]), nchar(y[j]))
        }
      }

      lev_sim[lower.tri(lev_sim, diag=TRUE)] <- NA

      indices <- data.frame(ind = which(lev_sim > doc_sim, arr.ind=TRUE))

      remove_abstracts <- sort(unique(as.numeric(indices$ind.col)))
    }
  }



  if (use_titles==TRUE){
    if (method=="quick"){
      remove_titles <- which(duplicated(tolower(tm::removePunctuation(df$title)))==TRUE)
    }

    if (method=="tokens"){

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
    if(method=="levenshtein"){
      lev_sim <- utils::adist(df$title)
      x <- df$title
      y <- df$title

      for(i in 1:length(x)){
        x[i] <- paste(quanteda::tokens_remove(quanteda::tokens(x[i]), litsearchr::custom_stopwords), collapse=" ")
        for(j in 1:length(y)){
          y[j] <- paste(quanteda::tokens_remove(quanteda::tokens(y[j]), litsearchr::custom_stopwords), collapse=" ")
          lev_sim[i,j] <- 1 - utils::adist(x[i], y[j])/max(nchar(x[i]), nchar(y[j]))
        }
      }

      lev_sim[lower.tri(lev_sim, diag=TRUE)] <- NA

      indices <- data.frame(ind = which(lev_sim > title_sim, arr.ind=TRUE))

      remove_titles <- sort(unique(as.numeric(indices$ind.col)))
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
#' @examples clean_keywords(BBWO_data)
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

  df$keywords <- gsub("  ", " ", df$keywords)
  df$keywords <- gsub("; ", ";", df$keywords)
  df$keywords <- gsub(" ;", ";", df$keywords)
  df$keywords <- gsub(";;", ";", df$keywords)

  return(df)
}


