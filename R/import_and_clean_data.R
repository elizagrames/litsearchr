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

deduplicate <- function(df){
  dedupe <- paste(df$source, df$volume, df$startpage, tolower(substring(df$title, 1, 5)))
  df <- df[which(duplicated(dedupe)==FALSE), ]
  return(df)
}

#' Remove duplicate studies and punctuation
#' @description This function removes entries from the database that are duplicated articles (e.g. articles that were returned by two different databases) based on the first 40 characters of the title. It also replaces all miscellaneous punctuation marks used to separate keywords and replaces them with a semicolon so that keywords properly separate in later steps.
#' @param df The dataset you want to deduplicate.
#' @param chars How many characters you want to include for deduplication (default is 40).
#' @return The cleaned dataset.
clean_keywords <- function(df, chars=40){
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


