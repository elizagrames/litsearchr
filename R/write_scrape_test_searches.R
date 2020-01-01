#' Select non-English languages to search in
#' @description This function chooses the best non-English languages to conduct searches in based on the topic of the review. The topics query a database of non-English language journals compiled from Ulrich; currently only STEM fields are supported.
#' @param key_topics a character vector of topics related to the topic of the search
#' @return a data frame of languages used by journals tagged with the key topics and a count of how many journals use that language.
#' @examples  get_languages(c("ecology", "conservation", "ornithology"))
get_languages <- function(key_topics){

  #key_topics <- c("conservation", "ecology")
  langs <- litsearchr::ulrich$Language[which(rowSums(sapply(key_topics, grepl, litsearchr::ulrich$SubjectCodes))>0)]

  combined_langs <- paste(langs, collapse=" | ")

  all_langs <- synthesisr::remove_punctuation(strsplit(combined_langs, " | ")[[1]])

  remove_these <- c(which(all_langs=="null"),
                    which(all_langs=="Yes"),
                    which(all_langs=="Multiple"),
                    which(all_langs=="languages"),
                    which(all_langs==""),
                    which(all_langs==","),
                    which(all_langs=="in"))

if(any(remove_these)){
  all_langs <- all_langs[-unique(remove_these)]
}

  lang_data <- as.data.frame(sort(table(all_langs), decreasing = TRUE))
  lang_data$translatable <- lang_data[,1] %in% litsearchr::possible_langs$Language
  colnames(lang_data) <- c("language", "count", "translatable")

  return(lang_data)
}


#' Translate search terms
#' @param search_terms a character vector of search terms
#' @param target_language a character vector of the language(s) to translate the search to
#' @param source_language a character vector of the language the search terms are currently in
#' @param API_key an API key for Google Translate (not available through litsearchr)
#' @description Takes groups of search terms and translates them into target language using the Google Translate API. This function is intended for use inside write_search(), not as a standalone function.
#'@examples  \dontrun{translate_search(search_terms=c("black-backed woodpecker"), target_language="fr")}
translate_search <- function(search_terms, target_language, source_language="en", API_key=API_key){
  words <- search_terms

  termlist <- words

  if(nchar(target_language)>2){
    this_one <- which(grepl(target_language, litsearchr::possible_langs$Language))
  }else{this_one <- which(grepl(target_language, litsearchr::possible_langs$Short))}

  trans_lang <- as.character(litsearchr::possible_langs$Short[this_one])
  trans_encod <- as.character(litsearchr::possible_langs$Encoding[this_one])

  for (i in 1:length(words)){
    if (!requireNamespace("translate", quietly = TRUE)){
      stop("translate package needed in order to translate search terms. Please install it.",
           call. = FALSE)
    } else {
    termlist[i] <- translate::translate(words[i], source=source_language, target=trans_lang, key = API_key)[[1]]
  }}

  return(termlist)
}

#' Check whether a word is long enough to stem
#' @param word the word or phrase to check
#' @description Checks if the stemmed form of a word is longer than 3 characters. Not intended as a standalone function and is called from write_search().
#'@examples  should_stem("habitat fragmentation")
should_stem <- function(word){
  splitup <- strsplit(word, " ")[[1]]
  for(i in 1:length(splitup)){
    wordcut <- SnowballC::wordStem(splitup[i], language="en")
    stem_length <- nchar(wordcut)

    if(i==1){
      if(stem_length > 3){
        words <- paste(wordcut, "* ", sep="")
      }
      if(stem_length <= 3){
        words <- paste(splitup[i], " ", sep="")
      }
    }
    if(i > 1){
      if(stem_length > 3){
        words <- paste(words, wordcut, "* ", sep="")
      }
      if(stem_length <= 3){
        words <- paste(words, splitup[i], " ", sep="")
      }
    }
  }

    words <- stringr::str_trim(words)

  return(words)
}

#' Write Boolean searches
#' @description Takes search terms grouped by concept group and writes Boolean searches in which terms within concept groups are separated by "OR" and concept groups are separated by "AND". Searches can be written in up to 53 languages, though the function defaults to only searching the top ten most used languages in a discipline using the choose_languages() function. The default for language options relies on searching a database of journals by discipline based on Ulrich's Periodicals Directory. Only scientific fields are included in this database. All supported languages can be seen with available_languages().
#' @param groupdata a list of character vectors, each of which is a concept group
#' @param API_key your Google Translate API key
#' @param languages a character vector of supported languages to write searches in.
#' @param exactphrase if set to \code{TRUE}, stemmed search terms with multiple words will be enclosed in quotes
#' @param directory the path to the directory where you want to save searches (defaults to current working directory)
#' @param stemming if TRUE, writes stemmed search (only when the current language is English)
#' @param verbose if TRUE, prints when each language is finished writing
#' @param writesearch if TRUE, saves the searches to .txt files in the specified directory
#' @return a list of search strings
#' @example inst/examples/write_search.R
write_search <- function (groupdata, API_key = NULL, languages = NULL, exactphrase = FALSE,
                                directory = "./", stemming = TRUE, verbose = TRUE, writesearch = FALSE){
  if (writesearch == TRUE) {
    if (utils::menu(c("yes", "no"), title = "This is going to write .txt files to your computer containing the search strings. Are you sure you want to write the files?") ==
        2) {
      writesearch <- FALSE
    }
  }

  search_list <- list()
  length(search_list) <- length(languages)

  for(i in 1:length(languages)){

    group_list <- c()

    for(j in 1:length(groupdata)){
      group_terms <- litsearchr::remove_redundancies(groupdata[[j]])

      if(languages[i]!="English"){
        translated_terms <- litsearchr::translate_search(paste(group_terms, collapse="; "), target_language = languages[i], API_key = API_key)
        group_terms <- strsplit(translated_terms, "; ")[[1]]
      }

      if(languages[i]=="English"){
        if(stemming==TRUE){
          group_terms <- sapply(group_terms, litsearchr::should_stem)
        }
      }

      if(exactphrase==TRUE){
        for(k in 1:length(group_terms)){
          if(length(strsplit(group_terms[k], " ")[[1]])>1){
            group_terms[k] <- paste("\"", group_terms[k], "\"", sep="")
          }
        }
      }

      if(exactphrase==FALSE){
        for(k in 1:length(group_terms)){
          if(length(strsplit(group_terms[k], " ")[[1]])>1){
            group_terms[k] <- paste("\\(", group_terms[k], "\\)", sep="")
          }
        }
      }

      group_terms[1] <- paste("\\(", group_terms[1], sep="")
      group_terms[length(group_terms)] <- paste(group_terms[length(group_terms)], "\\)", sep="")
      full_group <- paste(group_terms, collapse=" OR ")

      group_list[j] <- full_group

      if(j==length(groupdata)){

        this_search <- paste(group_list, collapse=" AND ")
        this_search <- paste("\\(", this_search, "\\)", sep="")

        if(writesearch==TRUE){
          filename <- paste(directory, "search-in", languages[i], ".txt", sep="")
          writeLines(this_search, filename)
        }

        search_list[[i]] <- this_search
        names(search_list)[[i]] <- languages[i]

        if (verbose == TRUE) {
          print(paste(languages[i], "is written"))
        }

        }

      }

  }

  search_list <- gsub("&#39;", "'", search_list)
  search_list <- gsub("\\\\", "", search_list)
    return(search_list)

  }

#' Remove redundant terms
#' @description Given a list of terms, removes redundant terms based on plurals, stemming, or partial matches
#' @param terms a character vector of terms
#' @param closure restrictions on how matches are detected; left requires matches to start with a term (e.g "burn" matches "burning"), right requires matches to end with a term (e.g. "burn" matches "postburn" but not "postburning"), full requires exact matches (e.g. "burn" only matches "burn"), and none allows terms to be embedded within matches.
#' @return a character vector with redundant terms removed
remove_redundancies <- function(terms, closure=c("left", "right", "full", "none")){

  terms <- switch (closure,
          "left" = {terms <- paste("__", terms, sep="")},
          "right" = {terms <- paste(terms, "\\b", sep="")},
          "full" = {terms <- paste("\\b", terms, "\\b", sep="")},
          "none" = {terms <- terms}
  )

  for (n in 1:length(terms)) {
    if (n == 1) {
      redundant <- c()
    }

    if(any(grep(terms[n], terms[-n]))) {
      detections <- grep(terms[n], terms)
      redundant <- append(redundant, detections[-which(detections == n)])
    }
    if (n == length(terms)) {
      redundant <- unique(redundant)
      if (length(redundant > 0)) {
        terms <- terms[-redundant]
      }
    }
  }
  if(closure!="none"){
    terms <- gsub("\\\\b", "", terms)
    terms <- gsub("__", "", terms)
  }
  return(terms)
}



#' Print possible search languages
#' @description Prints a list of languages that write_search() can write searches in.
#'@examples available_languages()
available_languages <- function(){
  print(litsearchr::possible_langs$Language)
}


#' Write a search to check title recall
#' @description Given a set of titles, writes a Boolean search that can be used in database title fields to check whether a search strategy retrieves titles.
#' @param titles a character vector of titles
#' @return a text string
#'@example inst/examples/write_titles.R
write_title_search <- function(titles){
  titlekeys <- synthesisr::get_tokens(synthesisr::remove_punctuation(titles), "English")

  title <- c()
  for (i in 1:length(titlekeys)){
    temp <- paste(titlekeys[[i]], collapse=" ")
    title[i] <- paste("\\(", temp, "\\)")
  }

  title_search <- paste(title, collapse=" OR ")
  title_search <- gsub("\\\\", "", title_search)

  return(title_search)
}


#' Scrapes hits from specified databases
#' @description Provides a wrapper function to scrape hits from databases that litsearchr can scrape
#' @param search_terms a list of character strings with grouped search terms.
#' @param URL the URL from searching in OATD, NDLTD, or OpenThesis
#' @param database a character with the database to scrape.
#' @param verbose if TRUE, prints which page of hits it has finished
#' @param writefile if TRUE, writes results to a .csv file in the working directory
#' @param directory the directory to save results to if writefile=TRUE
#' @return a database of hits (if yes is selected from the menu prompt, the hits will also be saved to your working directory)
#' @examples \dontrun{scrape_hits(search_terms=list(c("picoides arcticus")), database="ndltd")}
scrape_hits <- function(search_terms=NULL, URL=NULL, database=c("oatd", "ndltd", "openthesis"),
                        verbose=TRUE, writefile=FALSE, directory="./"){
  if(database=="oatd"){
    hits <- litsearchr::scrape_oatd(search_terms = search_terms, URL=URL, verbose=verbose, writefile = writefile, directory=directory)
  }
  if(database=="ndltd"){
    hits <- litsearchr::scrape_ndltd(search_terms = search_terms, URL=URL, verbose=verbose, writefile = writefile, directory=directory)
  }
  if(database=="openthesis"){
    hits <- litsearchr::scrape_openthesis(search_terms = search_terms, URL=URL, verbose=verbose, writefile = writefile, directory=directory)
  }
  return(hits)
}



#' Scrapes results from Open Access Theses and Dissertations
#' @description Scrapes hits from OATD. Query length is limited by server requests, which can be triggered either by excessively long queries.
#' @param search_terms a list of character strings with grouped search terms
#' @param URL the URL from conducting a search in the database
#' @param writefile if TRUE, writes results to a .csv file in the working directory
#' @param verbose if TRUE, prints which page of hits it has finished
#' @param languages which language to search in; available languages can be viewed with available_languages().
#' @param stemming if TRUE, keywords will be truncated and stem from root word forms (only if language is English)
#' @param exactphrase if TRUE, keyword phrases will be placed in quotes to ensure exact phrases are returned#' @return a database of hits (if yes is selected from the menu prompt, the hits will also be saved to your working directory)
#' @param directory the directory to save results to if writefile=TRUE
#' @return a data frame containing the results of the search
#' @examples \dontrun{scrape_oatd(search_terms=list(c("black-backed woodpecker", "picoides arcticus")))}
scrape_oatd <- function(search_terms=NULL, URL=NULL, writefile=FALSE, verbose=TRUE, languages="English", stemming=FALSE, exactphrase=TRUE, directory="./"){

  if(is.null(URL)==FALSE){
    search_strat <- strsplit(URL, "?q=")[[1]][2]
  }

  if(is.null(search_terms)==FALSE){
    search_strat <- litsearchr::write_search(search_terms, languages=languages, stemming=TRUE, exactphrase=TRUE)[[1]]
    search_strat <- gsub("\\)","%29",gsub("\\(", "%28", gsub("\"", "%22", gsub(" ", "+", gsub(" \\)", "%29", gsub("\\( ", "%28", gsub("\\\\", "",  gsub(" OR ", "+OR+", search_strat))))))))

  }
  if(length(search_strat)==0){
    print("Error. No search terms or URL provided. Aborting.")
  }

  if(length(search_strat)>0){
    base_URL <- "https://oatd.org/oatd/search?q="


    firstURL <- paste(base_URL, search_strat, sep="")
    OATD <- as.character(xml2::read_html(firstURL))

    base_site <- firstURL

    if(writefile==TRUE){
      if(utils::menu(c("yes", "no"), title="This will write results to a .csv file. Do you want to save the results to a .csv file?")==2){
        save_results <- FALSE
      }
    }

    tmp <- strsplit(OATD, "total matches.<")
    total_hits <- as.numeric(strsplit(strsplit(tmp[[1]][1], "Showing records ")[[1]][2], "\n")[[1]][2])
    npages <- c(seq(1, floor(total_hits), 30))

    for (k in 1:length(npages)){
      URLpage <- paste(base_site, "&amp;start=", npages[k], sep="")
      webpage <- xml2::read_html(URLpage)
      OATD <- as.character(webpage)
      sploatd <- strsplit(OATD, "div class=\"result\"")[[1]][-1]
      sploatd <- gsub("</em>", "", sploatd)
      sploatd <- gsub("<em class=\"hilite\">", "", sploatd)

      for(i in 1:length(sploatd)){
        university <- stringr::str_trim(strsplit(strsplit(strsplit(sploatd[i], ".png")[[1]][2], "</p")[[1]][1], "\n")[[1]][2])
        author <- stringr::str_trim(strsplit(strsplit(sploatd[i], ".\n<span>")[[1]][2], "</span")[[1]][1])
        title <- stringr::str_trim(strsplit(strsplit(sploatd[i], "etdTitle\"><span>")[[1]][2], "</span")[[1]][1])

        abstract <- stringr::str_trim(strsplit(strsplit(strsplit(sploatd[i], "closeField")[[1]][2], "</span>")[[1]][2], "</div")[[1]][1])

        thesis <- cbind(author, university, title, abstract)
        if(i==1){df <- thesis}
        if(i>1){df <- rbind(df, thesis)}
      }

      if(k==1){dataset <- df}
      if(k>1){dataset <- rbind(dataset, df)}
      if(verbose==TRUE){print(paste("Done with page number", ((npages[k]-1)/30)+1, "of", (max((npages-1)/30)+1), sep=" "))}

    }

    dataset <- as.data.frame(dataset)
    dataset$database <- rep("oatd_scrape", nrow(dataset))

    if(writefile==TRUE){write.csv(dataset, paste(directory, "oatd_hits.csv", sep=""))}
    return(dataset)

  }}

#' Scrapes results from the NDLTD Global ETD Search
#' @description Scrapes hits from the Networked Digital Library of Theses and Dissertations Global ETD Search.
#' @param search_terms a list of character strings with grouped search terms
#' @param URL the URL from conducting a search in the database
#' @param writefile if TRUE, writes results to a .csv file in the working directory
#' @param verbose if TRUE, prints which page of hits it has finished
#' @param languages which language to search in; available languages can be viewed with available_languages().
#' @param stemming if TRUE, keywords will be truncated and stem from root word forms (only if language is English)
#' @param exactphrase if TRUE, keyword phrases will be placed in quotes to ensure exact phrases are returned#' @return a database of hits (if yes is selected from the menu prompt, the hits will also be saved to your working directory)
#' @param where where in a thesis or dissertation to search (options are description or title)
#' @param directory the directory to save results to if writefile=TRUE
#' @return a data frame containing the results of the search
#' @examples \dontrun{scrape_ndltd(search_terms=list(c("black backed woodpecker", "picoides arcticus")))}
scrape_ndltd <- function(search_terms=NULL, URL=NULL, writefile=FALSE, verbose=TRUE, languages="English",
                         stemming=FALSE, exactphrase=TRUE, where="description", directory="./"){

  if(is.null(URL)==FALSE){
    search_strat <- strsplit(URL, "?q=")[[1]][2]
  }

  if(is.null(search_terms)==FALSE){
    search_strat <- litsearchr::write_search(search_terms, languages=languages, stemming=TRUE, exactphrase=TRUE)[[1]]
    search_strat <- gsub("\\)","%29",gsub("\\(", "%28", gsub("\"", "%22", gsub(" ", "+", gsub(" \\)", "%29", gsub("\\( ", "%28", gsub("\\\\", "",  gsub(" OR ", "+OR+", search_strat))))))))

  }
  if(length(search_strat)==0){
    print("Error. No search terms or URL provided. Aborting.")
  }

  if(length(search_strat)>0){
    if(writefile==TRUE){
      if(utils::menu(c("yes", "no"), title="This will write results to a .csv file. Do you want to save the results to a .csv file?")==2){
        save_results <- FALSE
      }
    }

    base_URL <- paste("http://search.ndltd.org/search.php?q=", where, "%3A", sep="")


    firstURL <- paste(base_URL, search_strat, "&start=", "0", sep="")
    ndltd <- as.character(xml2::read_html(firstURL))

    base_site <- firstURL

    total_hits <- as.numeric(stringr::str_trim(strsplit(strsplit(
      strsplit(strsplit(ndltd, "Search results")[[1]][2], "seconds")[[1]][1], "of")[[1]][2], "\\(")[[1]][1]))
    npages <- seq(0, floor(total_hits), 10)
    if(npages[length(npages)]==total_hits){npages <- npages[-length(npages)]}

    for (k in 1:length(npages)){
      URLpage <- paste(base_site, "&start=", npages[k], sep="")
      webpage <- xml2::read_html(URLpage)

      NDLTD <- as.character(webpage)
      NDspl <- strsplit(NDLTD, "<tr>")[[1]][-1]
      NDspl[length(NDspl)] <- strsplit(NDspl[length(NDspl)], "</tr>")[[1]][1]


      for (i in 1:length(NDspl)){
        init_title <- strsplit(strsplit(NDspl[i], "</h4>")[[1]][1], ">")
        title <- strsplit(init_title[[1]][length(init_title[[1]])], "<")[[1]][1]

        author <- stringr::str_trim(strsplit(
          strsplit(strsplit(NDspl[i], "/h4>")[[1]][2], "<em>")[[1]][2], "</em>")[[1]][1])

        date <- stringr::str_trim(strsplit(
          strsplit(strsplit(NDspl[i], "/h4>")[[1]][2], "<em>")[[1]][3], "</em>")[[1]][1])

        init_abstract <- strsplit(strsplit(strsplit(
          strsplit(strsplit(NDspl[i], "/h4>")[[1]][2], "<em>")[[1]][3], "</em>")[[1]][2], "/div>")[[1]][1], ">")

        abstract <- stringr::str_trim(gsub("<", "", gsub("\n", " ", init_abstract[[1]][length(init_abstract[[1]])])))

        thesis <- cbind(author, date, title, abstract)
        if(i==1){df <- thesis}
        if(i>1){df <- rbind(df, thesis)}

      }

      if(k==1){dataset <- df}
      if(k>1){dataset <- rbind(dataset, df)}
      if(verbose==TRUE){print(paste("Done with page number", npages[k]/10, "of", max(npages/10), sep=" "))}

    }
    dataset <- as.data.frame(dataset)
    dataset$database <- rep("ndltd_scrape", nrow(dataset))

    if(writefile==TRUE){write.csv(dataset, paste(directory, "ndltd_hits.csv", sep=""))}
    return(dataset)

  }
}

#' Scrapes results from OpenThesis
#' @description Scrapes hits from OpenThesis.
#' @param search_terms a list of character strings with grouped search terms
#' @param URL the URL from conducting a search in the database
#' @param writefile if TRUE, writes results to a .csv file in the working directory
#' @param verbose if TRUE, prints which page of hits it has finished
#' @param languages which language to search in; available languages can be viewed with available_languages().
#' @param stemming if TRUE, keywords will be truncated and stem from root word forms (only if language is English)
#' @param exactphrase if TRUE, keyword phrases will be placed in quotes to ensure exact phrases are returned#' @return a database of hits (if yes is selected from the menu prompt, the hits will also be saved to your working directory)
#' @param directory the directory to save results to if writefile=TRUE
#' @return a data frame containing the results of the search
#' @examples \dontrun{scrape_openthesis(search_terms=list(c("picoides arcticus")))}
scrape_openthesis <- function(search_terms=NULL, URL=NULL, writefile=FALSE, verbose=TRUE, languages="English", stemming=FALSE, exactphrase=TRUE, directory="./"){

  if(is.null(URL)==FALSE){
    search_strat <- strsplit(strsplit(URL, "?queryString=")[[1]][2], "&from=")[[1]][1]
  }

  if(is.null(search_terms)==FALSE){
    search_strat <- litsearchr::write_search(search_terms, languages=languages, stemming=TRUE, exactphrase=TRUE)[[1]]
    search_strat <- gsub("\\)","%29",gsub("\\(", "%28", gsub("\"", "%22", gsub(" ", "+", gsub(" \\)", "%29", gsub("\\( ", "%28", gsub("\\\\", "",  gsub(" OR ", "+OR+", search_strat))))))))

  }
  if(length(search_strat)==0){
    print("Error. No search terms or URL provided. Aborting.")
  }

  if(length(search_strat)>0){

    if(writefile==TRUE){
      if(utils::menu(c("yes", "no"), title="This will write results to a .csv file. Do you want to save the results to a .csv file?")==2){
        save_results <- FALSE
      }
    }

    base_URL1 <- "http://www.openthesis.org/search/search.html?queryString="
    base_URL2 <- "&repeatSearch=true&from=searchResults&offset="
    base_site <- paste(base_URL1, search_strat, base_URL2, sep="")


    firstURL <- paste(base_site, "0", "&max=5", sep="")
    openth <- as.character(xml2::read_html(firstURL))

    nhits <- stringr::str_trim(gsub("</strong>", "", gsub("<strong>", "", strsplit(strsplit(strsplit(strsplit(openth, "class=\"results\"")[[1]][2], "query")[[1]][1], "results")[[1]][1], "of")[[1]][2])))
    nhits <- as.numeric(nhits)

    npages <- seq(0, floor(nhits/100)*100, 100)

    for(k in 1:length(npages)){
      URLpage <- paste(base_site, npages[k], "&max=100", sep="")

      webpage <- xml2::read_html(URLpage)
      opth <- as.character(webpage)

      remove_junk <- strsplit(strsplit(opth, "<table")[[1]][2], "table>")[[1]][1]
      splopth <- strsplit(remove_junk, "class=\"title")[[1]][-1]

      for (i in 1:length(splopth)){
        url <- paste(strsplit(strsplit(splopth[i], "a href=\"")[[1]][2], ".html")[[1]][1], ".html", sep="")
        title <- strsplit(strsplit(strsplit(splopth[i], "</a")[[1]][1], "a href")[[1]][2], ">")[[1]][2]
        author <- strsplit(strsplit(splopth[i], "\n                                        <td>")[[1]][2], "</td")[[1]][1]
        date <- strsplit(strsplit(splopth[i], "\n                                        <td>")[[1]][3], "</td")[[1]][1]
        university <- strsplit(strsplit(strsplit(strsplit(splopth[i], "\n                                        <td>")[[1]][3], "</td")[[1]][2], ".html\">")[[1]][2], "</a")[[1]][1]
        type <- strsplit(strsplit(splopth[i], "\n                                        <td>")[[1]][4], "</td")[[1]][1]

        thesis <- cbind(url, title, author, date, university, type)
        if (i ==1){df <- thesis}
        if (i > 1){df <- rbind(df, thesis)}
      }

      if (k==1){dataset <- df}
      if (k>1){dataset <- rbind(dataset, df)}
      if(verbose==TRUE){print(paste("Done with page number", npages[k]/100, "of", max(npages/100), sep=" "))}
    }

    dataset <- as.data.frame(dataset)
    dataset$database <- rep("openthesis_scrape", nrow(dataset))

    if(writefile==TRUE){write.csv(dataset, paste(directory, "openthesis_hits.csv", sep=""))}
    return(dataset)

  }
}

#' Check the recall of a search strategy
#' @description Checks a list of known articles against the results of a search to see how many the search retrieves.
#' @param true_hits a character vector of titles for articles that should be returned
#' @param retrieved_articles a character vector of titles for articles returned by a search
#' @param min_sim the minimum similarity between two titles to be considered for manual review
#' @param new_stopwords any common words in the titles that should be ignored when computing similarity to avoid false matches
#' @return a table of the best match for each true title from the search results along with a title similarity score
#' @examples check_recall(true_hits=c("Picoides arcticus"), retrieved_articles=c("Picoides tridactylus"))
check_recall <- function (true_hits, retrieved_articles, min_sim = 0.6, new_stopwords = NULL) {
  custom_stopwords <- litsearchr::add_stopwords(new_stopwords = new_stopwords)
  titlekeys <- synthesisr::get_tokens(synthesisr::remove_punctuation(true_hits), "English")
  positions <- list()
  length(positions) <- length(titlekeys)
  for (i in 1:length(titlekeys)) {
    article <- titlekeys[[i]]
    for (j in 1:length(article)) {
      temp <- stringr::str_detect(synthesisr::remove_punctuation(tolower(retrieved_articles)),
                                  article[j])
      hits <- which(temp == TRUE)
      if (length(hits) > 0) {
        for (k in 1:length(hits)) {
          if (i == 1) {
            if (k == 1) {
              positions[[i]] <- hits[k]
            }
            if (k > 1) {
              positions[[i]] <- c(positions[[i]], hits[k])
            }
          }
          if (i > 1) {
            positions[[i]] <- c(positions[[i]], hits[k])
          }
        }
      }
    }

    similarity <- table(positions[[i]])/length(article)

    similarity <- sort(similarity[which(similarity > min_sim)],
                       decreasing = TRUE)
    if (length(similarity) > 0){
      best_match <- similarity[1]
      similarity_entry <- as.data.frame(cbind(as.character(true_hits[i]),
                                              as.character(retrieved_articles[as.numeric(names(best_match))]),
                                              as.numeric(best_match)))}

    if (length(similarity)==0){
      similarity_entry <- as.data.frame(cbind(as.character(true_hits[i]), "NA", "NA"))
    }

    if (i == 1) {
      similarity_table <- similarity_entry
    }
    if (i > 1) {
      similarity_table <- rbind(similarity_table, similarity_entry)
    }
  }
  colnames(similarity_table) <- c("Title", "Best_Match", "Similarity")
  return(similarity_table)
}

#' Get precision and recall of a search
#' @description Measures the performance of a search by precision (specificity) and recall (sensitivity).
#' @param no_desired the number of  articles that should be returned
#' @param no_hits the number of good hits that a search found (can be found with check_recall)
#' @param no_articles the total number of articles that a search found
#' @return the sensitivity, precision, and number needed to process
#' @examples search_performance(no_desired=10, no_hits=9, no_articles=27)
search_performance <- function(no_desired, no_hits, no_articles){
  sensitivity <- round(no_hits/no_desired*100, 3)
  precision <- round(no_hits/no_articles*100, 3)
  NNP <- round(1/precision*100, 3)
  output <- rbind(sensitivity, precision, NNP)
  rownames(output) <- c("Sensitivity (%)", "Precision  (%)", "Number needed to process")
  return(output)
}
