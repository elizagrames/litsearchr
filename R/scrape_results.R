#' Scrapes hits from specified databases
#' @description Provides a wrapper function to scrape hits from databases that litsearchr can scrape
#' @param search_terms a list of character strings with grouped search terms.
#' @param URL the URL from searching in OATD, NDLTD, or OpenThesis
#' @param database a character with the database to scrape.
#' @param verbose if TRUE, prints which page of hits it has finished
#' @param writefile if TRUE, writes results to a .csv file in the working directory
#' @return a database of hits (if yes is selected from the menu prompt, the hits will also be saved to your working directory)
scrape_hits <- function(search_terms=NULL, URL=NULL, database=c("oatd", "ndltd", "openthesis"),
                        verbose=TRUE, writefile=TRUE){
  if(database=="oatd"){
    hits <- litsearchr::scrape_oatd(search_terms = search_terms, URL=URL, verbose=verbose, writefile = writefile)
  }
  if(database=="ndltd"){
    hits <- litsearchr::scrape_ndltd(search_terms = search_terms, URL=URL, verbose=verbose, writefile = writefile)
  }
  if(database=="openthesis"){
    hits <- litsearchr::scrape_openthesis(search_terms = search_terms, URL=URL, verbose=verbose, writefile = writefile)
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
#' @return a data frame containing the results of the search
scrape_oatd <- function(search_terms=NULL, URL=NULL, writefile=TRUE, verbose=TRUE, languages="English", stemming=TRUE, exactphrase=TRUE){

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

    if(writefile==TRUE){write.csv(dataset, "oatd_hits.csv")}
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
#' @return a data frame containing the results of the search
scrape_ndltd <- function(search_terms=NULL, URL=NULL, writefile=TRUE, verbose=TRUE, languages="English",
                         stemming=TRUE, exactphrase=TRUE, where="description"){

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

    if(writefile==TRUE){write.csv(dataset, "ndltd_hits.csv")}
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
#' @return a data frame containing the results of the search
scrape_openthesis <- function(search_terms=NULL, URL=NULL, writefile=TRUE, verbose=TRUE, languages="English", stemming=TRUE, exactphrase=TRUE){

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

    if(writefile==TRUE){write.csv(dataset, "openthesis_hits.csv")}
    return(dataset)

  }
  }

