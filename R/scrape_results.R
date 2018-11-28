#' Scrapes hits from specified databases
#' @description Provides a wrapper function to scrape hits from databases that litsearchr can scrape
#' @param search_terms a list of character strings with grouped search terms.
#' @param database a character with the database to scrape.
#' #' @param verbose if TRUE, prints which page of hits it has finished
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


#' Scrapes results from Google Scholar
#' @description Scrapes hits from Google Scholar. Limited to 256 characters and 60 queries.
#' @param search_terms a list of character strings with grouped search terms
#' @param URL the URL from conducting a search in the database
#' @param writefile if TRUE, writes results to a .csv file in the working directory
#' @param verbose if TRUE, prints which page of hits it has finished
#' @param languages which language to search in; available languages can be viewed with available_languages().
#' @param stemming if TRUE, keywords will be truncated and stem from root word forms (only if language is English)
#' @param exactphrase if TRUE, keyword phrases will be placed in quotes to ensure exact phrases are returned
#' @return a database of hits (if yes is selected from the menu prompt, the hits will also be saved to your working directory)
scrape_google_scholar <- function(search_terms=NULL, URL=NULL, writefile=TRUE, verbose=TRUE, languages="English", stemming=TRUE, exactphrase=TRUE){

  if(is.null(URL)==FALSE){
    search_strat <- strsplit(strsplit(URL, "&q=")[[1]][2], "&btnG")[[1]][1]
  }

  if(is.null(search_terms)==FALSE){
    search_strat <- litsearchr::write_search(search_terms, languages=languages, stemming=TRUE, exactphrase=TRUE)[[1]]
    search_strat <- gsub("\\)", "%29", gsub("\\(", "%28", gsub(" ", "%20", gsub("\\\\", "",  gsub(" OR ", "+OR+", search_strat)))))

  }
  if(length(search_strat)==0){
    print("Error. No search terms or URL provided. Aborting.")
  }

  if(length(search_strat)>0){


  if(nchar(search_strat)<=256){

    if(writefile==TRUE){if(utils::menu(c("yes", "no"),
                                title="This will write a .csv to your working directory with all the hits returned. Are you sure that you want to do that?")==2){
      writefile <- FALSE
    }
    }

    base_URL <- "https://scholar.google.com/scholar?"

    first_search <- gsub(" ", "+", paste(base_URL, "start=0&num=20&q=", search_strat, sep=""))
    firstpage <- as.character(xml2::read_html(first_search))
    nhits <- gsub(",", "", strsplit(strsplit(firstpage, " results \\(")[[1]][1], "About ")[[1]][2])
    npages <- floor(as.numeric(nhits)/10)
    if(npages > 100){npages <- 100} # apparently Google Scholar only goes up to 100 pages

    for(l in 1:npages){

      if(l==1){
        webpage <- firstpage
      }
      if(l>1){
        search_string <- gsub(" ", "+", paste(base_URL, "start=", l*10, "&num=20&q=", search_strat, sep=""))
        webpage <- as.character(xml2::read_html(search_string))
      }

      if(stringr::str_detect(webpage, "Please show you're not a robot")==TRUE){
        print(paste("Ran into captcha issues at page ", l, sep=""))
      }
      if(stringr::str_detect(webpage, "Please show you're not a robot")==FALSE){


        articles <- strsplit(strsplit(strsplit(webpage, "gs_res_ccl_mid")[[1]][2], "gs_res_ccl_bot")[[1]][1], "data-rp")[[1]][-1]

        for(i in 1:length(articles)){
          current_article <- articles[i]
          titleblock <- strsplit(strsplit(current_article, "<h3")[[1]][2], "</h3>")[[1]][1]

          if(stringr::str_detect(titleblock, "CITATION")==TRUE){
            title <- strsplit(titleblock, "</span></span>")[[1]][2]
          }
          if(stringr::str_detect(titleblock, "CITATION")==FALSE){
            title <- strsplit(strsplit(strsplit(titleblock, "<a href=")[[1]][2], "\">")[[1]][2], "</a>")[[1]][1]
          }

          title <- gsub("<b>", "", gsub("</b>", "", title))

          authorblock <- strsplit(strsplit(current_article, "gs_a")[[1]][2], "</div>")[[1]][1]

          URLcontainsdash <- FALSE
          if(stringr::str_detect(strsplit(authorblock, "-")[[1]][1], "<a href") & stringr::str_detect(strsplit(authorblock, "-")[[1]][1], "</a>")==FALSE){URLcontainsdash <- TRUE}
          if(URLcontainsdash==TRUE){
            authors <- strsplit(authorblock, "-")[[1]][2]
          }
          if(URLcontainsdash==FALSE){
            authors <- strsplit(authorblock, "-")[[1]][1]
          }



          author_list <- strsplit(authors, ",")[[1]]

          for(j in 1:length(author_list)){
            if(stringr::str_detect(author_list[j], "</a>")==TRUE){
              if(stringr::str_detect(author_list[j], "<a href")==TRUE){
                author <- strsplit(strsplit(strsplit(author_list[j], "<a href")[[1]][2], "\">")[[1]][2], "</a>")[[1]][1]
              }
              if(stringr::str_detect(author_list[j], "<a href")==FALSE){
                author <- strsplit(strsplit(author_list[j], "\">")[[1]][2], "</a>")[[1]][1]
              }
            }
            if(stringr::str_detect(author_list[j], "</a>")==FALSE){
              author <- gsub("\">", "", author_list[j])
            }
            if(j==1){authors <- author}
            if(j>1){authors <- paste(authors, author, sep="; ")}
          }

          if(URLcontainsdash==TRUE){
            journal <- strsplit(strsplit(strsplit(strsplit(current_article, "gs_a")[[1]][2], "</div>")[[1]][1], "-")[[1]][3], ",")[[1]][1]
            year <- strsplit(strsplit(strsplit(strsplit(current_article, "gs_a")[[1]][2], "</div>")[[1]][1], "-")[[1]][3], ",")[[1]][2]
          }

          if(URLcontainsdash==FALSE){
            journal <- strsplit(strsplit(strsplit(strsplit(current_article, "gs_a")[[1]][2], "</div>")[[1]][1], "-")[[1]][2], ",")[[1]][1]
            year <- strsplit(strsplit(strsplit(strsplit(current_article, "gs_a")[[1]][2], "</div>")[[1]][1], "-")[[1]][2], ",")[[1]][2]
          }

          if(is.na(journal)==TRUE){
            journal <- ""
          }

          if(is.na(year)==TRUE){
            year <- journal
            journal <- ""
          }

          if(stringr::str_detect(year, "\\.")){
            journal <- year
            year <- ""
          }

          if(stringr::str_detect(current_article, "Cited by")==TRUE){
            citations <- strsplit(strsplit(strsplit(strsplit(current_article, "gs_ri")[[1]][2], "gs_fl")[[1]][2],"Cited by")[[1]][2], "</a>")[[1]][1]
          }
          if(stringr::str_detect(current_article, "Cited by")==FALSE){
            citations <- ""
          }

          article_entry <- cbind(title, authors, journal, year, citations)
          if(i==1){dataset <- article_entry}
          if(i >1){dataset <- rbind(dataset, article_entry)}
        }

        if(l==1){study_data <- dataset}
        if(l>1){study_data <- rbind(study_data, dataset)}

      }

      if(verbose==TRUE){print(paste("Done with page", l, "out of", npages)      )      }
      }
    study_data <- as.data.frame(study_data)
    study_data$database <- rep("googlescholar_scrape", nrow(study_data))

    if(writefile==TRUE){
      write.csv(study_data, "google_scholar_hits.csv")
    }

    return(study_data)
  }


  if(nchar(search_strat) > 256){
    print("Error: Google Scholar only allows search strings under 256 characters. Aborting.")}


  }
}

#' Scrapes results from JSTOR
#' @description Scrapes hits from JSTOR. Limited to 256 characters and must access through institutional login.
#' @param search_terms a list of character strings with grouped search terms
#' @param URL the URL from conducting a search in the database
#' @param writefile if TRUE, writes results to a .csv file in the working directory
#' @param verbose if TRUE, prints which page of hits it has finished
#' @param languages which language to search in; available languages can be viewed with available_languages().
#' @param stemming if TRUE, keywords will be truncated and stem from root word forms (only if language is English)
#' @param exactphrase if TRUE, keyword phrases will be placed in quotes to ensure exact phrases are returned#' @return a database of hits (if yes is selected from the menu prompt, the hits will also be saved to your working directory)
scrape_jstor <- function(search_terms=NULL, URL=NULL, writefile=TRUE, verbose=TRUE, languages="English", stemming=TRUE, exactphrase=TRUE){

  if(is.null(URL)==FALSE){
    search_strat <- strsplit(strsplit(URL, "?Query=")[[1]][2], "&acc=on")[[1]][1]
  }

  if(is.null(search_terms)==FALSE){
    search_strat <- litsearchr::write_search(search_terms, languages=languages, stemming=TRUE, exactphrase=TRUE)[[1]]
    search_strat <- gsub("\\)", "%29", gsub("\\(", "%28", gsub(" ", "+", gsub("\\\\", "",  gsub(" OR ", "+OR+", search_strat)))))
    search_strat <- gsub("\"", "'", search_strat)

  }
  if(length(search_strat)==0){
    print("Error. No search terms or URL provided. Aborting.")
  }

  if(length(search_strat)>0){
  base_URL1 <- "https://www.jstor.org/action/doAdvancedSearch?searchType=facetSearch&page="
  base_URL2 <- "&sd=&ed=&c3=AND&c4=AND&f5=all&c5=AND&f1=all&acc=off&f3=all&group=none&f0=all&c6=AND&c1=AND&c2=AND&q0="
  base_URL3 <- "&f4=all&f6=all&f2=all"

  if(writefile==TRUE){if(utils::menu(c("yes", "no"),
                              title="This will write a .csv to your working directory with all the hits returned. Are you sure that you want to do that?")==2){
    writefile <- FALSE
  }
  }



  if(nchar(search_strat)<=256){

    x <- xml2::read_html("https://www.jstor.org/")
    firstURL <- paste(base_URL1, "1", base_URL2, search_strat, base_URL3, sep="")
    firstpage <- as.character(xml2::read_html(firstURL))

    nhits <- as.numeric(strsplit(strsplit(firstpage, "data-result-count=\"")[[1]][2], "\"")[[1]][1])
    npages <- ceiling(nhits/25)

    for(w in 1:npages){
      if(w==1){website <- firstpage}
      if(w>1){
        URL <- paste(base_URL1, w, base_URL2, search_strat, base_URL3, sep="")
        website <- as.character(xml2::read_html(URL))

      }

      articles <- strsplit(website, "row result-item")[[1]][-1]

      for(i in 1:length(articles)){
        current_article <- articles[i]
        type <- stringr::str_trim(strsplit(strsplit(current_article, "badge\">")[[1]][2], "</div>")[[1]][1])
        title <- stringr::str_trim(strsplit(strsplit(strsplit(current_article, "title\"")[[1]][2], "</div>")[[1]][1], "\n")[[1]][2])
        authorlist <- strsplit(strsplit(strsplit(current_article, "contrib\"")[[1]][2], "</div>")[[1]][1], "</a>")[[1]]
        for(m in 1:length(authorlist)){
          author <- strsplit(authorlist[m], "\">")[[1]][2]
          if(m==1){authors <- author}
          if(m>1){
            if(is.na(author)==FALSE){authors <- paste(authors, author, sep="; ")}
          }
        }

        journal <- strsplit(strsplit(current_article, "<cite>")[[1]][2], "</cite>")[[1]][1]
        pubinfo <- stringr::str_trim(strsplit(strsplit(current_article, "</cite>")[[1]][2], "</div>")[[1]][1])
        keyword_list <- c()
        keyword_list[1] <- "no keywords</a>"

        if(stringr::str_detect(current_article, "Topics: ")==TRUE){
          keyword_list <- strsplit(strsplit(strsplit(current_article, "Topics: ")[[1]][2], "</div>")[[1]][1], "\">")[[1]][-1]
        }

        if(stringr::str_detect(current_article, "Topic: ")==TRUE){
          keyword_list <- strsplit(strsplit(strsplit(current_article, "Topic: ")[[1]][2], "</div>")[[1]][1], "\">")[[1]][-1]
        }

        for(k in 1:length(keyword_list)){
          keyword <- strsplit(keyword_list[k], "</a>")[[1]][1]
          if(k==1){keywords <- keyword}
          if(k>1){keywords <- paste(keywords, keyword, sep="; ")}
        }

        doi <- strsplit(strsplit(current_article, "doi:")[[1]][2], " --")[[1]][1]

        entry <- cbind(title, authors, journal, pubinfo, keywords, type, doi)
        if(i==1){dataset <- entry}
        if(i>1){dataset <- rbind(dataset, entry)}
      }

      if(w==1){study_data <- dataset}
      if(w>1){study_data <- rbind(study_data, dataset)}
      if(verbose==TRUE){print(paste("Done with page", w, "out of", npages)      )      }

      }

    study_data <- as.data.frame(study_data)

    study_data$database <- rep("jstor_scrape", nrow(study_data))
    if(writefile==TRUE){
      write.csv(study_data, "jstor_hits.csv")
    }

    return(study_data)

  }

  if(nchar(search_strat) > 256){
    print("Error: JSTOR only allows search strings under 256 characters. Aborting.")}

  }
}

#' Scrapes results from Scopus
#' @description Scrapes hits from Scopus. Does not currently extract abstracts - only titles and publication information. Must login through institutional access.
#' @param search_terms a list of character strings with grouped search terms
#' @param URL the URL from conducting a search in the database
#' @param writefile if TRUE, writes results to a .csv file in the working directory
#' @param verbose if TRUE, prints which page of hits it has finished
#' @param languages which language to search in; available languages can be viewed with available_languages().
#' @param stemming if TRUE, keywords will be truncated and stem from root word forms (only if language is English)
#' @param exactphrase if TRUE, keyword phrases will be placed in quotes to ensure exact phrases are returned#' @return a database of hits (if yes is selected from the menu prompt, the hits will also be saved to your working directory)
scrape_scopus <- function(search_terms=NULL, URL=NULL, writefile=TRUE, verbose=TRUE, languages="English", stemming=TRUE, exactphrase=TRUE){


  if(is.null(URL)==FALSE){
    search_strat <- strsplit(strsplit(URL, "TITLE-ABS-KEY-AUTH")[[1]][2], "&origin=")[[1]][1]
  }

  if(is.null(search_terms)==FALSE){
    search_strat <- litsearchr::write_search(search_terms, languages=languages, stemming=TRUE, exactphrase=TRUE)[[1]]
    search_strat <- gsub("\"","%22",gsub("\\)", "%29", gsub("\\(", "%28", gsub(" ", "+", gsub("\\\\", "",  gsub(" OR ", "+OR+", search_strat))))))

  }
  if(length(search_strat)==0){
    print("Error. No search terms or URL provided. Aborting.")
  }

  if(length(search_strat)>0){

  if(writefile==TRUE){if(utils::menu(c("yes", "no"),
                              title="This will write a .csv to your working directory with all the hits returned. Are you sure that you want to do that?")==2){
    writefile <- FALSE
  }
  }

  scopus_base <- "https://www.scopus.com/results/results.uri?sort=plf-f&src=s&sid=f68938845668763794e120e1ed0927e2&sot=a&sdt=a&sl=24&s=TITLE-ABS-KEY-AUTH"

  search_string <- paste(scopus_base, search_strat, "&cl=t&offset=", 1, sep="")

  firstpage <- as.character(xml2::read_html(search_string))

  if(stringr::str_detect(firstpage, "No documents were found")){print(paste("No hits found."))}
  if(stringr::str_detect(firstpage, "No documents were found")==FALSE){

    total_hits <- as.numeric(strsplit(strsplit(strsplit(firstpage, "scr_refineResults")[[1]][2], "value=\"")[[1]][2], "\"><input id")[[1]][1])

    npages <- ceiling(total_hits/20)

    for(j in 1:npages){
      if(j==1){webpage <- firstpage}
      if(j>1){
        next_page <- paste(search_string, "&cl=t&offset=", seq(1, npages*20, 20)[j], sep="")
        webpage <- as.character(xml2::read_html(next_page))
      }

      articles <- strsplit(webpage, "Show document details")[[1]][-1]
      articles[length(articles)] <- strsplit(articles[length(articles)], "Show all related documents based on shared references")[[1]][1]

      for(k in 1:length(articles)){

        title <- strsplit(strsplit(articles[k], ">")[[1]][2], "<")[[1]][1]
        id <- strsplit(strsplit(articles[k], "checkbox\" value=\"")[[1]][2], "\">")[[1]][1]
        doi <- strsplit(strsplit(articles[k], "data-doi=\"")[[1]][2], "\">")[[1]][1]

        authorblock <- strsplit(articles[k], "<td>")[[1]][2]
        if(stringr::str_detect(authorblock,"No author name available")){authors <- ""}

        if(stringr::str_detect(authorblock,"No author name available")==FALSE){
          authors <- strsplit(strsplit(articles[k], "<td>")[[1]][2], "Show author details\">")[[1]][-1]
          for(l in 1:length(authors)){
            author <- strsplit(authors[l], "</")[[1]][1]
            if(l==1){authors <- author}
            if(l>1){authors <- paste(authors, author, sep="; ")}
          }
        }
        year <- strsplit(strsplit(strsplit(articles[k], "</td>")[[1]][3], ">\n")[[1]][2], "\n")[[1]][1]


        publication <- gsub("\n", "", strsplit(strsplit(articles[k], "<td>")[[1]][3], "<div")[[1]][1])

        if(stringr::str_detect(articles[k], "data-publisher")){
          split_characters <- "data-publisher"
          publication <- strsplit(strsplit(strsplit(articles[k], split_characters)[[1]][2], "</a")[[1]][1], "\">")[[1]][2]
        }
        if(stringr::str_detect(articles[k], "Show source title details")){
          split_characters <- "Show source title details"
          publication <- strsplit(strsplit(strsplit(articles[k], split_characters)[[1]][2], "</a")[[1]][1], "\">")[[1]][2]
        }

        addinfo <- strsplit(strsplit(articles[k], "additionalContent")[[1]][2], "</div>")[[1]][1]
        splitinfo <- strsplit(addinfo, "<span>")[[1]][-1]
        volume <- ""; issue <- ""; pages <- ""

        if(length(splitinfo > 0)){
          for(l in 1:length(splitinfo)){

            if(stringr::str_detect(splitinfo[l], "\\(")==FALSE & stringr::str_detect(splitinfo[l], "\npp.")==FALSE){
              volume <- strsplit(splitinfo[l], "</span>")[[1]][1]
            }

            if(stringr::str_detect(splitinfo[l], "\\(")==TRUE){
              issue <- strsplit(splitinfo[l], "</span>")[[1]][1]
            }

            if(stringr::str_detect(splitinfo[l], "\npp.")==TRUE){
              pages <- strsplit(strsplit(splitinfo[l], "</span>")[[1]][1], "\n")[[1]][3]
            }

          }
        }


        article_entry <- cbind(id, title, authors, year, publication, volume, issue, pages, doi)
        if(k==1){page_entry <- article_entry}
        if(k>1){page_entry <- rbind(page_entry, article_entry)}
      }

      if(j==1){study_data <- page_entry}
      if(j>1){study_data <- rbind(study_data, page_entry)}
      if(verbose==TRUE){print(paste("Done with page", j, "out of", npages)      )      }

      }

    study_data <- as.data.frame(study_data)
    study_data$database <- rep("scopus_scrape", nrow(study_data))


    if(writefile==TRUE){
      write.csv(study_data, "scopus_hits.csv")
    }

    return(study_data)
  }

  }
}

#' Scrapes results from WorldCat
#' @description Scrapes hits from WorldCat. Query length is limited by server requests, which can be triggered either by excessively long queries or by vague queries that return too many results.
#' @param search_terms a list of character strings with grouped search terms
#' @param URL the URL from conducting a search in the database
#' @param writefile if TRUE, writes results to a .csv file in the working directory
#' @param verbose if TRUE, prints which page of hits it has finished
#' @param languages which language to search in; available languages can be viewed with available_languages().
#' @param stemming if TRUE, keywords will be truncated and stem from root word forms (only if language is English)
#' @param exactphrase if TRUE, keyword phrases will be placed in quotes to ensure exact phrases are returned#' @return a database of hits (if yes is selected from the menu prompt, the hits will also be saved to your working directory)
scrape_worldcat <- function(search_terms=NULL, URL=NULL, writefile=TRUE, verbose=TRUE, languages="English", stemming=TRUE, exactphrase=TRUE){

  if(is.null(URL)==FALSE){
    search_strat <- strsplit(URL, "search?q=")[[1]][2]
  }

  if(is.null(search_terms)==FALSE){
    search_strat <- litsearchr::write_search(search_terms, languages=languages, stemming=TRUE, exactphrase=TRUE)[[1]]
    search_strat <- gsub("\"","%22",gsub("\\)", "%29", gsub("\\(", "%28", gsub(" ", "+", gsub("\\\\", "",  gsub(" OR ", "+OR+", search_strat))))))

  }
  if(length(search_strat)==0){
    print("Error. No search terms or URL provided. Aborting.")
  }

  if(length(search_strat)>0){

  if(writefile==TRUE){if(utils::menu(c("yes", "no"),
                              title="This will write a .csv to your working directory with all the hits returned. Are you sure that you want to do that?")==2){
    writefile <- FALSE
  }
  }
  URL1 <- "http://www.worldcat.org/search?q="

    search_string <- paste(URL1, search_strat, "&start=1", sep="")

  firstpage <- as.character(xml2::read_html(search_string))

  if(stringr::str_detect(firstpage, "Request-URI Too Large")==FALSE){

    total_hits <- as.numeric(strsplit(strsplit(firstpage, "> of about <strong>")[[1]][2], "</strong>")[[1]][1])
    npages <- floor(total_hits/10)
    if(floor(total_hits/10)==ceiling(total_hits/10)){npages <- floor((total_hits-1)/10)}
    if(npages==0){npages <- 1}

    for(h in 1:npages){
      if(h==1){
        website <- firstpage
      }
      if(h>1){
        current_URL <- paste(URL1, search_strat, "&start=", paste(h,"1", sep=""), sep="")
        website <- as.character(xml2::read_html(current_URL))
      }

      articles <- strsplit(website, "result details")[[1]][-1]

      for(i in 1:length(articles)){
        current_article <- articles[i]
        title <- strsplit(strsplit(strsplit(strsplit(current_article, "class=\"name\">")[[1]][2], "</div")[[1]][1],"<strong>")[[1]][2], "</strong>")[[1]][1]
        authors <- strsplit(strsplit(current_article, ">by ")[[1]][2], "</div")[[1]][1]
        type <- strsplit(strsplit(current_article, "itemType\">")[[1]][2], "</span>")[[1]][1]
        language <- strsplit(strsplit(current_article, "itemLanguage\">")[[1]][2], "</span>")[[1]][1]
        if(stringr::str_detect(current_article, "itemPublisher")){
          publication <- strsplit(strsplit(current_article, "itemPublisher\">")[[1]][2], "</span>")[[1]][1]
          pubinfo <- ""
        }
        if(stringr::str_detect(current_article, "type\">Publication: ")){
          pub <- strsplit(strsplit(current_article, "type\">Publication: ")[[1]][2], "</div>")[[1]][1]
          publication <- strsplit(pub, ",")[[1]][1]
          pubinfo <- strsplit(pub, ",")[[1]][2]
        }
        article_entry <- cbind(title, authors, type, language, publication, pubinfo)
        if(i==1){page_hits <- article_entry}
        if(i>1){page_hits <- rbind(page_hits, article_entry)}
      }
      if(h==1){study_data <- page_hits}
      if(h>1){study_data <- rbind(study_data, page_hits)}
      if(verbose==TRUE){print(paste("Done with page", h, "out of", npages)      )      }

    }
    study_data <- as.data.frame(study_data)

    study_data$database <- rep("worldcat_scrape", nrow(study_data))

    if(writefile==TRUE){
      write.csv(study_data, "WorldCat_hits.csv")
    }

    return(study_data)
  }

  if(stringr::str_detect(firstpage, "Request-URI Too Large")==TRUE){
    print("Error: Request to server too long. Modify your search string to include fewer terms. Aborting.")
  }
  }
}

#' Scrapes results from Web of Science databases
#' @description Scrapes hits from databases hosted on Web of Science. The exported session must be left active while running the script and you must sign in through your institutional login.
#' @param writefile if TRUE, writes results to a .csv file in the working directory
#' @param qid if you have run more than one query in a session, check your search history to change the query id
#' @param verbose if TRUE, prints percentage updates every now and then so you know it is still running
#' @return a dataframe of hits returned
scrape_WoS <- function(URL=NULL, writefile=TRUE, qid="1", verbose=TRUE, panicmode=FALSE, panicrate=100){

  if(panicmode==TRUE){
    if(utils::menu(c("yes", "no"),
                   title="You've turned on panic mode. This will write a file with the data set at the specified panic rate (i.e. number of hits). Are you sure you want to do this?")==2){
      panicmode <- FALSE
    }
  }

  sessionID <- strsplit(strsplit(URL, "&SID=")[[1]][2], "&")[[1]][1]
  dbID <- strsplit(strsplit(URL, "product=")[[1]][2], "&")[[1]][1]

  if(writefile==TRUE){if(utils::menu(c("yes", "no"),
                              title="This will write a .csv to your working directory with all the hits returned. Are you sure that you want to do that?")==2){
    writefile <- FALSE
  }
  }

  base_URL1 <- "http://apps.webofknowledge.com/full_record.do?product="
  base_URL2 <- "&search_mode=GeneralSearch&qid="

  firstsite <- as.character(xml2::read_html(paste(base_URL1, dbID, base_URL2, qid, "&SID=", sessionID, "&page=1&doc=1", sep="")))

  total_hits <- stringr::str_trim(strsplit(strsplit(strsplit(strsplit(firstsite, "paginationNext")[[1]][1], "paginationForm")[[1]][2], "of")[[1]][2], "<a ")[[1]][1])
  total_hits <- as.numeric(gsub(",", "", total_hits))

  if(total_hits > 100000){total_hits <- 100000}

  for(i in 1:total_hits){
    if(i==1){website <- firstsite}
    if(i>1){
      search_string <- paste(base_URL1, dbID, base_URL2, qid, "&SID=", sessionID, "&page=1&doc=", i, sep="")
      website <- as.character(xml2::read_html(search_string))
    }

    article <- strsplit(strsplit(as.character(website), "FRleftColumn")[[1]][2], "tmp_associatedDigitalRecords_records")[[1]][1]
    title <- strsplit(strsplit(article, "title\">\n<value>")[[1]][2], "</value></p>")[[1]][1]

    authorlist <- strsplit(strsplit(strsplit(article, "By:</span>")[[1]][2], "block-record-info-source")[[1]][1],
                           "</a>")[[1]]

    for(j in 1:(length(authorlist)-1)){
      author <- strsplit(authorlist[j], "\">")[[1]][2]
      if(j==1){authors <- author}
      if(j>1){authors <- paste(authors, author, sep="; ")}
    }

    publication <- strsplit(strsplit(article, "sourceTitle\">\n<value>")[[1]][2], "</value")[[1]][1]
    pubinfo <- strsplit(strsplit(article, "block-record-info-source-values")[[1]][2], "block-record-info")[[1]][1]
    vol <- strsplit(strsplit(pubinfo, "Volume:</span>")[[1]][2], "</p>")[[1]][1]
    issue <- strsplit(strsplit(pubinfo, "Issue:</span>")[[1]][2], "</p>")[[1]][1]
    pgs <- strsplit(strsplit(pubinfo, "Pages:</span>")[[1]][2], "</p>")[[1]][1]
    doi <- strsplit(strsplit(pubinfo, "DOI:</span>")[[1]][2], "</p>")[[1]][1]
    date <- strsplit(strsplit(pubinfo, "Published:</span>")[[1]][2], "</p>")[[1]][1]
    type <- strsplit(strsplit(pubinfo, "Document Type:</span>")[[1]][2], "</p>")[[1]][1]

    abstract <- strsplit(strsplit(article, "Abstract</div>\n<p class=\"FR_field\">")[[1]][2], "</p>\n</div>")[[1]][1]
    address <- strsplit(strsplit(article, "Addresses:</span>")[[1]][2], "</p>")[[1]][1]
    email <- strsplit(strsplit(article, "mailto:")[[1]][2], "\">")[[1]][1]

    # This needs to be cleaned up still
    taxa <- strsplit(strsplit(article, "Taxonomic Data:</span>")[[1]][2], "</table>")[[1]][1]

    keywords <- strsplit(strsplit(article, "Miscellaneous Descriptors:</span>\n<value>")[[1]][2], "</value>")[[1]][1]
    language <- strsplit(strsplit(article, "Language:</span>")[[1]][2], "</p>")[[1]][1]
    id <- strsplit(strsplit(article, "Accession Number:</span>")[[1]][2], "</p>")[[1]][1]
    issn <- strsplit(strsplit(article, "ISSN:</span>")[[1]][2], "</p>")[[1]][1]

    entry <- cbind(id, title, abstract, authors, publication, vol, issue, pgs, doi,
                   date, type, address, email, taxa, keywords, language, issn)

    if(i==1){dataset <- entry}
    if(i>1){dataset <- rbind(dataset, entry)}
    if(verbose==TRUE){if(i/total_hits*100==floor(i/total_hits*100)){print(paste(i/total_hits*100, "% done.", sep=""))}}
    if(panicmode==TRUE){
      if(i/panicrate==floor(i/panicrate)){
        filename <- paste("WoS_panic_", i, ".csv", sep="")
        write.csv(dataset, filename)
      }
    }
  }
  dataset <- as.data.frame(dataset)

  dataset$database <- rep("wos_scrape", nrow(dataset))

  if(writefile==TRUE){
    write.csv(dataset, "WebOfScience_hits.csv")

  }
  return(dataset)
}

#' Scrapes results from Ingenta Connect
#' @description Scrapes hits from Ingenta Connect. Query length is limited by server requests, which can be triggered either by excessively long queries or by vague queries that return too many results. Must login through institutional access.
#' @param search_terms a list of character strings with grouped search terms
#' @param URL the URL from conducting a search in the database
#' @param writefile if TRUE, writes results to a .csv file in the working directory
#' @param verbose if TRUE, prints which page of hits it has finished
#' @param languages which language to search in; available languages can be viewed with available_languages().
#' @param stemming if TRUE, keywords will be truncated and stem from root word forms (only if language is English)
#' @param exactphrase if TRUE, keyword phrases will be placed in quotes to ensure exact phrases are returned#' @return a database of hits (if yes is selected from the menu prompt, the hits will also be saved to your working directory)
scrape_ingenta <- function(search_terms=NULL, URL=NULL, writefile=TRUE, verbose=TRUE, languages="English", stemming=TRUE, exactphrase=TRUE){

  if(is.null(URL)==FALSE){
    search_strat <- strsplit(URL, "value1=")[[1]][2]
  }

  if(is.null(search_terms)==FALSE){
    search_strat <- litsearchr::write_search(search_terms, languages=languages, stemming=TRUE, exactphrase=TRUE)[[1]]
    search_strat <- gsub("\\)","%29",gsub("\\(", "%28", gsub("\"", "%22", gsub(" ", "+", gsub(" \\)", "%29", gsub("\\( ", "%28", gsub("\\\\", "",  gsub(" OR ", "+OR+", search_strat))))))))

  }
  if(length(search_strat)==0){
    print("Error. No search terms or URL provided. Aborting.")
  }

  if(length(search_strat)>0){


  if(writefile==TRUE){if(utils::menu(c("yes", "no"),
                              title="This will write a .csv to your working directory with all the hits returned. Are you sure that you want to do that?")==2){
    writefile <- FALSE
  }
  }

  base_URL <- "https://www.ingentaconnect.com/search/article?option1=tka&value1="

  first_search <- gsub(" ", "+", paste("https://www.ingentaconnect.com/search?option1=tka&value1=", search_strat, "&pageSize=100&page=1", sep=""))
  firstpage <- as.character(xml2::read_html(first_search))

  nhits <- strsplit(strsplit(firstpage, "<span class=\"rust\">")[[1]][2], "</span>")[[1]][1]
  if(stringr::str_detect(nhits, " of ")==TRUE){
    nhits <- as.numeric(strsplit(nhits, " of ")[[1]][2])
  }
  nhits <- as.numeric(nhits)

  for(l in 1:nhits){

      search_string <- paste(base_URL, search_strat, "&pageSize=100&index=", l, sep="")
      webpage <- as.character(xml2::read_html(search_string))

    title <- strsplit(strsplit(webpage, "DC.title\" content=\"")[[1]][2], "\">\n")[[1]][1]
    publisher <- strsplit(strsplit(webpage, "DC.publisher\" content=\"")[[1]][[2]], "\">\n")[[1]][1]
    if(stringr::str_detect(webpage, "DC.creator")){
      authorlist <- strsplit(strsplit(webpage, "DC.creator\" content=\"")[[1]][2], "\">\n<meta name=\"")[[1]][1]
    }
    if(stringr::str_detect(webpage, "DC.identifier")){
      authorlist <- strsplit(strsplit(webpage, "DC.identifier\" content=\"")[[1]][1], "DC.creator\" content=\"")[[1]][-1]
      doi <- strsplit(strsplit(webpage, "DC.identifier\" content=\"")[[1]][[2]], "\">\n")[[1]][1]
    }

    for(a in 1:length(authorlist)){
      author <- strsplit(authorlist[a], "\">")[[1]][1]
      if(a==1){
        authors <- author
      }
      if(a>1){
        authors <- paste(authors, author, sep="; ")
      }
    }

    date <- strsplit(strsplit(webpage, "DCTERMS.issued\" content=\"")[[1]][[2]], "\">\n")[[1]][1]
    pubinfo <- strsplit(strsplit(webpage, "DCTERMS.bibliographicCitation\" content=\"")[[1]][[2]], "\">\n")[[1]][1]
    abstract <- gsub("\n", "", strsplit(strsplit(webpage, "<div id=\"Abst\" class=\"tab-pane active\">")[[1]][2], "</div>")[[1]][1])

    entry <- cbind(title, abstract, authors, pubinfo, date, doi, publisher)

    if(l==1){
      study_data <- entry
    }
    if(l>1){
      study_data <- rbind(study_data, entry)
    }
    if(verbose==TRUE){
      if(l/nhits*100==floor(l/nhits*100)){
        print(paste(l/nhits*100, "% Done", sep="")      )      }

    }

  }
  study_data <- as.data.frame(study_data)

  study_data$database <- rep("ingenta_scrape", nrow(study_data))

  if(writefile==TRUE){
    write.csv(study_data, "ingenta_hits.csv")
  }

  return(study_data)
  }
}

#' Scrapes results from PubMed
#' @description Scrapes hits from PubMed. Query length is limited by server requests, which can be triggered either by excessively long queries.
#' @param search_terms a list of character strings with grouped search terms
#' @param URL the URL from conducting a search in the database
#' @param writefile if TRUE, writes results to a .csv file in the working directory
#' @param verbose if TRUE, prints which page of hits it has finished
#' @param languages which language to search in; available languages can be viewed with available_languages().
#' @param stemming if TRUE, keywords will be truncated and stem from root word forms (only if language is English)
#' @param exactphrase if TRUE, keyword phrases will be placed in quotes to ensure exact phrases are returned#' @return a database of hits (if yes is selected from the menu prompt, the hits will also be saved to your working directory)
scrape_pubmed <- function(search_terms=NULL, URL=NULL, writefile=TRUE, verbose=TRUE, languages="English", stemming=TRUE, exactphrase=TRUE){

  if(is.null(URL)==FALSE){
    search_strat <- strsplit(URL, "term=")[[1]][2]
  }

  if(is.null(search_terms)==FALSE){
    search_strat <- litsearchr::write_search(search_terms, languages=languages, stemming=TRUE, exactphrase=TRUE)[[1]]
    search_strat <- gsub("\"", "%22", gsub("\\)", "%29", gsub("\\(", "%28", gsub(" ", "%20", gsub("\\\\", "",  gsub(" OR ", "%20OR%20", search_strat))))))

  }
  if(length(search_strat)==0){
    print("Error. No search terms or URL provided. Aborting.")
  }

  if(length(search_strat)>0){

  base_URL <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pmc&retmax=100000&sort=date&term="
  if(writefile==TRUE){if(utils::menu(c("yes", "no"),
                              title="This will write a .csv to your working directory with all the hits returned. Are you sure that you want to do that?")==2){
    writefile <- FALSE
  }
  }


  firstURL <- paste(base_URL, search_strat, sep="")
  firstpage <- as.character(xml2::read_xml(firstURL))

  nhits <- as.numeric(strsplit(strsplit(firstpage, "<Count>")[[1]][2], "</Count>")[[1]][1])

  IDs <- strsplit(strsplit(strsplit(firstpage, "<IdList>")[[1]][2], "</IdList>")[[1]][1], "<Id>")[[1]][-1]

  for(i in 1013:length(IDs)){
    id <- strsplit(IDs[i], "</Id")[[1]][1]

    URL <- paste("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC", id, sep="")
    webpage <- as.character(xml2::read_html(URL))

    title <- strsplit(strsplit(webpage, "<title>")[[1]][2], "</title>")[[1]][1]
    authors <- strsplit(strsplit(webpage, "citation_authors\" content=\"")[[1]][2], "\">")[[1]][1]
    source <- strsplit(strsplit(webpage, "citation_journal_title\" content=\"")[[1]][2], "\">")[[1]][1]
    year <- strsplit(strsplit(webpage, "citation_date\" content=\"")[[1]][2], "\">")[[1]][1]
    volume <- strsplit(strsplit(webpage, "citation_volume\" content=\"")[[1]][2], "\">")[[1]][1]
    doi <- strsplit(strsplit(webpage, "citation_doi\" content=\"")[[1]][2], "\">")[[1]][1]
    url <- strsplit(strsplit(webpage, "og:url\" content=\"")[[1]][2], "\">")[[1]][1]
    abstract <- strsplit(strsplit(webpage, "Abstract</h2>")[[1]][2], "</div>")[[1]][1]
    keywords <- strsplit(strsplit(webpage, "Keywords: </strong><span class=\"kwd-text\">")[[1]][2], "</span")[[1]][1]

    entry <- cbind(id, title, abstract, keywords, authors, year, source, volume, doi, url)

    if(i==1){
      study_data <- entry
    }
    if(i >1){
      study_data <- rbind(study_data, entry)
    }
print(i)
    if(verbose==TRUE){
      if(i/length(IDs)*10==floor(i/length(IDs)*10)){
        print(paste(i/length(IDs)*100, "% Done", sep="")      )      }

      }
  }
  study_data <- as.data.frame(study_data)

  study_data$database <- rep("pubmed_scrape", nrow(study_data))

  if(writefile==TRUE){
    write.csv(study_data, "pubmed_hits.csv")
  }
  return(study_data)

  }
  }

#' Scrapes results from CAB Direct
#' @description Scrapes hits from CAB Direct. Query length is limited by server requests, which can be triggered either by excessively long queries. Must login through institutional access.
#' @param search_terms a list of character strings with grouped search terms
#' @param URL the URL from conducting a search in the database
#' @param writefile if TRUE, writes results to a .csv file in the working directory
#' @param verbose if TRUE, prints which page of hits it has finished
#' @param languages which language to search in; available languages can be viewed with available_languages().
#' @param stemming if TRUE, keywords will be truncated and stem from root word forms (only if language is English)
#' @param exactphrase if TRUE, keyword phrases will be placed in quotes to ensure exact phrases are returned#' @return a database of hits (if yes is selected from the menu prompt, the hits will also be saved to your working directory)
scrape_CABDirect <- function(search_terms=NULL, URL=NULL, writefile=TRUE, verbose=TRUE, languages="English", stemming=TRUE, exactphrase=TRUE){

  if(is.null(URL)==FALSE){
    search_strat <- strsplit(URL, "?q=")[[1]][2]
  }

  if(is.null(search_terms)==FALSE){
    search_strat <- litsearchr::write_search(search_terms, languages=languages, stemming=TRUE, exactphrase=TRUE)[[1]]
    search_strat <- gsub("\"", "%22", gsub("\\)", "%29", gsub("\\(", "%28", gsub(" ", "%20", gsub("\\\\", "",  gsub(" OR ", "%20OR%20", search_strat))))))

  }
  if(length(search_strat)==0){
    print("Error. No search terms or URL provided. Aborting.")
  }

  if(length(search_strat)>0){

  base_URL <- "https://www.cabdirect.org/cabdirect/search/?q="

  if(writefile==TRUE){if(utils::menu(c("yes", "no"),
                              title="This will write a .csv to your working directory with all the hits returned. Are you sure that you want to do that?")==2){
    writefile <- FALSE
  }
  }


  firstURL <- paste(base_URL, search_strat, "&rows=100", sep="")
  firstpage <- as.character(xml2::read_html(firstURL))

  nhits <- as.numeric(stringr::str_trim(strsplit(strsplit(strsplit(firstpage, "cd4NumberOfSearchResults")[[1]][2], "</span>")[[1]][1], "\n")[[1]][2]))

  npages <- floor(nhits/100)+1
  if(is.na(npages)==TRUE){print("No hits found.")}
  if(is.na(npages)==FALSE){

  for(i in 1:npages){
    if(i==1){
      webpage <- firstpage
    }
    if(i>1){
      URL <- paste(base_URL, search_strat, "&rows=100&page=", i, sep="")
      webpage <- as.character(xml2::read_html(URL))
    }

    articles <- strsplit(strsplit(strsplit(webpage, "col-sm-12 cd4-content-entry-list")[[1]][2], "clearfix form-inline sorting light-green")[[1]][1], "list-content")[[1]][-1]

    for(k in 1:length(articles)){
      title <- strsplit(strsplit(articles[k], ";\">")[[1]][2], "</a")[[1]][1]
      authorlist <- strsplit(strsplit(strsplit(articles[k], "Author\\(s\\)")[[1]][2], "<strong>")[[1]][1], "</a>")[[1]]

      for(a in 1:length(authorlist)){
        author <- strsplit(authorlist[a], "\">")[[1]][2]
        if(a==1){
          authors <- author
        }
        if(a>1){
          if(is.na(author)==FALSE)
            authors <- paste(authors, author, sep="; ")
        }
      }
      publisher <- strsplit(strsplit(articles[k], "Publisher</strong> : ")[[1]][2], "</p")[[1]][1]
      pubinfo <- "Not avail"

      if(stringr::str_detect(articles[k], "Bulletin</strong> : ")){
        source <- strsplit(strsplit(strsplit(articles[k], "Bulletin</strong> : ")[[1]][2], "\">")[[1]][2], "</a>")[[1]][1]
        pubinfo <- strsplit(strsplit(strsplit(strsplit(articles[k], "Bulletin</strong> : ")[[1]][2], "\">")[[1]][2], "</a>")[[1]][2], "</p>")[[1]][1]
      }

      if(stringr::str_detect(articles[k], "Journal article</strong> : ")){
        source <- strsplit(strsplit(strsplit(articles[k], "Journal article</strong> : ")[[1]][2], "\">")[[1]][2], "</a>")[[1]][1]
        pubinfo <- strsplit(strsplit(strsplit(strsplit(articles[k], "Journal article</strong> : ")[[1]][2], "\">")[[1]][2], "</a>")[[1]][2], "</p>")[[1]][1]
      }

      entry <- cbind(title, authors, publisher, source, pubinfo)
      if(k==1){
        page_data <- entry
      }
      if(k>1){
        page_data <- rbind(page_data, entry)
      }

    }

    if(i==1){
      study_data <- page_data
    }
    if(i>1){
      study_data <- rbind(study_data, page_data)
    }
if(verbose==TRUE){print(paste("Done with page", i, "out of", npages))
  }

      }
  study_data <- as.data.frame(study_data)

  study_data$database <- rep("cabdirect_scrape", nrow(study_data))

  if(writefile==TRUE){
    write.csv(study_data, "CABDirect_hits.csv")
  }
  return(study_data)
  }
  }
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
scrape_ndltd <- function(search_terms=NULL, URL=NULL, writefile=TRUE, verbose=TRUE, languages="English", stemming=TRUE, exactphrase=TRUE, where=c("description", "title")){

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

