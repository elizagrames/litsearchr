search_terms <- list(c("black-backed woodpecker", "picoides arcticus"),
                     c("fire", "burn"))

## need a way to specify where to search (presumed title, abs, key when available)
## make these all connect to the litsearchr import option using the database tag

#### DONE: Google Scholar ####
## only goes up to 256 characters
## will stop at 60 pages due to captcha issues


scrape_google_scholar <- function(search_terms, writefile=TRUE){

  search_strat <- write_search(search_terms, languages= "English", exactphrase=TRUE)
  search_strat <- gsub("\\)", "%29", gsub("\\(", "%28", gsub(" ", "", gsub("\\\\", "",  gsub(" OR ", "+OR+", search_strat)))))
  if(nchar(search_strat)<=256){

    if(writefile==TRUE){if(menu(c("yes", "no"),
                                title="This will write a .csv to your working directory. Are you sure that you want to do that?")==2){
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
    }
    study_data <- as.data.frame(study_data)
    study_data$database <- rep("google_scholar_scrape", nrow(study_data))

    if(writefile==TRUE){
      write.csv(species_data, "google_scholar_hits.csv")
    }

    return(study_data)
  }


  if(nchar(search_strat) > 256){
    print("Error: Google Scholar only allows search strings under 256 characters. Aborting.")}


}


#### DONE: JSTOR ####
## only goes up to 256 characters

scrape_jstor <- function(search_terms, writefile=TRUE){

  base_URL1 <- "https://www.jstor.org/action/doAdvancedSearch?searchType=facetSearch&page="
  base_URL2 <- "&sd=&ed=&c3=AND&c4=AND&f5=all&c5=AND&f1=all&acc=off&f3=all&group=none&f0=all&c6=AND&c1=AND&c2=AND&q0="
  base_URL3 <- "&f4=all&f6=all&f2=all"

  if(writefile==TRUE){if(menu(c("yes", "no"),
                              title="This will write a .csv to your working directory. Are you sure that you want to do that?")==2){
    writefile <- FALSE
  }
  }


  search_strat <- write_search(search_terms, languages= "English", exactphrase=TRUE, stemming=FALSE)
  search_strat <- gsub("\\)", "%29", gsub("\\(", "%28", gsub(" ", "+", gsub("\\\\", "",  gsub(" OR ", "+OR+", search_strat)))))
  if(nchar(search_strat)<=256){

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
    }

    study_data <- as.data.frame(study_data)

    study_data$database <- rep("JSTOR_scrape", nrow(study_data))
    if(writefile==TRUE){
      write.csv(study_data, "jstor_hits.csv")
    }

    return(study_data)

  }

  if(nchar(search_strat) > 256){
    print("Error: JSTOR only allows search strings under 256 characters. Aborting.")}

}

#### NO ABSTRACTS: Scopus ####

## need to modify this to grab the url of the hit and pull abstracts and keywords from there
## currently it pulls the eid, but is running into problems scraping that

scrape_scopus <- function(search_terms, writefile=TRUE){

  if(writefile==TRUE){if(menu(c("yes", "no"),
                              title="This will write a .csv to your working directory. Are you sure that you want to do that?")==2){
    writefile <- FALSE
  }
  }

  scopus_base <- "https://www.scopus.com/results/results.uri?sort=plf-f&src=s&sid=f68938845668763794e120e1ed0927e2&sot=a&sdt=a&sl=24&s=TITLE-ABS-KEY-AUTH"

  search_strat <- write_search(search_terms, languages= "English", exactphrase=TRUE, stemming=FALSE)
  search_strat <- gsub("\"","'",gsub("\\)", "%29", gsub("\\(", "%28", gsub(" ", "+", gsub("\\\\", "",  gsub(" OR ", "+OR+", search_strat))))))
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
            if(l==1){author_list <- author}
            if(l>1){author_list <- paste(author_list, author, sep="; ")}
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


        article_entry <- cbind(id, title, author_list, year, publication, volume, issue, pages, doi)
        if(k==1){page_entry <- article_entry}
        if(k>1){page_entry <- rbind(page_entry, article_entry)}
      }

      if(j==1){study_data <- page_entry}
      if(j>1){study_data <- rbind(study_data, page_entry)}
    }

    study_data <- as.data.frame(study_data)
    study_data$database <- rep("Scopus_scrape", nrow(study_data))


    if(writefile==TRUE){
      write.csv(study_data, "scopus_hits.csv")
    }

    return(study_data)
  }

}

#### DONE: WorldCat ####

## has some undefined query limit
## probably don't actually want to use
## probably has 2048 URI length


scrape_worldcat <- function(search_terms, writefile=TRUE){

  if(writefile==TRUE){if(menu(c("yes", "no"),
                              title="This will write a .csv to your working directory. Are you sure that you want to do that?")==2){
    writefile <- FALSE
  }
  }
  URL1 <- "http://www.worldcat.org/search?q="

  search_strat <- write_search(search_terms, languages= "English", exactphrase=TRUE, stemming=FALSE)
  search_strat <- gsub("\"","%22",gsub("\\)", "%29", gsub("\\(", "%28", gsub(" ", "+", gsub("\\\\", "",  gsub(" OR ", "+OR+", search_strat))))))
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
    }
    study_data <- as.data.frame(study_data)

    study_data$database <- rep("WorldCat_scrape", nrow(study_data))

    if(writefile==TRUE){
      write.csv(study_data, "WorldCat_hits.csv")
    }

    return(study_data)
  }

  if(stringr::str_detect(firstpage, "Request-URI Too Large")==TRUE){
    print("Error: Request to server too long. Modify your search string to include fewer terms. Aborting.")
  }
}

#### DONE: WoS ####

sessionID <- "6Co3BUExHQQ8vWNdDXL"

search_WoS <- function(sessionID, dbID="UA", writefile=TRUE, qid="1", verbose=TRUE){

  if(writefile==TRUE){if(menu(c("yes", "no"),
                              title="This will write a .csv to your working directory. Are you sure that you want to do that?")==2){
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
      search_string <- paste(base_URL, qid, "&SID=", sessionID, "&page=1&doc=", i, sep="")
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
  }
  study_data <- as.data.frame(study_data)

  study_data$database <- rep("WoS_scrape", nrow(study_data))

  if(writefile==TRUE){
    write.csv(dataset, "WebOfScience_hits.csv")

  }
  return(dataset)
}



#### DONE: Ingenta Connect ####
## uri limit, presumed 2048

scrape_ingenta <- function(search_terms, writefile=TRUE){
  if(writefile==TRUE){if(menu(c("yes", "no"),
                              title="This will write a .csv to your working directory. Are you sure that you want to do that?")==2){
    writefile <- FALSE
  }
  }

  base_URL <- "https://www.ingentaconnect.com/search/article?option1=tka&value1="
  search_strat <- write_search(search_terms, languages= "English", exactphrase=TRUE)
  search_strat <- gsub("\\)","%29",gsub("\\(", "%28", gsub("\"", "%22", gsub(" ", "+", gsub(" \\)", "%29", gsub("\\( ", "%28", gsub("\\\\", "",  gsub(" OR ", "+OR+", search_strat))))))))

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

  }
  study_data <- as.data.frame(study_data)

  study_data$database <- rep("Ingenta_scrape", nrow(study_data))

  if(writefile==TRUE){
    write.csv(study_data, "ingenta_hits.csv")
  }

  return(study_data)
}


#### DONE: PubMed ####
# query is limited to 2048 characters because of URI

scrape_pubmed <- function(search_terms, writefile=TRUE){

  base_URL <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pmc&retmax=100000&sort=date&term="
  if(writefile==TRUE){if(menu(c("yes", "no"),
                              title="This will write a .csv to your working directory. Are you sure that you want to do that?")==2){
    writefile <- FALSE
  }
  }

  search_strat <- write_search(search_terms, languages= "English", exactphrase=TRUE, stemming=FALSE)
  search_strat <- gsub("\"", "%22", gsub("\\)", "%29", gsub("\\(", "%28", gsub(" ", "%20", gsub("\\\\", "",  gsub(" OR ", "%20OR%20", search_strat))))))

  firstURL <- paste(base_URL, search_strat, sep="")
  firstpage <- as.character(xml2::read_xml(firstURL))

  nhits <- as.numeric(strsplit(strsplit(firstpage, "<Count>")[[1]][2], "</Count>")[[1]][1])

  IDs <- strsplit(strsplit(strsplit(firstpage, "<IdList>")[[1]][2], "</IdList>")[[1]][1], "<Id>")[[1]][-1]

  for(i in 1:length(IDs)){
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
  }
  study_data <- as.data.frame(study_data)

  study_data$database <- rep("PubMed_scrape", nrow(study_data))

  if(writefile==TRUE){
    write.csv(study_data, "pubmed_hits.csv")
  }
  return(study_data)

}



#### DONE: CABDirect ####
# 2048 character limit (presumed)

scrape_CABDirect <- function(search_terms, writefile=TRUE){
  base_URL <- "https://www.cabdirect.org/cabdirect/search/?q="

  if(writefile==TRUE){if(menu(c("yes", "no"),
                              title="This will write a .csv to your working directory. Are you sure that you want to do that?")==2){
    writefile <- FALSE
  }
  }

  search_strat <- write_search(search_terms, languages= "English", exactphrase=TRUE, stemming=FALSE)
  search_strat <- gsub("\"", "%22", gsub("\\)", "%29", gsub("\\(", "%28", gsub(" ", "%20", gsub("\\\\", "",  gsub(" OR ", "%20OR%20", search_strat))))))

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
  }
  study_data <- as.data.frame(study_data)

  study_data$database <- rep("CABDirect_scrape", nrow(study_data))

  if(writefile==TRUE){
    write.csv(study_data, "CABDirect_hits.csv")
  }
  return(study_data)
  }
}

##### testing area #####

#googlesch <- scrape_google_scholar(search_terms = search_terms)

jstor <- scrape_jstor(search_terms = search_terms)
scopus <- scrape_scopus(search_terms = search_terms)
worldcat <- scrape_worldcat(search_terms = search_terms)
ingenta <- scrape_ingenta(search_terms = search_terms)
pubmed <- scrape_pubmed(search_terms = search_terms)
cabdirect <- scrape_CABDirect(search_terms = search_terms)


#### bielefeld ####

# needs %28 %29 instead of ( )
# doesn't return many hits for complex queries


#### wiley online library ####

# server error, presumed URI > 2048

#### doaj ####

# unknown query limit
# not many hits for complex queries

