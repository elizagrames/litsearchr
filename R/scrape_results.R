#### google scholar ####

## only goes to 256 characters
search_terms <- list(c("bird", "avian", "avifauna"), c("forest", "woodlot"), c("fragment*"))
x <- litsearchr::write_search(search_terms, languages = "English")

scrape_google_scholar <- function(search_terms, writefile=TRUE){
  
  search_string <- c()
  
  for(s in 1:length(search_terms)){
    termlist <- paste(search_terms[[s]], collapse="\"+OR+\"")
    search_string <- 
  }
  
  if(writefile==TRUE){if(menu(c("yes", "no"), 
                              title="This will write a .csv to your working directory. Are you sure that you want to do this?")==2){
    writefile <- FALSE
  }
  }
  
  base_URL <- "https://scholar.google.com/scholar?"
  woodpeckers <- read.csv("picidae_testlist.csv", header=FALSE, stringsAsFactors = FALSE)
  
  for(k in 1:2){
    
    species <- paste(paste("%22", woodpeckers[k,1], "+", woodpeckers[k,2],"%22", sep=""), "+OR+", "%22", woodpeckers[k,3], "%22", sep="")
    
    first_search <- gsub(" ", "+", paste(base_URL, "start=0&num=20&q=", species, sep=""))
    firstpage <- as.character(xml2::read_html(first_search))
    nhits <- gsub(",", "", strsplit(strsplit(firstpage, " results \\(")[[1]][1], "About ")[[1]][2])
    npages <- floor(as.numeric(nhits)/10)
    if(npages > 100){npages <- 100} # apparently Google Scholar only goes up to 100 pages
    
    for(l in 1:npages){
      
      if(l==1){
        webpage <- firstpage
      }
      if(l>1){
        search_string <- gsub(" ", "+", paste(base_URL, "start=", l*10, "&num=20&q=", species, sep=""))
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
        
        if(l==1){species_data <- dataset}
        if(l>1){species_data <- rbind(species_data, dataset)}
        
      }
    }
    if(writefile==TRUE){
      filename <- paste(woodpeckers[k,1], woodpeckers[k,2], "googlescholar.csv", sep="-")
      write.csv(species_data, filename)
    }
    return(species_data)
  }
}


##### jstor ####

## note: only 256 character searches allowed

woodpeckers <- read.csv("picidae_testlist.csv", header=FALSE, stringsAsFactors = FALSE)

base_URL1 <- "https://www.jstor.org/action/doAdvancedSearch?searchType=facetSearch&page="
base_URL2 <- "&sd=&ed=&c3=AND&c4=AND&f5=all&c5=AND&f1=all&acc=off&f3=all&group=none&f0=all&c6=AND&c1=AND&c2=AND&q0=%22"
base_URL3 <- "%22&f4=all&f6=all&f2=all"

for(k in 1:nrow(woodpeckers)){
  
  species <- paste(paste(woodpeckers[k,1], "%20", woodpeckers[k,2],"%22", sep=""), "%20OR%20", "%22", woodpeckers[k,3], sep="")
  species <- gsub(" ", "%20", species)
  
  firstURL <- paste(base_URL1, "1", base_URL2, species, base_URL3, sep="")
  firstpage <- as.character(xml2::read_html(firstURL))
  
  nhits <- as.numeric(strsplit(strsplit(firstpage, "data-result-count=\"")[[1]][2], "\"")[[1]][1])
  npages <- ceiling(nhits/25)
  
  for(w in 1:npages){
    if(w==1){website <- firstpage}
    if(w>1){
      URL <- paste(base_URL1, w, base_URL2, species, base_URL3, sep="")
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
    
    if(w==1){species_dataset <- dataset}
    if(w>1){species_dataset <- rbind(species_dataset, dataset)}
  }
  
  filename <- paste(woodpeckers[k,1],"-", woodpeckers[k,2], "-jstor.csv", sep="")
  write.csv(species_dataset, filename)
}

#### scopus ####

## need to modify this to grab the url of the hit and pull abstracts and keywords from there 

woodpeckers <- read.csv("../Pub_bias/Species_lists/picidae_testlist.csv", stringsAsFactors = FALSE, header=FALSE)

woodpeckers <- woodpeckers[1:20, ]

scopus_base <- "https://www.scopus.com/results/results.uri?sort=plf-f&src=s&sid=f68938845668763794e120e1ed0927e2&sot=a&sdt=a&sl=24&s=TITLE-ABS-KEY-AUTH%28"

starttime <- Sys.time()
for(i in 1:nrow(woodpeckers)){
  species <- paste(paste("%28%22", woodpeckers[i,1], "+", woodpeckers[i,2],"%22%29", sep=""), "+OR+", "%28%22", woodpeckers[i,3], "%22%29%29", sep="")
  search_string <- gsub(" ", "+", paste(scopus_base, species, sep=""))
  firstpage <- as.character(xml2::read_html(search_string))
  
  if(stringr::str_detect(firstpage, "No documents were found")){print(paste("No hits found for", woodpeckers[i, 3]))}
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
        #split out title, authors, etc... here
        title <- strsplit(strsplit(articles[k], ">")[[1]][2], "<")[[1]][1]
        
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
        
        
        article_entry <- cbind(title, author_list, year, publication, volume, issue, pages)
        if(k==1){page_entry <- article_entry}
        if(k>1){page_entry <- rbind(page_entry, article_entry)}
      }
      
      if(j==1){species_records <- page_entry}
      if(j>1){species_records <- rbind(species_records, page_entry)}
    }
    filename <- paste(woodpeckers[i,1], "-", woodpeckers[i,2], "-scopus.csv", sep="")
    write.csv(species_records, filename)
  }
  if(i==1){multispecies_hits <- total_hits}
  if(i>1){multispecies_hits <- multispecies_hits+total_hits}
  
}
endtime <- Sys.time()

time_per_hit <- as.numeric((endtime-starttime)/multispecies_hits)


#### worldcat ####

## has some undefined query limit
## probably don't actually want to use
## probably has 2048 URI length

woodpeckers <- read.csv("./picidae_testlist.csv", stringsAsFactors = FALSE)

URL1 <- "http://www.worldcat.org/search?q=%22"
URL2 <- "%22&fq=&dblist=638&start="

for(k in 1:nrow(woodpeckers)){
  species <- gsub(" ", "+", paste(paste(woodpeckers[k,1], woodpeckers[k,2], sep="+"), "%22 OR %22", woodpeckers[k,3], sep=""))
  
  firstURL <- paste(URL1, species, URL2, "1", "&qt=50", sep="")
  firstpage <- as.character(xml2::read_html(firstURL))
  
  total_hits <- as.numeric(strsplit(strsplit(firstpage, "> of about <strong>")[[1]][2], "</strong>")[[1]][1])
  npages <- floor(total_hits/10)
  if(floor(total_hits/10)==ceiling(total_hits/10)){npages <- floor((total_hits-1)/10)}
  if(npages==0){npages <- 1}
  
  for(h in 1:npages){
    if(h==1){
      website <- firstpage
    }
    if(h>1){
      current_URL <- paste(URL1, species, URL2, paste(npages,"1", sep=""), "&qt=50", sep="")
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
    if(h==1){species_dataset <- page_hits}
    if(h>1){species_dataset <- rbind(species_dataset, page_hits)}
  }
  filename <- paste(woodpeckers[k,1], "-", woodpeckers[k,2], "-WorldCat.csv", sep="")
  write.csv(species_dataset, filename)
}

#### wos ####

## needs to be given a session ID, not a query

starttime <- Sys.time()

base_URL <- "http://apps.webofknowledge.com/full_record.do?product=BCI&search_mode=AdvancedSearch&qid=1&SID=5EnW4l2xNXmc4YPqfTe&page=1&doc="

firstpage <- as.character(xml2::read_html(paste(base_URL, "1", sep="")))
firstsite <- as.character(xml2::read_html(firstpage))
total_hits <- stringr::str_trim(strsplit(strsplit(strsplit(strsplit(firstsite, "paginationNext")[[1]][1], "paginationForm")[[1]][2], "of")[[1]][2], "<a ")[[1]][1])
total_hits <- as.numeric(gsub(",", "", total_hits))

if(total_hits > 100000){total_hits <- 100000}

for(i in 1:1000){
  if(i==1){website <- firstsite}
  if(i>1){
    URL <- paste(base_URL, i, sep="")
    website <- as.character(xml2::read_html(URL))
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
  if(i/10==floor(i/10)){print(i/1000)}
}

write.csv(dataset, "WoSCC.csv")

endtime <- Sys.time()
runtime <- endtime - starttime



#### sciencedirect ####
# search only supports 8 boolean operators

#### agricola ####
# has some unknown query limit
# uses ? for truncation instead of * 
# probably 2048 characters

#### pubmed ####

# query is limited to 2048 characters because of URI
# otherwise works normally

#### cab direct ####
# 2048 character limit (presumed)

#### ingenta connect ####
## uri limit, presumed 2048

#### bielefeld ####

# needs %28 %29 instead of ( )
# doesn't return many hits for complex queries

#### wiley online library ####

# server error, presumed URI > 2048

#### doaj ####

# unknown query limit
# not many hits for complex queries

