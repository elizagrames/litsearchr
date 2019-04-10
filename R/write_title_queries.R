
# This function is inspired on a litsearchr's function named write_title_search()

#' Write a search to check title recall
#' @description Given a set of titles, writes two Boolean searches that can be used to find titles in Web of Science and/or Scopus
#' @param titles a character vector of titles
#' @param folder a character specifying where to save the outputs 
#' @return 1. a list of text strings and, if wanted, it saves two .txt files containing the search queries needed for Web of Science and Scopus in the indicated folder.
#'@example inst/examples/write_titles.R

write_title_queries <- function(titles, save_queries=TRUE, folder=NULL){
  
  titles.str <- paste0("(",'"',titles,'"',")")
  titles.list <- paste(titles.str,collapse=" OR ")
  
  titles.query.wos <- paste0("TS=(",titles.list,")")
  titles.query.scopus <- paste0("TITLE (",titles.list,")")
  
  if(save_queries==TRUE){
    if(utils::menu(c("yes", "no"),
                   title="This will save the search queries to .txt files in your working directory or specified directory. Do you want litsearchr to save save the search queries?")==1){
      if(is.null(folder)){
        
        sink("search_query_Web_of_Science.txt")
        cat(titles.query.wos)
        sink()
        
        sink("search_query_Scopus.txt")
        cat(titles.query.scopus)
        sink()
        
      } else{
        
        sink(paste0(folder,"/search_query_Web_of_Science.txt"))
        cat(titles.query.wos)
        sink()
        
        sink(paste0(folder,"/search_query_Scopus.txt"))
        cat(titles.query.scopus)
        sink()
        
      }
    }
  }
  
  query.list <- list(titles.query.wos,titles.query.scopus)
  names(query.list) <- c("Web_of_Science","Scopus")
  return(query.list)
  
}