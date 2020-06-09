#' Import results of a search
#' @description Given a file or directory containing bibliographic files, import and assemble search results
#' @param directory a path to a directory containing search results to import
#' @param file a file of search results to import
#' @param verbose if TRUE, prints status updates
#' @details If a .bib or .ris file returns an error, there is likely a character encoding or formatting issue with the file. Please try uploading your .bib or .ris file to a reference manager (e.g. Zotero) and re-exporting it as a .bib or .ris, which often clears up formatting issues that import_results cannot detect.
#' @return a data frame of assembled search results
import_results <-  function(directory = NULL,
                            file=NULL,
                            verbose = TRUE) {
  if(!is.null(directory)){
    filename <- list.files(directory, full.names = TRUE)
  }else if(!is.null(file)){
    filename <- file
  }else{stop(print("Supply either a directory or a file containing search results."))}

  df <-
    synthesisr::read_refs(filename = filename,
                            return_df = TRUE,
                            verbose = verbose)
  return(df)
}


#' Remove duplicate entries from a data frame
#' @description Calls the deduplicate function from synthesisr to flag and remove duplicate entries from a data frame
#' @param df the data frame to deduplicate
#' @param field the name or index of the column to check for duplicate values
#' @param method the manner of duplicate detection; exact removes exact text duplicates, stringdist removes duplicates below a similarity threshold, and fuzzy uses fuzzdist matching
#' @return a deduplicated data frame
#' @example inst/examples/remove_duplicates.R
remove_duplicates <-  function(df,
                               field,
                               method = c("string_osa",  "fuzzdist", "exact")) {
  df <-
    synthesisr::deduplicate(
      data = df,
      match_by = field,
      method = method,
      to_lower = TRUE,
      rm_punctuation = TRUE
    )
  return(df)

}




#' Remove duplicate studies and punctuation
#' @description Replaces all miscellaneous punctuation marks used to separate keywords and replaces them with a semicolon so that keywords properly separate in later steps.
#' @param keywords a character vector containing keywords to clean
#' @return a data frame with keyword punctuation standardized
#' @example inst/examples/clean_keywords.R
clean_keywords <- function(keywords){
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
    keywords <- gsub(removals[i], keywords, replacement="")
  }

  # replace keyword separators with standardized semicolon
  replacements <- c(", ",
                    ",",
                    "/",
                    ";;",
                    ", ",
                    "\\[",
                    "\\]", "\\band\\b"
  )
  for (i in 1:length(replacements)){
    keywords <- gsub(replacements[i], keywords, replacement=";")
  }

  keywords <- gsub("  ", " ", keywords)
  keywords <- gsub("; ", ";", keywords)
  keywords <- gsub(" ;", ";", keywords)
  if(any(grepl(";;", keywords))){
    while(any(grepl(";;", keywords))){
      keywords <- gsub(";;", ";", keywords)
    }
  }

  return(keywords)
}
