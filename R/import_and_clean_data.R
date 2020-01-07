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
    filename <- paste(directory, list.files(directory), sep="")
  }else if(!is.null(file)){
    filename <- file
  }else{stop(print("Supply either a directory or a file containing search results."))}

  df <-
    synthesisr::import_refs(filename = filename,
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
                               method = c("stringdist",  "fuzzdist", "exact")) {
  dups <-
    synthesisr::find_duplicates(
      data = df,
      match_variable = field,
      match_function = method,
      to_lower = TRUE,
      rm_punctuation = TRUE
    )
  df <- synthesisr::deduplicate(df, dups)
  return(df)

}
