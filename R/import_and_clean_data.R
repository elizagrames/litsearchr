#' Print databases/platform exports that litsearchr can import or search in
#' @description Prints a data frame of platforms, databases, and download methods that litsearchr recognizes
#' @examples usable_databases()
usable_databases <- function() {
  print(litsearchr::database_list)
}


#' Import results of a search
#' @description Given a file or directory, calls the import_results function from the synthesisr package to import and assemble search results
#' @param directory a path to a directory containing search results to import
#' @param filename a path to a filename containing search results to import
#' @param save_dataset if TRUE, saves the full search results to a .csv
#' @param save_directory the path to a directory where search results will be saved if save_dataset is set to TRUE
#' @param verbose if TRUE, prints status updates
#' @return a data frame of assembled search results
#' @example inst/examples/import_results.R
import_results <-  function(directory = NULL,
                            filename = NULL,
                            save_dataset = FALSE,
                            verbose = TRUE,
                            save_directory = "./") {
  df <- synthesisr::import_results(
    directory = directory,
    filename = filename,
    save_dataset = save_dataset,
    save_directory = save_directory,
    verbose = verbose
  )

  return(df)
}


#' Remove duplicate entries from a data frame
#' @description Calls the deduplicate function from synthesisr to flag and remove duplicate entries from a data frame
#' @param df the data frame to deduplicate
#' @param field the name or index of the column to check for duplicate values
#' @param method the manner of duplicate detection; quick removes exact text duplicates, similarity removes duplicates below a similarity threshold, and fuzzy uses fuzzdist matching
#' @param language the language to use if method is set to similarity
#' @param cutoff_distance the threshold below which articles are marked as duplicates by the similarity method
#' @return a deduplicated data frame
remove_duplicates <-  function(df,
                               field,
                               method = c("quick", "similarity", "fuzzy"),
                               language = "English",
                               cutoff_distance = 2) {
  df <-
    synthesisr::deduplicate(
      df = df,
      field = field,
      method = method,
      language = language,
      cutoff_distance = cutoff_distance
    )
  return(df)
}
