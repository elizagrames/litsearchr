#' Import results of naive searches
#'
#' @description This function takes results of searches in BIOSIS, Zoological Record, or Scopus and converts them to the format used in this package. If you have keywords from a different database, make sure there are columns for id, title, abst (Abstract), key (keywords), and mods (other options, in this case, models).
#' @param file This function assumes you have exported full records to a .csv.
#' @param database Which of the three supported databases your records are from.
#' @return A dataset with just the relevant columns, in the same order, so that results from the three databases can be combined with rbind.
import_naive <- function(file, database=c("Scopus", "ZooRec", "BIOSIS")){
  df <- read.csv(file, header=TRUE, stringsAsFactors = FALSE)
  if (database=="Scopus"){
    df <- dplyr::select(df,
                        id=EID,
                        title=Title,
                        abst=Abstract,
                        key=Author.Keywords,
                        mods=Source)
    df$mods <- rep("", length(df$mods))
  }
  if (database=="ZooRec"){
    df <- dplyr::select(df,
                        id=UT,
                        title=TI,
                        abst=AB,
                        key=BD,
                        mods=HP)
    df$mods <- rep("", length(df$mods))
  }
  if (database=="BIOSIS"){
    df <- dplyr::select(df,
                        id=UT,
                        title=TI,
                        abst=AB,
                        key=MI,
                        mods=MQ)
  }

  return(df)
}
