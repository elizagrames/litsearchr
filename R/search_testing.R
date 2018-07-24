#' Check search strategy precision and sensitivity
#' @description Calculates precision and sensitivity of a search given a set of known articles that should be included in the search and the complete and deduplicated results of conducting a search.
#' @param relevant_studies a data frame where the first column is titles of articles deemed relevant to a review.
#' @param retrieved_studies a data frame where the first column is titles of articles retrieved by a search (with duplicates removed)
#' @param min_sim the minimum similarity for an article to be counted as being retrieved
#' @param print_sim_min the minimum similarity for an article to be returned for manual review
#' @return a list with two objects: a table of search metrics (precision, number needed to process, and sensitivity) and a data frame of all titles whose similarity score is greater than print_sim_min
check_search_strategy <- function(relevant_studies, retrieved_studies, min_sim=.8, print_sim_min=.5){
  df <- rbind(relevant_studies, retrieved_studies)
  colnames(df)[1] <- "text"
  comparison_corpus <- make_corpus(df)
  comparison_dfm <- quanteda::dfm(comparison_corpus,
                                  remove = custom_stopwords,
                                  remove_numbers=TRUE,
                                  remove_punct=TRUE,
                                  remove_symbols=TRUE,
                                  remove_separators=TRUE,
                                  remove_twitter=TRUE,
                                  remove_hyphens=TRUE,
                                  remove_url=TRUE)

  dfm_similarity <- quanteda::textstat_simil(comparison_dfm, margin = "documents")
  sim_mat <- as.matrix(dfm_similarity)
  sim_mat[lower.tri(sim_mat, diag=TRUE)] <- NA
  sim_mat <- as.data.frame(sim_mat)[seq(1, nrow(relevant_studies), 1), seq((nrow(relevant_studies)+1), ncol(sim_mat), 1)]

  indices <- data.frame(ind = which(sim_mat > print_sim_min, arr.ind=TRUE))
  indices$doc1 <- rownames(sim_mat)[indices$ind.row]
  indices$doc2 <- colnames(sim_mat)[indices$ind.col]
  indices$sim_score <- sim_mat[which(sim_mat > print_sim_min, arr.ind=TRUE)]
  indices$title1 <- relevant_studies$title[indices$ind.row]
  indices$title2 <- retrieved_stu$title[indices$ind.col]
  indices$source1 <-df$source[indices$ind.row]
  indices$source2 <- rep("retrieved_studies", nrow(indices))

  possible_hits <- which(indices$source1!=indices$source2)
  check_score <- indices[possible_hits,]
  check_score <- check_score[order(check_score$sim_score, decreasing=TRUE),]

  Precision <- round(length(which(check_score$sim_score > min_sim))/nrow(retrieved_studies), 5)
  NNP <- round(1/Precision, 5)
  Sensitivity <- round(length(which(check_score$sim_score > min_sim))/nrow(relevant_studies), 5)

  metrics <- rbind(Precision, NNP, Sensitivity)
  colnames(metrics) <- "Search metrics"

  output <- list(metrics, check_score)
  names(output) <- c("Search_metrics", "Positive_hits")
  return(output)
}
