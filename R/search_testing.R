#' Check search strategy precision and sensitivity
#' @description NOTE: this function takes a long time for large searches; use check_recall() along with search_performance() for large datasets. Calculates precision and sensitivity of a search given a set of known articles that should be included in the search and the complete and deduplicated results of conducting a search.
#' @param relevant_studies a character vector of article titles deemed relevant to a review
#' @param retrieved_studies a character vector of article titles retrieved by a search (with duplicates removed)
#' @param min_sim the minimum similarity for an article to be counted as being retrieved
#' @param print_sim_min the minimum similarity for an article to be returned for manual review
#' @return a list with two objects: a table of search metrics (precision, number needed to process, and sensitivity) and a data frame of all titles whose similarity score is greater than print_sim_min
check_search_strategy <- function(relevant_studies, retrieved_studies, min_sim=.8, print_sim_min=.5){
  df <- as.data.frame(append(relevant_studies, retrieved_studies))
  df$source <- c(rep("relevant", length(relevant_studies)),
                 rep("retrieved", length(retrieved_studies)))
  colnames(df) <- c("text", "source")
  df$text <- as.character(df$text)
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
  sim_mat <- as.data.frame(sim_mat)[seq(1, length(relevant_studies), 1), seq((length(relevant_studies)+1), ncol(sim_mat), 1)]

  indices <- data.frame(ind = which(sim_mat > print_sim_min, arr.ind=TRUE))
  indices$doc1 <- rownames(sim_mat)[indices$ind.row]
  indices$doc2 <- colnames(sim_mat)[indices$ind.col]
  indices$sim_score <- sim_mat[which(sim_mat > print_sim_min, arr.ind=TRUE)]
  indices$title1 <- df$text[indices$ind.row]
  indices$title2 <- df$text[indices$ind.col]
  indices$source1 <-df$source[indices$ind.row]
  indices$source2 <- df$source[indices$ind.col]

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

#' Check the recall of a search strategy
#' @description Checks a list of known articles against the results of a search to see how many the search retrieves.
#' @param true_hits a character vector of titles for articles that should be returned
#' @param retrieved_articles a character vector of titles for articles returned by a search
#' @value a table of the best match for each true title from the search results along with a title similarity score
check_recall <- function(true_hits, retrieved_articles, min_sim=.6){
  titlekeys <- quanteda::tokens_remove(quanteda::tokens(tm::removePunctuation(tolower(true_hits))), custom_stopwords)

  positions <- list()
  length(positions) <- length(titlekeys)

  for (i in 1:length(titlekeys)){
    article <- titlekeys[[i]]
    for (j in 1:length(article)){
      temp <- stringr::str_detect(tm::removePunctuation(tolower(retrieved_articles)), article[j])
      hits <- which(temp==TRUE)
      if (length(hits) > 0){
        for (k in 1:length(hits)){
          if (i == 1){
            if (k == 1){positions[[i]] <- hits[k]}
            if (k > 1){positions[[i]] <- c(positions[[i]], hits[k])}
          }
          if (i > 1){
            positions[[i]] <- c(positions[[i]], hits[k])
          }
        }
      }
    }

    similarity <- table(positions[[i]])/length(article)
    similarity <- sort(similarity[which(similarity > min_sim)], decreasing=TRUE)
    best_match <- similarity[1]
    similarity_entry <- as.data.frame(cbind(true_hits[i], as.character(retrieved_articles[as.numeric(names(best_match))]), as.numeric(best_match)))
    if (i == 1){similarity_table <- similarity_entry}
    if (i > 1){similarity_table <- rbind(similarity_table, similarity_entry)}
  }
  colnames(similarity_table) <- c("Title", "Best_match", "Similarity")
  return(similarity_table)

}

#' Get precision and recall of a search
#' @description Measures the performance of a search by precision (specificity) and recall (sensitivity).
#' @param no_desired the number of  articles that should be returned
#' @param no_hits the number of good hits that a search found (can be found with check_recall)
#' @param no_articles the total number of articles that a search found
#' @value the sensitivity, precision, and number needed to process
search_performance <- function(no_desired, no_hits, no_articles){
  sensitivity <- round(no_hits/no_desired*100, 3)
  precision <- round(no_hits/no_articles*100, 3)
  NNP <- round(1/precision*100, 3)
  output <- rbind(sensitivity, precision, NNP)
  rownames(output) <- c("Sensitivity (%)", "Precision  (%)", "Number needed to process")
  return(output)
}
