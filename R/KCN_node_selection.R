#' Create a keyword co-occurrence network
#' @description Creates a keyword co-occurrence network from an adjacency matrix trimmed to remove rare terms.
#' @param search_dfm a document-feature matrix created with create_dfm()
#' @param min_studies the minimum number of studies a term must occur in to be included
#' @param min_occurrences the minimum total number of times a term must occur (counting repeats in the same document)
#' @return an igraph weighted graph
create_network <- function(search_dfm, min_studies=3, min_occurrences = 3){
  trimmed_mat <- quanteda::dfm_trim(search_dfm, min_termfreq = min_occurrences, min_docfreq = min_studies)
  search_mat <- quanteda::fcm(trimmed_mat, context = "document", count = "boolean", tri=FALSE)
  search_mat <- as.matrix(search_mat)
  search_graph <- igraph::graph.adjacency(t(search_mat),
                                        weighted=TRUE,
                                        mode="undirected",
                                        diag=FALSE)
  return(search_graph)
}

#' Subset strength data from a graph
#' @description Selects only the node strength data from a graph.
#' @param graph an igraph graph
#' @param importance_method a character specifying the importance measurement to be used; takes arguments of "strength", "eigencentrality", "alpha", "betweenness", "hub" or "power"
#' @return a data frame of node strengths, ranks, and names
make_importance <- function(graph, importance_method){
  if (importance_method=="strength") {importance <- sort(igraph::strength(graph))}
  if (importance_method=="eigencentrality"){importance <- sort(igraph::eigen_centrality(graph))}
  if (importance_method=="alpha"){importance <- sort(igraph::alpha_centrality(graph))}
  if (importance_method=="betweenness"){importance <- sort(igraph::betweenness(graph))}
  if (importance_method=="hub"){importance <- sort(igraph::hub_score(graph))}
  if (importance_method=="power"){importance <- sort(igraph::power_centrality(graph))}
  importance_data <- cbind(seq(1, length(importance), 1), as.numeric(importance))
  colnames(importance_data) <- c("rank", "importance")
  importance_data <- as.data.frame(importance_data)
  importance_data$nodename <- names(importance)
  importance_data$rank <- as.numeric(importance_data$rank)
  importance_data$importance <- as.numeric(importance_data$importance)
  return(importance_data)
}

#' Subset n-grams from node names
#' @description Selects only nodes from a graph whose node names are at least n-grams, where n is the minimum number of words in the node name. The default n-gram is a 2+-gram, which captures potential keyword terms that are at least two words long. The reason for this is that unigrams (terms with only one word) are detected more frequently, but are also generally less relevant to finding keyword terms.
#' @param graph an igraph object
#' @param n a minimum number of words in an n-gram
#' @param importance_method a character specifying the importance measurement to be used; takes arguments of "strength", "eigencentrality", "alpha", "betweenness", "hub" or "power"
#' @return a data frame of node names, strengths, rank
select_ngrams <- function(graph, n=2, importance_method="strength"){
  importance_data <- make_importance(graph, importance_method = importance_method)
  ngrams <- importance_data[which(sapply(strsplit(as.character(importance_data$nodename), " "), length) >= n),]
  return(ngrams)
  }


#' Subset unigrams from node names
#' @description Selects only nodes from a graph whose node names are at single words.
#' @param graph an igraph object
#' @param importance_method a character specifying the importance measurement to be used; takes arguments of "strength", "eigencentrality", "alpha", "betweenness", "hub" or "power"
#' @return a data frame of node names, strengths, rank
select_unigrams <- function(graph, importance_method="strength"){
  importance_data <- make_importance(graph, importance_method = importance_method)
  unigrams <- importance_data[which(sapply(strsplit(as.character(importance_data$nodename), " "), length) == 1),]
  return(unigrams)
}

#' Find optimal knot placements
#' @description This function finds optimal knot placement given the degrees of your unique node strength graph and how many knots to allow. Degrees refers to the polynomial degree; for straight lines, use degree of 1 or for a curve use degree 2. Increasing the number of knots increases the fit and flexibility of the spline curve and presents more options for the cutoff strength.
#' @param importance_data a dataset of unique node strengths and their ranks
#' @param degrees the degree of the polynomial for the curve of unique node strengths
#' @param knot_num the number of knots to allow
#' @return a vector of knot placements
find_knots <- function(importance_data, degrees=2, knot_num=1){
  knotselect <- freeknotsplines::freepsgen(importance_data$rank, importance_data$importance,
                                           degree=degrees, numknot=knot_num, seed=5, stream=0)
  knots <- knotselect@optknot
  return(knots)
}

#' Fit spline model to node strengths
#' @description Fits a basis spline to the curve of ranked unique node strengths.
#' @param importance_data a dataset of ranked unique node strengths
#' @param degrees the same degrees used to find knot placement in \code{find_knots}
#' @param knot_num the same number of knots used to find knot placement in \code{find_knots}
#' @param knots The vector of optimal knots returned from \code{find_knots}
#' @return a fitted spline model
fit_splines <- function(importance_data, degrees=2, knot_num=1, knots){
  spline_b <- splines2::bSpline(as.numeric(importance_data$rank), knots=knots, degree=degrees, numknot=knot_num, intercept=TRUE)
  spline_fit <- lm(as.numeric(importance_data$importance) ~ spline_b)
  return(spline_fit)
}


#' Find node cutoff strength
#' @description Find the minimum node strength to use as a cutoff point for important nodes.
#' @param graph The complete graph.
#' @param method The spline fit finds tipping points in the ranked order of node strengths to use as cutoffs. The cumulative fit option finds the node strength cutoff point at which a certain percent of the total strength of the graph is captured (e.g. the fewest nodes that contain 80\% of the total strength).
#' @param cum_pct if using method cumulative, the total percent of node strength to capture
#' @param degrees if using method spline, the degrees of the polynomial curve that approximates the ranked unique node strengths
#' @param knot_num if using method spline, the number of knots to allow
#' @param diagnostics if set to TRUE, saves plots of either the fit splines and residuals or the curve of cumulative node strength and cutoff point
#' @param importance_method a character specifying the importance measurement to be used; takes arguments of "strength", "eigencentrality", "alpha", "betweenness", "hub" or "power"
#' @return a vector of suggested node cutoff strengths
find_cutoff <- function(graph, method=c("spline", "cumulative"), cum_pct=0.8, degrees=2, knot_num=1, diagnostics=TRUE, importance_method="strength"){

    importance_data <- make_importance(graph, importance_method=importance_method)

  if (method == "spline") {
      knots <- find_knots(importance_data, degrees=degrees, knot_num=knot_num)
      cut_points <- floor(knots)
      cut_strengths <- (importance_data$importance)[cut_points]

      if (diagnostics == TRUE){
        spline_fit <- fit_splines(importance_data, degrees=degrees, knot_num=knot_num, knots=knots)
        plot(importance_data$rank, importance_data$importance,
             main="Spline model fit",
             xlab="Rank", ylab="Node strength (unique)")
        lines(importance_data$rank,spline_fit$fit,col="red",lwd=3)
        abline(v=knots, col="blue", lwd=2)

        par(mfrow=c(1,2))
        plot(importance_data$rank, spline_fit$resid, xlab="Rank", ylab="Residual", main="Residuals along the x-axis (rank)")
        abline(h=0, col="red")
        abline(lm(spline_fit$resid ~ importance_data$rank), col="blue", lty=2)
        plot(importance_data$importance, spline_fit$resid, xlab="Strength", ylab="Residual", main="Residuals along the y-axis (strength)")
        abline(lm(spline_fit$resid ~ importance_data$importance), col="blue", lty=2)
        abline(h=0, col="red")
      }
    }

  if (method == "cumulative"){
    cum_str <- max(cumsum(sort(importance_data$importance)))
    cut_point <- (which(cumsum(sort(importance_data$importance, decreasing = TRUE))>=cum_str*cum_pct))[1]
    cut_strengths <- as.numeric(sort(as.numeric(importance_data$importance), decreasing = TRUE)[cut_point])

    if (diagnostics == TRUE){
      plot(cumsum(sort(importance_data$importance)), type="l", ylab="Cumulative node strength", main="Cumulative sum of ranked node strength")
      abline(v=cut_point, col="blue")
      legend("topleft", legend = c("Point at which cumulative percent is to the right of the line"), lwd=2, col="blue")

      hist(importance_data$importance, 100,
           main="Histogram of node strengths", xlab="Node strength")
      abline(v=cut_strengths, col="blue")
      legend("topright", legend = c("Node strength cutoff"), lwd=2, col="blue")
    }
  }
  return(cut_strengths)
}

#' Extract potential keywords
#' @description Extracts keywords identified as important and writes them to a plain text file.
#' @param reduced_graph a reduced graph with only important nodes created with reduce_grah()
#' @param savekeywords if TRUE, saves the keywords to a plain text file
#' @param makewordle if TRUE, creates a wordcloud image of the important keywords sized relative to node strength
#' @return a list of potential keywords to consider
get_keywords <- function(reduced_graph, savekeywords=TRUE, makewordle=TRUE){
  potential_keys <- names(igraph::V(reduced_graph))
  if (savekeywords == TRUE){writeLines(potential_keys, "potential-keywords.txt") }
  if (makewordle == TRUE) {make_wordle(reduced_graph)}
  return(potential_keys)
}


#' Create reduced graph of important nodes
#' Takes the full graph and reduces it to only include nodes (and associated edges) greater than the cutoff strength for important nodes.
#' @param graph the full graph object
#' @param cutoff_strength the minimum node strength to be included in the reduced graph
#' @param importance_method a character specifying the importance measurement to be used; takes arguments of "strength", "eigencentrality", "alpha", "betweenness", "hub" or "power"
#' @return an igraph graph with only important nodes
reduce_graph <- function(graph, cutoff_strength, importance_method="strength"){
  importance_data <- make_importance(graph, importance_method = importance_method)
  important_nodes <- importance_data$nodename[which(importance_data$importance >= cutoff_strength)]
  reduced_graph <- igraph::induced_subgraph(graph, v=important_nodes)
  return(reduced_graph)
}


#' Create keyword co-occurrence network with only ngrams
#' @description Reduces the full keyword co-occurrence network to only include nodes with 2+ words or only unigrams. This is useful for separating commonly used words from distinct phrases.
#' @param graph an igraph object
#' @param min_ngrams a number; the minimum number of words to consider an ngram
#' @param unigrams if TRUE, returns a subset of the network where each node is a unigram
#' @return an igraph object
make_ngram_graph <- function(graph, min_ngrams=2, unigrams=FALSE){
  if (unigrams==FALSE){ngrams <- select_ngrams(graph, min_ngrams)}
  if (unigrams==TRUE){ngrams <- select_unigrams(graph)}

  ngram_graph <- igraph::induced_subgraph(graph, v=ngrams$nodename)
  return(ngram_graph)
}

