#' Find node strength and rank
#' @description Creates a dataset of graph node strengths and ranks.
#' @param graph An \code{igraph} graph object.
#' @return Dataset of unique node strengths and in ranked order.
make_strengths <- function(graph){
  NS <- igraph::strength(graph)
  unique_NS <- sort((NS))
  strength_data <- cbind(seq(1, length(unique_NS)), unique_NS)
  colnames(strength_data) <- c("rank", "strength")
  strength_data <- as.data.frame(strength_data)
  return(strength_data)
}

#' Find optimal knot placements
#' @description This function finds optimal knot placement given the degrees of your unique node strength graph and how many knots to allow. Degrees refers to the polynomial degree; for straight lines, use degree of 1 or for a curve use degree 2. Increasing the number of knots increases the fit and flexibility of the spline curve and presents more options for the cutoff strength.
#' @param strength_data A dataset of unique node strengths and their ranks.
#' @param degrees The degree of the polynomial for the curve of unique node strengths.
#' @param knot_num The number of knots to allow.
#' @return A vector of knot placements.
find_knots <- function(strength_data, degrees=2, knot_num=1){
  knotselect <- freeknotsplines::freepsgen(strength_data$rank, strength_data$strength,
                                           degree=degrees, numknot=knot_num, seed=5, stream=0)
  knots <- knotselect@optknot
  return(knots)
}

#' Fit spline model to node strengths
#' @description Fits a basis spline to the curve of ranked unique node strengths.
#' @param strength_data The dataset of ranked unique node strenghts.
#' @param degrees The same degrees used to find knot placement in \code{find_knots}.
#' @param knot_num The same number of knots used to find knot placement in \code{find_knots}.
#' @param knots The vector of optimal knots returned from \code{find_knots}.
#' @return The fitted spline model.
fit_splines <- function(strength_data, degrees=2, knot_num=1, knots){
  spline_b <- splines2::bSpline(strength_data$rank, knots=knots, degree=degrees, numknot=knot_num, intercept=TRUE)
  spline_fit <- lm(strength_data$strength ~ spline_b)
  return(spline_fit)
}


#' Find node cutoff strength
#' @description Find the minimum node strength to use as a cutoff point for important nodes.
#' @param graph The complete graph.
#' @param method The spline fit finds tipping points in the ranked order of node strengths to use as cutoffs. The cumulative fit option finds the node strength cutoff point at which a certain percent of the total strength of the graph is captured (e.g. the fewest nodes that contain 80% of the total strength).
#' @param cum_pct If using method cumulative, the total percent of node strength to capture.
#' @param degrees If using method spline, the degrees of the polynomial curve that approximates the ranked unique node strengths.
#' @param knot_num If using method spline, the number of knots to allow.
#' @param diagnostics If set to TRUE, saves plots of either the fit splines and residuals or the curve of cumulative node strength and cutoff point.
#' @return The cutoff strength vector.
find_cutoff <- function(graph, method=c("spline", "cumulative"), cum_pct=0.8, degrees=2, knot_num=1, diagnostics=TRUE){
  if (method == "spline") {
    strength_data <- make_strengths(graph)
    knots <- find_knots(strength_data, degrees=degrees, knot_num=knot_num)
    cut_points <- floor(knots)
    cut_strengths <- (strength_data$strength)[cut_points]

    if (diagnostics == TRUE){
      spline_fit <- fit_splines(strength_data, degrees=degrees, knot_num=knot_num, knots=knots)
      png("spline-model.png", width = 8, height = 8, units = 'in', res = 400)
      plot(strength_data$rank, strength_data$strength,
           main="Spline model fit",
           xlab="Rank", ylab="Node strength (unique)")
      lines(strength_data$rank,spline_fit$fit,col="red",lwd=3)
      abline(v=knots, col="blue", lwd=2)
      dev.off()

      png("spline-model-residuals.png", width = 8, height = 8, units = 'in', res = 400)
      par(mfrow=c(1,2))
      plot(strength_data$rank, spline_fit$resid, xlab="Rank", ylab="Residual")
      abline(h=0, col="red")
      plot(strength_data$strength, spline_fit$resid, xlab="Strength", ylab="Residual")
      abline(h=0, col="red")
      dev.off()
    }

  }
  if (method == "cumulative"){
    cum_str <- max(cumsum(sort(strength(graph))))
    cut_point <- length(which(cumsum(sort(strength(graph), decreasing = TRUE))>=cum_str*cum_pct))
    cut_strengths <- as.numeric(sort(strength(graph))[cut_point])

    if (diagnostics == TRUE){
      png("cumulative-node-strength.png", width = 8, height = 8, units = 'in', res = 400)
      plot(cumsum(sort(strength(graph))), type="l", ylab="Cumulative node strength")
      abline(v=cut_point, col="blue")
      dev.off()
    }
  }
  return(cut_strengths)
}

#' Extract potential keywords
#' @description Extracts keywords identified as important and writes them to a plain text file.
#' @param reduced_graph The reduced graph with only important nodes.
#' @param savekeywords If TRUE, saves the keywords to a plain text file.
#' @param makewordle If TRUE, creates a wordcloud image of the important keywords sized by node strength.
#' @return The list of potential keywords to consider.
get_keywords <- function(reduced_graph, savekeywords=TRUE, makewordle=TRUE){
  potential_keys <- names(V(reduced_graph))
  if (savekeywords == TRUE){ writeLines(potential_keys, "potential-keywords.txt") }
  if (makewordle == TRUE) {make_wordle(reduced_graph)}
  return(potential_keys)
}


