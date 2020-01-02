
#' Add new stopwords to ignore
#' @description Allows the user to add additional stopwords to the built-in English stopwords list.
#' @param new_stopwords a character vector of new words to add
#' @return an updated vector of custom stopwords to remove from text
#' @examples add_stopwords(new_stopwords=c("19th century", "abiotic factors", "elsevier direct"))
add_stopwords <- function(new_stopwords){
  custom_stopwords <- sort(unique(append(litsearchr::custom_stopwords, new_stopwords)))
  return(custom_stopwords)
}

#' Extract potential keywords from abstracts and titles
#' @description Extracts potential keyword terms from text (e.g. titles and abstracts)
#' @param text A character object of text from which to extract terms
#' @param keywords A character vector of keywords tagged by authors and/or databases if using method="tagged"
#' @param method The method of extracting keywords; options are fakerake (a quick implementation similar to Rapid Automatic Keyword Extraction), RAKE, or tagged for author-tagged keywords
#' @param min_freq a number, the minimum occurrences of a potential term
#' @param ngrams if TRUE, only extracts phrases with word count greater than a specified n
#' @param n the minimum word count for ngrams
#' @param language the language of input data to use for stopwords
#' @return a character vector of potential keyword terms
#' @example inst/examples/extract_terms.R
extract_terms <- function(text=NULL,
                          keywords=NULL,
                          method=c("fakerake", "RAKE", "tagged"),
                          min_freq=2,
                          ngrams=TRUE, n=2,
                          language="English"){

  if(length(text)>1){text <- paste(text, collapse = " ")}
  if(!is.null(text)){text <- tolower(text)}

  if(language=="English"){stopwords <- litsearchr::custom_stopwords}else{this_language <- which(stringr::str_detect(litsearchr::possible_langs$Language, language)==TRUE)
  language_code <- as.character(litsearchr::possible_langs$Short[this_language])
  stopwords <- stopwords::stopwords(language=language_code, source = "stopwords-iso")}

  if(method=="fakerake"){
    if(is.null(text)){print("Please specify a body of text from which to extract terms.")}else{
      terms <- litsearchr::fakerake(text, stopwords)}
  }

  if(method=="RAKE"){
    if(is.null(text)){print("Please specify a body of text from which to extract terms.")
    }else if (!requireNamespace("rapidraker", quietly = TRUE)){
      stop("You need to have rapidraker and rJava installed in order to use the RAKE algorithm. Please install rapidraker or choose a different method of extracting terms.",
           call. = FALSE)} else {

             terms <- rapidraker::rapidrake(text, stop_words=stopwords, stem=FALSE)}
  }

  if(method=="tagged"){
    if(is.null(keywords)){print("Please specify a vector of keywords from which to extract terms")} else{
      cleaned_keywords <- paste(gsub("([-])[[:punct:]]", ";", keywords), collapse=";")
      terms <- stringr::str_trim(strsplit(cleaned_keywords, ";")[[1]])}
  }

  terms <- synthesisr::remove_punctuation(terms)


  freq_terms <- names(table(terms))[which(table(terms)>=min_freq)]
  if(ngrams==TRUE){

    freq_terms <- freq_terms[which(sapply(strsplit(as.character(freq_terms), " "), length) >= n)]

  }

  return(freq_terms)

}

#' Quick keyword extraction
#' @description Extracts potential keywords from text separated by stopwords
#' @param text A string object to extract terms from
#' @param stopwords A character vector of stopwords to remove
#' @return A character vector of potential keywords
fakerake <- function(text, stopwords){

  stops <- paste("\\b", stopwords, "\\b", sep="")
  stops <- unique(append(stops, c(",", "\\.", ":", ";", "\\[", "\\]", "/", "\\(", "\\)", "\"", "&", "=", "<", ">")))
  hyphens <- c(" - ", " -", "- ", "-")

  for(i in 1:length(hyphens)){
    text <- gsub(hyphens[i], "--", text)
  }

  text <- tolower(text)

  for(i in 1:length(stops)){
    text <- gsub(stops[i], "__", text)
  }

  split_terms <- strsplit(text, "__")[[1]]
  removals <- unique(append(which(split_terms==" "), which(split_terms=="")))
  if(length(removals>0)){split_terms <- split_terms[-removals]}
  pre_terms <- sapply(split_terms, stringr::str_trim)
  short_terms <- which(nchar(pre_terms)<3)
  if(length(short_terms)>0){terms <- pre_terms[-short_terms]}else{terms <- pre_terms}
  names(terms) <- NULL

  terms <- gsub("--", "-", terms)

  terms <- synthesisr::remove_punctuation(terms, remove_hyphens = FALSE)
  terms <- synthesisr::remove_numbers(terms)

  if(any(grepl("  ", terms))){
    while(any(grepl("  ", terms))){
      terms <- gsub("  ", " ", terms)
    }
  }

  # remove trailing spaces
  if(requireNamespace("stringr", quietly=TRUE)){
    terms <- stringr::str_trim(terms)
  }

  return(terms)
}

#' Create a document-feature matrix
#' @description Given a character vector of document information, creates a document-feature matrix.
#' @param elements a character vector of document information (e.g. document titles or abstracts)
#' @param features a character vector of terms to use as document features
#' @param closure restrictions on how keywords are detected; left requires terms to start with a keyword (e.g "burn" matches "burning"), right requires terms to end with a keyword (e.g. "burn" matches "postburn" but not "postburning"), full requires exact matches (e.g. "burn" only matches "burn"), and none allows keywords to be embedded within terms.
#' @return a matrix with documents as rows and terms as columns
#' @example inst/examples/create_dfm.R
create_dfm <-
  function(elements, features, closure="full") {
    dfm <-
      synthesisr::create_dfm(
        elements = elements,
features=features,
closure=closure,
ignore_case=TRUE)
    return(dfm)
  }

#' Create a keyword co-occurrence network
#' @description Creates a keyword co-occurrence network from an adjacency matrix trimmed to remove rare terms.
#' @param search_dfm a document-feature matrix created with create_dfm()
#' @param min_studies the minimum number of studies a term must occur in to be included
#' @param min_occurrences the minimum total number of times a term must occur (counting repeats in the same document)
#' @return an igraph weighted graph
#' @example inst/examples/create_network.R
create_network <- function(search_dfm, min_studies=3, min_occurrences = 3){
  presences <- search_dfm
  presences[which(presences>0)] <- 1
  study_counts <- which(as.numeric(colSums(presences))<min_studies)
  if(length(study_counts)>0){
    search_dfm <- search_dfm[,-study_counts]
  }

  occur_counts <- which(colSums(search_dfm)<min_occurrences)
  if(length(occur_counts)>0){
    search_dfm <- search_dfm[,-occur_counts]
  }

  dropped_studies <- which(rowSums(search_dfm)<1)

  if(length(dropped_studies)>0){
    search_dfm <- search_dfm[-dropped_studies,]
  }
  trimmed_mat <- t(search_dfm) %*% search_dfm

  search_mat <- as.matrix(trimmed_mat)
  search_graph <- igraph::graph.adjacency(search_mat,
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
#' @example inst/examples/make_importance.R
make_importance <- function(graph, importance_method="strength"){
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
#' @example inst/examples/select_ngrams.R
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
#' @example inst/examples/select_ngrams.R
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
#' @example inst/examples/find_knots.R
find_knots <- function(importance_data, degrees=2, knot_num=1){
  if (!requireNamespace("freeknotsplines", quietly = TRUE)){
    stop("freeknotsplines needed to select knots using the spline method. Please install it.",
         call. = FALSE)
  } else {
  knotselect <- freeknotsplines::freelsgen(importance_data$rank, importance_data$importance,
                                           degree=degrees, numknot=knot_num, seed=5, stream=0)
  knots <- knotselect@optknot
  return(knots)}
}

#' Fit spline model to node strengths
#' @description Fits a basis spline to the curve of ranked unique node strengths.
#' @param importance_data a dataset of ranked unique node strengths
#' @param degrees the same degrees used to find knot placement in \code{find_knots}
#' @param knot_num the same number of knots used to find knot placement in \code{find_knots}
#' @param knots The vector of optimal knots returned from \code{find_knots}
#' @return a fitted spline model
#' @example inst/examples/fit_splines.R
fit_splines <- function(importance_data, degrees=2, knot_num=1, knots){
  if (!requireNamespace("splines2", quietly = TRUE)){
    stop("splines2 needed to use the spline method. Please install it.",
         call. = FALSE)
  } else {
  spline_b <- splines2::bSpline(as.numeric(importance_data$rank), knots=knots, degree=degrees, numknot=knot_num, intercept=TRUE)
  spline_fit <- lm(as.numeric(importance_data$importance) ~ spline_b)
  return(spline_fit)}
}


#' Find node cutoff strength
#' @description Find the minimum node strength to use as a cutoff point for important nodes.
#' @param graph The complete graph.
#' @param method The spline fit finds tipping points in the ranked order of node strengths to use as cutoffs. The cumulative fit option finds the node strength cutoff point at which a certain percent of the total strength of the graph is captured (e.g. the fewest nodes that contain 80\% of the total strength).
#' @param percent if using method cumulative, the total percent of node strength to capture
#' @param degrees if using method spline, the degrees of the polynomial curve that approximates the ranked unique node strengths
#' @param knot_num if using method spline, the number of knots to allow
#' @param importance_method a character specifying the importance measurement to be used; takes arguments of "strength", "eigencentrality", "alpha", "betweenness", "hub" or "power"
#' @return a vector of suggested node cutoff strengths
#' @example inst/examples/find_cutoff.R
find_cutoff <- function(graph, method=c("spline", "cumulative"), percent=0.8, degrees=2,
                        knot_num=1, importance_method="strength"){

  importance_data <- make_importance(graph, importance_method=importance_method)

  if (method == "spline") {
    knots <- find_knots(importance_data, degrees=degrees, knot_num=knot_num)
    cut_points <- floor(knots)
    cut_strengths <- (importance_data$importance)[cut_points]

  }

  if (method == "cumulative"){
    cum_str <- max(cumsum(sort(importance_data$importance)))
    cut_point <- (which(cumsum(sort(importance_data$importance, decreasing = TRUE))>=cum_str*percent))[1]
    cut_strengths <- as.numeric(sort(as.numeric(importance_data$importance), decreasing = TRUE)[cut_point])
  }
  return(cut_strengths)
}

#' Extract potential keywords
#' @description Extracts keywords identified as important.
#' @param reduced_graph a reduced graph with only important nodes created with reduce_grah()
#' @return a character vector of potential keywords to consider
#' @example inst/examples/get_keywords.R
get_keywords <- function(reduced_graph){
  potential_keys <- names(igraph::V(reduced_graph))
  return(potential_keys)
}


#' Create reduced graph of important nodes
#' Takes the full graph and reduces it to only include nodes (and associated edges) greater than the cutoff strength for important nodes.
#' @param graph the full graph object
#' @param cutoff_strength the minimum node importance to be included in the reduced graph
#' @param importance_method a character specifying the importance measurement to be used; takes arguments of "strength", "eigencentrality", "alpha", "betweenness", "hub" or "power"
#' @return an igraph graph with only important nodes
#' @example inst/examples/reduce_graph.R
reduce_graph <- function(graph, cutoff_strength, importance_method="strength"){
  importance_data <- make_importance(graph, importance_method = importance_method)
  important_nodes <- importance_data$nodename[which(importance_data$importance >= cutoff_strength)]
  reduced_graph <- igraph::induced_subgraph(graph, v=important_nodes)
  return(reduced_graph)
}

