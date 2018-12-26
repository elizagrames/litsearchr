#' Create a corpus from a data frame
#' @description Calls the corpus function from quanteda to create a corpus from the data frame with search hits.
#' @param df a data frame where at least one column is called 'text'
#' @return a corpus object
#' @examples  make_corpus(df=BBWO_data)
make_corpus <- function(df){
  search_corpus <- quanteda::corpus(df)
  return(search_corpus)
}

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
#' @description Uses the RAKE (Rapid Automatic Keyword Extractor) from rapidraker to extract potential keyword terms from titles and abstracts.
#' @param df a dataframe created with import_scope
#' @param type if generating keywords from title and/or abstract, use "RAKE"; if extracting author or database tagged keywords, use "tagged"
#' @param new_stopwords a character vector of stopwords to ignore
#' @param min_freq a number, the minimum occurrences of a potential term
#' @param title include titles if TRUE
#' @param abstract include abstracts if TRUE
#' @param ngrams if TRUE, only extracts phrases with word count greater than a specified n
#' @param n the minimum word count for ngrams
#' @return a character vector of potential keyword terms
#' @examples extract_terms(df=BBWO_data, type="RAKE")
extract_terms <- function(df, type=c("RAKE", "tagged"), new_stopwords=NULL, min_freq=2, title=TRUE, abstract=TRUE, ngrams=TRUE, n=2){
  if(type=="RAKE"){
  if (title == TRUE){
    if (abstract == TRUE){
      article_subjects <- paste(df$title, df$abstract, collapse=". ")
    }
    if (abstract == FALSE){
      article_subjects <- paste(df$title, collapse=". ")
    }
  }
  if (title == FALSE){
    if (abstract == TRUE){
      article_subjects <- paste(df$abstract, collapse=". ")
    }
    if (abstract == FALSE){print("You aren't selecting any text to pass to RAKE!")}
  }

  possible_terms <- rapidraker::rapidrake(tolower(article_subjects),
                                          stop_words = add_stopwords(new_stopwords),
                                          stem=FALSE)
  likely_terms <- possible_terms[[1]]$keyword[which(possible_terms[[1]]$freq >= min_freq)]
  if (ngrams==TRUE){
    likely_terms <- likely_terms[which(sapply(strsplit(as.character(likely_terms), " "), length) >= n)]
  }
  return(likely_terms)
  }

  if(type=="tagged"){
    cleaned_keywords <- clean_keywords(df)$keyword
    possible_terms <- paste(df$keywords, collapse=";")
    possible_terms <- strsplit(possible_terms, ";")[[1]]
    possible_terms <- stringr::str_trim(tm::removePunctuation(possible_terms))

    term_freq_table <- table(possible_terms)

    actual_terms <- tolower(names(term_freq_table)[which(term_freq_table >= min_freq)])
    if(length(which(actual_terms=="")>0)){actual_terms <- actual_terms[-which(actual_terms=="")]}
    if (ngrams==TRUE){
      actual_terms <- actual_terms[which(sapply(strsplit(as.character(actual_terms), " "), length) >= n)]
    }

    return(actual_terms)
  }

}

#' Make a dictionary from keywords
#' @description Combines actual keywords and likely keywords into a dictionary object using the as.dictionary function from quanteda.
#' @param terms a list object including at least one character vector of search terms
#' @return a quanteda dictionary object
#' @examples make_dictionary(terms=extract_terms(BBWO_data, type="RAKE", min_freq=3))
make_dictionary <- function(terms=NULL){
  complete_keywords <- terms[[1]]
  for(i in 1:length(terms)){
    complete_keywords <- unique(tolower(append(complete_keywords, terms[[i]])))
  }

  my_dic <- as.data.frame(cbind(complete_keywords, complete_keywords))
  colnames(my_dic) <- c("word", "sentiment")

  my_dic <- quanteda::as.dictionary(my_dic)

  return(my_dic)
}

#' Create document-term matrix
#' @description Calls the dfm function from quanteda to create a document-term matrix of terms included in a custom dictionary of potential terms.
#' @param corpus a corpus object
#' @param my_dic a dictionary object
#' @param custom_stopwords a character vector of words to ignore
#' @return a quanteda dfm object
#' @example inst/examples/create_dfm.R
create_dfm <- function(corpus=make_corpus(df), my_dic=make_dictionary(), custom_stopwords=add_stopwords(NULL)){

  search_dfm <- quanteda::dfm(corpus,
                              stem = FALSE,
                              remove=litsearchr::custom_stopwords,
                              remove_numbers=TRUE,
                              remove_punct=TRUE,
                              remove_symbols=TRUE,
                              remove_separators=TRUE,
                              remove_twitter=TRUE,
                              remove_hyphens=TRUE,
                              remove_url=TRUE,
                              dictionary=my_dic,
                              tolower=TRUE)

  return(search_dfm)

}

#' Create a keyword co-occurrence network
#' @description Creates a keyword co-occurrence network from an adjacency matrix trimmed to remove rare terms.
#' @param search_dfm a document-feature matrix created with create_dfm()
#' @param min_studies the minimum number of studies a term must occur in to be included
#' @param min_occurrences the minimum total number of times a term must occur (counting repeats in the same document)
#' @return an igraph weighted graph
#' @examples create_network(BBWO_dfm)
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
#' @examples make_importance(graph=BBWO_graph, importance_method="strength")
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
#' @examples select_ngrams(graph=litsearchr::BBWO_graph, n=2, importance_method="strength")
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
#' @examples select_unigrams(graph=litsearchr::BBWO_graph, importance_method="strength")
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
#' @example inst/examples/fit_splines.R
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
#' @param diagnostics if set to TRUE, prints plots of either the fit splines and residuals or the curve of cumulative node strength and cutoff point
#' @param importance_method a character specifying the importance measurement to be used; takes arguments of "strength", "eigencentrality", "alpha", "betweenness", "hub" or "power"
#' @return a vector of suggested node cutoff strengths
#' @examples find_cutoff(litsearchr::BBWO_graph, method="cumulative", cum_pct=0.8, diagnostics=FALSE)
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
           xlab="Rank", ylab="Node importance (unique)")
      lines(importance_data$rank,spline_fit$fit,col="red",lwd=3)
      abline(v=knots, col="blue", lwd=2)

      plot(importance_data$rank, spline_fit$resid, xlab="Rank", ylab="Residual", main="Residuals along the x-axis (rank)")
      abline(h=0, col="red")
      abline(lm(spline_fit$resid ~ importance_data$rank), col="blue", lty=2)
      plot(importance_data$importance, spline_fit$resid, xlab="Importance", ylab="Residual", main="Residuals along the y-axis (importance)")
      abline(lm(spline_fit$resid ~ importance_data$importance), col="blue", lty=2)
      abline(h=0, col="red")
    }
  }

  if (method == "cumulative"){
    cum_str <- max(cumsum(sort(importance_data$importance)))
    cut_point <- (which(cumsum(sort(importance_data$importance, decreasing = TRUE))>=cum_str*cum_pct))[1]
    cut_strengths <- as.numeric(sort(as.numeric(importance_data$importance), decreasing = TRUE)[cut_point])

    if (diagnostics == TRUE){
      plot(cumsum(sort(importance_data$importance)), type="l", ylab="Cumulative node importance", main="Cumulative sum of ranked node importance")
      abline(v=cut_point, col="blue")
      legend("topleft", legend = c("Cutoff rank"), lwd=2, col="blue")

      hist(importance_data$importance, 100,
           main="Histogram of node importance", xlab="Node importance")
      abline(v=cut_strengths, col="blue")
      legend("topright", legend = c("Node importance cutoff"), lwd=2, col="blue")
    }
  }
  return(cut_strengths)
}

#' Extract potential keywords
#' @description Extracts keywords identified as important.
#' @param reduced_graph a reduced graph with only important nodes created with reduce_grah()
#' @param savekeywords if TRUE, saves the keywords to a plain text file
#' @param makewordle if TRUE, creates a wordcloud image of the important keywords sized relative to node strength
#' @param directory the directory to save results to if savekeywords=TRUE
#' @return a list of potential keywords to consider
#' @examples get_keywords(reduce_graph(litsearchr::BBWO_graph, cutoff_strength=15))
get_keywords <- function(reduced_graph, savekeywords=FALSE, makewordle=FALSE, directory="./"){
  if(savekeywords==TRUE){
    if(utils::menu(c("yes", "no"), title="This will write keywords to a plain text file. Do you want to save keywords to a file?")==2){
      savekeywords <- FALSE
    }
  }
  potential_keys <- names(igraph::V(reduced_graph))
  if (savekeywords == TRUE){writeLines(potential_keys, paste(directory, "potential-keywords.txt", sep="")) }
  if (makewordle == TRUE) {make_wordle(reduced_graph)}
  return(potential_keys)
}


#' Create reduced graph of important nodes
#' Takes the full graph and reduces it to only include nodes (and associated edges) greater than the cutoff strength for important nodes.
#' @param graph the full graph object
#' @param cutoff_strength the minimum node importance to be included in the reduced graph
#' @param importance_method a character specifying the importance measurement to be used; takes arguments of "strength", "eigencentrality", "alpha", "betweenness", "hub" or "power"
#' @return an igraph graph with only important nodes
#' @examples reduce_graph(graph=litsearchr::BBWO_graph, cutoff_strength=15, importance_method="strength")
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
#' @examples make_ngram_graph(graph=litsearchr::BBWO_graph, min_ngrams=2, unigrams=FALSE)
make_ngram_graph <- function(graph, min_ngrams=2, unigrams=FALSE){
  if (unigrams==FALSE){ngrams <- select_ngrams(graph, min_ngrams)}
  if (unigrams==TRUE){ngrams <- select_unigrams(graph)}

  ngram_graph <- igraph::induced_subgraph(graph, v=ngrams$nodename)
  return(ngram_graph)
}

#' Condense a keyword co-occurrence network by removing irrelevant terms
#' @description Eliminates the rejected terms from the manual keyword review and rebuilds the network with the eliminated terms removing, making it recentralize on the terms marked as relevant in the manual stage.
#' @param full_graph the full graph produced with create_network()
#' @param rejected_terms a character vector of terms rejected in the manual review stage
#' @param previous_rejected_terms a list of character vectors rejected in previous manual reviews. If this is the first iteration, it should be set to NULL.
#' @return the full graph with the rejected term nodes deleted
#' @examples condense_network(full_graph=litsearchr::BBWO_graph, rejected_terms=c("actual increase"))
condense_network <- function(full_graph, rejected_terms, previous_rejected_terms=NULL){
  old_nodenames <- names(igraph::V(full_graph))

  for(i in 1:length(rejected_terms)){
    position <- which(old_nodenames==rejected_terms[i])
    if(i==1){removals <- position}
    if(i>1){removals <- append(removals, position)}
  }

  if(!is.null(previous_rejected_terms)){
    for(i in 1:length(previous_rejected_terms)){
      terms <- previous_rejected_terms[[i]]
      for(j in 1:length(terms)){
        position <- which(old_nodenames==terms[j])
        removals <- append(removals, position)
      }
    }

  }

  new_graph <- igraph::delete.vertices(full_graph, removals)
  return(new_graph)

}


#' Extracts new terms from a condensed network for manual consideration
#' @description Given a reduced graph after reducing the new graph returned from condense_network() and the previous reduced graphs considered, this function outputs any new search terms found in the condensed network that haven't been previously considered.
#' @param reduced_graph the reduced form of the condensed graph
#' @param previous_graphs a list object of any previously considered reduced graphs. If this is the first iteration, this should only be the reduced graph of the full network.
#' @return a character vector of new search terms to consider
#' @example inst/examples/get_condensed_terms.R
get_condensed_terms <- function(reduced_graph, previous_graphs){

  for(i in 1:length(previous_graphs)){
    if(i==1){
      considered_terms <- names(igraph::V(previous_graphs[[i]]))
    }
    if(i>1){
      considered_terms <- unique(append(considered_terms, names(igraph::V(previous_graphs[[i]]))))
    }
  }

  search_terms <- litsearchr::get_keywords(reduced_graph=reduced_graph, savekeywords = FALSE, makewordle = FALSE)

  new_terms <- c()
  for(i in 1:length(search_terms)){
    if(sum(which(stringr::str_detect(considered_terms, search_terms[i])==TRUE))==0){
      new_terms <- append(new_terms, search_terms[i])
    }
  }

  return(new_terms)

}

#' Retrieves terms similar to included terms
#' @description Given a list of terms selected for inclusion, returns other terms from the network that are similar.
#' @param grouped_terms a list of character vectors with terms
#' @param graph a graph object of the full keyword co-occurrence
#' @param considered_terms a character vector of terms you already considered and rejected
#' @return a named number vector of node strengths
get_similar_terms <- function(grouped_terms, graph, considered_terms=NULL){

  all_terms <- names(igraph::V(graph))
  all_strengths <- igraph::strength(graph)

  for(h in 1:length(grouped_terms)){
    if(h==1){my_terms <- grouped_terms[[h]]}
    if(h>1){
      my_terms <- append(my_terms, grouped_terms[[h]])
    }
  }

  unigrams <- strsplit(paste(my_terms, collapse=" "), " ")[[1]]
  for(i in 1:length(unigrams)){
    current_term <- litsearchr::should_stem(unigrams[i])
    if(i==1){potential_terms <- c()}
    for(j in 1:length(all_terms)){
      if(stringr::str_detect(all_terms[j], current_term)){
        if(stringr::str_detect(paste(my_terms, considered_terms, collapse="; "), all_terms[j])==FALSE){
          potential_terms <- unique(append(potential_terms, all_terms[j]))
        }
      }
    }
    if(i==length(unigrams)){
      for(k in 1:length(potential_terms)){
        x <- which(all_terms==potential_terms[k])
        if(k==1){
          NS <- all_strengths[x]
        }
        if(k>1){
          NS <- append(NS, all_strengths[x])
        }
      }
    }
  }
  return(NS)
}

