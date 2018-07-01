#' Create a wordcloud
#' @description Makes a wordcloud of keywords identified as important and changes size of text to correspond to node strength.
#' @param graph a reduced graph of only important nodes
#' @param colorchoice a vector listing colors to use for the wordcloud
#' @return a wordcloud
make_wordle <- function(graph, colorchoice=c("black")){
  wordle <- as.data.frame(cbind(names(V(graph)), strength(graph)))
  colnames(wordle) <- c("label", "size")
  if (!requireNamespace("wordcloud", quietly = TRUE)){
    stop("wordcloud needed for this function to work. Please install it.",
         call. = FALSE)
  }
  wordcloud::wordcloud(words = as.character(wordle$label),
                       freq = (as.numeric(wordle$size)^2), min.freq = 1,
                       max.words=1000, random.order=FALSE, rot.per=0.2,
                       colors=colorchoice, scale=c(2,.5))
}

#' Plot full keyword co-occurrence network
#' @description Passes plotting parameters to igraph to suppress node names and show the structure of the full network.
#' @param graph an igraph object
#' @param graphcolor a color choice for the graph edges and node frames
#' @return a plot of the full network structure
print_full_network <- function(graph, graphcolor="black"){
  require(igraph, quietly = TRUE)
  plot(graph,
     vertex.label.color="#00000000", vertex.label.cex=.5,
     vertex.size=sqrt(igraph::strength(graph)), vertex.color="white", vertex.frame.color=graphcolor,
     edge.width=sqrt(E(graph)$weight), edge.color=graphcolo, edge.arrow.size=.25)
}
