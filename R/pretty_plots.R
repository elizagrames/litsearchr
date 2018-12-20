#' Create a wordcloud
#' @description Makes a wordcloud of keywords identified as important and changes size of text to correspond to node strength.
#' @param graph a reduced graph of only important nodes
#' @param colorchoice a vector listing colors to use for the wordcloud
#' @return plots a wordcloud
#' @examples make_wordle(reduce_graph(litsearchr::BBWO_graph, 15, importance_method="strength"))
make_wordle <- function(graph, colorchoice=c("#006E6D")){
  wordle <- as.data.frame(cbind(names(igraph::V(graph)), igraph::strength(graph)))
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
#' @param color_gradient if TRUE, the intensity of the color for each node scales with node strength
#' @return a plot of the full network structure
#' @examples plot_full_network(reduce_graph(litsearchr::BBWO_graph, 15, importance_method="strength"))
plot_full_network <- function(graph, graphcolor="#006E6D", color_gradient=TRUE){
  node_color <- graphcolor
  if (color_gradient==TRUE){
    color_intensity <- igraph::strength(graph)
    color_set <- ((log(color_intensity-min(color_intensity)+1)/log(max(color_intensity-min(color_intensity)+1)))*100)
    color_set <- floor(as.numeric(color_set/.390625))
    alpha_codes <- litsearchr::color_alphas$code[color_set]
    color_list <- paste(graphcolor, alpha_codes, sep="")
    node_color <- color_list
  }

  igraph::plot.igraph(graph,
                      vertex.label.color="#000000", vertex.label.cex=.5,
                      vertex.size=sqrt(igraph::strength(graph)),
                      vertex.color=node_color, vertex.frame.color=node_color,
                      edge.width=.25, edge.color="black", edge.arrow.size=.25)
}

#' Plot a co-occurrence network in 3D
#' @description Plots a keyword co-occurrence network as an interactive, 3D RGL object. Doesn't really serve any purpose other than seeing node density, but it looks cool. Node sizes are the square root of node strength; edge widths are fixed at .25. Depends on the rgl package.
#' @param graph an igraph object
#' @param graphcolor a valid color name or hex code for the graph nodes
#' @return an RGL plot of the graph
#' @examples plot_3D_network(reduce_graph(litsearchr::BBWO_graph, 15, importance_method="strength"))
plot_3D_network <- function(graph, graphcolor="#006E6D"){
  if (!requireNamespace("rgl", quietly = TRUE)){
    stop("rgl needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (requireNamespace("rgl", quietly=TRUE)){
    igraph::rglplot(graph,
                    layout=igraph::layout.fruchterman.reingold(graph, dim=3),
                    vertex.label.color="#000000", vertex.label.cex=.5,
                    vertex.size=sqrt(igraph::strength(graph)),
                    vertex.color=graphcolor, vertex.frame.color=graphcolor,
                    edge.width=.25, edge.color="black", edge.arrow.size=.25)
  }

 }

