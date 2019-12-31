#' Plot full keyword co-occurrence network
#' @description Passes plotting parameters to igraph to suppress node names and show the structure of the full network.
#' @param graph an igraph object
#' @param graphcolor a color choice for the graph edges and node frames
#' @param color_gradient if TRUE, the intensity of the color for each node scales with node strength
#' @return a plot of the full network structure
#' @examples plot_network(reduce_graph(litsearchr::BBWO_graph, 15, importance_method="strength"))
plot_network <- function(graph, graphcolor="#006E6D", color_gradient=TRUE){
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

