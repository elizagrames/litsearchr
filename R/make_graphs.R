#' Convert occurrence matrix to graph
#' @description Converts the presence-absence dataset to a keyword co-occurrence network stored as an \code{igraph} object.
#' @param trimmed_data The presence-absence dataset that has been trimmed down to only include common keywords.
#' @return An \code{igraph} graph object.
make_graph <- function(trimmed_data, printplot=TRUE){
  data_mat <- as.matrix(trimmed_data)
  co_occ <- t(data_mat) %*% data_mat
  graph <- igraph::graph.adjacency(co_occ,
                                   weighted=TRUE,
                                   mode="undirected",
                                   diag=FALSE)

  if (printplot == TRUE){
    png("original-network.png", width = 8, height = 8, units = 'in', res = 400)
    require("igraph", quietly=TRUE)
    plot(graph,
         vertex.label.color="#00000000", vertex.label.cex=.5,
         vertex.size=sqrt(igraph::strength(graph)), vertex.color="white", vertex.frame.color="black",
         edge.width=sqrt(E(graph)$weight), edge.color="black", edge.arrow.size=.25)
    dev.off()
    }
  return(graph)
}

#' Create reduced graph of important nodes
#' Takes the full graph and reduces it to only include nodes (and associated edges) greater than the cutoff strength for important nodes.
#' @param graph The full graph object.
#' @param cutoff_strength The minimum node strength to be included in the reduced graph.
#' @return An \code{igraph} graph with only important nodes.
reduce_graph <- function(graph, cutoff_strength, printplot=TRUE){
  NS <- strength(graph)
  node_index <- as.data.frame(cbind(names(NS[1]), (NS[[1]])))

  for (i in 2:length(NS)){
    entry <- as.data.frame(cbind(names(NS[i]), NS[[i]]))
    node_index <- rbind(node_index, entry)
  }

  node_index$V2 <- as.numeric(as.character(node_index$V2))
  important_nodes <- which(node_index$V2 >= cutoff_strength)

  reduced_graph <- induced.subgraph(graph, v=important_nodes)
  if (printplot == TRUE){
    png("reduced-network.png", width = 8, height = 8, units = 'in', res = 400)
    require("igraph", quietly=TRUE)
    plot(reduced_graph,
         vertex.label.color="#000000", vertex.label.cex=.5,
         vertex.size=sqrt(igraph::strength(reduced_graph)), vertex.color="white", vertex.frame.color="black",
         edge.width=sqrt(E(reduced_graph)$weight), edge.color="black", edge.arrow.size=.25)
    dev.off()
  }

  return(reduced_graph)
}
