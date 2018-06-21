#' Create a wordcloud
#' @description Makes a wordcloud of keywords identified as important and changes size of text to correspond to node strength.
#' @param graph The reduced graph of only important nodes.
#' @param colorchoice A vector listing colors to use for the wordcloud.
#' @return Saves wordcloud to a .png file.
make_wordle <- function(graph, colorchoice=c("black")){
  wordle <- as.data.frame(cbind(names(V(graph)), strength(graph)))
  colnames(wordle) <- c("label", "size")
  png("wordcloud.png", width = 8, height = 8, units = 'in', res = 400)
  if (!requireNamespace("wordcloud", quietly = TRUE)){
    stop("wordcloud needed for this function to work. Please install it.",
         call. = FALSE)
  }
  wordcloud::wordcloud(words = as.character(wordle$label),
                       freq = (as.numeric(wordle$size)^2), min.freq = 1,
                       max.words=1000, random.order=FALSE, rot.per=0.2,
                       colors=colorchoice, scale=c(2,.5))
  dev.off()
}
