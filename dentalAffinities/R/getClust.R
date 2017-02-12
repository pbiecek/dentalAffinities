#' Create a clustering diagram for given distance matrix
#'
#' @param mat distance matrix
#'
#' @return ggplot2 plot
#' @export
#' @importFrom clust agnes
#' @import ggplot2
getClust <- function(mat) {
  grupy <- agnes(as.dist(mat), method = "ward")
  fviz_dend(grupy, rect = TRUE, main = "Clustering: Ward method") +
    scale_y_continuous(expand = c(0.2,0))
}
