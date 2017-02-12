#' Create a clustering diagram for given distance matrix
#'
#' @param mat distance matrix
#'
#' @importFrom cluster agnes
#' @importFrom stats as.dendrogram
#' @importFrom ggdendro ggdendrogram
#' @import ggplot2
#' @return ggplot2 plot
#' @export
getClust <- function(mat) {
  grupy <- agnes(as.dist(mat), method = "ward")
  dg <- as.dendrogram(grupy)
  ggdendrogram(dg)
}
