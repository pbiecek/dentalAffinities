#' Create a MDS diagram for given distance matrix
#'
#' @param mat distance matrix
#' @param metric metric (cmdscale) or non metric (isoMDS) scaling?
#'
#' @return ggplot2 plot
#' @export
#' @importFrom MASS isoMDS
#' @importFrom stats cmdscale
#' @import ggplot2
getMDS <- function(mat, metric = TRUE) {
  if (metric) {
    np <- cmdscale(as.dist(mat), k = 2)
  } else {
    np <- isoMDS(as.dist(mat), k = 2)$points
  }
  df <- data.frame(site = rownames(np), x = np[,1], y = np[,2])
  ggplot(df, aes(x,y)) +
    geom_point(size = 2) +
    scale_x_continuous(expand = c(0.05,0.05)) +
    scale_y_continuous(expand = c(0.05,0.05)) +
    coord_fixed() +
    geom_text(aes(label = site), vjust = -0.3) +
    xlab("") + ylab("") + theme_classic() +
    ggtitle("MDS diagram")
}
