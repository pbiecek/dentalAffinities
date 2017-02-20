#' Create a Czekanowski diagram (MaCzek) for given distance matrix
#'
#' Based on Czekanowski diagrams as described in http://antropologia.uw.edu.pl/MaCzek/maczek.html
#'
#' @param mat distance matrix
#'
#' @return ggplot2 plot
#' @import ggplot2
#' @export
getCzekanowski <- function(mat) {
  # permute
  np <- cmdscale(as.dist(mat), k = 1)
  ord <- names(np[order(np[,1]),])
  mat <- mat[order(np[,1]), order(np[,1])]

  eps <- min(mat[mat!=0])/3
  diag(mat) = eps

  ndf <- as.data.frame(as.table(1/mat))

  ggplot(ndf, aes(Var1, Var2, size=Freq)) +
    geom_point(shape = 15) +
    theme_classic() +
    scale_size_continuous(range = c(0,15)) +
    theme(legend.position="none") +
    labs(title = "Czekanowski Diagram",
         x = "", y = "")
}
