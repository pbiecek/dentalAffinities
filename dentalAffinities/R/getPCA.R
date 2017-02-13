#' Create a PCA biplot
#'
#' @param mat distance matrix
#'
#' @import ggplot2
#' @return ggplot2 plot
#' @export
getPCA <- function(df) {
  for (i in 4:ncol(df))
    df[,i] <- ifelse(is.na(df[,i]),
                     mean(df[,i], na.rm = TRUE),
                     df[,i])
  tmp <- df[,-(1:3)]
  inx <- apply(tmp,2, function(x) diff(range(x))) > 0
  tmp <- tmp[,which(inx)]

  ggbiplot(princomp(tmp), groups = df[,2], ellipse=TRUE) +
    ggtitle("PCA plot") + theme_classic() + xlab("") + ylab("")
}
