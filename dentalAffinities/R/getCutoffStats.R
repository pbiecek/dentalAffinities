#' Create a cutoff statistics
#'
#' @param df dataframe with variables
#'
#' @import ggplot2
#' @return data frame with summary statistics
#' @export
getCutoffStats <- function(df) {
  tmp <- df[,-(1:3)]
  res <- lapply(1:ncol(tmp), function (i) {
      cutoffs <- sort(unique(na.omit(tmp[,i])))
      if (length(cutoffs) > 1) {
        p.vals <- sapply(1:length(cutoffs), function(j) {
          if (j == 1) {
            c(p.value = -1,
              statistic = -1)
          } else {
            c(p.value = chisq.test(table(tmp[,i] >= cutoffs[j], df[,2]))$p.value,
              statistic = chisq.test(table(tmp[,i] >= cutoffs[j], df[,2]))$statistic[[1]])
          }
        })
        nam <- paste("<=", cutoffs[-length(cutoffs)], "; >=", cutoffs[-1])
        colnames(p.vals) <- c("",nam)
        data.frame(variable = colnames(tmp)[i], cutoffs = c("",nam), t(p.vals))
      }
  })
  res <- do.call(rbind, res)
  res[,3] <- sapply(res[,3], function(x) as.character(signif(x, 2)))
  res[which(res[,4] < 0),3] = ""
  res[which(res[,4] < 0),1] = ""
  res[,4] <- signif(res[,4], 4)
  res[which(res[,4] < 0),4] = NA
  rownames(res) <- 1:nrow(res)
  res
}
