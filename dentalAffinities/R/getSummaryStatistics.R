#' Create a summary statistics
#'
#' @param df dataframe with variables
#'
#' @return data frame with summary statistics
#' @import tidyr
#' @import dplyr
#' @export
getSummaryStatistics <- function(df) {
  tmp <- df[,-(1:3)]
  res <- na.omit(gather(tmp, var, val))
  ndf <- res %>% group_by(var, val) %>% summarise(n = n()) %>% spread(val, n, fill = NA)
  ndf <- as.data.frame(ndf)
  colnames(ndf)[1] <- "Frequency of values"
  ndf
}
