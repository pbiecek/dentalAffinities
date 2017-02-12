#' Read Data From Excel File
#'
#' First three collumns in the file should contain ID, SITE and SEX,
#' following columns shall contain trait scores in ordered scale.
#' Moreover, it is assumed, that the second row is names THRESHOLD
#' and contains cut off values.
#'
#' @param file xlsx file with data
#'
#' @return a data frame with data
#' @importFrom openxlsx read.xlsx
#' @export

loadData <- function(file) {
  file <- "~/GitHub/dentalAffinities/docs/test_file.xlsx"
  dat <- read.xlsx(file, 1)
  THRESHOLD <- dat[1,]
  dat <- dat[-1,]

  for (i in 4:ncol(dat)) {
    dat[,i] <- (dat[,i] >= THRESHOLD[1,i]) + 0
  }

  dat
}

