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
#  file <- "~/GitHub/dentalAffinities/docs/test_file.xlsx"
  read.xlsx(file, 1)
}

