#' Anscombe transformation
#' @param n number of observations
#' @param p proportions
#' @return theta MMD
#' @export
theta_Anscombe <- function(n,p) { asin((n/(n+3/4))*(1-2*p)) }

#' Freeman & Tukey transformation
#' @param n number of observations
#' @param p proportions
#' @return theta MMD
#' @export
theta_Freeman <- function(n,p) { 0.5*(asin(1-(2*p*n/(n+1)))+asin(1-(2*((p*n)+1)/(n+1)))) }

#' Freeman & Tukey correction
#' @param nA number of observations in the first group
#' @param pA proportions in the first group
#' @param nB number of observations in the second group
#' @param pB proportions in the second group
#' @param theta function for theta MMD calculations
#' @return corrected theta MMD
#' @export
thetadiff_Freeman <- function(nA,pA,nB,pB, theta) { (theta(nA,pA) - theta(nB,pB))^2 - (1/(nA+0.5) + 1/(nB+0.5)) }

#' Grewal correction
#' @param nA number of observations in the first group
#' @param pA proportions in the first group
#' @param nB number of observations in the second group
#' @param pB proportions in the second group
#' @param theta function for theta MMD calculations
#' @return corrected theta MMD
#' @export
thetadiff_Grewal <- function(nA,pA,nB,pB, theta) { (theta(nA,pA) - theta(nB,pB))^2 - (1/nA + 1/nB) }

#' Uncorrected formula
#' @param nA number of observations in the first group
#' @param pA proportions in the first group
#' @param nB number of observations in the second group
#' @param pB proportions in the second group
#' @param theta function for theta MMD calculations
#' @return corrected theta MMD
#' @export
thetadiff_uncorrected <- function(nA,pA,nB,pB, theta) { (theta(nA,pA) - theta(nB,pB))^2 }

#' Extracts Mn and Mp matricex from binary data
#' @param binary_trait_data matrix with binary data
#' @return List with two matrices for Mn and Mp
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @export
get_Mn_Mp <- function(binary_trait_data) {
 colnames(binary_trait_data)[1:3] <- c("id", "site", "sex")
 binary_trait_data_long <- gather(binary_trait_data, trait, value, -(1:3))
 gr <- summarise(group_by(binary_trait_data_long, site, trait),
           n = length(na.omit(value)),
           p = ifelse(n>0, mean(value > 0, na.rm=TRUE),0.5))
 Mn <- spread(gr[,c("site","trait", "n")], trait, n)
 Mp <- spread(gr[,c("site","trait", "p")], trait, p)

 list(Mn = Mn, Mp = Mp)
}
