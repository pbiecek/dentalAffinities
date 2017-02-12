## Anscombe transformation
theta_Anscombe <- function(n,p) { asin((n/(n+3/4))*(1-2*p)) }

## Freeman & Tukey transformation
theta_Freeman <- function(n,p) { 0.5*(asin(1-(2*p*n/(n+1)))+asin(1-(2*((p*n)+1)/(n+1)))) }

## Freeman & Tukey correction
thetadiff_Freeman <- function(nA,pA,nB,pB, theta) { (theta(nA,pA) - theta(nB,pB))^2 - (1/(nA+0.5) + 1/(nB+0.5)) }

## Grewal correction
thetadiff_Grewal <- function(nA,pA,nB,pB, theta) { (theta(nA,pA) - theta(nB,pB))^2 - (1/nA + 1/nB) }

## uncorrected formula
thetadiff_uncorrected <- function(nA,pA,nB,pB, theta) { (theta(nA,pA) - theta(nB,pB))^2 }

get_Mn_Mp <- function(binary_trait_data) {
 colnames(binary_trait_data)[1:3] <- c("id", "site", "sex")
 binary_trait_data_long <- gather(binary_trait_data, trait, value, -(1:3))
 gr <- summarise(group_by(binary_trait_data_long, site, trait),
           n = length(na.omit(value)),
           p = mean(value > 0, na.rm=TRUE))
 Mn <- spread(gr[,c("site","trait", "n")], trait, n)
 Mp <- spread(gr[,c("site","trait", "p")], trait, p)
 list(Mn = Mn, Mp = Mp)
}
