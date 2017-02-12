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
