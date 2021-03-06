#' Calcuate Mahalanobis D2 Measures
#'
#' Based on Lyle W. Konigsberg scripts (tdistR.zip) http://lylek.ucoz.org/index.html
#' with some additional cleaning and regularisation
#'
#' @param binary_trait_data a data frame with binary data
#' @param deltamin will replace any negative distance
#'
#' @export
calculateD2 <- function(binary_trait_data, deltamin= 0.01) {
  # remove columns with wrong data (only NA or 1)
  idx <- c(1:3,which(apply(binary_trait_data[,-(1:3)], 2, function(x) length(unique(na.omit(x)))) > 1) + 3)
  binary_trait_data <- binary_trait_data[,idx]
  # stop
  colnames(binary_trait_data)[1:3] <- c("id", "site", "sex")
  X <- binary_trait_data[,-(1:3)]
  binary_trait_data <- binary_trait_data[!is.na(binary_trait_data$site),]
  tmp <- dentalAffinities::get_Mn_Mp(binary_trait_data)
  Sites <- tmp[[1]][1]
  Mn <- tmp[[1]][-1]
  Mp <- tmp[[2]][-1]
  # remove traits with 0 observations
  idx <- which(!apply(Mn == 0, 2, any))
  X <- X[,idx]
  Mn <- Mn[,idx]
  Mp <- Mp[,idx]

  n0 = Mn*Mp
  n1 = Mn*(1-Mp)
  # correction for 0
  n0[n1 == 0] = n0[n1 == 0] - 0.5
  n1[n1 == 0] = .5
  n1[n0 == 0] = n1[n0 == 0] - 0.5
  n0[n0 == 0] = .5
  # calculate z
  z <- apply(n1/(n0+n1), 1:2, qnorm)
  rownames(z) = Sites$site

  N.sites = nrow(Sites)
  N.traits = ncol(X)
  n.cases = nrow(X)

  # here calculate R
    R = diag(N.traits)
    N.R = diag(N.traits)
    n.unique=N.traits*(N.traits-1)/2
    for(k in 1:N.sites)
    {
      icount = 0
      sto = X[binary_trait_data[,2] == Sites$site[k],]
      for(i in 1:(N.traits-1)){
        for(j in (i+1):N.traits){
          icount=icount+1
          trait.ij=as.vector(table(sto[,c(j,i)]))
          if(sum(trait.ij==0)>=2 | length(trait.ij)<4){
            r=0
            calc.please=F
          } else{
            calc.please=T
            KDELTA=1
            DELTA=0
            if(trait.ij[1]==0 | trait.ij[4]==0) KDELTA=2
            if(trait.ij[2]==0 | trait.ij[3]==0) KDELTA=KDELTA+2

            if(KDELTA==2) DELTA=.5
            if(KDELTA==3) DELTA=-.5
            if(trait.ij[1]==0 & trait.ij[4]==0){
              r=-1
              calc.please=F
            }
            if(trait.ij[2]==0 & trait.ij[3]==0){
              r=1
              calc.please=F
            }
            trait.ij=trait.ij+DELTA*c(1,-1,-1,1)
          }

          if(calc.please==T) r = psych::tetrachoric(trait.ij,correct=F)$rho

          N.cell = sum(trait.ij)

          N.R[i,j] = N.R[i,j] + N.cell
          N.R[j,i] = N.R[j,i] + N.cell

          R[i,j] = R[i,j] + r * N.cell
          R[j,i] = R[j,i] + r * N.cell
        }
      }
    }

    R = R/N.R
    rownames(R) = colnames(X)
    colnames(R) = colnames(X)

  # stop calculations of R

  I = diag(N.sites)
  o = rep(1,N.sites)
  J = o %*% t(o)
  w = o/sum(o)
  Delta = (I - o %*% t(w)) %*% z
  Cp = Delta %*% solve(R) %*% t(Delta)
  D2 = (Cp*I) %*% J + J %*% (Cp*I) - 2*Cp

  # replace negative values
  D2[D2 <= 0] <- deltamin

  rownames(D2) = Sites$site
  colnames(D2) = Sites$site

  list(MMDMatrix = D2, SDMatrix = NULL, SigMatrix = NULL)
}
