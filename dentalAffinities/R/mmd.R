#' Calcuate MMD Measures
#'
#' Base on Arkadiusz Soltysiak ,,Technical Note: An R script for Smith's Mean Measure of Divergence''
#'
#' @param M_n Matrix with number of observations for each trait and each site
#' @param M_p Matrix with proportions for each trait and each site
#' @param thetadiff Function for correction of the MMD
#' @param theta Function for MMD calculations
#'
#' @return a list with tree matrices: MMDMatrix - distance, SDMatrix - standard deviation, SigMatrix - significance
#' @export
calculateMMD <- function(M_n, M_p, thetadiff, theta) {
  # remove traits with n == 0
  ind <- which(!apply(M_n == 0, 2, any))
  M_p <- M_p[,ind]
  M_n <- M_n[,ind]
  # end - remove traits with n == 0

  VarMatrix <- M_n[1:2, 2:length(M_n[1, ])]
  MMDMatrix <- matrix(0, length(M_n[, 1]), length(M_n[, 1]))

  for (a in seq_along(VarMatrix[1, ])) {
    for (b in seq_along(MMDMatrix[, 1])) {
      for (c in seq_along(MMDMatrix[1, ])) {
        MMDMatrix[b,c] <- thetadiff(M_n[b,a+1], M_p[b,a+1], M_n[c,a+1], M_p[c,a+1], theta)
      }
    }

    for (b in seq_along(MMDMatrix[, 1])) {
      for (c in seq_along(MMDMatrix[1, ])) {
        if (b >= c) {
          MMDMatrix[b, c] = 0
        }
      }
    }

    VNeg <- 0
    VPos <- 0
    for (b in seq_along(MMDMatrix[, 1])) {
      for (c in seq_along(MMDMatrix[1, ])) {
        if (MMDMatrix[b, c] > 0) {
          VPos = VPos + 1
        }
        if (MMDMatrix[b, c] < 0) {
          VNeg = VNeg + 1
        }
      }
    }

    VarMatrix[1, a] = sum(MMDMatrix)
    VarMatrix[2, a] = VPos / (VPos + VNeg)
  }

  VarStatus <- t(VarMatrix)

  ## -------------SECTION D: MMD MATRIX------------------------------------------

  MMDMatrix <- matrix(0, length(M_n[, 1]), length(M_n[, 1]))
  dimnames(MMDMatrix) <- list(M_n[, 1], M_n[, 1])

  for (a in seq_along(MMDMatrix[, 1])) {
    for (b in seq_along(MMDMatrix[1, ])) {
      MMDVect <- vector("double", length(M_n[1, ]) - 1)
      for (i in seq_along(MMDVect)) {
        MMDVect[i] <- thetadiff(M_n[a,i+1], M_p[a,i+1], M_n[b,i+1], M_p[b,i+1], theta)
      }
      MMDMatrix[a, b] <- sum(MMDVect) / length(MMDVect)
    }
  }

  ## forced 0 when a sample is compared to itself
  for (a in seq_along(MMDMatrix[,1])) { MMDMatrix[a,a] = 0 }

  ## -------------SECTION E: SD MATRIX-------------------------------------------

  ## standard deviation for MMD, Sjovold's formula
  SDMatrix <- matrix(0, length(M_n[, 1]), length(M_n[, 1]))
  dimnames(SDMatrix) <- list(M_n[, 1], M_n[, 1])
  SDDiff <- function(nA, nB) {
    (1 / nA + 1 / nB) ^ 2
  }

  for (a in seq_along(MMDMatrix[, 1])) {
    for (b in seq_along(MMDMatrix[1, ])) {
      SDVect <- vector("double", length(M_n[1, ]) - 1)
      for (i in seq_along(SDVect)) {
        SDVect[i] <- SDDiff(M_n[a, i + 1], M_n[b, i + 1])
      }
      SDMatrix[a, b] <- sqrt(sum(SDVect) * 2 / length(SDVect) ^ 2)
    }
  }

  ## -------------SECTION F: SIGNIFICANCE MATRIX---------------------------------

  ## statistical significance
  SigMatrix <- matrix(1, length(M_n[, 1]), length(M_n[, 1]))
  dimnames(SigMatrix) <- list(M_n[, 1], M_n[, 1])

  for (a in seq_along(MMDMatrix[, 1])) {
    for (b in seq_along(MMDMatrix[1, ])) {
      dist <- MMDMatrix[a, b] / SDMatrix[a, b]
      SigMatrix[a, b] = round((1 - pnorm(dist)) * 2, digits = 8)
      if (MMDMatrix[a, b] < 0)
        SigMatrix[a, b] = 1
    }
  }
  list(MMDMatrix = MMDMatrix, SDMatrix = SDMatrix, SigMatrix = SigMatrix)
}
