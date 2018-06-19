#' Checking correlations for feasibility
#' @description The function returns the lower and upper bounds of the
#'  correlation coefficients of each pair of discrete variables given their
#'   marginal distributions is uniformed
#'   , i.e., returns the range of feasible bivariate
#'    correlations.
#' @param start a number to set up the min value for the discrete variable.
#' @param end a number to set up the max value for the discrete variable.
#' @param number.of.scores a number represent  how many discrete variables.
#'
#' @return The functions returns a list of two matrices:
#' the former contains the lower bounds, the latter the upper bounds of the
#'  feasible pairwise correlations (on the extra-diagonal elements)
#' @import GenOrd
#' @export
#'
#' @examples
#' Corrmatrix.limit(0,8,4)
Corrmatrix.limit <- function(start,end, number.of.scores) {
  #Get the marginal probability distribution

  p<-1/(end-start+1)

  for (i in 0:(end-start-2)+1){

    p<-c(p,p[i]+(1/(end-start+1)))
  }

  p<-round_preserve_sum(p,3)


  mar<-rep(list(p), number.of.scores)

  #Get support list

  sup <- rep(list(start:end), number.of.scores)

  #Check for plausible correlation matrix

  corrcheck (marginal = mar, support = sup)
}
