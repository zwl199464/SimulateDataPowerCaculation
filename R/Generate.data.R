
#' Drawing a sample of discrete data
#' @description  The function draws a sample from a multivariate discrete
#' variable with correlation matrix \code{"corMatrix"} and
#' uniformed marginal distributions with specific range from \code{"start"} to
#' \code{"end"}
#' @param start a number to set up the min value for the discrete variable.
#' @param end a number to set up the max value for the discrete variable.
#' @param number.of.scores a number represent  how many discrete variables.
#' @param corMatrix a  correlation matrix.
#' @param x the sample size.
#'
#' @return a \code{"x"} times \code{"number.of.scores"} matrix of values drawn
#'  from the \code{"number.of.scores"}-variate discrete
#'  r.v. with the uniform marginal distributions and desired correlation matrix
#' @import GenOrd
#' @export
#'
#' @examples
#' DDF<-matrix(as.double(data.matrix(DF)),4,4)
#' Generate.data(0,8, 4,DDF, 100)
Generate.data <- function(start,end, number.of.scores, corMatrix, x) {

  ####################################Genarate data

  #Get the marginal probability distribution

  p<-1/(end-start+1)

  for (i in 0:(end-start-2)+1){

    p<-c(p,p[i]+(1/(end-start+1)))
  }

  p<-round_preserve_sum(p,3)


  mar<-rep(list(p), number.of.scores)

  #Get support list

  sup <- rep(list(start:end), number.of.scores)


  sigm<-data.matrix(corMatrix)

  ordsample(n=x, marginal = mar, Sigma = sigm,support = sup)

}


