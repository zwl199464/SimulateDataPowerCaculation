


#' Data prepare for doing power plot
#'@description The function is use to calculate the pca and spca power with desired times
#' of simulation \code{"iterations"}, effect size \code{"beta"}, standard
#'  deviation \code{"sd"}, and the target dataframe \code{"dat"} and shove them into one single
#'  data frame ready to be plot.
#' @param from a number that is the start of the effect size value
#' @param to a number that is the end of the effect size value
#' @param by a number that is the interval of the effect size value
#' @param dat a data frame that contain the data you want to calculate the power
#' @param sd the desired standard deviation
#' @param iterations the desired interation for power calculaition,big number takes more time but more accurate
#' @param core a number to create cluster with desired number of cores
#' @param seed set the seed number for reproduce purpose
#'
#' @return a data frame contain all the power for pca and spca
#' @import foreach
#' @import stats
#' @import doParallel
#' @import parallel
#' @export
#'
#' @examples
#' plotData(0.1,0.3,0.1,dat,5)
plotData <- function(from, to, by, dat, sd, iterations=100,core=7,seed=17) {

  PlotData <- list() #initiate list
  counter <- 1
  #get the power for pca and spca for every different beta
  for(i in seq(from = from, to = to, by =by )) {
    pca<-pca.power(dat,core=core,beta=i,sd=sd,iterations=iterations,seed)
    spca<-spca.power(dat,core=core,beta=i,sd=sd,iterations=iterations,seed)
    res.power<-data.frame(cbind(i,pca,spca))
    names(res.power)<-c("beta","pcaPower","spcaPower")
    PlotData[[counter]]<-res.power
    counter <- counter + 1
    close(pipe("<-WEILU-ZHAO-PC:11867"))
    PlotData
  }
  PlotData_F<-data.frame(do.call(rbind, PlotData))

  return(PlotData_F)


}




