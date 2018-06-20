
#' Power caculation for pca
#'
#'@description The function is use to calculate the spca power with desired times
#' of simulation \code{"iterations"}, effect size \code{"beta"}, standard
#'  deviation \code{"sd"}, and the target dataframe \code{"dat"}.
#' @param dat a datafram that contains discrete data.
#' @param core a number to create cluster with desired number of cores
#' @param beta a effect size parameter for the mean of the simulated biomarkers
#' @param sd the standard deviation of the simulated biomarkers
#' @param iterations how many times you want to simulate the biomarkers
#' @param seed set the seed number for reproduce purpose
#'
#' @return a single number for the power of spca procedure
#' @import foreach
#' @import psych
#' @import stats
#' @import doParallel
#' @import parallel
#' @export
#'
#' @examples
#' spca.power(dat)



spca.power <- function(dat,core=7,beta=0.5,sd=5,iterations=1000,seed=17) {
  set.seed(seed)

  # Create cluster with desired number of cores

  cl <- makeCluster(core)

  # Register cluster

  registerDoParallel(cl)

  i<-NULL

  # using r.drop to select variable: condition (r.drop < 0.7)

  # get the average

  av<-apply(dat[alpha(dat)$item.stats$r.drop>=0.7],1,mean)

  ############# SPCA

  dat<-dat[alpha(dat)$item.stats$r.drop>=0.7]

  spcs <- prcomp ( dat, center=T, scale. = T)

  spc1.scores <- cbind(dat,  spcs$x[, "PC1"])


  #Foreach loop to get the p-value of association test


  x <- foreach(i = 1:nrow(dat)) %dopar%{

    bio <- rnorm(iterations, mean = beta*av[i], sd = sd)
  }

  x <- data.frame(matrix(unlist(x),nrow(dat),iterations,byrow = TRUE))

  p <- foreach(i = 1:iterations) %dopar%{

    p<-summary(lm (x[,i] ~ spc1.scores[,ncol(dat)+1]))$coefficients[2,4]

    k='SPCA'

    return(cbind(k,i, p))
  }

  # Get the p-value table

  df <- data.frame(do.call(rbind, p)) # turn it into a data frame

  df$p <- as.numeric (as.character(df$p))

  colnames(df)<-c('method','iteration','p_value')

  spcaPower<-sum(df$p_value<0.05)/iterations
  
  stopImplicitCluster()

  return(spcaPower)
}
