% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotData.R
\name{plotData}
\alias{plotData}
\title{Data prepare for doing power plot}
\usage{
plotData(from, to, by, dat, sd, iterations = 100, core = 7, seed = 17)
}
\arguments{
\item{from}{a number that is the start of the effect size value}

\item{to}{a number that is the end of the effect size value}

\item{by}{a number that is the interval of the effect size value}

\item{dat}{a data frame that contain the data you want to calculate the power}

\item{sd}{the desired standard deviation}

\item{iterations}{the desired interation for power calculaition,big number takes more time but more accurate}

\item{core}{a number to create cluster with desired number of cores}

\item{seed}{set the seed number for reproduce purpose}
}
\value{
a data frame contain all the power for pca and spca
}
\description{
The function is use to calculate the pca and spca power with desired times
of simulation \code{"iterations"}, effect size \code{"beta"}, standard
 deviation \code{"sd"}, and the target dataframe \code{"dat"} and shove them into one single
 data frame ready to be plot.
}
\examples{
plotData(0.1,0.3,0.1,dat,5)
}
