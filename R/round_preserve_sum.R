#' Round a vector and make sure it adds up to a limit
#'
#' @description
#' This is useful for rounding a set of probabilities and making sure they sum to 1 or forcing percentages sum to 100.
#'
#' @param x A vector that will rounded
#' @param digits The precision of the rounding. Set to 2 for probabilities to show as .15.
#'
# @importFrom utils tail
#'
# examples@
# commute <- c(`Less than 15 minutes`= 1678, `Less than 30 minutes` = 2421, `Less than 45 minutes` = 2171, `Less than 60 minutes` = 968,  `Less than 90 minutes` = 524, `90 or more minutes` = 193)
# percentsBroken <- round(commute / sum(commute), 2) * 100
# percents <- round_preserve_sum(commute / sum(commute), 2) * 100
# sum(percents)
#'
#' @source https://biostatmatt.com/archives/2902
#' @export

round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- utils::tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

