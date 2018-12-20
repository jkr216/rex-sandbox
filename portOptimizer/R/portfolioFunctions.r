#' Portfolio Return
#'
#' @param x Weight vector of the portfolio
#' @param expectedReturns Vector of the expected returns
#'
#' @return scalar
#' @export
#'
#' @examples calc_port_return(x, expectedReturns)
calc_port_return <- function(x, expectedReturns){
  sum(x * expectedReturns)
}

#' Portfolio Risk (Standard Deviation)
#'
#' @param x Weight vector of the portfolio
#' @param expectedCov Covariance matrix with dim(length(x), length(x))
#'
#' @return scalar
#' @export
#'
#' @examples calc_port_risk(x, expectedCov)
calc_port_risk <- function(x, expectedCov){  # used to calculate tracking error also
  as.numeric(t(x) %*% expectedCov %*% x) ^ 0.5
}

#' Tracking error
#'
#' @param x Weight vector of the portfolio
#' @param benchX Weight vector of the benchmark
#' @param expectedCov Covariance matrix with dim(length(x), length(x))
#'
#' @return scalar
#' @export
#'
#' @examples calc_port_TE(x, benchX, expectedCov)
calc_port_TE <- function(x, benchX, expectedCov){
  calc_port_risk(x - benchX, expectedCov)
}

#' Number of nonzero-weighted asset
#'
#' @param x Weight vector of the portfolio
#'
#' @return scalar
#' @export
#'
#' @examples calc_nnonzeroassets(x)
calc_nnonzeroassets <- function(x){
  sum(x != 0)
}

#' Weighted Average
#'
#' A simple function with many uses.  Besides the obvious use of weighted average to calculate something like portfolio yield,
#' this can be used to find the weight of a group (set) of assets. For example, if the weights vector has 1 for each asset in
#' a group and zero for the others, then the result will be the allocation to that group.  Consider a set of many assets
#' including US Large and US Small.  If the US Large asset has a weight of 0.25 and the US Small has a weight of -0.25, then
#' as long as the result is >= 0, US Large will be at least 75% of the sum of US Large plus US Small. Such an approach is
#' useful for setting a variety of constraints.  To arrive at those coefficients we used basic algebra:
#'      USL / (USL + USS) >= 0.75  (where USL is US Large and USS is US Small)
#'      USL >= 0.75USL + 0.75USS so
#'      0.25USL - 0.75USS >= 0.
#' so for this constraint we create a vector of coefficients (weights) with +0.25 for USL, -0.75 for USS, and 0 for the others.
#'
#' @param x Weight vector of the proposed portfolio
#' @param weights Coefficients to use as weights
#'
#' @return Scalar
#' @export
#'
#' @examples calc_weighted(x, weights)
calc_weighted <- function(x, weights){
  sum(x * weights)
}

#' Turnover
#' Turnover is defined such that if the entire portfolio is sold and repurchases, the value is 1.
#'
#' @param x Weight vector of the proposed portfolio
#' @param curWeights Weight vector of the current portfolio
#'
#' @return a scalar
#' @export
#'
#' @examples calc_turnover(x, curWeights)
calc_turnover <- function(x, curWeights){
  return(sum(abs(x - curWeights))/2)
}

#' Minimum nonzero portfolio weight
#' Function works for long-only portfolios and would need to be modified for long-short.
#' If LB is missing, the minimum nonzero weight is returned.  If LB is not missing, then
#' the smallest nonzero weights for asset classes without a zero LB is returned.
#'
#' @param x Weight vector of the portfolio
#' @param LB Optional lower bound of weights
#'
#' @return scalar
#' @export
#'
#' @examples calc_min_nonzero_weight(x)
calc_min_nonzero_weight <- function(x, LB){
  if(missing(LB)){
    temp <- x[x>0]
  } else {
    temp <- x[LB == 0 & x > 0]
  }
  return(ifelse(length(temp) == 0, Inf, min(temp)))
}
