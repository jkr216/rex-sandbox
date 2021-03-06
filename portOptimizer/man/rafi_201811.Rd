% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.r
\docType{data}
\name{rafi_201811}
\alias{rafi_201811}
\title{Expected returns and covariance matrix from Research Affiliates November 2018.}
\format{expected.returns: a data frame with 27 rows (asset classes) and 11 variables
\describe{
  \item{Asset Class}{name of asset class, character}
  \item{Index Name}{ a representative index}
  \item{Expected Return (Nominal)}{10 year nominal forecast return in decimal}
  \item{Expected Return (Real)}{10 year real forecast return in decimal}
  \item{Average Net Yield}{forecast income in decimal}
  \item{Capital Growth}{in decimal}
  \item{Valuation Change}{includes valuation change and currency in decimal}
  \item{Diversification Return}{in decimal}
  \item{Residual}{in decimal}
  \item{Volatility}{in decimal}
  \item{Sharpe Ratio}{return per unit of risk}
}}
\source{
Research Affiliates \url{https://interactive.researchaffiliates.com/asset-allocation#!/?currency=USD&model=ER&scale=LINEAR&terms=NOMINAL}
}
\usage{
rafi_201811
}
\description{
Forecasted 10 year values for 27 asset classes from Research Affiliates. The data set contains a list of 3 items.
}
\keyword{datasets}
