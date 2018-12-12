#' Expected returns and covariance matrix from Research Affiliates November 2018.
#' 
#' Forecasted 10 year values for 27 asset classes from Research Affiliates. The data set contains a list of 3 items. 
#' 
#' 
#' @format expected.returns: a data frame with 27 rows (asset classes) and 11 variables
#' \describe{
#'   \item{Asset Class}{name of asset class, character}
#'   \item{Index Name}{ a representative index}
#'   \item{Expected Return (Nominal)}{10 year nominal forecast return in decimal}
#'   \item{Expected Return (Real)}{10 year real forecast return in decimal}
#'   \item{Average Net Yield}{forecast income in decimal}
#'   \item{Capital Growth}{in decimal}
#'   \item{Valuation Change}{includes valuation change and currency in decimal}
#'   \item{Diversification Return}{in decimal}
#'   \item{Residual}{in decimal}
#'   \item{Volatility}{in decimal}
#'   \item{Sharpe Ratio}{return per unit of risk}
#' }
#' 
#' @format expected.cov: a data frame with 27 rows and 28 columns. Each element is the 10 year forecast covariance
#' \describe{
#'  \item{Asset Class}{Name of the asset class}
#' }
#' 
#' @format as.of.date the month and year of the RAFI source file.
#' 
#' @source Research Affiliates \url{https://interactive.researchaffiliates.com/asset-allocation#!/?currency=USD&model=ER&scale=LINEAR&terms=NOMINAL}
"rafi_201811"