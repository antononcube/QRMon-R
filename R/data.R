#' Heteroscedastic distribution data
#'
#' The data is generated with the code:
#' \code{xs <- seq(-3, 3, 0.01)}
#' \code{dfDistributionData <- purrr::map_df( xs, function(x) { data.frame( X = x, Y = exp(-x^2) + rnorm( n = 1, mean = 0, sd =  0.15 * sqrt( abs( 1.5 - x) / 1.5 ) ) ) })}.
#'
#' @format A data frame with 601 rows and 2 columns.
#' \describe{
#'     \item{X}{x-coordinate}
#'     \item{Y}{y-coordiante}
#' }
"dfDistributionData"

#' Orlando temperature data
#'
#' The data is daily temperature in Orlando, Florida, USA
#' from 2015-01-01 to 2019-01-01.
#'
#' The data was obtained with the Mathematica code:
#' \code{WeatherData[{"Orlando", "USA"}, "Temperature", {{2015, 1, 1}, {2019, 1, 1}, "Day"}]}.
#'
#' @format A data frame with 1461 rows and 2 columns.
#' \describe{
#'     \item{Time}{Date as the number of seconds from 1900-01-01 00:00}
#'     \item{Temperature}{Temperature in Celsius}
#' }
"dfTemperatureData"

#' GE stock data
#'
#' GE stock data from 2014-01-01 to 2019-01-01.
#'
#' @format A data frame with 1258 rows and 2 columns.
#' \describe{
#'     \item{Time}{Date}
#'     \item{Value}{Adjusted stock price in US dolars}
#' }
"dfFinancialData"
