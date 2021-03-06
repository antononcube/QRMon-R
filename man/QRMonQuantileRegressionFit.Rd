% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QRMon.R
\name{QRMonQuantileRegressionFit}
\alias{QRMonQuantileRegressionFit}
\title{Quantile regression function basis fit.}
\usage{
QRMonQuantileRegressionFit(
  qrObj,
  formula,
  probabilities = c(0.25, 0.5, 0.75),
  ...
)
}
\arguments{
\item{qrObj}{An QRMon object.}

\item{formula}{A formula.}

\item{probabilities}{A numeric vector with quantile probabilities.}

\item{...}{Arguments for  \code{\link{quantreg::rq}}.}
}
\value{
A QRMon object.
}
\description{
Finds the quantile regression objects for the specified
quantile probabilities using a specified formula.
}
\details{
The formula has to use "Value" and "Regressor".
For example: \code{Value ~ sin(1+3*Regressor)}.
The obtained regression objects are assigned/appended to
\code{qrObj$RegressionObjects}.
For more computational details see \code{\link{quantreg::rq}}.
}
\seealso{
Other Regression functions: 
\code{\link{QRMonPickPathPoints}()},
\code{\link{QRMonPredict}()},
\code{\link{QRMonQuantileRegression}()},
\code{\link{QRMonSeparateToFractions}()},
\code{\link{QRMonSeparate}()}
}
\concept{Regression functions}
