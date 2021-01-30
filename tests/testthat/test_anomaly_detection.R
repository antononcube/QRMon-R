context("Anomaly detection workflow")
library(QRMon)

test_that("Anomaly detection by residuals using threshold", {

  qrObj1 <-
    QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
    QRMonQuantileRegression( df = 12, probabilities = 0.5 )

  dfOutliers1 <-
    qrObj1 %>%
    QRMonFindAnomaliesByResiduals( threshold = 5, relativeErrorsQ = FALSE ) %>%
    QRMonTakeValue

  expect_s3_class( dfOutliers1, "data.frame")

  expect_true( nrow(dfOutliers1) < floor( 0.2 * nrow( dfTemperatureData ) ) )
})


## External outlier identifier, same as in the implementation in OutlierIdentifiers,
## https://github.com/antononcube/R-packages/tree/master/OutlierIdentifiers

SPLUSQuartileIdentifierParameters <-
  function (data)
  {
    if (length(data) <= 4) {
      xL <- min(data, na.rm = TRUE)
      xU <- max(data, na.rm = TRUE)
    }
    else {
      res <- quantile(data, c(1/4, 3/4), na.rm = TRUE)
      xL <- res[[1]]
      xU <- res[[2]]
    }
    c(xL - 1.5 * (xU - xL), xU + 1.5 * (xU - xL))
  }

TopOutlierPosition <-
  function (data, identifier = SPLUSQuartileIdentifierParameters, lowerAndUpperThresholds = identifier(data)) {
    which(data >= lowerAndUpperThresholds[[2]])
  }

test_that("Anomaly detection by residuals using outlier identifier", {

  qrObj2 <-
    QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
    QRMonQuantileRegression( df = 12, probabilities = 0.5 )

  dfOutliers2 <-
    qrObj2 %>%
    QRMonFindAnomaliesByResiduals( threshold = NULL,
                                   outlierIdentifier = NULL,
                                   relativeErrorsQ = FALSE ) %>%
    QRMonTakeValue

  expect_s3_class( dfOutliers2, "data.frame")

  expect_true( nrow(dfOutliers2) < floor( 0.2 * nrow( dfTemperatureData ) ) )


  dfOutliers3 <-
    qrObj2 %>%
    QRMonFindAnomaliesByResiduals( threshold = NULL,
                                   outlierIdentifier = function(x) TopOutlierPosition( x, identifier = SPLUSQuartileIdentifierParameters ),
                                   relativeErrorsQ = FALSE ) %>%
    QRMonTakeValue

  expect_s3_class( dfOutliers3, "data.frame")

  expect_true( nrow(dfOutliers3) < floor( 0.2 * nrow( dfTemperatureData ) ) )

})

