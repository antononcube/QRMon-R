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

  expect_warning(
    dfOutliers2 <-
      qrObj1 %>%
      QRMonFindAnomaliesByResiduals( threshold = 5, relativeErrorsQ = FALSE, probability = 0.8 ) %>%
      QRMonTakeValue,
    "Cannot find" )

  expect_s3_class( dfOutliers2, "data.frame")

  expect_true( nrow(dfOutliers2) < floor( 0.2 * nrow( dfTemperatureData ) ) )

  expect_equal( dfOutliers1, dfOutliers2 )

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

test_that("Anomalies detection by residuals using outlier identifier", {

  qrObj2 <-
    QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
    QRMonQuantileRegression( df = 12, probabilities = 0.5 )

  dfOutliers2 <-
    qrObj2 %>%
    QRMonFindAnomaliesByResiduals( threshold = NULL,
                                   outlierIdentifier = NULL,
                                   relativeErrorsQ = FALSE,
                                   probability = NULL ) %>%
    QRMonTakeValue

  expect_s3_class( dfOutliers2, "data.frame")

  expect_true( nrow(dfOutliers2) < floor( 0.2 * nrow( dfTemperatureData ) ) )


  dfOutliers3 <-
    qrObj2 %>%
    QRMonFindAnomaliesByResiduals( threshold = NULL,
                                   outlierIdentifier = function(x) TopOutlierPosition( x, identifier = SPLUSQuartileIdentifierParameters ),
                                   relativeErrorsQ = FALSE,
                                   probability = NULL ) %>%
    QRMonTakeValue

  expect_s3_class( dfOutliers3, "data.frame")

  expect_true( nrow(dfOutliers3) < floor( 0.2 * nrow( dfTemperatureData ) ) )


  expect_warning(
    dfOutliers4 <-
      qrObj2 %>%
      QRMonFindAnomaliesByResiduals( threshold = NULL,
                                     outlierIdentifier = function(x) TopOutlierPosition( x, identifier = SPLUSQuartileIdentifierParameters ),
                                     relativeErrorsQ = FALSE,
                                     probability = 0.4 ) %>%
      QRMonTakeValue,
    "Cannot find"
  )

  expect_s3_class( dfOutliers4, "data.frame")

  expect_true( nrow(dfOutliers4) < floor( 0.2 * nrow( dfTemperatureData ) ) )

  expect_equivalent( dfOutliers4, dfOutliers3 )

})


test_that("Variance anomalies detection", {

  qrObj3 <-
    QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
    QRMonQuantileRegression( df = 12, probabilities = c(0.25, 0.75) )

  dfVarOutliers1 <-
    qrObj3 %>%
    QRMonFindVarianceAnomalies( outlierIdentifier = NULL,
                                positionsQ = FALSE ) %>%
    QRMonTakeValue

  expect_s3_class( dfVarOutliers1, "data.frame")

  expect_true( nrow(dfVarOutliers1) < floor( 0.45 * nrow( dfTemperatureData ) ) )


  dfVarOutliers2 <-
    qrObj3 %>%
    QRMonFindVarianceAnomalies( outlierIdentifier = NULL,
                                positionsQ = TRUE ) %>%
    QRMonTakeValue

  expect_type( dfVarOutliers2, "integer")

  expect_true( length(dfVarOutliers2) < floor( 0.45 * nrow( dfTemperatureData ) ) )

})

