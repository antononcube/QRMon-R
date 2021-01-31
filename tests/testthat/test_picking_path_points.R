context("Picking path points")
library(QRMon)

test_that("Picking path points", {

  qrObj1 <-
    QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
    QRMonQuantileRegression( df = 12, probabilities = 0.5 )

  dfPath1 <-
    qrObj1 %>%
    QRMonPickPathPoints( threshold = 2.5, pickAboveThresholdQ = FALSE, relativeErrorsQ = FALSE ) %>%
    QRMonTakeValue

  dfPath2 <-
    qrObj1 %>%
    QRMonPickPathPoints( threshold = 2.5, pickAboveThresholdQ = TRUE, relativeErrorsQ = FALSE ) %>%
    QRMonTakeValue

  dfPath3 <-
    qrObj1 %>%
    QRMonPickPathPoints( threshold = 2.5, pickAboveThresholdQ = FALSE, relativeErrorsQ = TRUE ) %>%
    QRMonTakeValue

  dfPath4 <-
    qrObj1 %>%
    QRMonPickPathPoints( threshold = 2.5, pickAboveThresholdQ = TRUE, relativeErrorsQ = TRUE ) %>%
    QRMonTakeValue


  expect_type( dfPath1, "list")

  expect_s3_class( dfPath1[[1]], "data.frame")

  expect_type( dfPath2, "list")

  expect_s3_class( dfPath2[[1]], "data.frame")

  expect_type( dfPath3, "list")

  expect_s3_class( dfPath3[[1]], "data.frame")

  expect_type( dfPath4, "list")

  expect_s3_class( dfPath4[[1]], "data.frame")

  expect_true( nrow(dfPath1[[1]]) >= nrow(dfTemperatureData) - nrow(dfPath2[[1]]) )

  expect_true( nrow(dfPath1[[1]]) != nrow(dfPath3[[1]]) )

  expect_true( nrow(dfPath2[[1]]) != nrow(dfPath3[[1]]) )

  expect_true( nrow(dfPath3[[1]]) >= nrow(dfTemperatureData) - nrow(dfPath4[[1]]) )

  expect_true( nrow(dfPath3[[1]]) != nrow(dfPath4[[1]]) )

})

