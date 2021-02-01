context("Picking path points")
library(QRMon)

test_that("Picking path points", {

  qrObj1 <-
    QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
    QRMonQuantileRegression( df = 12, probabilities = 0.5 )

  lsAbsoluteErrors <- (qrObj1 %>% QRMonErrors( relativeErrorsQ = FALSE ) %>% QRMonTakeValue)[[1]]$Error
  lsRelativeErrors <- (qrObj1 %>% QRMonErrors( relativeErrorsQ = TRUE ) %>% QRMonTakeValue)[[1]]$Error

  expect_true( is.numeric(lsAbsoluteErrors))
  expect_true( is.numeric(lsRelativeErrors))

  dfPath1 <-
    qrObj1 %>%
    QRMonPickPathPoints( threshold = median(lsAbsoluteErrors) * 0.98, pickAboveThresholdQ = FALSE, relativeErrorsQ = FALSE ) %>%
    QRMonTakeValue

  dfPath2 <-
    qrObj1 %>%
    QRMonPickPathPoints( threshold = median(lsAbsoluteErrors) * 1.02, pickAboveThresholdQ = TRUE, relativeErrorsQ = FALSE ) %>%
    QRMonTakeValue

  dfPath3 <-
    qrObj1 %>%
    QRMonPickPathPoints( threshold = median(lsRelativeErrors) * 0.98, pickAboveThresholdQ = FALSE, relativeErrorsQ = TRUE ) %>%
    QRMonTakeValue

  dfPath4 <-
    qrObj1 %>%
    QRMonPickPathPoints( threshold = median(lsRelativeErrors) * 1.02, pickAboveThresholdQ = TRUE, relativeErrorsQ = TRUE ) %>%
    QRMonTakeValue


  expect_type( dfPath1, "list")

  expect_s3_class( dfPath1[[1]], "data.frame")

  expect_type( dfPath2, "list")

  expect_s3_class( dfPath2[[1]], "data.frame")

  expect_type( dfPath3, "list")

  expect_s3_class( dfPath3[[1]], "data.frame")

  expect_type( dfPath4, "list")

  expect_s3_class( dfPath4[[1]], "data.frame")

  expect_true( nrow(dfPath1[[1]]) + nrow(dfPath2[[1]]) <= nrow(dfTemperatureData) )

  expect_equal(
    length( intersect( dfPath1[[1]], dfPath2[[1]]) ),
    length(
      intersect(
        lsAbsoluteErrors[lsAbsoluteErrors <= median(lsAbsoluteErrors) * 0.98],
        lsAbsoluteErrors[lsAbsoluteErrors >= median(lsAbsoluteErrors) * 1.02]))
  )

  expect_true( nrow(dfPath3[[1]]) + nrow(dfPath4[[1]]) <= nrow(dfTemperatureData) )

  expect_equal(
    length( intersect( dfPath3[[1]], dfPath4[[1]]) ),
    length(
      intersect(
        lsRelativeErrors[lsRelativeErrors <= median(lsRelativeErrors) * 0.98],
        lsRelativeErrors[lsRelativeErrors >= median(lsRelativeErrors) * 1.02]))
  )
})

