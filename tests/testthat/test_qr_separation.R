context("Expected quantile fractions")
library(QRMon)


## In some sense these test prove that the
## Quantile Regression implementation is corect
## together with the separation implementation.

qFracs0 <-
  QRMonUnit( dfTemperatureData ) %>%
  QRMonQuantileRegression( df = 12, degree = 3, quantiles = seq(0.2,0.8,0.2) ) %>%
  QRMonSeparateToFractions() %>%
  QRMonTakeValue()

qFracs1 <-
  QRMonUnit( dfDistributionData ) %>%
  QRMonQuantileRegression( df = 6, degree = 3, quantiles = seq(0.1,0.9,0.2) ) %>%
  QRMonSeparateToFractions() %>%
  QRMonTakeValue()

test_that("Correct fractions for temperature data", {
  expect_true( abs( qFracs0[["0.2"]] - 0.2) < 0.01 )
  expect_true( abs( qFracs0[["0.4"]] - 0.4) < 0.01 )
  expect_true( abs( qFracs0[["0.6"]] - 0.6) < 0.01 )
  expect_true( abs( qFracs0[["0.8"]] - 0.8) < 0.01 )
})

test_that("Correct fractions for distribution data", {
  expect_true( abs( qFracs1[["0.1"]] - 0.1) < 0.01 )
  expect_true( abs( qFracs1[["0.3"]] - 0.3) < 0.01 )
  expect_true( abs( qFracs1[["0.5"]] - 0.5) < 0.01 )
  expect_true( abs( qFracs1[["0.7"]] - 0.7) < 0.01 )
  expect_true( abs( qFracs1[["0.9"]] - 0.9) < 0.01 )
})
