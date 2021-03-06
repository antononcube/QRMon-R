context("Expected quantile probabilities")
library(QRMon)


## In some sense these test prove that the
## Quantile Regression implementation is correct
## together with the separation implementation.

qFracs0 <-
  QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
  QRMonQuantileRegression( df = 12, degree = 3, probabilities = seq(0.2,0.8,0.2) ) %>%
  QRMonSeparateToFractions() %>%
  QRMonTakeValue()

qFracs1 <-
  QRMonUnit( setNames( dfDistributionData, c("Regressor", "Value") ) ) %>%
  QRMonQuantileRegression( df = 6, degree = 3, probabilities = seq(0.1,0.9,0.2) ) %>%
  QRMonSeparateToFractions() %>%
  QRMonTakeValue()

qFracs2 <-
  QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
  QRMonQuantileRegression( df = 6, degree = 3, probabilities = seq(0.2,0.8,0.2) ) %>%
  QRMonSeparateToFractions(cumulativeQ = FALSE) %>%
  QRMonTakeValue()

qFracs3 <-
  QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
  QRMonQuantileRegressionFit( Value ~ Regressor, probabilities = seq(0.2,0.8,0.2) ) %>%
  QRMonSeparateToFractions(cumulativeQ = FALSE) %>%
  QRMonTakeValue()


test_that("Correct cumulative probabilities for temperature data", {
  expect_true( abs( qFracs0[["0.2"]] - 0.2) < 0.01 )
  expect_true( abs( qFracs0[["0.4"]] - 0.4) < 0.01 )
  expect_true( abs( qFracs0[["0.6"]] - 0.6) < 0.01 )
  expect_true( abs( qFracs0[["0.8"]] - 0.8) < 0.01 )
})

test_that("Correct cumulative probabilities for distribution data", {
  expect_true( abs( qFracs1[["0.1"]] - 0.1) < 0.01 )
  expect_true( abs( qFracs1[["0.3"]] - 0.3) < 0.01 )
  expect_true( abs( qFracs1[["0.5"]] - 0.5) < 0.01 )
  expect_true( abs( qFracs1[["0.7"]] - 0.7) < 0.01 )
  expect_true( abs( qFracs1[["0.9"]] - 0.9) < 0.01 )
})

test_that("Correct B-spline fit, non-cumulative probabilities for temperature data", {
  expect_true( abs( qFracs2[["0.2"]] - 0.2) < 0.05 )
  expect_true( abs( qFracs2[["0.4"]] - 0.2) < 0.05 )
  expect_true( abs( qFracs2[["0.6"]] - 0.2) < 0.05 )
  expect_true( abs( qFracs2[["0.8"]] - 0.2) < 0.05 )
})

test_that("Correct formula fit, non-cumulative probabilities for temperature data", {
  expect_true( abs( qFracs3[["0.2"]] - 0.2) < 0.05 )
  expect_true( abs( qFracs3[["0.4"]] - 0.2) < 0.05 )
  expect_true( abs( qFracs3[["0.6"]] - 0.2) < 0.05 )
  expect_true( abs( qFracs3[["0.8"]] - 0.2) < 0.05 )
})


## Separation of external data
## The list lsSep can be visualized with:
## ggplot( dplyr::bind_rows( lsSep, .id = "Prob" ) ) + geom_point( aes( x = Regressor, y = Value, color = Prob) )

test_that("Separate external data", {

  qrObj4 <-
    QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
    QRMonQuantileRegression( df = 6, degree = 3, probabilities = seq(0.2,0.8,0.2) )

  lsSep <- qrObj4 %>% QRMonSeparate( dfTemperatureData[ sample(1:nrow(dfTemperatureData), floor(0.7 * nrow(dfTemperatureData))), ], cumulativeQ = FALSE) %>% QRMonTakeValue

  expect_type( lsSep, "list" )

  expect_equal( names(lsSep), names(qrObj4 %>% QRMonTakeRegressionObjects) )

  qFracs4 <- qrObj4 %>% QRMonSeparateToFractions(cumulativeQ = FALSE) %>% QRMonTakeValue()

  expect_true( abs( qFracs4[["0.2"]] - 0.2) < 0.05 )
  expect_true( abs( qFracs4[["0.4"]] - 0.2) < 0.05 )
  expect_true( abs( qFracs4[["0.6"]] - 0.2) < 0.05 )
  expect_true( abs( qFracs4[["0.8"]] - 0.2) < 0.05 )

})

