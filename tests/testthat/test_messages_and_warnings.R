context("Messages and warnings")
library(QRMon)

test_that("Lift to monad messages", {
  expect_warning( QRMonUnit( dfTemperatureData ),
                  "The argument data is expected to be a data frame with columns.*" )

  expect_warning( QRMonUnit( dfFinancialData ),
                  "Converting dates into seconds.*" )

  expect_warning( QRMonUnit( c(32, "aa") ),
                  "The columns 'Regressor' and 'Value' of the argument data are expected to be numeric." )

  expect_warning( QRMonUnit( data.frame( X = as.character(1:10), Y = "b" ) ),
                  "The columns 'Regressor' and 'Value' of the argument data are expected to be numeric." )

  expect_warning( QRMonUnit( data.frame( X = as.character(1:10), Y = "b" ) ),
                  "The argument data is expected to be a data frame with columns." )
})


test_that("Quantile regresssion warnings", {

  expect_warning( QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
                    QRMonQuantileRegression( probabilities = c("a", "b", "c") ),
                  "The argument probabilities is expected to be a numeric vector with elements between 0 and 1.*" )

  expect_warning( QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
                    QRMonQuantileRegression( probabilities = 1:10 ),
                  "The argument probabilities is expected to be a numeric vector with elements between 0 and 1.*" )

  expect_warning( QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
                    QRMonQuantileRegressionFit( Value ~ Regressor, probabilities = 1:10 ),
                  "The argument probabilities is expected to be a numeric vector with elements between 0 and 1.*" )

  expect_warning( QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
                    QRMonQuantileRegressionFit( 1 + 3, probabilities = 1:10/10 ),
                  "The argument formula is expected to be a formula object.*" )

  expect_warning( QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
                    QRMonPredict(),
                  "Calculate regression quantiles first.*" )

  expect_warning( QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
                    QRMonQuantileRegression( df = 12 ) %>%
                    QRMonPredict( c("a", "b") ),
                  "The argument newdata is expected to be.*" )

  expect_warning( QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
                    QRMonOutliers(),
                  "Calculate \\(top and bottom\\) regression quantiles first.*" )

})


test_that("Simulation warnings", {

  expect_warning( QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
                    QRMonSimulate(),
                  "Calculate regression quantiles first.*" )

  expect_warning( QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
                    QRMonQuantileRegression( df = 12, probabilities = 1:10/10 ) %>%
                    QRMonSimulate( "a" ),
                  "The argument n is expected to be a number.*" )

  expect_warning( QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
                    QRMonQuantileRegression( df = 12, probabilities = 1:10/10 ) %>%
                    QRMonSimulate( points = c("a", "b", "c") ),
                  "The argument points is expected to be a numeric vector or NULL.*" )

  expect_warning( QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
                    QRMonQuantileRegression( df = 12, probabilities = 1:10/10 ) %>%
                    QRMonSimulate( 12, method = "Blah" ),
                  "The argument method should be one of.*" )

})


test_that("Monad elements warnings", {

  expect_warning( QRMonUnit( setNames( dfTemperatureData, c("Time", "Value") ) ) %>%
                    QRMonOutliersPlot(),
                  "Cannot find Outliers.")

})

