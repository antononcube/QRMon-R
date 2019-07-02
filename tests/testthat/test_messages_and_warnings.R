context("Messages and warnings")
library(QRMon)

test_that("Lift to monad messages", {
  expect_warning( QRMonUnit( dfTemperatureData ), "The argument data.*" )
  expect_warning( QRMonUnit( dfFinancialData ), "Converting dates into seconds.*" )
})

test_that("Quantile regresssion warnings", {
  expect_warning( QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
                    QRMonPredict(),
                  "Calculate regression quantiles first.*" )
  expect_warning( QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
                    QRMonOutliers(),
                  "Calculate \\(top and bottom\\) regression quantiles first.*" )
  expect_warning( QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
                    QRMonSimulate(),
                  "Calculate regression quantiles first.*" )
})

test_that("Monad elements warnings", {
  expect_warning( QRMonUnit( setNames( dfTemperatureData, c("Time", "Value") ) ) %>%
                    QRMonOutliersPlot(),
                  "Cannot find Outliers.")
})
