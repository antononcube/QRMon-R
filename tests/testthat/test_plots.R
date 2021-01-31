context("Plots")
library(QRMon)


qrObj <-
  QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
  QRMonQuantileRegression( df = 12, degree = 3, probabilities = 1:10/10 ) %>%
  QRMonOutliers


qrObj2 <-
  QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
  QRMonQuantileRegression( df = 12, degree = 3, probabilities = 0.2 ) %>%
  QRMonOutliers

qrObj3 <-
  QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
  QRMonQuantileRegression( df = 12, degree = 3, probabilities = 0.8 ) %>%
  QRMonOutliers

test_that("Plot functions", {

  expect_s3_class( qrObj %>% QRMonPlot( echoQ = FALSE ) %>% QRMonTakeValue, "ggplot")

  expect_s3_class( qrObj %>% QRMonPlot( echoQ = FALSE,
                                        dataLineColor = "blue",
                                        datePlotQ = TRUE) %>% QRMonTakeValue, "ggplot")

  expect_s3_class( qrObj %>% QRMonOutliersPlot( echoQ = FALSE,
                                                datePlotQ = TRUE,
                                                plotRegressionCurvesQ = TRUE) %>% QRMonTakeValue, "ggplot")

  expect_s3_class( qrObj %>% QRMonOutliersPlot( echoQ = FALSE,
                                                datePlotQ = FALSE,
                                                plotRegressionCurvesQ = FALSE) %>% QRMonTakeValue, "ggplot")

  expect_s3_class( qrObj2 %>% QRMonOutliersPlot( echoQ = FALSE,
                                                 datePlotQ = TRUE,
                                                 plotRegressionCurvesQ = TRUE) %>% QRMonTakeValue, "ggplot")

  expect_s3_class( qrObj2 %>% QRMonOutliersPlot( echoQ = FALSE,
                                                 datePlotQ = FALSE,
                                                 plotRegressionCurvesQ = FALSE) %>% QRMonTakeValue, "ggplot")

  expect_s3_class( qrObj3 %>% QRMonOutliersPlot( echoQ = FALSE,
                                                 datePlotQ = TRUE,
                                                 plotRegressionCurvesQ = TRUE) %>% QRMonTakeValue, "ggplot")

  expect_s3_class( qrObj3 %>% QRMonOutliersPlot( echoQ = FALSE,
                                                 datePlotQ = FALSE,
                                                 plotRegressionCurvesQ = FALSE) %>% QRMonTakeValue, "ggplot")

  expect_s3_class( qrObj %>% QRMonConditionalCDFPlot( echoQ = FALSE,
                                                      regressorValues = c(3690144000, 3690403200, 3690489600),
                                                      dateOrigin = "1900-01-01",
                                                      quantileGridLinesQ = TRUE) %>% QRMonTakeValue, "ggplot")

  expect_s3_class( qrObj %>% QRMonConditionalCDFPlot( echoQ = FALSE,
                                                      regressorValues = c(3690144000, 3690403200, 3690489600),
                                                      dateOrigin = NULL,
                                                      quantileGridLinesQ = FALSE) %>% QRMonTakeValue, "ggplot")

})
