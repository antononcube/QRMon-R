context("Data rescaling")
library(QRMon)

qrObj1 <-
  QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
  QRMonRescale

qrObj2 <-
  QRMonUnit( setNames( dfTemperatureData, c("Regressor", "Value") ) ) %>%
  QRMonRescale(TRUE, TRUE)

testData <- dfFinancialData
testData[[1]] <- 12

qrObj3 <-
  QRMonUnit( setNames( testData, c("Regressor", "Value") ) ) %>%
  QRMonRescale()

test_that( "Rescaling produces points in [0,1].", {
  expect_true( min(QRMonTakeData(qrObj1)$Regressor) == 0 && max(QRMonTakeData(qrObj1)$Regressor) == 1 )

  expect_true( min(QRMonTakeData(qrObj2)$Regressor) == 0 && max(QRMonTakeData(qrObj2)$Regressor) == 1 )
  expect_true( min(QRMonTakeData(qrObj2)$Value) == 0 && max(QRMonTakeData(qrObj2)$Value) == 1 )
})


test_that( "Rescaling of a constant column produces a column of 0's.", {
  expect_true( min(QRMonTakeData(qrObj3)$Regressor) == 0 && max(QRMonTakeData(qrObj3)$Regressor) == 0 )
})

