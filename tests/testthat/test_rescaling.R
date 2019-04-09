context("Data rescaling")
library(QRMon)

qrObj1 <-
  QRMonUnit( dfTemperatureData ) %>%
  QRMonRescale

qrObj2 <-
  QRMonUnit( dfTemperatureData ) %>%
  QRMonRescale(TRUE, TRUE)

testData <- dfFinancialData
testData[[1]] <- 12

qrObj3 <-
  QRMonUnit(  testData ) %>%
  QRMonRescale()

test_that( "Rescaling produces points in [0,1].", {
  expect_true( min(QRMonTakeData(qrObj1)$Regressor) == 0 && max(QRMonTakeData(qrObj1)$Regressor) == 1 )

  expect_true( min(QRMonTakeData(qrObj2)$Regressor) == 0 && max(QRMonTakeData(qrObj2)$Regressor) == 1 )
  expect_true( min(QRMonTakeData(qrObj2)$Value) == 0 && max(QRMonTakeData(qrObj2)$Value) == 1 )
})


test_that( "Rescaling of a constant column produces a column of 0's.", {
  expect_true( min(QRMonTakeData(qrObj3)$Regressor) == 0 && max(QRMonTakeData(qrObj3)$Regressor) == 0 )
})

