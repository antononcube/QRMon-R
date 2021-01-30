context("Missing values deletion")
library(QRMon)

## Put NAs at random locations
set.seed(998)
dfTemperatureData2 <- dfTemperatureData
dfTemperatureData2[[1]][ sample(1:nrow(dfTemperatureData),15) ] <- NA
dfTemperatureData2[[2]][ sample(1:nrow(dfTemperatureData),20) ] <- NA

## Rename columns
dfTemperatureData2 <- setNames( dfTemperatureData2, c("Regressor", "Value") )

## Different onboarding methods
qrObj1 <-
  QRMonUnit( dfTemperatureData2 ) %>%
  QRMonDeleteMissing()

dfOnboarded <- qrObj1 %>% QRMonTakeData

qrObj2 <-
  QRMonUnit( dfTemperatureData2 )

qrObj3 <-
  QRMonUnit( ) %>%
  QRMonSetData(dfTemperatureData2)


test_that( "No missing values after data onboarding.", {

  expect_true( mean(complete.cases(dfTemperatureData2)) < 1 )

  expect_equal( dfOnboarded[ complete.cases(dfOnboarded), ], dfOnboarded )

})


test_that( "Equivalence of onboarding methods over data with missing values.", {

  expect_equal( qrObj1 %>% QRMonTakeData, qrObj2 %>% QRMonTakeData  )

  expect_equal( qrObj1 %>% QRMonTakeData, qrObj3 %>% QRMonTakeData  )

})
