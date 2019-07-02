context("Data simulation")
library(QRMon)

set.seed(345)

probsTest <- seq(0, 1, 0.2)

qsTest <- quantile( x = CDFRandomPoints( data.frame( Probability = 0:10/10, Quantile = 0:10/10 ), 1000 ), probs = probsTest )

test_that( "Simple CDF simulation of uniform data.", {

  expect_true(  mean( (qsTest - probsTest) < 0.01 ) == 1 )

})

test_that( "Simple CDF simulation with QR.", {

  ## This should also be fast.
  expect_is( dfSim <-
               QRMonUnit( setNames( dfFinancialData, c("Regressor", "Value") ) ) %>%
               QRMonQuantileRegression( df = 3, probabilities = probsTest ) %>%
               QRMonSimulate( n = 1000, method = "CDF" ) %>%
               QRMonTakeValue(),
             "data.frame"
  )

  expect_equal( names( dfSim ), c( "Regressor", "Value" ) )

  expect_is( svec <- quantile( x = dfSim$Value, probs = probsTest, na.rm = F), "numeric" )

  expect_is( tvec <- quantile( x = dfFinancialData$Value, probs = probsTest), "numeric" )

  expect_equal( mean( abs( (svec - tvec) / tvec ) < 0.2 ), 1 )

})


## More tests with QRMon objects have to be added...

