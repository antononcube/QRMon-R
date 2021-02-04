context("PDF and CDF approximations")
library(QRMon)

## Mathematica code
#
# SeedRandom[324];
# lsProbs = Range[0, 1, 0.1];
# lsQVals = Quantile[RandomVariate[NormalDistribution[12, 2], 1000], lsProbs]
#
# (*  {5.70207, 9.49462, 10.3429, 11.1017, 11.5924, 12.0951, 12.6487, 13.2351, 13.7898, 14.9138, 17.5765} *)
#
# Integrate[x*PDF[NormalDistribution[12, 2], x], {x, -\[Infinity], \[Infinity]}]
#
# (* 12 *)

## Further verifications can be done with the plots:
#
# xs <- rnorm( n = 200, mean = 12, sd = 2)
# plot( data.frame( x=xs, y=sapply(xs, pdfRes)))
# plot( data.frame( x=xs, y=sapply(xs, cdfRes)))


test_that("Expected value approximation", {

  res <- ExpectedValueApproximation( probabilities = 0:10/10,
                                     quantiles = c(5.70207, 9.49462, 10.3429, 11.1017, 11.5924, 12.0951, 12.6487, 13.2351, 13.7898, 14.9138, 17.5765))

  expect_type( res, "double")

  expect_true( length(res) == 1 )

  expect_true( abs(res - 12) / 12  < 0.1 )

})


test_that("PDF approximation", {

  pdfRes <- PDFApproximation( probabilities = 0:10/10,
                              quantiles = c(5.70207, 9.49462, 10.3429, 11.1017, 11.5924, 12.0951, 12.6487, 13.2351, 13.7898, 14.9138, 17.5765))

  expect_true( "function" %in% class(pdfRes) )

  set.seed(445)
  vals <- sapply( rnorm( n = 20, mean = 12, sd = 2), pdfRes)

  expect_type( vals, "double" )

  expect_equal( mean(vals <= 1), 1 )

  expect_equal( mean(vals >= 0), 1 )

})


test_that("CDF approximation", {

  cdfRes <- CDFApproximation( probabilities = 0:10/10,
                              quantiles = c(5.70207, 9.49462, 10.3429, 11.1017, 11.5924, 12.0951, 12.6487, 13.2351, 13.7898, 14.9138, 17.5765))

  expect_true( "function" %in% class(cdfRes) )

  set.seed(343)
  vals <- sapply( rnorm( n = 20, mean = 12, sd = 2), cdfRes)

  expect_type( vals, "double" )

  expect_equal( mean(vals <= 1), 1 )

  expect_equal( mean(vals >= 0), 1 )

})
