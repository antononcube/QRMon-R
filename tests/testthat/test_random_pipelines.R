context("Random pipelines")
library(QRMon)

set.seed(5427)
numberOfPipelines <- 20

pipelineLevels <-
  list( level1 = c( "QRMonUnit()",
                    "QRMonUnit( dfDistributionData )",
                    "QRMonUnit( dfFinancialData )",
                    "QRMonUnit( dfTemperatureData )" ),

        level2 = c( "QRMonRescale",
                    "QRMonRescale(TRUE)",
                    "QRMonRescale(FALSE, TRUE)",
                    "QRMonEchoDataSummary",
                    "QRMonEchoDataSummary()" ),

        level3 = c( "QRMonQuantileRegression",
                    "QRMonQuantileRegression( df = 6, degree = 7, probabilities = 0.5)",
                    "QRMonQuantileRegression( df = 12, probabilities = c(0.1,0.9))",
                    "QRMonQuantileRegression( df = 12, probabilities = 1:5/6)",
                    "QRMonQuantileRegression( df = 12, probabilities = runif( runif(1, min = 1, max = 6), min = 0, max = 1) )"),

        level4 = c( "QRMonPredict()",
                    "QRMonPredict",
                    "QRMonErrors()",
                    "QRMonErrors",
                    "QRMonOutliers()",
                    "QRMonSimulate()",
                    "QRMonSimulate(100, method = 'CDF')",
                    "QRMonPickPathPoints(0.2)",
                    "QRMonConditionalCDF( 0 )" )
  )

randomPipelines <-
  purrr::map( 1:numberOfPipelines,
              function(x) {
                rp <- Reduce( function(a,x) { c( a, sample(x,1) )}, init = c(), x = pipelineLevels )
                parse( text = paste( rp, collapse = " %>% " ))
              })

qrMonRes <- purrr::map( randomPipelines, purrr::safely(eval))

checkRes <- purrr::map_lgl( qrMonRes, function(x) is.na(x$result) || is.list(x$result) && class(x$result) == "QRMon" )

test_that( "Random pipelines produce NA's or QRMon S3 objects.", {
  expect_true( sum(is.na(checkRes)) == 0 )
  expect_true( sum(is.na(checkRes)) == 0 && mean(checkRes) == 1 )
})
