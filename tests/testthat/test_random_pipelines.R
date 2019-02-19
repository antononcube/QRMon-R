context("Random pipelines")
library(QRMon)

set.seed(5427)
numberOfPipelines <- 20

pipelineLevels <-
  list( level1 = c( "QRMonUnit()",
                    "QRMonUnit( dfDistributionData )",
                    "QRMonUnit( dfFinancialData )",
                    "QRMonUnit( dfTemperatureData )" ),
        level2 = c( "QRMonQuantileRegression( df = 6, degree = 7, quantiles = 0.5)",
                    "QRMonQuantileRegression( df = 12, quantiles = c(0.1,0.9))",
                    "QRMonQuantileRegression( df = 12, quantiles = 1:5/6)"),
        level3 = c( "QRMonPredict()",
                    "QRMonErrors()",
                    "QRMonOutliers()",
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
  expect_true( sum(is.na(checkRes)) == 0 && mean(checkRes) == 1 )
})
