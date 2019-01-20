# Quantile Regression workflows monad in R 

This repository is for the R implementation of **Q**uantile **R**egression **Mon**ad (QRMon).

The R-implementation follows the Mathematica QRMon package ["MonadicQuantileRegression.m"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicQuantileRegression.m).
The Mathematica QRMon package is extensively documented with 
["A monad for Quantile Regression workflows"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/A-monad-for-Quantile-Regression-workflows.md).

Here is an example:

    data("airquality")
    qrmon <-
      QRMonUnit( setNames( airquality[, c("Day", "Ozone")], c("Time", "Value") ) ) %>%
      QRMonQuantileRegression( df = 12, degree = 3, quantiles = seq(0.2,0.8,0.2) ) %>% 
      QRMonPlot()
