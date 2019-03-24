# Quantile Regression workflows monad in R 

This repository is for the R implementation of a software monad for Quantile Regression
workflows called **Q**uantile **R**egression **Mon**ad (QRMon).

The R-implementation follows the Mathematica QRMon package ["MonadicQuantileRegression.m"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicQuantileRegression.m).
The Mathematica QRMon package is extensively documented with 
["A monad for Quantile Regression workflows"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/A-monad-for-Quantile-Regression-workflows.md).

Here is how to install the package:

    devtools::install_github("antononcube/QRMon-R")

Here is an example:

    qrmon <-
      QRMonUnit( dfTemperatureData ) %>%
      QRMonEchoDataSummary() %>%
      QRMonQuantileRegression( df = 16, degree = 3, fractions = seq(0.1,0.9,0.2) ) %>%
      QRMonPlot( datePlotQ = TRUE, dateOrigin = "1900-01-01" )
      
For detailed explanations see the vignette 
["Rapid making of Quantile Regression workflows"](https://htmlpreview.github.io/?https://github.com/antononcube/QRMon-R/blob/master/notebooks/rapid-making-of-qr-workflows.html).
