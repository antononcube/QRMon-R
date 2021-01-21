# Quantile Regression Monad in R 

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

This repository is for the R implementation of a software monad for Quantile Regression
workflows called **Q**uantile **R**egression **Mon**ad (QRMon).

The R-implementation follows the Mathematica `QRMon` package 
["MonadicQuantileRegression.m"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicQuantileRegression.m),
[AAp1].
The Mathematica `QRMon` package is extensively documented with 
["A monad for Quantile Regression workflows"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/A-monad-for-Quantile-Regression-workflows.md),
[AA1].

The usage of this R implementation is explained in detail in the vignette 
["Rapid making of Quantile Regression workflows"](https://htmlpreview.github.io/?https://github.com/antononcube/QRMon-R/blob/master/notebooks/rapid-making-of-qr-workflows.html).

Here is how to install the package:

    devtools::install_github("antononcube/QRMon-R")

Here is a workflow (pipeline) example:

    qrmon <-
      QRMonUnit( dfTemperatureData ) %>%
      QRMonEchoDataSummary() %>%
      QRMonQuantileRegression( df = 16, degree = 3, probabilities = seq(0.1,0.9,0.2) ) %>%
      QRMonPlot( datePlotQ = TRUE, dateOrigin = "1900-01-01" )
      
There is a Domain Specific Language (DSL) parser-interpreter implemented in [Raku](https://raku.org) 
that can be used to generate `QRMon` code using natural language commands; see
[AAr1].

## References

### Articles, books

[RK1] Roger Koenker, 
[Quantile Regression](https://books.google.com/books/about/Quantile_Regression.html?id=hdkt7V4NXsgC), 
Cambridge University Press, 2005.

[RK2] Roger Koenker,
["Quantile Regression in R: a vignette"](https://cran.r-project.org/web/packages/quantreg/vignettes/rq.pdf),
(2006),
[CRAN](https://cran.r-project.org/).

[AA1] Anton Antonov,
["A monad for Quantile Regression workflows"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/A-monad-for-Quantile-Regression-workflows.md),
(2018),
[MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).

### Packages

[RKp1] Roger Koenker,
[`quantreg`](https://cran.r-project.org/web/packages/quantreg/index.html),
[CRAN](https://cran.r-project.org/).

[AAp1] Anton Antonov,
[Quantile Regression Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/QuantileRegression.m),
(2014),
[MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).

[AAp2] Anton Antonov,
[Monadic Quantile Regression Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicQuantileRegression.m),
(2018),
[MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).

[AAp3] Anton Antonov,
[`QuantileRegression`](https://resources.wolframcloud.com/FunctionRepository/resources/QuantileRegression),
(2019),
[Wolfram Function Repository](https://resources.wolframcloud.com/FunctionRepository/resources/QuantileRegression).

### Repositories

[AAr1] Anton Antonov,
[DSL::English::QuantileRegressionWorkflows in Raku](https://github.com/antononcube/Raku-DSL-English-QuantileRegressionWorkflows),
(2020),
[GitHub/antononcube](https://github.com/antononcube/Raku-DSL-English-QuantileRegressionWorkflows)