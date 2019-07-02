##===========================================================
## Quantile Regression workflows monad in R
## Copyright (C) 2019  Anton Antonov
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
## Written by Anton Antonov,
## antononcube @@@ gmail ... com,
## Windermere, Florida, USA.
##===========================================================


#' @import purrr
#' @import magrittr
#' @import splines
#' @import quantreg
#' @import ggplot2
NULL

##===========================================================
## QRMon (Quantile Regression Monad) failure symbol
##===========================================================

#' Failure symbol for QRMon.
#' @description Failure symbol for the monad QRMon.
#' @export
QRMonFailureSymbol <- NA

#' Failure test for an QRMon object.
#' @description Test is an QRMon object a failure symbol.
#' @export
QRMonFailureQ <- function(x) { mean(is.na(x)) }


##===========================================================
## QRMon Unit
##===========================================================

#' Make a QRMon unit (object.)
#' @description Creates a monad object.
#' @param data A vector or a two-column matrix or data frame.
#' @return An S3 class "QRMon". In other words, a list with the attribute "class" set to "QRMon".
#' @export
QRMonUnit <- function( data = NULL ) {

  res <- list( Value = NULL, Data = NULL, RegressionObjects = list(), Outliers = NULL )
  attr(res, "class") <- "QRMon"

  if( !is.null(data) ) {
    res <- QRMonSetData( res, data )
    if( QRMonFailureQ(res) ) { return(QRMonFailureSymbol) }
  }

  res
}

#' Make a QRMon unit.
#' @description A synonym of \code{QRMonUnit}.
#' @export
QRMonObject <- QRMonUnit


##===========================================================
## Setters and getters
##===========================================================

#' Set the value in a QRMon object.
#' @description Sets the value in a QRMon monad object.
#' @param qrObj An QRMon object.
#' @param value The new value.
#' @return A QRMon object.
#' @details Assigns \code{value} to \code{qrObj$Value}.
#' @family Set/Take functions
#' @export
QRMonSetValue <- function( qrObj, value ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  qrObj$Value <- value
  qrObj
}

#' Take the value in a QRMon object.
#' @description Takes the value from QRMon monad object.
#' @param qrObj An QRMon object.
#' @return Just \code{qrObj$Value}.
#' @family Set/Take functions
#' @export
QRMonTakeValue <- function( qrObj ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  qrObj$Value
}

##-----------------------------------------------------------

#' Set event records.
#' @description Sets an event records data frame into the monad object.
#' @param qrObj An QRMon object.
#' @param data A data frame with event records.
#' @return An QRMon object.
#' @family Set/Take functions
#' @export
QRMonSetData <- function( qrObj, data ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  if ( is.vector(data) ) {

    QRMonSetData( qrObj, data.frame( Regressor = 1:length(data), Value = data ) )

  } else if ( ( is.matrix(data) || is.data.frame(data) ) && ncol(data) == 1 ) {

    QRMonSetData( qrObj, data.frame( Regressor = 1:nrow(data), Value = data[,1] ) )

  } else if ( is.matrix(data) || is.data.frame(data) ) {

    expectedColNames <- c("Regressor", "Value")

    if( ! ( is.data.frame(data) && length(intersect( colnames(data), expectedColNames)) == length(expectedColNames) ) ) {
      warning( paste( "The argument data is expected to be a data frame with columns: {", paste(expectedColNames, collapse =", "), "}."), call. = TRUE)
      warning( paste0( "Proceeding by renaming the first columm \"", colnames(data)[[1]], "\" as \"Regressor\" ",
                       "and renaming the second columm \"", colnames(data)[[2]], "\" as \"Value\"." ), call. = TRUE )
      data <- data[,1:2]
      colnames(data) <- c( "Regressor", "Value" )
    }

    if( sum( class(data$Regressor) %in% c( "Date", "POSSIXt" ) ) > 0 ) {
      warning( "Converting dates into seconds.", call. = TRUE)
      data$Regressor <- as.numeric(data$Regressor, "second")
    }

    if( !is.numeric(data$Regressor) || !is.numeric(data$Value) ) {
      warning( "The columns 'Regressor' and 'Value' of the argument data are expected to be numeric.", call. = TRUE)
      return(QRMonFailureSymbol)
    }

    qrObj$Data <- data[, expectedColNames]

    qrObj$Data <- qrObj$Data[ complete.cases(qrObj$Data), ]

    qrObj
  }
}

#' Take event records.
#' @description Takes the event records data frame from the monad object.
#' @param qrObj An QRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A data frame or \code{QRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
QRMonTakeData <- function( qrObj, functionName = "QMonTakeData" ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  if( !QRMonDataCheck( qrObj, functionName = functionName,  logicalResult = TRUE) ) {
    return(QRMonFailureSymbol)
  }

  qrObj$Data
}


##-----------------------------------------------------------

#' Set regression functions.
#' @description Sets a list of regression functions into the monad object.
#' @param qrObj An QRMon object.
#' @param regressionObjects A list of regression objects.
#' @return An QRMon object.
#' @family Set/Take functions
#' @export
QRMonSetRegressionObjects <- function( qrObj, regressionObjects ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  if( !( is.null(regressionObjects) || is.list(regressionObjects) ) ) {
    warning("The argument regressionObjects is expected to be NULL or a list of regression functions.", call. = TRUE)
    return(QRMonFailureSymbol)
  }

  qrObj$RegressionObjects <- regressionObjects

  qrObj
}

#' Take regression objects
#' @description Takes the regression objects from the monad object.
#' @param qrObj An QRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{QRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
QRMonTakeRegressionObjects <- function( qrObj, functionName = "QRMonTakeRegressionObjects" ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  if( !QRMonMemberPresenceCheck( qrObj, memberName = "RegressionObjects", functionName = functionName,logicalResult = TRUE) ) {
    return(QRMonFailureSymbol)
  }
  qrObj$RegressionObjects
}


##-----------------------------------------------------------

#' Take outliers.
#' @description Takes the outliers from the monad object.
#' @param qrObj An QRMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{QRMonFailureSymbol}.
#' @family Set/Take functions
#' @export
QRMonTakeOutliers <- function( qrObj, functionName = "QRMonTakeOutliers" ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  if( !QRMonMemberPresenceCheck( qrObj, memberName = "Outliers", functionName = functionName,  logicalResult = TRUE) ) {
    return(QRMonFailureSymbol)
  }
  qrObj$Outliers
}


##===========================================================
## Data presence check
##===========================================================

#' Check presence of required data.
#' @description Checks does an QRMon object have event records, entity attributes, and computation specification.
#' @param qrObj An QRMon object.
#' @param functionName A name of the delegating function (if any).
#' @param logicalResult Should the result be logical value?
#' @return If \code{logicalValue} is FALSE the result is QRMon object; if TRUE the result is logical value.
#' @export
QRMonDataCheck <- function( qrObj, functionName = NULL, logicalResult = FALSE ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  res <- TRUE

  if( is.null(functionName) || nchar(functionName) == 0 ) {
    functionName <- ""
  } else {
    functionName <- paste0( functionName, ":: ")
  }

  if( is.null(qrObj$Data) ) {
    warning( paste0( functionName, "Cannot find data"), call. = TRUE)
    res <- FALSE
  }

  if( logicalResult ) { res }
  else if ( !logicalResult && !res) { QRMonFailureSymbol }
  else { qrObj }
}


##===========================================================
## Member presence check
##===========================================================

#' General member presence check.
#' @description A general function for checking the presence of a data member in an QRMon object.
#' @param qrObj An QRMon object.
#' @param memberName The name of the member to be checked.
#' @param memberPrettyName A pretty member name (for messages).
#' @param functionName The name of the delegating function.
#' @param logicalResult Should the result be logical value?
#' @return A logical value or an QRMon object.
#' @export
QRMonMemberPresenceCheck <- function( qrObj, memberName, memberPrettyName = memberName, functionName = "", logicalResult = FALSE ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  res <- TRUE

  if( nchar(functionName) > 0 ) { functionName <- paste0( functionName, ":: ") }

  if( is.null(qrObj[[memberName]]) ) {
    warning( paste0( functionName, paste0("Cannot find ", memberPrettyName, ".") ), call. = TRUE )
    res <- FALSE
  }

  if( logicalResult ) { res }
  else if ( !logicalResult && !res) { QRMonFailureSymbol }
  else { qrObj }
}


##===========================================================
## Echo function appliction of over monad's value
##===========================================================

#' Function application to monad's value.
#' @description Apply a function to the "Value" element/member of the monad object.
#' @param qrObj An QRMon object.
#' @param f A function to be applied to \code{qrObj$Value}.
#' @return A QRMon object.
#' @details Prints \code{f(qrObj$Value)}.
#' @export
QRMonEchoFunctionValue <- function( qrObj, f ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  print( f(qrObj$Value) )

  qrObj
}


##===========================================================
## Data summary
##===========================================================

#' Data summary.
#' @description Summarize data and print the summary.
#' @param qrObj An QRMon object.
#' @return A QRMon object.
#' @details Prints data dimensions and summary.
#' @export
QRMonEchoDataSummary <- function( qrObj ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  data <- QRMonTakeData( qrObj = qrObj, functionName = "QRMonEchoDataSummary" )
  if( QRMonFailureQ(data) ) { return(QRMonFailureSymbol) }

  print( list( Dimensions = dim(data), Summary = summary(data) ) )

  qrObj
}

##===========================================================
## Rescale
##===========================================================

#' Rescale data.
#' @description Rescale the data along axes specification.
#' @param qrObj An QRMon object.
#' @param regressorAxisQ Should the data be rescaled along the regressor axis?
#' @param valueAxisQ Should the data be rescaled along the value axis?
#' @details The rescaled data replaces \code{qrObj$Data}.
#' @return A QRMon object.
#' @export
QRMonRescale <- function( qrObj, regressorAxisQ = TRUE, valueAxisQ = FALSE ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  data <- QRMonTakeData( qrObj = qrObj, functionName = "QRMonRescale" )
  if( QRMonFailureQ(data) ) { return(QRMonFailureSymbol) }

  if ( regressorAxisQ ) {
    if( max(data$Regressor) > min(data$Regressor) ) {
      data$Regressor <- ( data$Regressor - min(data$Regressor) ) / ( max(data$Regressor) - min(data$Regressor) )
    } else {
      data$Regressor <- 0
    }
  }

  if ( valueAxisQ ) {
    if( max(data$Value) > min(data$Value) ) {
      data$Value <- ( data$Value - min(data$Value) ) / ( max(data$Value) - min(data$Value) )
    } else {
      data$Value <- 0
    }
  }

  qrObj$Data <- data
  qrObj
}


##===========================================================
## Quantile regression
##===========================================================

#' Quantile regression B-splines basis fit.
#' @description Finds the quantile regression objects for the specified
#' quantile probabilities using a B-spline basis.
#' @param qrObj An QRMon object.
#' @param df Degrees of freedom;
#' same as \code{df} of \code{\link{splines::bs}}.
#' @param knots The internal breakpoints that define the spline;
#' same as \code{knots} of \code{\link{splines::bs}}.
#' @param degree  Degree of the piecewise polynomial;
#' same as \code{degree} of \code{\link{splines::bs}}.
#' @param probabilities A numeric vector with quantile probabilities.
#' @param ... Additional arguments for \code{\link{splines::bs}}.
#' @return A QRMon object.
#' @details The obtained regression objects are assigned/appended to the
#' \code{qrObj$RegressionObjects}.
#' For more computational details see \code{\link{quantreg::rq}}.
#' This function can be seen as a splines-basis shortcut of
#' \code{\link{QRMonQuantileRegressionFit}}.
#' @family Regression functions
#' @export
QRMonQuantileRegression <- function( qrObj, df, knots = NULL, degree = 3, probabilities = c(0.25, 0.5, 0.75), ... ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  if( !( is.numeric(probabilities) && mean( probabilities >= 0 ) == 1 && mean( probabilities <= 1) == 1 ) ){
    warning("The argument probabilities is expected to be a numeric vector with elements between 0 and 1.", call. = TRUE )
    return(QRMonFailureSymbol)
  }

  data <- QRMonTakeData( qrObj = qrObj, functionName = "QRMonQuantileRegression" )
  if( QRMonFailureQ(data) ) { return(QRMonFailureSymbol) }

  rqFits <-
    purrr::map(
      probabilities,
      function(tau) { quantreg::rq(Value ~ splines::bs(Regressor, df = df, knots = knots, degree = degree, ...), tau = tau, data = data ) })
  names(rqFits) <- probabilities

  qrObj <- qrObj %>% QRMonSetRegressionObjects( c( qrObj %>% QRMonTakeRegressionObjects(), rqFits ) )

  qrObj
}


##===========================================================
## Quantile regression fit
##===========================================================

#' Quantile regression function basis fit.
#' @description Finds the quantile regression objects for the specified
#' quantile probabilities using a specified formula.
#' @param qrObj An QRMon object.
#' @param formula A formula.
#' @param probabilities A numeric vector with quantile probabilities.
#' @param ... Arguments for  \code{\link{quantreg::rq}}.
#' @return A QRMon object.
#' @details
#' The formula has to use "Value" and "Regressor".
#' For example: \code{Value ~ sin(1+3*Regressor)}.
#' The obtained regression objects are assigned/appended to
#' \code{qrObj$RegressionObjects}.
#' For more computational details see \code{\link{quantreg::rq}}.
#' @family Regression functions
#' @export
QRMonQuantileRegressionFit <- function( qrObj, formula, probabilities = c(0.25, 0.5, 0.75), ... ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  data <- QRMonTakeData( qrObj = qrObj, functionName = "QRMonQuantileRegressionFit" )
  if( QRMonFailureQ(data) ) { return(QRMonFailureSymbol) }

  if( !is_formula(formula) ) {
    warning( "The argument formula is expected to be a formula object.", call. = TRUE)
    return(QRMonFailureSymbol)
  }

  rqFits <-
    purrr::map(
      probabilities,
      function(tau) {
        quantreg::rq( formula = formula, data = data, tau = tau, ... )
      })
  names(rqFits) <- probabilities

  qrObj <- qrObj %>% QRMonSetRegressionObjects( c( qrObj %>% QRMonTakeRegressionObjects(), rqFits ) )

  qrObj
}


##===========================================================
## Predict (evaluate) with regression objects
##===========================================================

#' Prediction with regression objects.
#' @description Predict values with the monad object regression objects
#' over specified new data.
#' @param qrObj An QRMon object.
#' @param newdata A numeric vector, a data frame with a column 'Regressor', or NULL.
#' @param ... parameters for \code{\link{quantreg::predict.rq}}.
#' @return A QRMon object.
#' @details The result of the evaluation of the regression objects
#' over the new data is a list of data frame. List's names are the
#' quantile probabilities that correspond to the regression objects.
#' The list is assigned to \code{qrObj$Value}.
#' @family Regression functions
#' @export
QRMonPredict <- function( qrObj, newdata = NULL, ... ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  regObjs <- QRMonTakeRegressionObjects( qrObj = qrObj, functionName = "QRMonPredict" )
  if( length(regObjs) == 0 || QRMonFailureQ(regObjs) ) {
    warning( "Calculate regression quantiles first.", call. = TRUE )
    return(QRMonFailureSymbol)
  }

  if( is.vector(newdata) ) {
    newdata <- data.frame( Regressor = newdata )
  }

  if( ! ( is.null(newdata) || is.data.frame(newdata) && sum( "Regressor" %in% colnames(newdata) ) == 1 ) ) {
    warning( "A numeric vector, data frame with a column 'Regressor', or NULL is expected for the argument newdata.", call. = TRUE )
    return(QRMonFailureSymbol)
  }

  if( is.null(newdata) ) {
    newdata <- QRMonTakeData(qrObj = qrObj)
  }

  if( is.data.frame(newdata) ) {
    newdata <- newdata[, "Regressor", drop = F]
  }

  qrRes <-
    purrr::map(
      names(regObjs),
      function(x) {
        res <- predict( regObjs[[x]], newdata = newdata, ... )
        data.frame( Regressor = newdata, Value = res )
      })
  names(qrRes) <- names(regObjs)

  qrObj$Value <- qrRes

  qrObj
}


##===========================================================
## Plot regression functions
##===========================================================

#' Plot with data and regression curves.
#' @description Plot the monad object data and regression functions (if any.)
#' @param qrObj An QRMon object.
#' @param dataPointsColor The color of the data points.
#' If NULL the data points are not plotted.
#' @param regressionCurvesColor The color of the regression curves.
#' If NULL the regression curves are not plotted.
#' @param datePlotQ Should the regressor axis have dates scale?
#' @param dateOrigin Same as the argument \code{origin} of \code{as.POSIXct}.
#' @param echoQ To echo the plot the or not?
#' @return A QRMon object.
#' @details The plot is made with \link{ggplot2}.
#' The plot is assigned to \code{qrObj$Value}.
#' @family Plot functions
#' @export
QRMonPlot <- function( qrObj,
                       dataPointsColor = 'gray60', regressionCurvesColor = ~ RegressionCurve,
                       datePlotQ = FALSE, dateOrigin = "1970-01-01",
                       echoQ = TRUE ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  if(  !is.null( qrObj$RegressionsFunctions ) ) {

    qrObj <- qrObj %>% QRMonPredict( newdata = NULL )
    if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

    qrDF <- qrObj %>% QRMonTakeValue()

    qrDF <- purrr::map_df(names(qrDF), function(x) cbind( RegressionCurve = x, qrDF[[x]], stringsAsFactors = FALSE ) )

  } else {
    qrDF <- NULL
  }

  resPlot <- ggplot2::ggplot()

  data <- qrObj %>% QRMonTakeData()

  if( datePlotQ ) {
    data$Regressor <- as.POSIXct( data$Regressor, origin = dateOrigin )
    if( !is.null(qrDF) ) {
      qrDF$Regressor <- as.POSIXct( qrDF$Regressor, origin = dateOrigin )
    }
  }

  if( !is.null(dataPointsColor) ) {

    resPlot <-
      resPlot +
      ggplot2::geom_point( data = data,
                           mapping = ggplot2::aes( x = Regressor, y = Value ), color = dataPointsColor )
  }

  if( !is.null(regressionCurvesColor) && !is.null(qrDF) ) {

    resPlot <-
      resPlot +
      if( is.character(regressionCurvesColor) ) {
        ggplot2::geom_line( data = qrDF, ggplot2::aes_( x = ~Regressor, y = ~Value, group = ~RegressionCurve ), color = regressionCurvesColor )
      } else {
        ggplot2::geom_line( data = qrDF, ggplot2::aes_( x = ~Regressor, y = ~Value, color = regressionCurvesColor ) )
      }
  }

  if( echoQ ) { print(resPlot) }

  qrObj$Value <- resPlot

  qrObj
}


##===========================================================
## Outlier finding
##===========================================================

#' Outliers finding.
#' @description Find the monad object data outliers using already found regression objects.
#' @param qrObj An QRMon object.
#' @return A QRMon object.
#' @details The outliers are assigned to \code{qrObj$Value} and \code{qrObj$Outliers}.
#' @family Outlier functions
#' @export
QRMonOutliers <- function( qrObj ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  regObjs <- QRMonTakeRegressionObjects( qrObj = qrObj, functionName = "QRMonOutliers" )
  if( length(regObjs) == 0 || QRMonFailureQ(regObjs) ) {
    warning( "Calculate (top and bottom) regression quantiles first.", call. = TRUE )
    return(QRMonFailureSymbol)
  }

  data <- QRMonTakeData( qrObj = qrObj, functionName = "QRMonOutliers" )
  if( QRMonFailureQ(data) ) { return(QRMonFailureSymbol) }

  outliers <-
    if( length(regObjs) == 1 && as.numeric(names(regObjs)[[1]]) < 0.5 ) {

      predVals <- predict( regObjs[[1]], newdata = data[ , "Regressor", drop=F ] )
      bottomOutliers <- data[ data$Value <= predVals, ]
      list( "bottomOutliers" = bottomOutliers )

    } else if ( length(regObjs) == 1 && as.numeric(names(regObjs)[[1]]) > 0.5  ) {

      predVals <- predict( regObjs[[1]], newdata = data[ , "Regressor", drop=F ] )
      topOutliers <- data[ data$Value >= predVals, ]
      list( "topOutliers" = topOutliers )

    } else {

      qs <- as.numeric( names(regObjs) )
      minInd <- which.min( qs )
      maxInd <- which.max( qs )

      predVals <- predict( regObjs[[minInd]], newdata = data[ , "Regressor", drop=F ] )
      bottomOutliers <- data[ data$Value <= predVals, ]

      predVals <- predict( regObjs[[maxInd]], newdata = data[ , "Regressor", drop=F ] )
      topOutliers <- data[ data$Value >= predVals, ]

      list( "bottomOutliers" = bottomOutliers, "topOutliers" = topOutliers )

    }

  qrObj$Value <- outliers
  qrObj$Outliers <- outliers

  qrObj
}


##===========================================================
## Outliers plot
##===========================================================

#' Outliers plot.
#' @description Plot the monad object data and found outliers.
#' @param qrObj An QRMon object.
#' @param plotRegressionCurvesQ Should the regression curves be plotted or not?
#' @param datePlotQ Should the regressor axis have dates scale?
#' @param dateOrigin Same as the argument \code{origin} of \code{as.POSIXct}.
#' @param echoQ To echo the plot the or not?
#' @return A QRMon object.
#' @details The outliers are assigned to \code{qrObj$Value} and \code{qrObj$Outliers}.
#' @family Outlier functions
#' @export
QRMonOutliersPlot <- function( qrObj, plotRegressionCurvesQ = TRUE,
                               datePlotQ = FALSE, dateOrigin = "1970-01-01",
                               echoQ = TRUE ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  data <- QRMonTakeData( qrObj = qrObj, functionName = "QRMonOutliersPlot" )
  if( QRMonFailureQ(data) ) { return(QRMonFailureSymbol) }

  outliers <- QRMonTakeOutliers( qrObj = qrObj, functionName = "QRMonOutliersPlot" )
  if( QRMonFailureQ(outliers) ) { return(QRMonFailureSymbol) }

  #plotDataDF <- cbind( "Type" = "data", data, stringsAsFactors = FALSE )
  plotDataDF <- NULL

  if( !is.null(outliers[["bottomOutliers"]]) ) {
   plotDataDF <- rbind( plotDataDF, cbind( "Type" = "bottomOutliers", outliers[["bottomOutliers"]], stringsAsFactors = FALSE ) )
  }

  if( !is.null(outliers[["topOutliers"]]) ) {
    plotDataDF <- rbind( plotDataDF, cbind( "Type" = "topOutliers", outliers[["topOutliers"]], stringsAsFactors = FALSE ) )
  }

  if( plotRegressionCurvesQ ) {
    resPlot <-
      qrObj %>%
      QRMonPlot( regressionCurvesColor = 'gray60', datePlotQ = datePlotQ, dateOrigin = dateOrigin, echoQ = FALSE ) %>%
      QRMonTakeValue()
  } else {
    resPlot <-
      qrObj %>%
      QRMonPlot( regressionCurvesColor = NULL, datePlotQ = datePlotQ, dateOrigin = dateOrigin, echoQ = FALSE ) %>%
      QRMonTakeValue()
  }

  if( datePlotQ ) {
    plotDataDF$Regressor <- as.POSIXct( plotDataDF$Regressor, origin = dateOrigin )
  }

  resPlot <-
    resPlot +
    geom_point( data = plotDataDF, mapping = aes( x = Regressor, y = Value, color = Type ) )

  if( echoQ ) { print(resPlot) }

  qrObj$Value <- resPlot

  qrObj
}


##===========================================================
## Errors
##===========================================================

#' Fit errors.
#' @description Find the fit errors of the monad object data and found regression objects.
#' @param qrObj An QRMon object.
#' @param relativeErrorsQ Should relative errors be computed?
#' @return A QRMon object.
#' @details The errors are assigned to \code{qrObj$Value}.
#' @family Errors
#' @export
QRMonErrors <- function( qrObj, relativeErrorsQ = TRUE ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  regObjs <- QRMonTakeRegressionObjects( qrObj = qrObj, functionName = "QRMonErrors" )
  if( length(regObjs) == 0 || QRMonFailureQ(regObjs) ) { return(QRMonFailureSymbol) }

  data <- QRMonTakeData( qrObj = qrObj, functionName = "QRMonErrors" )
  if( QRMonFailureQ(data) ) { return(QRMonFailureSymbol) }

  res <-
    purrr::map( names(regObjs),
                   function(x) {
                     errs <- predict( regObjs[[x]], newdata = data[, "Regressor", drop = F] ) - data[, "Value" ]

                     if( relativeErrorsQ ) {
                       errs <- errs / ifelse( data[, "Value" ] == 0, 1, data[, "Value" ] )
                     }

                     data.frame( Regressor = data[, "Regressor"], Error = errs )
                   })
  names(res) <- names(regObjs)

  qrObj$Value <- res

  qrObj
}


##===========================================================
## Errors plot
##===========================================================

#' Fit errors plot.
#' @description Find and plot the fit errors of the monad object data and found regression objects.
#' @param qrObj An QRMon object.
#' @param relativeErrorsQ Should relative errors be computed?
#' @param echoQ To echo the plot the or not?
#' @return A QRMon object.
#' @details The errors plot are assigned to \code{qrObj$Value}.
#' @family Errors
#' @export
QRMonErrorsPlot <- function( qrObj, relativeErrorsQ = TRUE, echoQ = TRUE ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  regObjs <- QRMonTakeRegressionObjects( qrObj = qrObj, functionName = "QRMonErrorsPlot" )
  if( length(regObjs) == 0 || QRMonFailureQ(regObjs) ) { return(QRMonFailureSymbol) }

  data <- QRMonTakeData( qrObj = qrObj, functionName = "QRMonErrorsPlot" )
  if( QRMonFailureQ(data) ) { return(QRMonFailureSymbol) }

  res <-
    purrr::map_df( names(regObjs),
                   function(x) {
                     errs <- predict( regObjs[[x]], newdata = data[, "Regressor", drop = F] ) - data[, "Value" ]

                     if( relativeErrorsQ ) {
                       errs <- errs / ifelse( data[, "Value" ] == 0, 1, data[, "Value" ] )
                     }

                     data.frame( RegressionCurve = x,
                                 Regressor = data[, "Regressor", drop = T],
                                 Error = errs,
                                 stringsAsFactors = F)
                   })

  resPlot <-
    ggplot2::ggplot(res) +
    ggplot2::geom_point( ggplot2::aes( x = Regressor, y = Error, color = RegressionCurve ) ) +
    ggplot2::geom_segment( ggplot2::aes(x = Regressor, xend = Regressor, y = 0, yend = Error, color = RegressionCurve ) ) +
    ggplot2::facet_wrap( ~RegressionCurve )

  qrObj$Value <- resPlot

  if( echoQ ) {
    print(resPlot)
  }

  qrObj
}


##===========================================================
## Conditional distribution and related functions
##===========================================================

#' CDF approximation.
#' @description Computes an approximated PDF function
#' using vectors of quantile probabilities and quantiles.
#' @param probabilities Quantile probabilities.
#' @param quantiles Quantiles.
#' @family Distribution functions
#' @export
CDFApproximation <- function( probabilities, quantiles ) {

  names(quantiles) <- NULL; names(probabilities) <- NULL

  if( length(probabilities) != length(quantiles) ) {
    stop( "The lengths of the arguments probabilities and quantiles are expected to be the same.", call. = TRUE )
  }

  ## splinefun( x = quantiles, y = probabilities, method = "natural" )
  approxfun( x = quantiles, y = probabilities, method = "linear" )
}


#' PDF approximation.
#' @description Computes an approximated PDF function
#' using vectors of quantiles and quantile values.
#' @param probabilities Quantile probabilities.
#' @param quantiles Quantile values.
#' @family Distribution functions
#' @export
PDFApproximation <- function( probabilities, quantiles ) {

  names(quantiles) <- NULL; names(probabilities) <- NULL

  if( length(probabilities) != length(quantiles) ) {
    stop( "The lengths of the arguments probabilities and quantiles are expected to be the same.", call. = TRUE )
  }

  xs <- ( quantiles[-length(quantiles)] + quantiles[-1] ) / 2
  ys <- diff(probabilities) / diff(quantiles)
  approxfun( x = xs, y = ys, method = "constant" )
}


#' Expected value approximation.
#' @description Computes an approximated expected value
#' using vectors of quantiles and quantile values.
#' @param probabilities Quantile probabilities.
#' @param quantiles Quantile values.
#' @family Distribution functions
#' @export
ExpectedValueApproximation <- function( probabilities, quantiles ) {

  names(quantiles) <- NULL; names(probabilities) <- NULL

  if( length(probabilities) != length(quantiles) ) {
    stop( "The lengths of the arguments probabilities and quantiles are expected to be the same.", call. = TRUE )
  }

  ## As the PDF x's and y's.
  xs <- ( quantiles[-length(quantiles)] + quantiles[-1] ) / 2
  ys <- diff(probabilities) / diff(quantiles)

  ## Expectation formulation: Integrate[ x * PDF[x], {x, -Infinity, Infinity} ]
  ys <- xs * ys

  res <- purrr::map_dbl( 1:(length(xs)-1), function(i) { ( xs[i+1] - xs[i] ) * ( ys[i+1] + ys[i] ) } )
  sum( res / 2 )
}


#' Conditional CDF computation.
#' @description Computes conditional CDF's for given regressor points.
#' @param qrObj An QRMon object.
#' @param regressorValues Regressor points to compute CDF's upon.
#' @return A QRMon object.
#' @details This function computes a list of
#' Cumulative Distribution Functions (CDF's) that correspond to the
#' elements of \code{regressorValues}.
#' The list of CDF's is assigned to \code{qrObj$Value}.
#' @family Distribution functions
#' @export
QRMonConditionalCDF <- function( qrObj, regressorValues ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  regObjs <- QRMonTakeRegressionObjects( qrObj = qrObj, functionName = "QRMonConditionalCDF" )
  if( length(regObjs) == 0 || QRMonFailureQ(regObjs) ) { return(QRMonFailureSymbol) }

  if( !is.numeric(regressorValues) ) {
    warning( "The argument regressorValues is expected to be a numeric vector.", call. = TRUE )
    return(QRMonFailureSymbol)
  }

  qvals <- qrObj %>% QRMonPredict( newdata = regressorValues ) %>% QRMonTakeValue()
  qvals <- purrr::map_df( names(qvals), function(x) cbind( Probability = x, qvals[[x]], stringsAsFactors = FALSE ) )
  ## The following code line makes a dependency with dplyr.
  ## qvals <- dplyr::bind_rows( qvals, .id = "Probability")
  qvals$Probability <- as.numeric( qvals$Probability )

  res <-
    purrr::map( split(qvals, qvals$Regressor), function(x) {
       CDFApproximation( probabilities = x$Probability, quantiles =  x$Value )
    } )

  qrObj$Value <- res
  qrObj
}


##===========================================================
## Conditional CDF plot
##===========================================================

#' Conditional CDF plot.
#' @description Computes conditional CDF's for given regressor points and makes corresponding plots.
#' @param qrObj An QRMon object.
#' @param regressorValues Regressor points to compute CDF's upon.
#' @param valueGridPoints Grid points to use for the value(response) variable.
#' If NULL the grid points are derived from response variable's range in the data.
#' @param dateOrigin Date origin if the regressor conversion to time-date.
#' If NULL no conversion is done.
#' Same as the argument \code{origin} of \code{as.POSIXct}.
#' @param quantileGridLinesQ Should the quantiles be indicated with vertical grid lines or not?
#' @param echoQ To echo the plot the or not?
#' @param ... Arguments for \code{\link{ggplot2::facet_wrap}}.
#' (In order to plot additional vertical lines.)
#' @return A QRMon object.
#' @details This function uses \code{\link{QRMonConditionalCDF}}.
#' @family Distribution functions
#' @export
QRMonConditionalCDFPlot <- function( qrObj, regressorValues, valueGridPoints = NULL,
                                     dateOrigin = NULL,
                                     quantileGridLinesQ = TRUE, echoQ = TRUE, ... ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  #focusPoint <- regressorValues[[1]]
  cdfFuncs <- qrObj %>% QRMonConditionalCDF( regressorValues = regressorValues ) %>% QRMonTakeValue

  if( is.null(valueGridPoints) ) {
    data <- qrObj %>% QRMonTakeData
    valueGridPoints <- seq( min(data$Value), max(data$Value), (max(data$Value) - min(data$Value)) / (100-1) )
  }

  qDF <-
    purrr::map_df(
      names(cdfFuncs), function(reg) {
        data.frame(  Regressor = as.numeric(reg),
                     Value = valueGridPoints,
                     CDF = purrr::map_dbl( valueGridPoints, function(x) cdfFuncs[[reg]](x) ) )
      })

  qDF <- qDF[ complete.cases(qDF), ]

  dfDist <- qrObj %>% QRMonPredict( newdata = regressorValues ) %>% QRMonTakeValue
  ## Note the dplyr dependence
  dfDist <- purrr::map_df( names(dfDist), function(x) cbind( Quantile = as.numeric(x), dfDist[[x]] ) )


  if( !is.null(dateOrigin) ) {
    qDF$Regressor <- as.POSIXct( qDF$Regressor, origin = dateOrigin )
    dfDist$Regressor <- as.POSIXct( dfDist$Regressor, origin = dateOrigin )
  }

  if( quantileGridLinesQ ) {

    res <-
      ggplot2::ggplot(qDF) +
      ggplot2::geom_line( ggplot2::aes( x = Value, y = CDF) ) +
      ggplot2::geom_vline( data = dfDist, mapping = aes(xintercept = Value), linetype = "dotted", color = "gray20", size = 0.5 ) +
      ggplot2::geom_text(data = dfDist, mapping = aes( x = Value, y=0, label = dfDist$Quantile ), size=2, angle=90, vjust=-0.4, hjust=0) +
      ggplot2::facet_wrap( ~Regressor, ... )

  } else {

    res <-
      ggplot2::ggplot(qDF) +
      ggplot2::geom_line( ggplot2::aes( x = Value, y = CDF) ) +
      ggplot2::facet_wrap( ~Regressor, ... )

  }

  if( echoQ ) {
    print(res)
  }

  qrObj$Value <- res

  qrObj
}


##===========================================================
## Pick path points
##===========================================================

#' Pick path points.
#' @description Pick points close to the regression functions using a specified threshold.
#' @param qrObj An QRMon object.
#' @param threshold The pick threshold.
#' @param pickAboveThresholdQ Should points be picked below or above the threshold?
#' @return A QRMon object.
#' @details The list of data frames is assigned to \code{qrObj$Value}.
#' @family Regression functions
#' @export
QRMonPickPathPoints <- function( qrObj, threshold, pickAboveThresholdQ = FALSE ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  regObjs <- QRMonTakeRegressionObjects( qrObj = qrObj, functionName = "QRMonPickPathPoints" )
  if( length(regObjs) == 0 || QRMonFailureQ(regObjs) ) { return(QRMonFailureSymbol) }

  data <- QRMonTakeData( qrObj = qrObj, functionName = "QRMonPickPathPoints" )
  if( QRMonFailureQ(data) ) { return(QRMonFailureSymbol) }

  if( ! ( is.numeric(threshold) && threshold >= 0) ) {
    warning( "The argument threshold is expected to be a non-negative number.", call. = TRUE )
    return( QRMonFailureSymbol )
  }

  ## Find the regression quantile values for each quantile.
  qrVals <- qrObj %>% QRMonPredict( newdata = NULL ) %>%  QRMonTakeValue()
  if( QRMonFailureQ(qrVals) ) { return(QRMonFailureSymbol) }

  res <-
    purrr::map(
      qrVals,
      function(x) {
        picks <- abs( x[,"Value"] - data[, "Value"] ) <= threshold
        if( pickAboveThresholdQ ) { picks <- ! picks }
        data[ picks, ]
      })
  names(res) <- names(qrVals)

  qrObj$Value <- res

  qrObj
}


##===========================================================
## Separate
##===========================================================

#' Separate data points.
#' @description Separates the argument by the regression functions in the context.
#' If no argument is given the data in the monad object is separated.
#' @param qrObj An QRMon object.
#' @param data A data frame of points to be separated.
#' @param cumulativeQ Should for each regression quantile find the points below it?
#' @param fractionsQ Should fractions instead of points be returned?
#' @return A QRMon object.
#' @details The result of the separation is a list of data frames assigned to \code{qrObj$Value}.
#' Each data frame of that list corresponds to the found regression quantiles.
#' If the data frame argument \code{data} has columns "Regressor" and "Value" those columns are used;
#' otherwise the first and second columns are treated as "Regressor" and "Value" respectively.
#' @family Regression functions
#' @export
QRMonSeparate <- function( qrObj, data = NULL, cumulativeQ = TRUE, fractionsQ = FALSE ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  regObjs <- QRMonTakeRegressionObjects( qrObj = qrObj, functionName = "QRMonSeparate" )
  if( length(regObjs) == 0 || QRMonFailureQ(regObjs) ) { return(QRMonFailureSymbol) }

  if( ! ( is.null(data) || is.data.frame(data) && ncol(data) >= 2 ) ) {
    warning( "The argument data is expected to be NULL or a data frame of points (with two numeric columns.)", call. = TRUE )
    return( QRMonFailureSymbol )
  }

  if( is.null(data) ) {

    data <- QRMonTakeData( qrObj = qrObj, functionName = "QRMonPickPathPoints" )
    if( QRMonFailureQ(data) ) { return(QRMonFailureSymbol) }

  } else {

    dataColNames <- c( "Regressor", "Value")
    if( colnames(data) %in% dataColNames ) {
      data <- data[, dataColNames]
    } else {
      data <- setName( data[, 1:2], dataColNames )
    }

  }

  ## Find the regression quantile values for each quantile.
  qrVals <- qrObj %>% QRMonPredict( newdata = data ) %>%  QRMonTakeValue()
  if( QRMonFailureQ(qrVals) ) { return(QRMonFailureSymbol) }

  ## The computation can be optimized.
  ## Currently is O[ Length[data] * Length[regressionFunctions] ] . Can be at least halved.

  indGroups <- purrr::map( qrVals, function(x) { x[,"Value"] >= data[, "Value"] } )
  names(indGroups) <- names(qrVals)

  if( cumulativeQ ) {

    pointGroups <- purrr::map( indGroups, function(x) data[x,] )
    names(pointGroups) <- names(indGroups)

  } else {
    ## Find complements of the indices that belong to pairs of consecutive quantile probabilities.

    indGroups <- purrr::map( indGroups, function(x) (1:nrow(data))[x] )
    indGroups <- indGroups[order(as.numeric(names(indGroups)))]

    if( length(indGroups) > 1 ) {
      indGroups2 <- purrr::pmap( list( indGroups[-1], indGroups[-length(indGroups)] ), function(x,y) setdiff(x,y) )
      indGroups <- setNames( c( indGroups[1], indGroups2 ), names(indGroups) )
    }

    pointGroups <- purrr::map( indGroups, function(x) data[x,] )
    names(pointGroups) <- names(indGroups)
  }

  if( fractionsQ ) {
    ## Note that here we change the result shape into a list of point counts.
    ## There is specialized QRMon signature (QRMonSeparateToFractions) in order to obtain this kind of result.
    ## It is much easier to compute this here than in that function.
    pointGroups <- purrr::map( pointGroups, function(x) nrow(x) / nrow(data) )
    names(pointGroups) <- names(indGroups)
  }

  qrObj <- QRMonSetValue(qrObj, pointGroups)
  qrObj
}


##===========================================================
## Separate to fractions
##===========================================================

#' Fractions of separated data points.
#' @description Separates the argument by the regression functions in the context
#' and compute the corresponding fractions.
#' If no argument is given the data in the monad object is separated.
#' @param qrObj An QRMon object.
#' @param data A data frame of points to be separated.
#' @param cumulativeQ Should for each regression quantile find the points below it?
#' @param fractionsQ Should fractions instead of points be returned?
#' @return A QRMon object.
#' @details The result is assigned to \code{qrObj$Value}.
#' For more details see \code{\link{QRMonSeparate}}.
#' (This function is just a shortcut to that one.)
#' @family Regression functions
#' @export
QRMonSeparateToFractions <- function( qrObj, data = NULL, cumulativeQ = TRUE ) {
  qrObj <- QRMonSeparate(qrObj = qrObj, data = data, cumulativeQ = cumulativeQ, fractionsQ = TRUE)
  qrObj$Value <- setNames( as.numeric(qrObj$Value), names(qrObj$Value) )
  qrObj
}


##===========================================================
## Simulation
##===========================================================

RandomPoint <- function(df){
  df <- df[ order(df$RegressionCurve), ]
  ind <- sample.int( n = nrow(df) - 1, size = 1, prob = diff(df$RegressionCurve) )
  runif( min = df$Value[ind], max = df$Value[ind+1], n = 1 )
}

#' Random points generation for a CDF.
#' @description Generates a vector of random points using a CDF data frame.
#' @param cdf A data frame with numeric columns "Probability" and "Quantile".
#' @param n Number of points.
#' @param randomFunction A function to generate a random point between the quantiles.
#' (It rarely makes sense to be different than \code{runif}.)
#' @return A numeric vector
#' @export
CDFRandomPoints <- function( cdf, n = 1, randomFunction = runif ) {

  if( !( is.data.frame(cdf) && sum( names(cdf) %in% c("Probability", "Quantile") ) == 2 ) ) {
    stop( "The argument cdf is expected to be a data frame with columns \"Probability\" and \"Quantile\".", call. = TRUE )
  }

  if( !( is.numeric( cdf$Probability) && is.numeric( cdf$Quantile ) ) ) {
    stop( "The columns of the argument cdf are expected to be numeric.", call. = TRUE )
  }

  cdf<- cdf[ order(cdf$Probability), ]
  ind <- sample.int( n = nrow(cdf) - 1, size = n, prob = diff(cdf$Probability), replace = TRUE )
  runif( min = cdf$Quantile[ind], max = cdf$Quantile[ind+1], n = n )

}

#' Simulate data points.
#' @description Simulate data points based on regression quantiles.
#' @param qrObj An QRMon object.
#' @param n Number of simulated points.
#' Ignored if \code{points} is not NULL.
#' @param points A numerical vector of regressor points.
#' If NULL \code{n} is used.
#' @param method A method specification string.
#' One of "CDF" or "ConditionalCDF".
#' @return A QRMon object.
#' @details If \code{method = "ConditionalCDF"} then
#' at each simulation regressor point:
#' (1) the corresponding regression quantiles values are found;
#' (2) then an interval of two regression probabilities is randomly picked;
#' (3) a random point is generated between interval's (conditional) quantiles.
#' If \code{method = "CDF"} then
#' (1) the quantiles of the values are found using the regression probabilities;
#' (2) for each simulation point an interval of two regression probabilities is randomly picked;
#' (3) a random point is generated between interval's quantiles.
#' Uniform distribution is used for the random point generation.
#' The obtained data frame with the simulated points is
#' assigned to \code{qrObj$Value}.
#' @export
QRMonSimulate <- function( qrObj, n = 100, points = NULL, method = "ConditionalCDF" ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  data <- qrObj %>% QRMonTakeData()

  if( ! ( is.null(n) || is.numeric(n) ) ) {
    warning( "The argument n is expected to be a number.", call. = TRUE )
    return(QRMonFailureSymbol)
  }

  if( ! ( is.null(points) || is.numeric(points) ) ) {
    warning( "The argument points is expected to be a numeric vector or NULL.", call. = TRUE )
    return(QRMonFailureSymbol)
  }

  if( ! ( is.character(method) && length(method) == 1 ) ) {
    warning( "The argument method should be one of 'CDF', 'ConditionalCDF'.", call. = TRUE )
    return(QRMonFailureSymbol)
  }

  ## We need to have some value for n in case both n and points are NULL.
  if( is.null(n) ) { n <- 100 }

  if( is.null(points) ) {
    points <- seq( min(data$Regressor), max(data$Regressor), ( max(data$Regressor) - min(data$Regressor) ) / (n-1) )
  }

  # This should not be done:
  # points <- sort(points)

  qrObj <- qrObj %>% QRMonPredict( points )
  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  simVals <- qrObj %>% QRMonTakeValue()

  simDF <-
    purrr::map_df( names(simVals),
                   function(x) cbind( RegressionCurve = as.numeric(x), simVals[[x]], stringsAsFactors = FALSE ) )

  regCurves <- sort( unique( simDF$RegressionCurve ) )

  if( length(regCurves) < 2 ) {
    warning( "More than one regression quantiles are expected.", call. = TRUE )
    return(QRMonFailureSymbol)
  }

  ## Simulation
  if( tolower(method) %in% tolower(c("ConditionalCDF", "RegressionQuantiles")) ) {

    simResDF <-
      simDF %>%
      dplyr::group_by( Regressor ) %>%
      dplyr::do( data.frame( Value = RandomPoint(.) ))

  } else if( tolower(method) %in% tolower(c("CDF", "Simple", "EmpiricalCDF", "Quantiles")) ) {

    df <- quantile( x = data$Value, probs = regCurves )
    df <- data.frame( Probability = regCurves, Quantile = df )

    simResDF <- data.frame( Regressor = points,
                            Value = CDFRandomPoints( cdf = df, n = length(points) ) )

  } else {

    warning( "The argument method should be one of 'CDF', 'ConditionalCDF'.", call. = TRUE )
    return(QRMonFailureSymbol)

  }


  qrObj$Value <- simResDF

  qrObj
}
