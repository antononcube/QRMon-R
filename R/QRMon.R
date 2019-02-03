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
#' @import devtools
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

#' Make a QRMon Unit
#' @description Creates a monad object.
#' @param data A vector or a two-column matrix or data frame.
#' @return An S3 class "QRMon". In other words, a list with the attribute "class" set to "QRMon".
#' @export
QRMonUnit <- function( data = NULL ) {

  res <- list( Value = NULL, Data = data, RegressionObjects = list(), Outliers = NULL )
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

    QRMonSetData( qrObj, data.frame( Time = 1:length(data), Value = data ) )

  } else if ( ( is.matrix(data) || is.data.frame(data) ) && ncol(data) == 1 ) {

    QRMonSetData( qrObj, data.frame( Time = 1:nrow(data), Value = data[,1] ) )

  } else if ( is.matrix(data) || is.data.frame(data) ) {

    expectedColNames <- c("Time", "Value")

    if( ! ( is.data.frame(data) && length(intersect( colnames(data), expectedColNames)) == length(expectedColNames) ) ) {
      warning( paste( "The argument data is expected to be a data frame with columns: {", paste(expectedColNames, collapse =", "), "}."), call. = TRUE)
      warning( paste0( "Proceeding by renaming the first columm \"", colnames(data)[[1]], "\" as \"Time\" ",
                       "and renaming the second columm \"", colnames(data)[[2]], "\" as \"Value\"." ), call. = TRUE )
      data <- data[,1:2]
      colnames(data) <- c( "Time", "Value" )
    }

    if( sum( class(data$Time) %in% c( "Date", "POSSIXt" ) ) > 0 ) {
      warning( "Converting dates into seconds.", call. = TRUE)
      data$Time <- as.numeric(data$Time, "second")
    }

    if( !is.numeric(data$Time) || !is.numeric(data$Value) ) {
      warning( "The columns 'Time' and 'Value' of the argument data are expected to be numeric.", call. = TRUE)
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

  if( !QRMonDataCheck( qrObj, functionName = functionName,  logicalResult = TRUE) ) {
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
    warning( paste0( functionName, "Cannot find event data"), call. = TRUE)
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
    warning( paste0( functionName, paste0("Cannot find ", memberPrettyName, "."), call. = TRUE) )
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
#' @description Summarize data.
#' using a B-spline basis.
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
## Quantile regression fit
##===========================================================

#' Quantile regression fit.
#' @description Finds the quantile regression objects for the specified quantiles
#' using a B-spline basis.
#' @param qrObj An QRMon object.
#' @param quantiles A numeric vector with quantiles.
#' @param ... parameters for \code{\link{quantreg::rq}}.
#' @return A QRMon object.
#' @details The obtained regression objects are assigned/appended to the
#' \code{qrObj$RegressionObjects}.
#' @family Regression functions
#' @export
QRMonQuantileRegression <- function( qrObj, quantiles = c(0.25, 0.5, 0.75), ... ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  data <- QRMonTakeData( qrObj = qrObj, functionName = "QRMonQuantileRegression" )
  if( QRMonFailureQ(data) ) { return(QRMonFailureSymbol) }

  rqFits <-
    purrr::map(
      quantiles,
      function(tau) { rq(Value ~ bs(Time, ...), tau = tau, data = data ) })
  names(rqFits) <- quantiles

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
#' @param newdata A numeric vector, a data frame with a column 'Time', or NULL.
#' @param ... parameters for \code{\link{quantreg::predict.rq}}.
#' @return A QRMon object.
#' @details The result of the evaluation of the regression objects
#' over the new data is a list that is assigned to \code{qrObj$Value}.
#' @family Regression functions
#' @export
QRMonPredict <- function( qrObj, newdata, ... ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  regObjs <- QRMonTakeRegressionObjects( qrObj = qrObj, functionName = "QRMonPredict" )
  if( QRMonFailureQ(regObjs) ) { return(QRMonFailureSymbol) }

  if( is.vector(newdata) ) {
    newdata <- data.frame( Time = newdata )
  }

  if( ! ( is.null(newdata) || is.data.frame(newdata) && sum( "Time" %in% colnames(newdata) ) == 1 ) ) {
    warning( "A numeric vector, data frame with a column 'Time', or NULL is expected for the argument newdata.", call. = TRUE )
    return(QRMonFailureSymbol)
  }

  if( is.null(newdata) ) {
    newdata <- QRMonTakeData(qrObj = qrObj)
  }

  if( is.data.frame(newdata) ) {
    newdata <- newdata[, "Time", drop = F]
  }

  qrRes <-
    purrr::map(
      names(regObjs),
      function(x) {
        res <- predict( regObjs[[x]], newdata = newdata, ... )
        data.frame( Time = newdata, Value = res )
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
#' @param datePlotQ Should the time axis have dates scale?
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

  qrObj <- qrObj %>% QRMonPredict( newdata = NULL )
  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  qrDF <- qrObj %>% QRMonTakeValue()

  qrDF <- purrr::map_df(names(qrDF), function(x) cbind( RegressionCurve = x, qrDF[[x]], stringsAsFactors = FALSE ) )

  resPlot <- ggplot()

  data <- qrObj %>% QRMonTakeData()

  if( datePlotQ ) {
    data$Time <- as.POSIXct( data$Time, origin = dateOrigin )
    qrDF$Time <- as.POSIXct( qrDF$Time, origin = dateOrigin )
  }

  if( !is.null(dataPointsColor) ) {

    resPlot <-
      resPlot +
      geom_point( data = data,
                  mapping = aes( x = Time, y = Value ), color = dataPointsColor )
  }

  if( !is.null(regressionCurvesColor) ) {
    resPlot <-
      resPlot +
      if( is.character(regressionCurvesColor) ) {
        geom_line( data = qrDF, aes_( x = ~Time, y = ~Value, group = ~RegressionCurve ), color = regressionCurvesColor )
      } else {
        geom_line( data = qrDF, aes_( x = ~Time, y = ~Value, color = regressionCurvesColor ) )
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
  if( QRMonFailureQ(regObjs) ) {
    warning( "Calculate (top and bottom) regression quantiles first.", call. = TRUE )
    return(QRMonFailureSymbol)
  }

  data <- QRMonTakeData( qrObj = qrObj, functionName = "QRMonOutliers" )
  if( QRMonFailureQ(data) ) { return(QRMonFailureSymbol) }

  outliers <-
    if( length(regObjs) == 1 && as.numeric(names(regObjs)[[1]]) < 0.5 ) {

      predVals <- predict( regObjs[[1]], newdata = data[ , "Time", drop=F ] )
      bottomOutliers <- data[ data$Value <= predVals, ]
      list( "bottomOutliers" = bottomOutliers )

    } else if ( length(regObjs) == 1 && as.numeric(names(regObjs)[[1]]) > 0.5  ) {

      predVals <- predict( regObjs[[1]], newdata = data[ , "Time", drop=F ] )
      topOutliers <- data[ data$Value >= predVals, ]
      list( "topOutliers" = topOutliers )

    } else {

      qs <- as.numeric( names(regObjs) )
      minInd <- which.min( qs )
      maxInd <- which.max( qs )

      predVals <- predict( regObjs[[minInd]], newdata = data[ , "Time", drop=F ] )
      bottomOutliers <- data[ data$Value <= predVals, ]

      predVals <- predict( regObjs[[maxInd]], newdata = data[ , "Time", drop=F ] )
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
#' @param datePlotQ Should the time axis have dates scale?
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
    plotDataDF$Time <- as.POSIXct( plotDataDF$Time, origin = dateOrigin )
  }

  resPlot <-
    resPlot +
    geom_point( data = plotDataDF, mapping = aes( x = Time, y = Value, color = Type ) )

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

  regObjs <- QRMonTakeRegressionObjects( qrObj = qrObj, functionName = "QRMonOutliers" )
  if( QRMonFailureQ(regObjs) ) { return(QRMonFailureSymbol) }

  data <- QRMonTakeData( qrObj = qrObj, functionName = "QRMonOutliers" )
  if( QRMonFailureQ(data) ) { return(QRMonFailureSymbol) }

  res <-
    purrr::map( names(regObjs),
                   function(x) {
                     errs <- predict( regObjs[[x]], newdata = data[, "Time", drop = F] ) - data[, "Value" ]

                     if( relativeErrorsQ ) {
                       errs <- errs / ifelse( data[, "Value" ] == 0, 1, data[, "Value" ] )
                     }

                     data.frame( Time = data[, "Time"], Error = errs )
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

  regObjs <- QRMonTakeRegressionObjects( qrObj = qrObj, functionName = "QRMonOutliers" )
  if( QRMonFailureQ(regObjs) ) { return(QRMonFailureSymbol) }

  data <- QRMonTakeData( qrObj = qrObj, functionName = "QRMonOutliers" )
  if( QRMonFailureQ(data) ) { return(QRMonFailureSymbol) }

  res <-
    purrr::map_df( names(regObjs),
                   function(x) {
                     errs <- predict( regObjs[[x]], newdata = data[, "Time", drop = F] ) - data[, "Value" ]

                     if( relativeErrorsQ ) {
                       errs <- errs / ifelse( data[, "Value" ] == 0, 1, data[, "Value" ] )
                     }

                     data.frame( RegressionCurve = x,
                                 Time = data[, "Time", drop = T],
                                 Error = errs,
                                 stringsAsFactors = F)
                   })

  resPlot <-
    ggplot(res) +
    geom_point( aes( x = Time, y = Error, color = RegressionCurve ) ) +
    geom_segment( aes(x = Time, xend = Time, y = 0, yend = Error, color = RegressionCurve ) ) +
    facet_wrap( ~RegressionCurve )

  qrObj$Value <- resPlot

  if( echoQ ) {
    print(resPlot)
  }

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
  if( QRMonFailureQ(regObjs) ) { return(QRMonFailureSymbol) }

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
#' If the data frame argument \code{data} has columns "Time" and "Value" those columns are used;
#' otherwise the first and second columns are treated as "Time" and "Value" respectively.
#' @family Regression functions
#' @export
QRMonSeparate <- function( qrObj, data = NULL, cumulativeQ = TRUE, fractionsQ = FALSE ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  regObjs <- QRMonTakeRegressionObjects( qrObj = qrObj, functionName = "QRMonSeparate" )
  if( QRMonFailureQ(regObjs) ) { return(QRMonFailureSymbol) }

  if( ! ( is.null(data) || is.data.frame(data) && ncol(data) >= 2 ) ) {
    warning( "The argument data is expected to be NULL or a data frame of points (with two numeric columns.)", call. = TRUE )
    return( QRMonFailureSymbol )
  }

  if( is.null(data) ) {

    data <- QRMonTakeData( qrObj = qrObj, functionName = "QRMonPickPathPoints" )
    if( QRMonFailureQ(data) ) { return(QRMonFailureSymbol) }

  } else {

    dataColNames <- c( "Time", "Value")
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
    ## Find complements of the indices that belong to pairs of consecutive quantiles.

    indGroups <- qs[order(as.numeric(indGroups))]

    if( length(indGroups) > 1 ) {
      indGroups2 <- purrr::pmap( list( indGroups[-1], indGroups[-length(indGroups)] ), function(x,y) setdiff(y,x) )
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
  ind <- sample.int( nrow(df) - 1, 1 )
  runif( min = df$Value[ind], max = df$Value[ind+1], n = 1 )
}

#' Simulate data points.
#' @description Simulate data points based on regression quantiles.
#' @param qrObj An QRMon object.
#' @param n Number of simulated points.
#' @return A QRMon object.
#' @details The data frame with the simulated points are assigned to \code{qrObj$Value}.
#' @export
QRMonSimulate <- function( qrObj, n = 100 ) {

  if( QRMonFailureQ(qrObj) ) { return(QRMonFailureSymbol) }

  df <- qrObj %>% QRMonTakeData()
  simPoints <- seq( min(df$Time), max(df$Time), ( max(df$Time) - min(df$Time) ) / n )

  qrObj <- qrObj %>% QRMonPredict( simPoints )
  simVals <- qrObj %>% QRMonTakeValue()

  simDF <-
    purrr::map_df( names(simVals),
                   function(x) cbind( RegressionCurve = as.numeric(x), simVals[[x]], stringsAsFactors = FALSE ) )

  simResDF <-
    simDF %>%
    dplyr::group_by( Time ) %>%
    dplyr::do( data.frame( Value = RandomPoint(.) ))

  qrObj$Value <- simResDF

  qrObj
}
