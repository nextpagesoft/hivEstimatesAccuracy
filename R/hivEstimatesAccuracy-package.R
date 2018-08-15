#' @docType package
#'
#' @name hivEstimatesAccuracy
#'
#' @title
#' Improving Accuracy of HIV Estimates in EU/EEA Countries
#'
#' @description
#' Improves accuracy of HIV estimates in EU/EEA countries.
#'
#' @author
#' Author: Magdalena Rosinska \email{<mrosinska@@pzh.gov.pl>}\cr
#' Author: Nikos Pantazis \email{<npantaz@@med.uoa.gr>}\cr
#' Author: Janusz Janiec \email{<jjaniec@@pzh.gov.pl>}\cr
#' Creator: Daniel Lewandowski \email{<daniel@@nextpagesoft.net>}
#'
#' @import data.table
#' @import ggplot2
#' @importFrom shiny HTML
#' @importFrom stats setNames lowess quantile sd var acf
#' @importFrom grid grid.newpage grid.layout viewport pushViewport
#' @importFrom utils unzip head
#'
NULL

#' mice.impute.pmm
#'
#' @param y y
#' @param ry ry
#' @param x x
#' @param wy wy
#' @param donors donors
#' @param matchtype matchtype
#' @param ridge ridge
#' @param ... extra parameters
#'
#' @export
mice.impute.pmm <- mice::mice.impute.pmm

#' mice.impute.logreg
#'
#' @param y y
#' @param ry ry
#' @param x x
#' @param wy wy
#' @param ... extra parameters
#'
#' @export
mice.impute.logreg <- mice::mice.impute.logreg

#' mice.impute.polyreg
#'
#' @param y y
#' @param ry ry
#' @param x x
#' @param wy wy
#' @param nnet.maxit nnet.maxit
#' @param nnet.trace nnet.trace
#' @param nnet.MaxNWts nnet.MaxNWts
#' @param ... extra parameters
#'
#' @export
mice.impute.polyreg <- mice::mice.impute.polyreg
