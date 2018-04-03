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
#' Creator: Daniel Lewandowski \email{<daniel@@nextpagesoft.net>}\cr
#' Author: Nikos Pantazis \email{<npantaz@@med.uoa.gr>}\cr
#' Author: Magdalena Rosińska \email{<mrosinska@@pzh.gov.pl>}\cr
#' Author: Janusz Janiec \email{<jjaniec@@pzh.gov.pl>}\cr
#'
#' @import data.table
#' @import graphics
#' @importFrom stats setNames lowess quantile sd var
#' @importFrom grid grid.newpage grid.layout viewport pushViewport
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
