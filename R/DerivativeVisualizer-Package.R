#' DerivativeVisualizer.
#'
#' @name DerivativeVisualizer
#' @docType package
NULL

defaults <- new.env()

defaults$range <- c(.9, 1.1) # Expressed as percentage of strike/buying price
defaults$granularity <- 0.01 # $0.01
defaults$short <- FALSE # Default to not shorting
defaults$size <- 100 # Default contract size of 100
