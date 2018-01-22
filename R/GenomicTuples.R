#' Representation and manipulation of genomic tuples.
#'
#' \pkg{GenomicTuples} defines general purpose containers for storing genomic
#' tuples. It aims to provide functionality for tuples of genomic co-ordinates
#' that are analogous to those available for genomic ranges in the
#' GenomicRanges Bioconductor package.
#'
#' Please refer to the vignettes to see how to use the \pkg{GenomicTuples}
#' package.
#'
#' @docType package
#' @name GenomicTuples-package
# NOTE: For simplicity, just import the entire BiocGenerics package
#' @import BiocGenerics
#' @references 
#' Peter F Hickey (2016). 
#' Representation and Manipulation of Genomic Tuples in R. 
#' \emph{JOSS}. URL \url{http://dx.doi.org/10.21105/joss.00020}
# Need these two tags to use Rcpp
#' @useDynLib GenomicTuples, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @import methods
NULL
