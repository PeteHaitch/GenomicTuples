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
#' @useDynLib GenomicTuples, .registration = TRUE
#' @import GenomicRanges
#' @import GenomeInfoDb
#' @import Rcpp
#' @import methods
#' @import BiocGenerics
#' @import S4Vectors
#' @importFrom Biobase validMsg
NULL