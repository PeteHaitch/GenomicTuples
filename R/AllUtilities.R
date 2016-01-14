### =========================================================================
### Helper functions not exported.
### For emphasis, __no function in this file will be exported__, because 
### `@keywords internal` applies to the whole file 
### (https://github.com/ramhiser/sparsediscrim/issues/26).
### This means that these functions will not be documented by roxygen2, even
### though the functions have roxygen2 tags.
### =========================================================================

#' Check whether all elements of a numeric vector are identical (within machine 
#' precision)
#' @param x a numeric vector.
# 
#' @return TRUE if all elements of the vector are identical (within machine 
#' precision). FALSE in all other cases, including if the vector contains any 
#' NAs.
#' 
#' @keywords internal
#' 
#' @note This function is based on Hadley and John's answer to 
#' http://stackoverflow.com/q/4752275. No check is made that \code{x} is a 
#' numeric vector.
.zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 0L){
    val <- TRUE
    warning("length(x) == 0L")
  } else if (length(x) == 1) {
    val <- TRUE
  } else{
    if (any(is.na(x)) & any(is.na(x))){
      val <- FALSE
    } else{
      val <- (abs(max(x) - min(x)) < tol)
    }
  }  
  val
}

#' Compute column-wise difference of matrices with possibly different number of 
#' rows. Do this by iterating over columns, treating them as vectors and 
#' then using R's native vector recycling.
#' @keywords internal
.matrixDiffWithRecycling <- function(x, y) {
  z <- matrix(NA_integer_, ncol = ncol(x), nrow = max(nrow(x), nrow(y)))
  for (i in seq_len(ncol(x))) {
    z[, i] <- x[, i] - y[, i]
  }
  z
}

<<<<<<< 475edacb3dfd3b80654842c50fcd9c9a984de33f
#' Convert a GTuples object to a data.table.
#' 
#' @param gt A GTuples object
#' @param When set to \code{TRUE}, the strand is set to "*".
#' @importFrom data.table as.data.table data.table
#' @importMethodsFrom GenomeInfoDb seqnames
#'        
#' @keywords internal
.GT2DT <- function(gt, ignore.strand = FALSE) {
  
  if (length(gt) == 0L) {
    return(data.table())
  }
  
  if (!isTRUEorFALSE(ignore.strand)) {
    stop("'ignore.strand' must be TRUE of FALSE")
  }
  seqnames <- S4Vectors:::decodeRle(seqnames(gt))
  if (ignore.strand) {
    strand <- strand(rep("*", length(gt)))
  } else {
    strand <- S4Vectors:::decodeRle(strand(gt))
  }
  tuples <- as.data.table(tuples(gt))
  cbind(data.table("seqnames" = seqnames,
                   "strand" = strand),
        tuples)
}
