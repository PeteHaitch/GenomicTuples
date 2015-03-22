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

# Function not currently required but may be useful; see c,GTuples-method
# TODO: Unit tests
# #' Find the lowest common class of a list of objects
# #' @keywords internal
# #' @param ... A list of objects to be compared
# #' @value The lowest common class if there is one, an error if there isn't one.
# .lcc <- function(...) {
#   args <- list(...)
#   classes <- lapply(args, is)
#   common_classes <- Reduce(f = "intersect", x = classes)
#   if (identical(common_classes, numeric(0))) {
#     stop("No common class")
#   } else {
#     common_classes[1]
#   }
# }
