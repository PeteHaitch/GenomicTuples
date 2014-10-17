### -------------------------------------------------------------------------
### size
###

#' @export
setGeneric("size", function(x) {
  standardGeneric("size")
})

### -------------------------------------------------------------------------
### IPD
###

#' @export
setGeneric("IPD", function(x) {
  standardGeneric("IPD")
})

### -------------------------------------------------------------------------
### tuples
###

# TODO: Perhaps tuples,GTuples-method should have a use.names argument?
#' @export
setGeneric("tuples", function(x) {
  standardGeneric("tuples")
})

#' @export
setGeneric("tuples<-", function(x, ..., value) {
  standardGeneric("tuples<-")
})