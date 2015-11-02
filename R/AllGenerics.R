### -------------------------------------------------------------------------
### size
###

#' @importFrom methods setGeneric
#' 
#' @export
setGeneric("size", function(x) {
  standardGeneric("size")
})

### -------------------------------------------------------------------------
### IPD
###

#' @importFrom methods setGeneric
#' 
#' @export
setGeneric("IPD", function(x) {
  standardGeneric("IPD")
})

### -------------------------------------------------------------------------
### tuples
###

# TODO: Perhaps tuples,GTuples-method should have a use.names argument?
#' @importFrom methods setGeneric
#' 
#' @export
setGeneric("tuples", function(x) {

  standardGeneric("tuples")
})

#' @importFrom methods setGeneric
#' 
#' @export
setGeneric("tuples<-", function(x, ..., value) {
  standardGeneric("tuples<-")
})
