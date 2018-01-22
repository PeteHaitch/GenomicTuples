### -------------------------------------------------------------------------
### size
###

#' @export
setGeneric("size", function(x) standardGeneric("size"))

### -------------------------------------------------------------------------
### IPD
###

#' @export
setGeneric("IPD", function(x) standardGeneric("IPD"))

### -------------------------------------------------------------------------
### tuples
###

#' @export
setGeneric("tuples", function(x) standardGeneric("tuples"))

#' @export
setGeneric("tuples<-", function(x, ..., value) standardGeneric("tuples<-"))

### -------------------------------------------------------------------------
### gtuples
###

# Similar to granges, i.e., it "squeezes" the GTuples out of a tuples-based 
# object.
# NOTE: No methods are implemented in the GenomicTuples package but might be 
#       implemented in a package for which there is a class that uses a 
#       GTuples object as a slot.
#' @export
setGeneric("gtuples", function(x, use.mcols = FALSE, ...) standardGeneric("gtuples"))

### -------------------------------------------------------------------------
### gtlist
###

# Similar to grglist, i.e., it "squeezes" the GTuplesList out of a tuples-based 
# object.
# NOTE: No methods are implemented in the GenomicTuples package but might be 
#       implemented in a package for which there is a class that uses a 
#       GTuplesList object as a slot.
#' @export
setGeneric("gtlist", function(x, use.mcols = FALSE, ...) standardGeneric("gtlist"))
