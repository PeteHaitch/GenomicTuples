### =========================================================================
### [p]mapCoords methods
### -------------------------------------------------------------------------
###

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### mapCoords
###
#' @export
setMethod("mapCoords", 
          c("GTuples", "GTuplesList"), 
          function(from, to, ..., ignore.strand = TRUE, elt.hits = FALSE) {
            stop(paste0(class(from), " do not currently support the ", 
                        "'mapCoords' method."))
          }
)

#' @export
setMethod("mapCoords", 
          c("GTuples", "GTuples"), 
          function(from, to, ..., ignore.strand = TRUE, elt.hits = FALSE) {
            stop(paste0(class(from), " do not currently support the ", 
                        "'mapCoords' method."))
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### pmapCoords
###

#' @export
setMethod("pmapCoords", 
          c("GTuples", "GTuplesList"), 
          function(from, to, ..., ignore.strand = TRUE, elt.hits = FALSE) {
            stop(paste0(class(from), " do not currently support the ", 
                        "'pmapCoords' method."))
          }
)
