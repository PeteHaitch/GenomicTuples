### =========================================================================
### mapCoords methods
### -------------------------------------------------------------------------
###

#' @export
setMethod("mapCoords", 
          c("GTuples", "GTuplesList"), 
          function(from, to, ..., ignore.strand = FALSE, elt.loc = FALSE, 
                   elt.hits = FALSE) {
            stop(paste0(class(from), " do not currently support the 'mapCoords' ", 
                        "method."))
          }
)
