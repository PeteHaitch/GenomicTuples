### =========================================================================
### mapCoords methods
### -------------------------------------------------------------------------
###

#' @export
setMethod("mapCoords", 
          c("GTuples", "GTuplesList"), 
          function(x, to, ..., ignore.strand = FALSE, elt.loc = FALSE, 
                   elt.hits = FALSE) {
            stop(paste0(class(x), " do not currently support the 'mapCoords' ", 
                        "method."))
          }
)