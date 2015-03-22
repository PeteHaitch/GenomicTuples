### =========================================================================
### "tile" methods
### -------------------------------------------------------------------------
###

#' @export
setMethod("tile", 
          "GTuples", 
          function(x, n, width) {
            stop(paste0(class(x), " do not currently support the 'tile' ", 
                        "method."))
          }
)
