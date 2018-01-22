### =========================================================================
### "tile" methods
### -------------------------------------------------------------------------
###

#' @importMethodsFrom IRanges tile
#' 
#' @export
setMethod("tile", 
          "GTuples", 
          function(x, n, width) {
            stop(class(x), " do not currently support the 'tile' method.")
          }
)
