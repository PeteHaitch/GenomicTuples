### =========================================================================
### Set operations
### -------------------------------------------------------------------------

### TODO: What's the impact of circularity on the set operations? This TODO 
### comes from the GenomicRanges/R/setops-methods.R

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### union(), intersect(), setdiff()
###

#' @export
setMethod("union", 
          c("GTuples", "GTuples"),
          function(x, y, ignore.strand = FALSE, ...) {
            stop(paste0(class(x), " do not currently support the 'union' ", 
                        "method."))
          }
)

#' @export
setMethod("intersect", 
          c("GTuples", "GTuples"),
          function(x, y, ignore.strand = FALSE, ...) {
            stop(paste0(class(x), " do not currently support the 'intersect' ", 
                        "method."))
          }
)

#' @export
setMethod("setdiff", 
          c("GTuples", "GTuples"),
          function(x, y, ignore.strand = FALSE, ...) {
            stop(paste0(class(x), " do not currently support the 'setdiff' ", 
                        "method."))
          }
)

### =========================================================================
### Parallel set operations
### -------------------------------------------------------------------------

#' @export
setMethod("punion", 
          c("GTuples", "GTuples"),
          function(x, y, fill.gap = FALSE, ignore.strand = FALSE, ...) {
            stop(paste0(class(x), " and ", class(y), " do not currently ", 
                        "support the 'punion' method."))
          }
)

#' @export
setMethod("punion", 
          c("GTuplesList", "GTuples"),
          function(x, y, fill.gap = FALSE, ...) {
            stop(paste0(class(x), " and ", class(y), " do not currently ", 
                        "support the 'punion' method."))
          }
)

#' @export
setMethod("punion", c("GTuples", "GTuplesList"),
          function(x, y, fill.gap = FALSE, ...) {
            stop(paste0(class(x), " and ", class(y), " do not currently ", 
                        "support the 'punion' method."))
          }
)

#' @export
setMethod("pintersect", 
          c("GTuples", "GTuples"),
          function(x, y, resolve.empty = c("none", "max.start", "start.x"), 
                   ignore.strand = FALSE, ...) {
            stop(paste0(class(x), " and ", class(y), " do not currently ", 
                        "support the 'pintersect' method."))
          }
)

#' @export
setMethod("pintersect", 
          c("GTuplesList", "GTuples"),
          function(x, y, resolve.empty = c("none", "max.start", "start.x"), 
                   ignore.strand = FALSE, ...) {
            stop(paste0(class(x), " and ", class(y), " do not currently ", 
                        "support the 'pintersect' method."))
          }
)

#' @export
setMethod("pintersect", 
          c("GTuples", "GTuplesList"),
          function(x, y, resolve.empty = c("none", "max.start", "start.x"), 
                   ignore.strand = FALSE, ...) {
            stop(paste0(class(x), " and ", class(y), " do not currently ", 
                        "support the 'pintersect' method."))
          }
)

#' @export
setMethod("pintersect", 
          c("GTuplesList", "GTuplesList"),
          function(x, y, ...) {
            stop(paste0(class(x), " and ", class(y), " do not currently ", 
                        "support the 'pintersect' method."))
          }
)

#' @export
setMethod("psetdiff",
          c("GTuples", "GTuples"),
          function(x, y, ignore.strand = FALSE, ...) {
            stop(paste0(class(x), " and ", class(y), " do not currently ", 
                        "support the 'psetdiff' method."))
          }
)

#' @export
setMethod("psetdiff", c
          ("GTuples", "GTuplesList"),
          function(x, y, ignore.strand = FALSE, ...) {
            stop(paste0(class(x), " and ", class(y), " do not currently ", 
                        "support the 'psetdiff' method."))
          }
)

#' @export
setMethod("psetdiff", 
          c("GTuplesList", "GTuplesList"),
          function(x, y, ...) {
            stop(paste0(class(x), " and ", class(y), " do not currently ", 
                        "support the 'psetdiff' method."))
          }
)

#' @export
setMethod("pgap", 
          c("GTuples", "GTuples"),
          function(x, y, ignore.strand = FALSE, ...) {
            stop(paste0(class(x), " and ", class(y), " do not currently ", 
                        "support the 'pgap' method."))
          }
)
