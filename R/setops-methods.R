### =========================================================================
### Set operations
### -------------------------------------------------------------------------

### TODO: What's the impact of circularity on the set operations (this comes 
###       from the GenomicRanges/R/setops-methods.R)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### union(), intersect(), setdiff()
###

#' @importFrom methods setMethod
#' 
#' @export
setMethod("union", 
          c("GTuples", "GTuples"),
          function(x, y, ignore.strand = FALSE, ...) {
            stop(class(x), " do not currently support the 'union' method.")
          }
)

#' @importFrom methods setMethod
#' 
#' @export
setMethod("intersect", 
          c("GTuples", "GTuples"),
          function(x, y, ignore.strand = FALSE, ...) {
            stop(class(x), " do not currently support the 'intersect' method.")
          }
)

#' @importFrom methods setMethod
#' 
#' @export
setMethod("setdiff", 
          c("GTuples", "GTuples"),
          function(x, y, ignore.strand = FALSE, ...) {
            stop(class(x), " do not currently support the 'setdiff' method.")
          }
)

### =========================================================================
### Parallel set operations
### -------------------------------------------------------------------------

#' @importFrom methods setMethod
#' @importMethodsFrom IRanges punion
#' 
#' @export
setMethod("punion", 
          c("GTuples", "GTuples"),
          function(x, y, fill.gap = FALSE, ignore.strand = FALSE, ...) {
            stop(class(x), " and ", class(y), " do not currently support the ", 
                 "'punion' method.")
          }
)

#' @importFrom methods setMethod
#' @importMethodsFrom IRanges punion
#' 
#' @export
setMethod("punion", 
          c("GTuplesList", "GTuples"),
          function(x, y, fill.gap = FALSE, ...) {
            stop(class(x), " and ", class(y), " do not currently support the ", 
                 "'punion' method.")
          }
)

#' @importFrom methods setMethod
#' @importMethodsFrom IRanges punion
#' 
#' @export
setMethod("punion", c("GTuples", "GTuplesList"),
          function(x, y, fill.gap = FALSE, ...) {
            stop(class(x), " and ", class(y), " do not currently support the ", 
                 "'punion' method.")
          }
)

#' @importFrom methods setMethod
#' @importMethodsFrom IRanges pintersect
#' 
#' @export
setMethod("pintersect", 
          c("GTuples", "GTuples"),
          function(x, y, resolve.empty = c("none", "max.start", "start.x"), 
                   ignore.strand = FALSE, ...) {
            stop(class(x), " and ", class(y), " do not currently support the ", 
                 "'pintersect' method.")
          }
)

#' @importFrom methods setMethod
#' @importMethodsFrom IRanges pintersect
#' 
#' @export
setMethod("pintersect", 
          c("GTuplesList", "GTuples"),
          function(x, y, resolve.empty = c("none", "max.start", "start.x"), 
                   ignore.strand = FALSE, ...) {
            stop(class(x), " and ", class(y), " do not currently support the ", 
                 "'pintersect' method.")
          }
)

#' @importFrom methods setMethod
#' @importMethodsFrom IRanges pintersect
#' 
#' @export
setMethod("pintersect", 
          c("GTuples", "GTuplesList"),
          function(x, y, resolve.empty = c("none", "max.start", "start.x"), 
                   ignore.strand = FALSE, ...) {
            stop(class(x), " and ", class(y), " do not currently support the ", 
                 "'pintersect' method.")
          }
)

#' @importFrom methods setMethod
#' @importMethodsFrom IRanges pintersect
#' 
#' @export
setMethod("pintersect", 
          c("GTuplesList", "GTuplesList"),
          function(x, y, ...) {
            stop(class(x), " and ", class(y), " do not currently support the ", 
                 "'pintersect' method.")
          }
)

#' @importFrom methods setMethod
#' @importMethodsFrom IRanges psetdiff
#' 
#' @export
setMethod("psetdiff",
          c("GTuples", "GTuples"),
          function(x, y, ignore.strand = FALSE, ...) {
            stop(class(x), " and ", class(y), " do not currently support the ", 
                 "'psetdiff' method.")
          }
)

#' @importFrom methods setMethod
#' @importMethodsFrom IRanges psetdiff
#' 
#' @export
setMethod("psetdiff", c
          ("GTuples", "GTuplesList"),
          function(x, y, ignore.strand = FALSE, ...) {
            stop(class(x), " and ", class(y), " do not currently support the ", 
                 "'psetdiff' method.")
          }
)

#' @importFrom methods setMethod
#' @importMethodsFrom IRanges psetdiff
#' 
#' @export
setMethod("psetdiff", 
          c("GTuplesList", "GTuplesList"),
          function(x, y, ...) {
            stop(class(x), " and ", class(y), " do not currently support the ", 
                 "'psetdiff' method.")
          }
)

#' @importFrom methods setMethod
#' @importMethodsFrom IRanges pgap
#' 
#' @export
setMethod("pgap", 
          c("GTuples", "GTuples"),
          function(x, y, ignore.strand = FALSE, ...) {
            stop(class(x), " and ", class(y), " do not currently support the ", 
                 "'pgap' method.")
          }
)
