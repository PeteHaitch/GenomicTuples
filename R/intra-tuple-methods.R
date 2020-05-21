### =========================================================================
### Intra-tuple methods
### -------------------------------------------------------------------------
###
### The methods documented in this page on this page override those in 
### GenomicRanges intra-range-methods.R. Basically, I allow some of these 
### methods (with modifications) and don't allow others, at least for now.
### shift()
### narrow()
### flank()
### promoters()
### reflect()
### resize()
### restrict()
### trim()
### Zooming (symmetrically scales the width).
###
### Some of these methods could be defined for GTuples via inheritance but they 
### would effectively just treat them as GRanges, which may not play nice with
### the internalPos slot. But, and more importantly, it's not clear to me that 
### these methods are sensible or necessary for intra-tuples; I'm happy to 
### implement these if there is a good use case.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### shift()
###

#' @importMethodsFrom IRanges shift
#' @importMethodsFrom stats4 update
#' 
#' @export
setMethod("shift", 
          "GTuples",
          function(x, shift = 0L, use.names = TRUE) {
            new_ranges <- shift(ranges(x), shift = shift, use.names = use.names)
            if (!is.null(x@internalPos)) {
              new_internalPos <- x@internalPos + as.integer(shift)
            } else{
              new_internalPos <- x@internalPos
            }
            update(x, ranges = new_ranges, internalPos = new_internalPos)
          }
)

#' @importMethodsFrom IRanges shift
#' 
#' @export
setMethod("shift", 
          "GTuplesList",
          function(x, shift = 0L, use.names = TRUE) {
            unlisted_x <- unlist(x, use.names = FALSE)
            if (!is(shift, "List")) 
              shift <- as(shift, "List")
            shift <- S4Vectors:::VH_recycle(shift, x, "shift", "x")
            unlisted_shift <- unlist(shift, use.names = FALSE)
            unlisted_ans <- shift(x = unlisted_x, 
                                  shift = unlisted_shift, 
                                  use.names = use.names)
            relist(unlisted_ans, x)
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### narrow()
###
### Method for GTuples defined via inheritance to GRanges

#' @importMethodsFrom IRanges narrow
#' 
#' @export
setMethod("narrow", 
          "GTuples", 
          function(x, start = NA, end = NA, width = NA, use.names = TRUE) {
            stop(class(x), " do not currently support the 'narrow' method.")
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### flank()
###

#' @importMethodsFrom IRanges flank
#' 
#' @export
setMethod("flank", 
          "GTuples", 
          function(x, width, start = TRUE, both = FALSE, use.names = TRUE, 
                   ignore.strand = FALSE) {
            stop(class(x), " do not currently support the 'flank' method.")
          }
)

#' @importMethodsFrom IRanges flank
#' 
#' @export
setMethod("flank", 
          "GTuplesList", 
          function(x, width, start = TRUE, both = FALSE, use.names = TRUE, 
                   ignore.strand=FALSE) { 
            stop(class(x), " do not currently support the 'flank' method.")
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### promoters()
###

#' @importMethodsFrom IRanges promoters
#' 
#' @export
setMethod("promoters", 
          "GTuples", 
          function(x, upstream = 2000, downstream = 200, ...) {
            stop(class(x), " do not currently support the 'promoters' method.")
          }
)

#' @importMethodsFrom IRanges promoters
#' 
#' @export
setMethod("promoters", 
          "GTuplesList", 
          function(x, upstream = 2000, downstream = 200, ...) { 
            stop(class(x), " do not currently support the 'promoters' method.")
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### reflect()
###

### NOTE: Not currently implemented for GenomicRanges and therefore not 
###       implemented for GTuples.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### resize()
###

#' @importMethodsFrom IRanges resize
#' 
#' @export
setMethod("resize", 
          "GTuples", 
          function(x, width, fix = "start", use.names = TRUE, 
                   ignore.strand = FALSE) {
            stop(class(x), " do not currently support the 'resize' method.")
          }
)

#' @importMethodsFrom IRanges resize
#' 
#' @export
setMethod("resize", 
          "GTuplesList", 
          function(x, width, fix = "start", use.names = TRUE, 
                   ignore.strand = FALSE) {
            stop(class(x), " do not currently support the 'resize' method.")
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### restrict()
###

#' @importMethodsFrom IRanges restrict
#' 
#' @export
setMethod("restrict", 
          "GTuplesList", 
          function(x, start = NA, end = NA, keep.all.ranges = FALSE, 
                   use.names = TRUE) { 
            stop(class(x), " do not currently support the 'restrict' method.")
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### trim()
###

# NOTE: trim for GTuples defined via inheritance to GenomicRanges.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Zooming (symmetrically scales the width).
###

#' @export
setMethod("Ops", 
          c("GTuples", "numeric"),
          function(e1, e2) {
            stop(class(e1), " do not currently support the 'zoom' method.")
          }
)
