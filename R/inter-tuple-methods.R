### =========================================================================
### Inter-tuple methods
### -------------------------------------------------------------------------
###

### The methods documented in this page on this page override those in 
### GenomicRanges inter-range-methods.R. Basically, I don't allow any of these 
### methods, at least for now.
### range()
### reduce()
### gaps()
### disjoin()
### isDisjoint()
### disjointBins()
###

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### range()
###

#' @export
setMethod("range", 
          "GTuples", 
          function(x, ..., ignore.strand = FALSE, na.rm = FALSE) {
            stop(paste0(class(x), " do not currently support the 'range' ", 
                        "method."))
          }
)

#' @export
setMethod("range", 
          "GTuplesList", 
          function(x, ..., ignore.strand = FALSE, na.rm = FALSE) { 
            stop(paste0(class(x), " do not currently support the 'range' ", 
                        "method."))
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### reduce()
###

#' @export
setMethod("reduce", 
          "GTuples", 
          function(x, drop.empty.ranges = FALSE, min.gapwidth = 1L, 
                   with.revmap = FALSE, with.mapping = FALSE, 
                   with.inframe.attrib = FALSE, ignore.strand = FALSE) { 
            stop(paste0(class(x), " do not currently support the 'reduce' ", 
                        "method."))
          }
)

#' @export
setMethod("reduce", 
          "GTuplesList", 
          function(x, drop.empty.ranges = FALSE, min.gapwidth = 1L, 
                   with.revmap = FALSE, with.mapping = FALSE, 
                   with.inframe.attrib = FALSE, ignore.strand = FALSE) {
            stop(paste0(class(x), " do not currently support the 'reduce' ",  
                        "method."))
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### gaps()
###

#' @export
setMethod("gaps", 
          "GTuples", 
          function(x, start = 1L, end = seqlengths(x)) {
            stop(paste0(class(x), " do not currently support the 'gaps' ",
                        "method."))
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### disjoin()
###

#' @export
setMethod("disjoin", 
          "GTuples", 
          function(x, start = 1L, end = seqlengths(x)) {
            stop(paste0(class(x), " do not currently support the 'disjoin' ", 
                        "method."))
          }
)

#' @export
setMethod("disjoin", 
          "GTuplesList", 
          function(x, start = 1L, end = seqlengths(x)) {
            stop(paste0(class(x), " do not currently support the 'disjoin' ", 
                        "method."))
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### isDisjoint()
###

#' @export
setMethod("isDisjoint", 
          "GTuples", 
          function(x, ignore.strand = FALSE) {
            stop(paste0(class(x), " do not currently support the ", 
                        "'isDisjoint' method."))
          }
)

#' @export
setMethod("isDisjoint", 
          "GTuplesList",
          function(x, ignore.strand = FALSE) {
            stop(paste0(class(x), " do not currently support the ", 
                        "'isDisjoint' method."))
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### disjointBins()
###

#' @export
setMethod("disjointBins", 
          "GTuples", 
          function(x, ignore.strand = FALSE) {
            stop(paste0(class(x), " do not currently support the ", 
                        "'disjointBins' method."))
          }
)
