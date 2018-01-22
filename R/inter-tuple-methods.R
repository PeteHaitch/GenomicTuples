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
            stop(class(x), " do not currently support the 'range' method.")
          }
)

#' @export
setMethod("range", 
          "GTuplesList", 
          function(x, ..., ignore.strand = FALSE, na.rm = FALSE) { 
            stop(class(x), " do not currently support the 'range' method.")
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### reduce()
###

#' @importMethodsFrom IRanges reduce
#' 
#' @export
setMethod("reduce", 
          "GTuples", 
          function(x, drop.empty.ranges = FALSE, min.gapwidth = 1L, 
                   with.revmap = FALSE, with.mapping = FALSE, 
                   with.inframe.attrib = FALSE, ignore.strand = FALSE) { 
            stop(class(x), " do not currently support the 'reduce' method.")
          }
)

#' @importMethodsFrom IRanges reduce
#' 
#' @export
setMethod("reduce", 
          "GTuplesList", 
          function(x, drop.empty.ranges = FALSE, min.gapwidth = 1L, 
                   with.revmap = FALSE, with.mapping = FALSE, 
                   with.inframe.attrib = FALSE, ignore.strand = FALSE) {
            stop(class(x), " do not currently support the 'reduce' method.")
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### gaps()
###

#' @importMethodsFrom IRanges gaps
#' @importMethodsFrom GenomeInfoDb seqlengths
#' 
#' @export
setMethod("gaps", 
          "GTuples", 
          function(x, start = 1L, end = seqlengths(x)) {
            stop(class(x), " do not currently support the 'gaps' method.")
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### disjoin()
###

#' @importMethodsFrom IRanges disjoin
#' 
#' @export
setMethod("disjoin", 
          "GTuples", 
          function(x, start = 1L, end = seqlengths(x)) {
            stop(class(x), " do not currently support the 'disjoin' method.")
          }
)

#' @importMethodsFrom IRanges disjoin
#' 
#' @export
setMethod("disjoin", 
          "GTuplesList", 
          function(x, start = 1L, end = seqlengths(x)) {
            stop(class(x), " do not currently support the 'disjoin' method.")
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### isDisjoint()
###

#' @importMethodsFrom IRanges isDisjoint
#' 
#' @export
setMethod("isDisjoint", 
          "GTuples", 
          function(x, ignore.strand = FALSE) {
            stop(class(x), " do not currently support the 'isDisjoint' method.")
          }
)

#' @importMethodsFrom IRanges isDisjoint
#' 
#' @export
setMethod("isDisjoint", 
          "GTuplesList",
          function(x, ignore.strand = FALSE) {
            stop(class(x), " do not currently support the 'isDisjoint' method.")
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### disjointBins()
###

#' @importMethodsFrom IRanges disjointBins
#' @export
setMethod("disjointBins", 
          "GTuples", 
          function(x, ignore.strand = FALSE) {
            stop(class(x), " do not currently support the 'disjointBins' ", 
                 "method.")
          }
)
