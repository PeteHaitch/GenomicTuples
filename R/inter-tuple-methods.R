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


#' @importFrom methods setMethod
#' 
#' @export
setMethod("range", 
          "GTuples", 
          function(x, ..., ignore.strand = FALSE, na.rm = FALSE) {
            stop(class(x), " do not currently support the 'range' method.")
          }
)

#' @importFrom methods setMethod
#' 
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

#' @importFrom methods setMethod
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

#' @importFrom methods setMethod
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

#' @importFrom methods setMethod
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

#' @importFrom methods setMethod
#' @importMethodsFrom IRanges disjoin
#' 
#' @export
setMethod("disjoin", 
          "GTuples", 
          function(x, start = 1L, end = seqlengths(x)) {
            stop(class(x), " do not currently support the 'disjoin' method.")
          }
)

#' @importFrom methods setMethod
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

#' @importFrom methods setMethod
#' @importMethodsFrom IRanges isDisjoint
#' 
#' @export
setMethod("isDisjoint", 
          "GTuples", 
          function(x, ignore.strand = FALSE) {
            stop(class(x), " do not currently support the 'isDisjoint' method.")
          }
)

#' @importFrom methods setMethod
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

#' @importFrom methods setMethod
#' @importMethodsFrom IRanges disjointBins
#' @export
setMethod("disjointBins", 
          "GTuples", 
          function(x, ignore.strand = FALSE) {
            stop(class(x), " do not currently support the 'disjointBins' ", 
                 "method.")
          }
)
