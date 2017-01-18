### =========================================================================
### GTuplesList objects
### -------------------------------------------------------------------------
###

#' @importFrom methods setClass
#' @export
setClass("GTuplesList",
         contains = c("GRangesList"),
         slots = list(
           unlistData = "GTuples",
           elementMetadata = "DataFrame"
         ),
         prototype = prototype(
           elementType = "GTuples"
         )
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

INVALID.GT.COLNAMES <- c("seqnames", "ranges", "strand",
                         "seqlevels", "seqlengths", "isCircular",
                         #"genome",
                         "start", "end", "width", "element",
                         "tuples", "internalPos", "size")

.valid.GTuplesList.mcols <- function(object) {
  
  object_mcols <- object@elementMetadata
  if (nrow(object_mcols) != length(object)) {
    return("'mcols(object)' has an incorrect number of rows")
  }
  if (any(INVALID.GT.COLNAMES %in% colnames(mcols(object)))) {
    return("names of metadata columns cannot be one of ", 
           paste0("\"", INVALID.GT.COLNAMES, "\"", collapse = ", "))
  }
  if (!is.null(rownames(object_mcols))) {
    return("'mcols(object)' cannot have row names")
  }
  NULL
}

.valid.GTuplesList <- function(x) {
  c(.valid.GTuplesList.mcols(x))
}

#' @importFrom S4Vectors setValidity2
setValidity2("GTuplesList", .valid.GTuplesList)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

#' @importFrom IRanges PartitioningByEnd
#' @importFrom methods is
#' 
#' @export
GTuplesList <- function(...) {
  listData <- list(...)
  if (length(listData) == 0L) {
    unlistData <- GTuples()
  } else {
    if (length(listData) == 1L && is.list(listData[[1L]])) {
      listData <- listData[[1L]]
    }
    if (!all(vapply(listData, is, logical(1L), "GTuples"))) {
      stop("all elements in '...' must be GTuples objects")
    }
    sizes <- vapply(listData, size, integer(1L))
    if (!.zero_range(sizes) && !isTRUE(all(is.na(sizes)))) {
      stop("all GTuples in '...' must have the same 'size'")
    }
    unlistData <- suppressWarnings(do.call("c", unname(listData)))
  }
  
  relist(unlistData, PartitioningByEnd(listData))
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Tuples methods
###

#' @include AllGenerics.R
#' 
#' @importFrom methods setMethod
#' 
#' @export
setMethod("size", 
          "GTuplesList", 
          function(x) {
            x@unlistData@size
          }
)

#' @importFrom methods setMethod
#' @importFrom S4Vectors List
#' 
#' @export
setMethod("tuples",
          "GTuplesList",
          function(x) {
            if (is.na(size(x))) {
              List(lapply(x, function(x){matrix()}))
            } else{
              unlisted_x <- unlist(x, use.names = FALSE)
              unlisted_ans <- tuples(unlisted_x)
              ans <- relist(unlisted_ans, x)
              ans
            }
          }
)

#' @importFrom methods setReplaceMethod
#' 
#' @export
setReplaceMethod("tuples", 
                 "GTuplesList",
                 function(x, value) {
                   tuples(x@unlistData) <- unlist(value, use.names = FALSE)
                   x
                 }
)

#' @include AllGenerics.R
#' 
#' @export
setMethod("IPD", 
          "GTuplesList", 
          function(x) {
            unlisted_x <- unlist(x, use.names = FALSE)
            relist(IPD(unlisted_x), x)
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion.
###

#' @importClassesFrom GenomicRanges GRanges
#' @importFrom GenomicRanges GRangesList
#' @importFrom methods setAs as
setAs("GTuplesList", "GRangesList", 
      function(from) {
        if (length(from)) {
          GRangesList(lapply(from, as, "GRanges"))
        } else {
          GRangesList()
        }
      }
)

# TODO (longterm): grglist when method is implemented in GenomicRanges

# NOTE: as.list, stack defined via inheritance to GRangesList.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting.
###

# "[", "[<-" and "[[<-" defined via inheritance to GRangesList

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Going from GTuples to GTuplesList with extractList() and family.
###

#' @importFrom methods setMethod
#' @importMethodsFrom IRanges relistToClass
#' 
#' @export
setMethod("relistToClass", 
          "GTuples", 
          function(x) {
            "GTuplesList"
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### show method.
###

#' @importMethodsFrom methods show
#' @importFrom methods setMethod
#' 
#' @export
setMethod("show", 
          "GTuplesList",
          function(object) {
            GenomicRanges:::showList(object, showGTuples, FALSE)
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Deconstruction/reconstruction of a GTuplesList into/from a GTuples
### object.
###
### For internal use only (not exported).
###

# Can use GenomicRanegs:::deconstructGRLintoGR and 
# GenomicRanges:::reconstructGRLfromGR via inheritance should these be 
# required.
