### =========================================================================
### Comparing and ordering genomic tuples
### -------------------------------------------------------------------------
### I. UNIQUE AND DUPLICATED ELEMENTS WITHIN A GTuples OBJECT
### ---------------------------------------------------------------
### Two elements of a GTuples object (i.e. two genomic tuples) are
### considered equal iff they are on the same underlying sequence and strand,
### and have the same positions. duplicated() and unique() on a
### GTuples object are conforming to this.
###
### II. ORDERING THE ELEMENTS OF A GTuples OBJECT
### ---------------------------------------------------
### The "natural order" for the elements of a GTuples object is to order
### them (a) first by sequence level, (b) then by strand, (c) then by pos_{1}, 
### ..., pos_{m}. This way, the space of genomic tuples is totally ordered.
### order(), sort(), and rank() on a GTuples object are using this
### "natural order".
###
### III. ELEMENT-WISE (AKA "PARALLEL") COMPARISON OF 2 GTuples OBJECTS
### ------------------------------------------------------------------------
### We want the "==", "!=", "<=", ">=", "<" and ">" operators between 2
### GTuples objects to be compatible with the "natural order" defined
### previously. Defining those operators when the 2 objects have *identical*
### seqlevels() is straighforward but we can in fact extend this comparison
### to the following situation:
###   (A) 'e1' and 'e2' have compatible sets of underlying sequences, that is,
###       'seqinfo(e1)' and 'seqinfo(e2)' can be merged.
###   (B) 'seqlevels(e1)' and 'seqlevels(e2)' are in the same order. Note that
###       (A) guarantees that the seqlevels of one is a subset of the seqlevels 
###       of the other. (B) is saying that this subset should be a subsequence.
### Pre-comparison step: if (A) and (B) are satisfied, then the 2 seqinfo() are
### merged and the seqlevels() of the result is assigned back to each object
### to pcompare. This is a way to have 2 objects with identical seqlevels()
### before the comparison can actually be performed and meaningful.
### The reason (B) is required for the pre-comparison step is because we want
### this step to preserve the original order of the seqlevels() in *both*
### objects. Without this precaution, the expected anti-symetric property of
### some operators would not be satisfied e.g. 'any(e1 < e2 & e2 < e1)' could
### be TRUE.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### pcompare() and related methods.
###
### pcompare() is based on the method defined for GRanges objects but cannot 
### explicity inherit the method due to the internalPos slot in GTuples 
### objects. However, unlike the pcompare() method defined for GRanges, the 
### pcompare() method for GTuples requires that both x and y have the same 
### length. I also define the element wise (aka "parallel") operators '<=' and 
### '=='. The other element wise operators (`!=`, `>=`, `<`, `>`) work 
### out-of-the-box on GTuples objects via inheritance from GRanges -> Vector.

# .pcompare_GTuples is adapted from GenomicRanges:::.pcompare_GenomicRanges.
# On 2 GTuples objects, it returns one of the 3 codes: If the first tuple in 
# the pair is "<" than the second tuple then the return value for that element 
# is < 0, if the first tuple in the pair is "==" the second tuple then the 
# return value is 0, and if the first tuple is ">" that the second tuple then 
# the return value is > 0.
#' @importMethodsFrom GenomeInfoDb seqlevels "seqlevels<-" seqinfo
#' @importMethodsFrom GenomicRanges granges
#' @importMethodsFrom S4Vectors pcompare
.pcompare_GTuples <- function(x, y) {
  
  # Different to comparing GRanges, I only allow comparison if x, y have same
  # length.
  # shortened error message because a long error trigger line formatting
  # that breaks the testthat error parser.

  # This is where .pcompare_GTuples really differs from .pcompare_GenomicRanges
  # NOTE: moved this up because the next 'if' will fail on NA != NA
  if (is.na(size(x)) || is.na(size(y))) {
    stop("Cannot pcompare empty '", class(x), "'.")
  }
  
  # Check 'size' is identical
  if (size(x) != size(y)) {
    stop("Cannot pcompare '", class(x), "' objects of different 'size'.")
  }
  
  if (size(x) <= 2L) {
    # Use the GRanges comparison method 
    # Can't use callNextMethod() because it breaks "<=", "<", etc.
    #callNextMethod()
    pcompare(granges(x), granges(y))
  } else {
    # Otherwise, use method specifically written for m-tuples (m > 2)
    
    # Pre-comparison step (see above for details).
    # merge() will fail if 'x' and 'y' don't have compatible underlying
    # sequences.
    seqinfo <- merge(seqinfo(x), seqinfo(y))
    seqlevels <- seqlevels(seqinfo)
    if (any(diff(match(seqlevels(y), seqlevels)) < 0L)) {
      stop("the 2 objects to pcompare have seqlevels in incompatible orders")
    }
    # This should only insert new seqlevels in the existing ones i.e. it
    # should NEVER drop or reorder existing levels
    seqlevels(x) <- seqlevels(y) <- seqlevels
    
    if (size(x) == 1L) {
      val <- .Call(Cpp_GenomicTuples_pcompareGTuples, 
                   as.integer(seqnames(x)) - as.integer(seqnames(y)), 
                   as.integer(strand(x)) - as.integer(strand(y)), 
                   as.matrix(start(x) - start(y))
      )
    } else if (size(x) > 1L) {
      # If lengths are equal then no need to recycle, which is faster.
      if (isTRUE(length(x) == length(y))) {
        val <- .Call(Cpp_GenomicTuples_pcompareGTuples, 
                     as.integer(seqnames(x)) - as.integer(seqnames(y)),
                     as.integer(strand(x)) - as.integer(strand(y)),
                     cbind(start(x) - start(y), x@internalPos - y@internalPos, 
                           end(x) - end(y))
        )
      } else {
        # Lengths are not equal so must recycle, which is slower.
        int_internal_pos <- .matrixDiffWithRecycling(x@internalPos, 
                                                     y@internalPos)
        val <- .Call(Cpp_GenomicTuples_pcompareGTuples, 
                     as.integer(seqnames(x)) - as.integer(seqnames(y)), 
                     as.integer(strand(x)) - as.integer(strand(y)),
                     cbind(start(x) - start(y), int_internal_pos, 
                           end(x) - end(y))
        )
      }
    }
    val
  }
}

#' @importFrom methods setMethod
#' @importMethodsFrom S4Vectors pcompare
#' 
#' @export
setMethod("pcompare", 
          c("GTuples", "GTuples"), 
          function(x, y) {
            .pcompare_GTuples(x, y)
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### duplicated()  
###
### Can't rely on using duplicated() via inheritance from Vector because 
### GTuples inherits from GRanges, rather than from `Vector` directly.
### Furthermore, simply deferring to the duplicated() method for GRanges would 
### only check pos1, posm of of each tuple, which is okay provided size < 3 but 
### not if size >= 3.
###
### unique() will work out-of-the-box on an GTuples object thanks to the 
### method for GRanges objects, which inherits from Vector objects.

# NOTE: Don't explicitly import duplicated.data.table but rely on the method 
#       being available since the S3 generic is available via base.
.duplicated.GTuples <- function(x, incomparables = FALSE, fromLast = FALSE) {
  if (!identical(incomparables, FALSE)) 
    stop("\"duplicated\" method for '", class(x), "' objects only accepts ", 
         "'incomparables = FALSE'")
  duplicated(.GT2DT(x), incomparables = incomparables, fromLast = fromLast)
}

# S3/S4 combo for duplicated.GTuples

# NOTE: This should really just use the @export tag. However, because 
#       BiocGenerics redefines duplicated as an S4 generic, roxygen2 doesn't 
#       see this as an S3 method but rather as a function, and so adds 
#       export(duplicated.GTuples) to the NAMESPACE rather than 
#       S3method(duplicated, GTuples).
# NOTE: Both export and define duplicated.GTuples as an S3 method so that (a) 
#       it can be called directly, (b) tab-completion on the name of the generic 
#       shows it, and (c) methods() doesn't asterisk it (based on advice 
#       from GenomicRanges' NAMESPACE).
#' @export
#' @rawNamespace S3method(duplicated, GTuples)
duplicated.GTuples <- function(x, incomparables = FALSE, ...) {
  .duplicated.GTuples(x, incomparables = incomparables, ...)
}

#' @importFrom methods setMethod
#' 
#' @export
setMethod("duplicated", 
          "GTuples", 
          .duplicated.GTuples
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### match()
###
### %in%, findMatches(), countMatches() will work out-of-the-box on GTuples 
### objects thanks to the method for Vector objects.

# Effectively just calls findOverlaps with type = equal.
#' @importFrom methods setMethod
#' @importFrom S4Vectors isSingleNumberOrNA isTRUEorFALSE
#' @importMethodsFrom GenomeInfoDb seqinfo
#' @importMethodsFrom IRanges findOverlaps
#' 
#' @export
setMethod("match", 
          c("GTuples", "GTuples"), 
          function(x, table, nomatch = NA_integer_, incomparables = NULL, 
                   ignore.strand = FALSE) {
            
            if (!isSingleNumberOrNA(nomatch)) {
              stop("'nomatch' must be a single number or NA")
            }
            if (!is.integer(nomatch)) {
              nomatch <- as.integer(nomatch)
            }
            if (!is.null(incomparables)) {
              stop("\"match\" method for GenomicRanges objects only accepts ",
                   "'incomparables = NULL'")
            }
            if (!isTRUEorFALSE(ignore.strand)) {
              stop("'ignore.strand' must be TRUE or FALSE")
            }
            ## Calling merge() is the way to check that 'x' and 'table' are 
            ## based on the same reference genome.
            merge(seqinfo(x), seqinfo(table))
            
            val <- findOverlaps(x, table, type = "equal", select = "first", 
                          ignore.strand = ignore.strand)
            val[is.na(val)] <- nomatch
            val
          }
)

# NOTE: Need to explicitly import %in%.
#' @importMethodsFrom S4Vectors %in%

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### selfmatch()
###

# Can't defer to selfMatch,GenomicRanges-method introduced in 
# https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/GenomicRanges@116115 
# so include this kludge to restore previous behaviour
#' @importFrom methods setMethod
#' @importFrom S4Vectors selfmatch
#' @export
setMethod("selfmatch", 
          "GTuples",
          function(x, ignore.strand = FALSE, ...) {
            if (!isTRUEorFALSE(ignore.strand)) {
              stop("'ignore.strand' must be TRUE or FALSE")
            }
            match(x, x, ignore.strand = ignore.strand, ...)
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### order() and related methods.
###
### The order() and rank() methods for GTuples objects are consistent with the 
### order implied by pcompare().
### is.unsorted() is a quick/cheap way of checking whether a GTuples
### object is already sorted, e.g., called prior to a costly sort.
### sort is defined via inheritance to GRanges

.GTuples.asIntegerTuples <- function(x, ignore.strand = FALSE) {
  a <- S4Vectors:::decodeRle(seqnames(x))
  if (ignore.strand) {
    b <- integer(length(x))
  } else {
    b <- S4Vectors:::decodeRle(strand(x))
  }
  c <- start(x)
  if (size(x) > 2L) {
    ipd <- IPD(x)
    # NOTE: This is somewhat inefficient since it constructs the matrix and then
    #       splits it into a list of vectors. 
    # TODO (longterm): Do this without the aforementioned overhead.
    rest <- split(ipd, rep(seq_len(ncol(ipd)), each = nrow(ipd)))
  } else {
    rest <- list()
  }
  c(list(a, b, c), rest)
}

#' @importFrom data.table := .GRP 
#' @importFrom methods callNextMethod setMethod
#' @importFrom S4Vectors isTRUEorFALSE
#' 
#' @export
setMethod("is.unsorted", "GTuples", 
          function(x, na.rm = FALSE, strictly = FALSE, ignore.strand = FALSE) {
            
            if (!identical(na.rm, FALSE)) {
              warning("\"is.unsorted\" method for '", class(x), "' objects ",
                      "ignores the 'na.rm' argument")
            }
            if (!isTRUEorFALSE(strictly)) {
              stop("'strictly' must be TRUE of FALSE")
            }
            if (is.na(size(x))) {
              return(FALSE)
            }
            
            if (size(x) < 3L) {
              callNextMethod()
            } else {
              # NOTE: This actually sorts the object (or, rather, the 
              #       data.table representation of the object). Despite this, 
              #       it's still very fast because data.table is so fast.
              key <- c("seqnames", "strand", paste0("pos", seq_len(size(x))))
              y <- .GT2DT(x, ignore.strand = ignore.strand)
              is.unsorted(y[, grp := .GRP, keyby = key][, grp], 
                          strictly = strictly)
            }
          }
)
#' @importFrom utils globalVariables
globalVariables("grp")

#' @importFrom data.table := .I setorderv
#' @importFrom S4Vectors isTRUEorFALSE
.order.GTuples <- function(x, decreasing = FALSE, ignore.strand = FALSE) {
  if (!isTRUEorFALSE(decreasing)) {
    stop("'decreasing' must be TRUE or FALSE")
  }
  key <- c("seqnames", "strand", paste0("pos", seq_len(size(x))))
  dt <- .GT2DT(x, ignore.strand)[, idx := .I]
  if (decreasing) {
    order <- -1L
  } else {
    order <- 1L
  }
  setorderv(dt, key, order = order)
  dt[, idx]
  # TODO: Use this slightly faster version once data.table:::forderv() is 
  #       exported.
  # data.table:::forderv(.GT2DT(x, ignore.strand), 
  #                      by = c("seqnames", "strand", paste0("pos", seq_len(size(x)))), 
  #                      sort = TRUE, 
  #                      retGrp = FALSE, 
  #                      order = order), 
  #                      na.last = na.last)
}
#' @importFrom utils globalVariables
globalVariables("idx")

# TODO: Support the 'ignore.strand' argument once order,GenomicRanges-method 
#       does.
#' @importFrom methods setMethod
#' @importFrom S4Vectors isTRUEorFALSE
#' @importMethodsFrom GenomeInfoDb seqnames
#' 
#' @export
setMethod("order", 
          "GTuples", 
          function(..., na.last = TRUE, decreasing = FALSE, 
                   method = c("shell", "radix")) {
            
            if (!isTRUEorFALSE(decreasing)) {
              stop("'decreasing' must be TRUE or FALSE")
            }
            
            args <- list(...)
            
            size <- sapply(args, size)
            
            if (all(is.na(size))) {
              return(integer(0L))
            }
            
            if (!.zero_range(size)) {
              stop("All '", class(args[[1]]), "' objects must have the same ", 
                   "'size'.")
            }
            
            if (length(args) == 1L) {
              return(.order.GTuples(args[[1L]], decreasing))
            } else {
              # TODO: Pass ignore.strand to .GTuples.asIntegerTuples once 
              #       order() supports this argument.
              order_args <- c(unlist(lapply(args, .GTuples.asIntegerTuples),
                                     recursive = FALSE, use.names = FALSE),
                              list(na.last = na.last, 
                                   decreasing = decreasing))
              do.call(order, order_args)
            }
          }
)

# NOTE: Need to (temporarily?) redefine sort,GTuples-method (see 
#       https://github.com/PeteHaitch/GenomicTuples/issues/31)
#' @importMethodsFrom S4Vectors extractROWS
.sort.GTuples <- function(x, decreasing = FALSE, ignore.strand = FALSE, by) {
  if (is.na(size(x))) {
    return(x)
  }
  if (missing(by)) {
    oo <- .order.GTuples(x, decreasing, ignore.strand)
  } else {
    if (!identical(ignore.strand, FALSE)) {
      warning("'ignore.strand' ignored when 'by' is specified")
    }
    oo <- S4Vectors:::orderBy(by, x, decreasing = decreasing)
  }
  extractROWS(x, oo)
}
### S3/S4 combo for sort.GenomicRanges
# NOTE: Both export and define sort.GTuples as an S3 method so that (a) it can 
#       be called directly, (b) tab-completion on the name of the generic 
#       shows it, and (c) methods() doesn't asterisk them (based on advice 
#       from GenomicRanges' NAMESPACE).
#' @rawNamespace S3method(sort, GTuples)
#' @export
sort.GTuples <- function(x, decreasing = FALSE, ...)
  .sort.GTuples(x, decreasing = decreasing, ...)
#' @importFrom methods setMethod
#' 
#' @export
setMethod("sort", "GTuples", .sort.GTuples)
