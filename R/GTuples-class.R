### =========================================================================
### GTuples objects: tuples of genomic positions
### -------------------------------------------------------------------------
###

setClassUnion(name = "matrixOrNULL", members = c("matrix", "NULL"))

#' @export
setClass("GTuples",
         contains = "GRanges",
         slots = list(
           internalPos = "matrixOrNULL", 
           size = "integer"),
         prototype = prototype(
           internalPos = NULL,
           size = NA_integer_)
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###

.valid.GTuples.pos <- function(object) {
  
  # Check tuples are sorted; only required if m > 1.
  if (isTRUE(object@size > 2L) && length(object) != 0L) {
    if (!.allTuplesSortedCpp(
      pos1 = start(object), 
      internal_pos = object@internalPos, 
      posm = end(object))
    ) {
      return(paste0("positions in each tuple must be sorted in strictly ", 
                    "increasing order, i.e. 'pos1' < ... < ", 
                    paste0("'pos", object@size, "'")))
    }
  } else if (isTRUE(object@size == 2L)) {
    if (isTRUE(any(object@ranges@width <= 1L))) {
      return(paste0("positions in each tuple must be sorted in strictly ", 
                    "increasing order, i.e. 'pos1' < 'pos2'"))
    }
  }
  
  # Check all tuples have positive positions.
  # Only need to check pos1 and posm because already checked 
  # pos1 < internalPos < posm
  # NB: min(x) < 0 is faster than any(x < 0)
  if (!is.na(object@size) && length(object) != 0L) {
    if (min(start(object)) < 0L || min(end(object)) < 0L) {
      return("positions in each tuple must be positive integers.")
    }
  }
  
  NULL
}

INVALID.GT.COLNAMES <- c(GenomicRanges:::INVALID.GR.COLNAMES, 
                         "tuples", "internalPos", "size")

.valid.GTuples.mcols <- function(object) {
  
  if (any(INVALID.GT.COLNAMES %in% colnames(mcols(object)))) {
    return(paste0("names of metadata columns cannot be one of ",
                  paste0("\"", INVALID.GT.COLNAMES, "\"", collapse = ", ")))
  }
  NULL
}

.valid.GTuples <- function(object) {
  
  # Include all .valid.GTuples.* functions in this vector
  msg <- c(.valid.GTuples.pos(object), .valid.GTuples.mcols(object))
  
  if (is.null(msg)) {
    return(TRUE)
  } else{
    msg
  }
}

#' @importFrom S4Vectors setValidity2
setValidity2("GTuples", .valid.GTuples)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom S4Vectors Rle
#' 
#' @export
GTuples <- function(seqnames = Rle(), 
                    tuples = matrix(), 
                    strand = Rle("*", length(seqnames)), 
                    ..., 
                    seqlengths = NULL, 
                    seqinfo = NULL) {
  
  # Only need to check the tuples, all others get checked by the GRanges 
  # constructor
  # Only check is numeric and later coerce to integer if necessary because of 
  # how R stores integers, e.g. 1 (numeric, not integer) vs. 1L (numeric and 
  # integer). User may skip the 'L' suffix but don't want to error on this.
  
  # Don't want tuples to contain mix of NA and non-NA
  NAs <- is.na(tuples)
  if (sum(NAs) > 0L && sum(!NAs) > 0L) {
    stop("'NA' detected in 'tuples'")
  }
  if (!all(is.na(tuples))) {
    if (!is.matrix(tuples) || !is.numeric(tuples)) {
      stop("'tuples' must be an integer matrix") 
    }
  }
  if (!is.integer(tuples)) {
    if (!all(is.na(tuples))) {
      warning("Converting 'tuples' to integer mode")
    }
    storage.mode(tuples) <- "integer"
  }
  
  # Get size of tuples
  if (all(is.na(tuples))) {
    size <- NA_integer_
  } else {
    size <- ncol(tuples)
  }
  
  # Create IRanges
  if (!is.na(size)) {
    ranges <- IRanges(start = tuples[, 1L], end = tuples[, size])
  } else {
    ranges <- IRanges()
  }
  
  # Create internalPos
  if (is.na(size) || size < 3L) {
    internalPos <- NULL
  } else {
    internalPos <- tuples[, seq.int(from = 2, to = size - 1, by = 1), 
                           drop = FALSE]
  }
  
  # Create GRanges
  gr <- GRanges(seqnames = seqnames, 
                ranges = ranges, 
                strand = strand, 
                seqlengths = seqlengths, 
                seqinfo = seqinfo, 
                ...)
  
  new("GTuples", gr, internalPos = internalPos, size = size)
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###

#' @importFrom S4Vectors isTRUEorFALSE
#' @importFrom stats setNames
setMethod("as.character", "GTuples", 
          function(x, ignore.strand = FALSE) {
            if (!isTRUEorFALSE(ignore.strand)) {
              stop("'ignore.strand' must be TRUE or FALSE")
            }
            if (length(x) == 0L) {
              return(setNames(character(0), names(x)))
            }
            ans <- paste0(seqnames(x), ":", 
                          apply(X = tuples(x), FUN = paste, MARGIN = 1, 
                                collapse = ","))
            names(ans) <- names(x)
            if (ignore.strand) {
              return(ans)
            }
            x_strand <- strand(x)
            if (all(x_strand == "*")) {
              return(ans)
            }
            setNames(paste0(ans, ":", x_strand), names(x))
          }
)

# NOTE: as.factor() via inheritance to GenomicRanges, which is okay because 
#       it calls the above as.character().
#' @importMethodsFrom S4Vectors as.factor
#' @importFrom S4Vectors decode
NULL

#' @importMethodsFrom GenomeInfoDb seqnames
#' @importFrom S4Vectors decode
#' 
#' @export
setMethod("as.data.frame", 
          "GTuples", 
          function(x, row.names = NULL, optional = FALSE, ...) {
            if (is.na(size(x))) {
              return(data.frame(seqnames = decode(seqnames(x)),
                                strand = decode(strand(x)),
                                stringsAsFactors = FALSE))
            } else {
              tuples <- tuples(x)
              if (missing(row.names)) {
                row.names <- names(x)
              }
              if (!is.null(names(x))) {
                names(x) <- NULL
              }
              mcols_df <- as.data.frame(mcols(x), ...)
              extraColumnNames <- GenomicRanges:::extraColumnSlotNames(x)
              extraColumnNames <- extraColumnNames[extraColumnNames != 
                                                     'internalPos']
              if (length(extraColumnNames) > 0L) {
                extraColumns <- GenomicRanges:::extraColumnSlotsAsDF(x)
                # Remove the internalPos slot from extraColumns to prevent it 
                # being twice-included.
                extraColumns <- extraColumns[colnames(extraColumns) != 
                                               'internalPos']
                mcols_df <- cbind(as.data.frame(extraColumns, ...), mcols_df)
              }
              data.frame(seqnames = decode(seqnames(x)), 
                         as.data.frame(tuples), 
                         strand = decode(strand(x)), 
                         mcols_df, 
                         row.names = row.names, 
                         stringsAsFactors = FALSE)
            }
          }
)

#' @importMethodsFrom GenomicRanges granges
#' @importMethodsFrom S4Vectors "mcols<-"
#' 
#' @export
setMethod("granges", 
          "GTuples",
          function(x, use.mcols = FALSE) {
            gr <- as(x, "GRanges")
            if (!use.mcols) {
              mcols(gr) <- NULL
            }
            gr
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Updating and cloning.
###
### An object is either 'update'd in place (usually with a replacement
### method) or 'clone'd (copied), with specified slots/fields overridden.
###
### From GenomicRanges-class.R "For an object with a pure S4 slot 
### representation, these both map to initialize. Reference classes will want 
### to override 'update'. Other external representations need further 
### customization." Note, however, that these are not exported from 
### GenomicRanges. 
###
### I think I can safely use these for GTuples via inheritance
### to GenomicRanges, but should be careful whenever using them and test well.
###

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining
###

### Not exported. 'x' *must* be an unnamed list of length >= 1 (not checked).

# NOTE: Without '@importMethodsFrom GenomeInfoDb merge' this doesn't work. 
#       This is despite merge being defined in base.
#' @importClassesFrom S4Vectors DataFrame
#' @importFrom S4Vectors isTRUEorFALSE
#' @importMethodsFrom GenomeInfoDb merge seqinfo seqnames
#' @importMethodsFrom IRanges ranges
#' @importMethodsFrom S4Vectors mcols
.unlist_list_of_GTuples <- function(x, ignore.mcols = FALSE) {
  
  if (!isTRUEorFALSE(ignore.mcols)) {
    stop("'ignore.mcols' must be TRUE or FALSE")
  }
  ans_class <- class(x[[1L]])
  ans_seqinfo <- do.call(merge, lapply(x, seqinfo))
  ans_seqnames <- do.call(c, lapply(x, seqnames))
  ans_ranges <- do.call(c, lapply(x, ranges))
  ans_strand <- do.call(c, lapply(x, strand))
  ans_internalPos <- do.call(rbind, lapply(x, function(xx) xx@internalPos))
  ans_size <- size(x[[1L]])
  if (ignore.mcols) {
    ans_mcols <- new("DataFrame", nrows = length(ans_ranges))
  } else {
    ans_mcols <- do.call(rbind, lapply(x, mcols, FALSE))
  }
  new(ans_class, 
      seqnames = ans_seqnames, 
      ranges = ans_ranges, 
      strand = ans_strand, 
      elementMetadata = ans_mcols, 
      seqinfo = ans_seqinfo, 
      size = ans_size, 
      internalPos = ans_internalPos)
}

#' @export
setMethod("c", 
          "GTuples", 
          function(x, ..., ignore.mcols = FALSE, recursive = FALSE) {
            if (!identical(recursive, FALSE)) {
              stop("'recursive' argument not supported")
            }
            if (missing(x)) {
              args <- unname(list(...))
            } else {
              args <- unname(list(x, ...))
            }
            # NOTE: "c" will error if there is no common class, e.g. 
            #        c(GRanges(), "1"). The following commented-out error makes 
            #        this explicit to the user for c,GTuples-method
            # if (!isTRUE(all(sapply(args, inherits, "GTuples")))) {
            #   stop("Cannot combine ", paste0(unique(sapply(args, class)),
            #                                  collapse = ' and '), " objects")
            # }
            sizes <- vapply(args, size, integer(1L))
            if (!.zero_range(sizes) && !isTRUE(all(is.na(sizes)))) {
              stop("Cannot combine ", 
                   paste0(unique(vapply(args, class, character(1L))), 
                          collapse = ' and '), 
                   " containing tuples of different 'size'.")
            }
            # "c" silently coerces to lowest common class, e.g., c(1, "next")
            # The following commented-out warning makes this explicit to the 
            # user for c,GTuples-method
#             if (!all(sapply(args, class) == class(args[[1]]))) {
#               warning(
#                 paste0("Not all elements are same class: ", 
#                        paste0(unique(sapply(args, class)), 
#                               collapse = ', '), 
#                        "\nResult will be coerced to lowest common ", 
#                        "class: ", 
#                        .lcc(x, ...)))
#             }
            .unlist_list_of_GTuples(args, ignore.mcols = ignore.mcols)
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters
###

# Defined via inheritance to GRanges or implemented in Tuples methods

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Splitting
###

# Via inheritance to split,Vector-method.

#' @importMethodsFrom S4Vectors split
NULL

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Setters
###

#' @importMethodsFrom GenomeInfoDb seqinfo<- seqnames<-
#' @importMethodsFrom IRanges ranges<-
NULL

# Defined via inheritance to GRanges or implemented in Tuples methods

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Tuples methods
###

#' @include AllGenerics.R
#' 
#' @export
setMethod("size", 
          "GTuples", 
          function(x) {
            x@size
          }
)

#' @include AllGenerics.R
#' 
#' @export
setMethod("tuples", 
          "GTuples", 
          function(x) {
            if (is.na(size(x))) {
              ans <- matrix()
            } else if (size(x) == 1L) {
              ans <- matrix(start(x), 
                            dimnames = list(NULL, paste0("pos", 
                                                         seq_len(size(x)))))
            } else{
              ans <- cbind(start(x), x@internalPos, end(x))
              colnames(ans) <- paste0("pos", seq_len(size(x)))
            }
            ans
          }
)

#' @importFrom IRanges IRanges
#' @importMethodsFrom stats4 update
#' 
#' @export
setReplaceMethod("tuples", 
                 "GTuples", 
                 function(x, value) {
                   if (!is(value, "matrix")) {
                     value <- as(value, "matrix")
                   }
                   mode(value) <- "integer"
                   n <- length(x)
                   k <- nrow(value)
                   if (k != n) {
                     stop(k, " elements in value to replace ", n, " elements")
                   }
                   m <- ncol(value)
                   if (m != size(x)) {
                     stop("Size of tuples in 'x' ", size(x), " not equal to ", 
                          "size of tuples in 'value' ", m)
                   }
                   if (is.na(m)) {
                     x
                   } else if (m == 1L) {
                     ranges <- IRanges(start = value[, 1L], end = value[, 1L])
                     internalPos <- NULL
                   } else if (m == 2L) {
                     ranges <- IRanges(start = value[, 1L], end = value[, 2L])
                     internalPos <- NULL
                   } else if (m > 2L) {
                     ranges <- IRanges(start = value[, 1], end = value[, m])
                     internalPos <- unname(value[, seq.int(from = 2L, 
                                                           to = m - 1L, 
                                                           by = 1L), 
                                                  drop = FALSE])
                   }
                   update(x, 
                          ranges = ranges, 
                          internalPos = internalPos,
                          check = TRUE)
                 }
)

#' @include AllGenerics.R
#' 
#' @export
setMethod("IPD", 
          "GTuples", 
          function(x) {
            size <- size(x)
            if (is.na(size)) {
              stop("Cannot compute IPD from an empty '", class(x), "'.")
            } else if (isTRUE(size == 1L)) {
              stop("It does not make sense to compute IPD when 'size' = 1.")
            } else if (isTRUE(size == 2L)) {
              ## width is not the same as distance ... at least for IRanges  
              ipd <- matrix(width(x) - 1L, ncol = 1L)
            } else {
              ipd <- .IPDCpp(
                pos1 = start(x), 
                internal_pos = matrix(x@internalPos, ncol = size - 2L), 
                posm = end(x)
              )
            }
            ipd
          }
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###

# extractROWS, "[" and replaceROWS defined via inheritance to methods 
# for GenomicRanges.

# TODO: Refactor to use replaceROWS(). This will make the code much simpler and 
#       avoid a lot of duplication with the above "replaceROWS" method 
#       (copied from GenomicRanges/GenomicRanges-class.R).
# TODO: Once "[<-",GenomicRanges-method uses replaceROWS,GenomicRanges-method, 
#       then should be able to migrate to using "[<-",GTuples-method defined 
#       via inheritance to "[<-",GenomicRanges-method. Currently, can't simply 
#       inherit via GenomicRanges because of the line 
#       "x_ecs[i, ecs_to_replace] <- value_ecs[ecs_to_replace]", which breaks 
#       because it tries to put a NULL in a NULL, i.e. it breaks if size(x) = 1 
#       or 2.
#' @importFrom S4Vectors DataFrame
#' @importFrom stats setNames
#' @importMethodsFrom GenomeInfoDb seqinfo "seqinfo<-" seqnames
#' @importMethodsFrom IRanges ranges
#' @importMethodsFrom stats4 update
#' @importMethodsFrom S4Vectors extractROWS mcols
#' 
#' @export
setReplaceMethod("[", 
                 "GTuples",
                 function(x, i, j, ..., value) {
                   if (!inherits(value, "GTuples")) {
                     stop("replacement value must be a '", class(x), "' object")
                   }
                   if (!identical(size(x), size(value))) {
                     stop("Cannot replace with tuples of a different 'size'")
                   }
                   seqinfo(x) <- merge(seqinfo(x), seqinfo(value))
                   seqnames <- seqnames(x)
                   ranges <- ranges(x)
                   internal_pos <- x@internalPos
                   strand <- strand(x)
                   ans_mcols <- mcols(x, FALSE)
                   # A kludge to extract any non-internalPos extra column slots
                   # from value as a DataFrame or return an appropriately sized 
                   # empty DataFrame
                   value_ec <- GenomicRanges:::extraColumnSlots(value)
                   value_ec <- value_ec[names(value_ec) != "internalPos"]
                   if (length(value_ec) > 0L) {
                     value_ecs <- GenomicRanges:::extraColumnSlots(value)
                     value_ecs <- DataFrame(value_ecs[names(value_ecs) != 
                                                        "internalPos"])
                   } else {
                     value_ecs <- DataFrame()
                     value_ecs@nrows <- length(value)
                   }
                   # A kludge to extract any non-internalPos extra column slots
                   # from x as a DataFrame or return an appropriately sized 
                   # empty DataFrame
                   x_ec <- GenomicRanges:::extraColumnSlots(x)
                   x_ec <- x_ec[names(x_ec) != "internalPos"]
                   if (length(x_ec) > 0L) {
                     x_ecs <- GenomicRanges:::extraColumnSlots(x)
                     x_ecs <- DataFrame(x_ecs[names(x_ecs) != 
                                                "internalPos"])
                   } else {
                     x_ecs <- DataFrame()
                     x_ecs@nrows <- length(x)
                   }
                   new_ecs <- value_ecs[!names(value_ecs) %in% names(x_ecs)]
                   ecs_to_replace <- intersect(names(value_ecs), names(x_ecs))        
                   if (missing(i)) {
                     seqnames[] <- seqnames(value)
                     ranges[] <- ranges(value)
                     strand[] <- strand(value)
                     if (missing(j))
                       ans_mcols[ , ] <- mcols(value, FALSE)
                     else
                       ans_mcols[ , j] <- mcols(value, FALSE)
                     if (length(new_ecs) > 0L)
                       ans_mcols[names(new_ecs)] <- new_ecs
                     x_ecs[ecs_to_replace] <- value_ecs[ecs_to_replace]
                   } else {
                     i <- extractROWS(setNames(seq_along(x), names(x)), i)
                     seqnames[i] <- seqnames(value)
                     ranges[i] <- ranges(value)
                     strand[i] <- strand(value)
                     internal_pos[i, ] <- value@internalPos
                     if (missing(j)) {
                       ans_mcols[i, ] <- mcols(value, FALSE)
                     }
                     else {
                       ans_mcols[i, j] <- mcols(value, FALSE)
                     }
                     if (length(new_ecs) > 0L) {
                       ans_mcols[i, names(new_ecs)] <- DataFrame(new_ecs)
                     }
                     if (length(ecs_to_replace) > 0L) {
                       x_ecs[i, ecs_to_replace] <- value_ecs[ecs_to_replace]
                     }
                   }
                   update(x, 
                          seqnames = seqnames, 
                          ranges = ranges,
                          strand = strand, 
                          elementMetadata = ans_mcols,
                          internalPos = internal_pos,
                          .slotList = as.list(x_ecs))
                 }
)

#' @importMethodsFrom S4Vectors replaceROWS
NULL

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### $ and $<- methods
###

# NOTE: Defined via inheritance to GenomicRanges method

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

# Ensure the internalPos matrix "sticks" during subsetting, etc.
setMethod(GenomicRanges:::extraColumnSlotNames, "GTuples",
          function(x) {
            c("internalPos")
          }
)

# The show method is adapted from that of GRanges
#' @importMethodsFrom S4Vectors mcols showAsCell
.makeNakedMatFromGTuples <- function(x) {
  lx <- length(x)
  nc <- ncol(mcols(x))
  if (!is.na(size(x))) {
    if (size(x) == 1L) {
      ans <- cbind(as.character(seqnames(x)), start(x), as.character(strand(x)))
    } else if (size(x) == 2L) {
      ans <- cbind(as.character(seqnames(x)), start(x), end(x), 
                   as.character(strand(x)))
    } else {
      ans <- cbind(as.character(seqnames(x)), start(x), x@internalPos, end(x), 
                   as.character(strand(x)))
    }
    colnames(ans) <- c("seqnames", paste0('pos', seq_len(size(x))), "strand")
  } else{
    ans <- cbind(as.character(seqnames(x)), as.character(strand(x)))
    colnames(ans) <- c("seqnames", "strand")
  }
  extraColumnNames <- GenomicRanges:::extraColumnSlotNames(x)
  extraColumnNames <- extraColumnNames[extraColumnNames != "internalPos"]
  if (length(extraColumnNames) > 0L) {
    ans <- do.call(cbind, c(list(ans), 
                            lapply(GenomicRanges:::extraColumnSlots(x), 
                                   showAsCell)))
  }
  if (nc > 0L) {
    tmp <- do.call(data.frame, c(lapply(mcols(x), showAsCell), 
                                 list(check.names = FALSE)))
    ans <- cbind(ans, `|` = rep.int("|", lx), as.matrix(tmp))
  }
  ans
}

# NOTE: Unlike GenomicRanges:::showGenomicRanges(), this does not implement 
#       the print.classinfo argument.
#' @importMethodsFrom GenomeInfoDb seqinfo
#' @importFrom S4Vectors makePrettyMatrixForCompactPrinting
showGTuples <- function(x, margin = "", print.classinfo = FALSE, 
                        print.seqinfo = FALSE) {
  if (!identical(print.classinfo, FALSE)) {
    stop("'print.classinfo' not implemented")
  }
  lx <- length(x)
  nc <- ncol(mcols(x))
  
  if (!is.na(size(x))) {
    cat(class(x), " object with ", lx, " x ", 
        ifelse(lx == 1L, paste0(size(x), "-tuple"), 
               paste0(x@size, "-tuples")), 
        " and ", nc, " metadata ", ifelse(nc == 1L, "column", "columns"), 
        ":\n", sep = "")
  } else{
    cat(class(x), " with 0 tuples and 0 metadata columns:\n", sep = "")
  }
  
  out <- makePrettyMatrixForCompactPrinting(x, .makeNakedMatFromGTuples)

  if (nrow(out) != 0L) { 
    rownames(out) <- paste0(margin, rownames(out))
  }
  print(out, quote = FALSE, right = TRUE)
  if (print.seqinfo) {
    cat(margin, "---\n", sep = "")
    cat(margin, "seqinfo: ", summary(seqinfo(x)), "\n", sep = "")
  }
}

#' @export
setMethod("show", 
          "GTuples", 
          function(object) {
            showGTuples(object, margin = "  ", print.seqinfo = TRUE)
          }
)
