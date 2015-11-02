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
  
  msg <- NULL
  
  # Check tuples are sorted; only required if m > 1.
  if (isTRUE(object@size > 2L) && length(object) != 0L) {
    if (!.Call(Cpp_GenomicTuples_allTuplesSorted, object@ranges@start, 
               object@internalPos, 
               object@ranges@start + object@ranges@width - 1L)
    ) {
      msg <- Biobase::validMsg(msg, 
                               paste0("positions in each tuple must be sorted ",
                                      "in strictly increasing order, i.e. ",
                                      "'pos1' < ... < ", 
                                      paste0("'pos", object@size, "'")))
    }
  } else if (isTRUE(object@size == 2L)) {
    if (isTRUE(any(object@ranges@width <= 1L))) {
      msg <- Biobase::validMsg(msg, 
                               paste0("positions in each tuple must be sorted in ", 
                                      "strictly increasing order, i.e. 'pos1' < ", 
                                      "'pos2'"))
    }
  }
  
  # Check all tuples have positive positions.
  # Only need to check pos1 and posm because already checked 
  # pos1 < internalPos < posm
  # NB: min(x) < 0 is faster than any(x < 0)
  if (!is.na(object@size) && length(object) != 0L) {
    if (min(object@ranges@start) < 0L || min(object@ranges@start + 
                                             object@ranges@width - 1L) < 0L) {
      msg <- Biobase::validMsg(msg, 
                               paste0("positions in each tuple must be ", 
                                      "positive integers."))
    }
  }
  
  msg
}

INVALID.GT.COLNAMES <- c("seqnames", "ranges", "strand",
                         "seqlevels", "seqlengths", "isCircular",
                         #"genome",
                         "start", "end", "width", "element",
                         "tuples", "internalPos", "size")

.valid.GTuples.mcols <- function(object) {

  msg <- NULL
  
  if (any(INVALID.GT.COLNAMES %in% colnames(mcols(object)))) {
    msg <- Biobase::validMsg(msg, 
                             paste0("names of metadata columns cannot be one of ",
                                    paste0("\"", INVALID.GT.COLNAMES, "\"", 
                                           collapse = ", ")))
  }
  msg
}

.valid.GTuples <- function(object) {
  
  # Include all .valid.GTuples.* functions in this vector
  msg <- c(.valid.GTuples.pos(object), .valid.GTuples.mcols(object))
  
  if (is.null(msg)){
    return(TRUE)
  } else{
    msg
  }
}

setValidity2("GTuples", .valid.GTuples)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

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
  # Don't want tuples to contain mix of NA and non-NA
  NAs <- is.na(tuples)
  if (sum(NAs) > 0 && sum(!NAs) > 0) {
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
    mode(tuples) <- "integer"
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

#' @export
setMethod("as.data.frame", 
          "GTuples", 
          function(x, row.names = NULL, optional = FALSE, ...) {
            if (is.na(size(x))) {
              data.frame()
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
              data.frame(seqnames = as.factor(seqnames(x)), 
                         as.data.frame(tuples), 
                         strand = as.factor(strand(x)), 
                         mcols_df, 
                         row.names = row.names, 
                         stringsAsFactors = FALSE)
            }
          }
)

#' @export
setMethod("granges", 
          "GTuples",
          function(x, use.mcols = FALSE) {
            if (!isTRUEorFALSE(use.mcols)) {
              stop("'use.mcols' must be TRUE or FALSE")
            }
            ans <- GRanges(seqnames = seqnames(x), 
                           ranges = ranges(x), 
                           strand = strand(x), 
                           seqinfo = seqinfo(x))
            if (use.mcols) {
              extraColumns <- GenomicRanges:::extraColumnSlotsAsDF(x)
              extraColumns <- extraColumns[colnames(extraColumns) != 
                                             'internalPos']
              mcols(ans) <- cbind(extraColumns, mcols(x))
            }
            ans
          }
)

setAs("GTuples", 
      "GRanges", 
      function(from) {
        granges(from)
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
  ans_size <- sapply(x, size)[1]
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
            # "c" will error if there is no common class, e.g. c(GRanges(), "1")
            # The following commented-out error makes this explicit to the user 
            # for c,GTuples-method
            #             if (!isTRUE(all(sapply(args, inherits, "GTuples")))) {
            #                 stop(paste0("Cannot combine ", 
            #                             paste0(unique(sapply(args, class)), 
            #                                    collapse = ' and '), 
            #                             " objects"))
            #             }
            if (!.zero_range(sapply(args, size)) && 
                !isTRUE(all(is.na(sapply(args, size))))) {
              stop(paste0("Cannot combine ", paste0(unique(sapply(args, class)), 
                                                    collapse = ' and '), 
                          " containing tuples of different 'size'."))
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

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Setters
###

# Defined via inheritance to GRanges or implemented in Tuples methods

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Tuples methods
###

#' @include AllGenerics.R
#' @export
setMethod("size", 
          "GTuples", 
          function(x) {
            x@size
          }
)

#' @include AllGenerics.R
#' @export
setMethod("tuples", 
          "GTuples", 
          function(x) {
            if (is.na(size(x))) {
              ans <- matrix()
            } else if (size(x) == 1L) {
              ans <- matrix(start(x), 
                            dimnames = list(NULL, paste0('pos', 
                                                         seq_len(size(x)))))
            } else{
              ans <- cbind(start(x), x@internalPos, end(x))
              colnames(ans) <- paste0('pos', seq_len(size(x)))
            }
            ans
          }
)

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
                     stop(paste0("Size of tuples in 'x' ", size(x), " not ", 
                                 "equal to size of tuples in 'value' ", m))
                   }
                   if (is.na(m)) {
                     x
                   } else if (m == 1L) {
                     ranges <- IRanges(start = value[, 1], end = value[, 1])
                     internalPos <- NULL
                   } else if (m == 2L) {
                     ranges <- IRanges(start = value[, 1], end = value[, 2])
                     internalPos <- NULL
                   } else if (m > 2L) {
                     ranges <- IRanges(start = value[, 1], end = value[, m])
                     internalPos <- unname(value[, seq.int(from = 2, 
                                                           to = m - 1, by = 1), 
                                                  drop = FALSE])
                   }
                   update(x, 
                          ranges = ranges, 
                          internalPos = internalPos,
                          check = TRUE)
                 }
)

#' @include AllGenerics.R
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
              ipd <- matrix(width(x) - 1L, ncol=1)
            } else {
              ipd <- .Call(Cpp_GenomicTuples_IPD, 
                           start(x), 
                           matrix(x@internalPos, ncol = size - 2), 
                           end(x)
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
# TODO: Should I explicitly define these via callNextMethod()

# TODO (copied from GenomicRanges/GenomicRanges-class.R): Refactor to use 
# replaceROWS(). This will make the code much simpler and avoid a lot of 
# duplication with the above "replaceROWS" method.
# TODO: Once "[<-",GenomicRanges-method uses replaceROWS,GenomicRanges-method, 
# then should be able to migrate to using "[<-",GTuples-method defined via 
# inheritance to "[<-",GenomicRanges-method. Can't simply inherit via 
# GenomicRanges because of the line 
# "x_ecs[i, ecs_to_replace] <- value_ecs[ecs_to_replace]", which breaks because
# it tries to put a NULL in a NULL, i.e. it breaks if size(x) = 1 or 2.
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

# extractROWS, "[", replaceROWS and "[<-" defined via inheritance to methods 
# for GenomicRanges.

# TODO: Should I explicitly define these via callNextMethod()

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

# Ensure the internalPos matrix "sticks" during subsetting, etc.
setMethod(GenomicRanges:::extraColumnSlotNames, 
          "GTuples",
          function(x) {
            c("internalPos")
          }
)

# The show method is adapted from that of GRanges
# TODO: Decide if I should support the print.classinfo argument?

.makeNakedMatFromGTuples <- function(x) {
  lx <- length(x)
  nc <- ncol(mcols(x))
  if (!is.na(x@size)) {
    if (x@size == 1L) {
      ans <- cbind(as.character(x@seqnames), x@ranges@start, 
                   as.character(x@strand))
    } else if (x@size == 2L) {
      ans <- cbind(as.character(x@seqnames), x@ranges@start, 
                   x@ranges@start + x@ranges@width - 1, as.character(x@strand))
    } else {
      ans <- cbind(as.character(x@seqnames), x@ranges@start, 
                   x@internalPos, x@ranges@start + x@ranges@width - 1, 
                   as.character(x@strand))
    }
    colnames(ans) <- c("seqnames", paste0('pos', seq_len(x@size)), "strand")
  } else{
    ans <- cbind(as.character(x@seqnames), as.character(x@strand))
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
    tmp <- do.call(data.frame, c(lapply(mcols(x), S4Vectors::showAsCell), 
                                 list(check.names = FALSE)))
    ans <- cbind(ans, `|` = rep.int("|", lx), as.matrix(tmp))
  }
  ans
}

# TODO: Need to keep this up to date with the show,GRanges-method
showGTuples <- function(x, margin = "", print.classinfo = FALSE, 
                        print.seqinfo = FALSE) {
  if (!identical(print.classinfo, FALSE)) {
    stop("'print.classinfo' not implemented")
  }
  lx <- length(x)
  nc <- ncol(mcols(x))
  
  if (!is.na(x@size)) {
    cat(class(x), " object with ", lx, " x ", 
        ifelse(lx == 1L, paste0(x@size, "-tuple"), 
               paste0(x@size, "-tuples")), 
        " and ", nc, " metadata ", ifelse(nc == 1L, "column", "columns"), 
        ":\n", sep = "")
  } else{
    cat(class(x), " with 0 tuples and 0 metadata columns:\n", sep = "")
  }
  
  out <- S4Vectors:::makePrettyMatrixForCompactPrinting(x, .makeNakedMatFromGTuples)
  # TODO: Try to implement 'print.classinfo', although low priority.
  ## These lines commented out because classinfo is more complicated for GTuples 
  ## objects than GRanges objects. For example, some of the `pos` information 
  ## is stored in an IRanges object while some is stored in a matrix.
  #if (print.classinfo) {
  #    .COL2CLASS <- c(seqnames = "Rle", ranges = "IRanges", 
  #        strand = "Rle")
  #    extraColumnNames <- extraColumnSlotNames(x)
  #    .COL2CLASS <- c(.COL2CLASS, getSlots(class(x))[extraColumnNames])
  #    classinfo <- makeClassinfoRowForCompactPrinting(x, .COL2CLASS)
  #    stopifnot(identical(colnames(classinfo), colnames(out)))
  #    out <- rbind(classinfo, out)
  #}
  
  if (nrow(out) != 0L){ 
    rownames(out) <- paste0(margin, rownames(out))
  }
  print(out, quote = FALSE, right = TRUE)
  if (print.seqinfo) {
    cat(margin, "---\n", sep = "")
    cat(margin, "seqinfo: ", summary(seqinfo(x)), "\n", sep ="")
  }
}

#' @export
setMethod("show", 
          "GTuples", 
          function(object) {
            showGTuples(object, margin="  ", print.seqinfo = TRUE)
          }
)
