### =========================================================================
### findOverlaps methods
### -------------------------------------------------------------------------

#' An internal function used by the findOverlaps,GTuples,GTuples-method when 
#' type = "equal".
#' 
#' @param query A GTuples instance
#' @param subject A GTuples instance
#' @param select See \code{findOverlaps} in the IRanges package for a 
#' description of this argument.
#' @param When set to \code{TRUE}, the strand information is ignored in the 
#' overlap calculations.
#' 
#' @importFrom data.table := .I setkeyv
#' @importFrom S4Vectors Hits selectHits
#' @importMethodsFrom GenomeInfoDb isCircular seqinfo seqlengths seqnames
#' @importMethodsFrom IRanges which
#' 
#' @keywords internal
.findEqual.GTuples <- function(query, subject, select, ignore.strand) {
  
  si <- merge(seqinfo(query), seqinfo(subject))
  # Put the tuples on circular seqlevels on the [1, seqlength]
  # NOTE: Not the most efficient code, but since this is a rare scenario it'll 
  #       probably do.
  ic <- isCircular(si)
  circular <- names(ic[!is.na(ic) & ic])
  for (c in circular) {
    q_circ <- which(seqnames(query) == c)
    s_circ <- which(seqnames(subject) == c)
    tuples(query[q_circ]) <- tuples(query[q_circ]) %% seqlengths(si)[c]
    tuples(subject[s_circ]) <- tuples(subject[s_circ]) %% seqlengths(si)[c]
  }

  q <- .GT2DT(query, ignore.strand = ignore.strand)[, q_idx := .I]
  s <- .GT2DT(subject, ignore.strand = ignore.strand)[, s_idx := .I]
  keycols <- c("seqnames", paste0("pos", seq_len(size(query))))
  setkeyv(q, keycols)
  setkeyv(s, keycols)
  # Find between query and subject allowing for strand.
  hits <- q[s, allow.cartesian = TRUE][
    !is.na(q_idx) & (strand == "*" | i.strand == "*" | strand == i.strand), 
    list(q_idx, s_idx)]
  selectHits(Hits(queryHits = hits[, q_idx], 
                  subjectHits = hits[, s_idx], 
                  queryLength = nrow(q), 
                  subjectLength = nrow(s)),
             select = select)
}
# To avoid WARNINGs about "Undefined global functions or variables" in
# R CMD check caused by the .findEqual.GTuples() function.
#' @importFrom utils globalVariables
globalVariables(c("q_idx", "s_idx", "i.strand"))

# There is a specially defined method for findOverlaps when both the query and 
# the subject are GTuples objects. This is to allow for "equal" matching 
# between GTuples. All other methods are defined via inheritance.
# If either the subject or the query is not a GTuples object then it defers to 
# the findOverlaps method defined for GRanges objects.
#' @importFrom methods as setMethod
#' @importFrom S4Vectors isSingleNumber selectHits
#' @importMethodsFrom IRanges findOverlaps
#' 
#' @export
setMethod("findOverlaps", signature = c("GTuples", "GTuples"), 
          function(query, subject, maxgap = 0L, minoverlap = 1L, 
                   type = c("any", "start", "end", "within", "equal"), 
                   select = c("all", "first", "last", "arbitrary"),
                   ignore.strand = FALSE) {
            
            # NOTE: Other arguments are checked by the method called by 
            #       callNextMethod() or otherwise do not need to be explicitly 
            #       checked.
            type <- match.arg(type)
            select <- match.arg(select)
            if (!isTRUEorFALSE(ignore.strand)) {
              stop("'ignore.strand' must be TRUE or FALSE")
            }

            if (identical(type, "equal")) {
              if (!isTRUE(maxgap == 0L)) {
                stop("'maxgap' must be 0 when 'type = equal', other values ", 
                     "not yet supported")
              } 
              if (!isTRUE(minoverlap == 1L)) {
                stop("'minoverlap' must be 1 when 'type = equal', other ", 
                     "values not yet supported")
              }
              if (is.na(size(query)) && is.na(size(subject))) {
                # NOTE: A GTuples with NA-size is the same as a GRanges object 
                #       with no ranges.
                return(selectHits(Hits(), select = select))
              } 
              if (!isTRUE(size(query) == size(subject))) {
                stop("Cannot findOverlaps between '", class(query), "' and '", 
                     class(subject), "' with 'type = \"equal\"' if they have ",
                     "different 'size'.")
              }
              .findEqual.GTuples(query, subject, select, ignore.strand)
            } else {
              if (isTRUE(size(query) > 2)) {
                # NOTE: No warning is given if size < 3 even though this in 
                #       fact performs a coercion from GTuples to GRanges (
                #       GTuples with size = 1 or 2 are basically GRanges).
                warning("'type' is not 'equal' so coercing 'query' and ", 
                        "'subject' to 'GRanges' objects (see docs for details)")
              }
            }
          }
)

# findOverlaps,GTuplesList,GTuples-method; 
# findOverlaps,GTuples,GTuplesList-method;
# findOverlaps,GTuplesList,GTuplesList-method all inherit from the appropriate
# GRanges/GRangesList methods.

### =========================================================================
### findOverlaps-based methods
### -------------------------------------------------------------------------

# NOTE: Copy of GenomicRanges::countOverlaps.definition (GenomicRanges v1.23.10)
#' @importMethodsFrom S4Vectors queryHits
.countOverlaps.definition <- function(query, subject,
                           maxgap = 0L, minoverlap = 1L, 
                           type = c("any", "start", "end", "within", "equal"), 
                           ignore.strand = FALSE)	{ 
  counts <- queryHits(findOverlaps(query, subject, 
                                   maxgap = maxgap, 
                                   minoverlap = minoverlap,	
                                   type = match.arg(type),
                                   ignore.strand = ignore.strand))
  structure(tabulate(counts, NROW(query)), names = names(query))
}		

#' @importFrom methods setMethod
#' @importMethodsFrom IRanges countOverlaps
#'
#' @export
setMethod("countOverlaps", signature = c("GTuples", "GTuples"),
          .countOverlaps.definition
)

# All others defined via inheritance.

### =========================================================================
### findOverlaps-based methods with GTuples/GTuplesList signatures 
### -------------------------------------------------------------------------
# These all inherit from the appropriate GRanges/GRangesList method
