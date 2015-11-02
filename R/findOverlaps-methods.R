### =========================================================================
### findOverlaps methods
### -------------------------------------------------------------------------

#' An internal function used by the findOverlaps,GTuples,GTuples-method when 
#' type = "equal".
#' 
#' @param query A GTuples instance
#' @param subject A GTuples instance
#' @param maxgap
#' 
#' @keywords internal
.findEqual.GTuples <- function(query, subject, maxgap, minoverlap, 
                               select, ignore.strand) {
  
  # Type is effectively hard-coded to 'equal' since this is the only value for
  # which this function should be called.
  type <- "equal"
  
  # merge() also checks that 'query' and 'subject' are based on the
  # same reference genome.
  seqinfo <- merge(seqinfo(query), seqinfo(subject))
  
  q_len <- length(query)
  s_len <- length(subject)
  q_seqnames <- seqnames(query)
  s_seqnames <- seqnames(subject)
  q_splitranges <- splitRanges(q_seqnames)
  s_splitranges <- splitRanges(s_seqnames)
  q_seqlevels_nonempty <- names(q_splitranges)[
    sapply(q_splitranges, length) > 0]
  s_seqlevels_nonempty <- names(s_splitranges)[
    sapply(s_splitranges, length) > 0]
  q_tuples <- unname(tuples(query))
  s_tuples <- unname(tuples(subject))
  if (ignore.strand) {
    q_strand <- rep.int(1L, q_len)
    s_strand <- rep.int(1L, s_len)
  } else {
    q_strand <- GenomicRanges:::.strandAsSignedNumber(strand(query))
    s_strand <- GenomicRanges:::.strandAsSignedNumber(strand(subject))
  }
  
  common_seqlevels <- intersect(q_seqlevels_nonempty, s_seqlevels_nonempty)
  # Apply over seqlevels
  results <- lapply(common_seqlevels, function(seqlevel) {
    if (isCircular(seqinfo)[seqlevel] %in% TRUE) {
      circle.length <- seqlengths(seqinfo)[seqlevel]
    } else {
      circle.length <- NA
    }
    q_idx <- q_splitranges[[seqlevel]]
    s_idx <- s_splitranges[[seqlevel]]
    # Apply over the (m - 1) x 2-tuples in an m-tuple, where m = size
    pair_hits <- lapply(seq_len(size(query) - 1L), function(i) {
      q_matrix <- extractROWS(q_tuples, q_idx)[, seq.int(i, i + 1, 1), 
                                               drop = FALSE]
      q_ranges <- IRanges(start = q_matrix[, 1L], end = q_matrix[, 2L])
      s_matrix <- extractROWS(s_tuples, s_idx)[, seq.int(i, i + 1, 1), 
                                               drop = FALSE]
      s_ranges <- IRanges(start = s_matrix[, 1L], end = s_matrix[, 2L])
      min.score <- IRanges:::min_overlap_score(maxgap, minoverlap)
      hits <- IRanges:::findOverlaps_NCList(q_ranges, s_ranges,
                                            min.score = min.score,
                                            type = type, select = "all",
                                            circle.length = circle.length)
      q_hits <- queryHits(hits)
      s_hits <- subjectHits(hits)
      compatible_strand <-
        extractROWS(q_strand, q_idx)[q_hits] *
        extractROWS(s_strand, s_idx)[s_hits] != -1L
      hits <- hits[compatible_strand]
      remapHits(hits, query.map = as.integer(q_idx),
                new.queryLength = q_len,
                subject.map = as.integer(s_idx),
                new.subjectLength = s_len)
    })
    # Intersect the pair-wise hits to form the tuple-wise hits
    Reduce(intersect, pair_hits)
  })
  
  # Combine the results.
  q_hits <- unlist(lapply(results, queryHits))
  if (is.null(q_hits)) {
    q_hits <- integer(0)
  }
  
  s_hits <- unlist(lapply(results, subjectHits))
  if (is.null(s_hits)) {
    s_hits <- integer(0)
  }
 
  selectHits(Hits(q_hits, s_hits, q_len, s_len), select = select)
}

# There is a specially defined method for findOverlaps when both the query and 
# the subject are GTuples objects. This is to allow for "equal" matching 
# between GTuples. All other methods are defined via inheritance.
# If either the subject or the query is not a GTuples object then it defers to 
# the findOverlaps method defined for GRanges objects. 
#' @export
setMethod("findOverlaps", signature = c("GTuples", "GTuples"), 
          function(query, subject, maxgap = 0L, minoverlap = 1L, 
                   type = c("any", "start", "end", "within", "equal"), 
                   select = c("all", "first", "last", "arbitrary"),
                   algorithm = c("nclist", "intervaltree"),
                   ignore.strand = FALSE) {

            # Argument matching
            if (!isSingleNumber(maxgap) || maxgap < 0L) {
              stop("'maxgap' must be a non-negative integer")
            }
            select <- match.arg(select)
            type <- match.arg(type)
            algorithm <- match.arg(algorithm)
            # TODO: Support maxgap and minoverlap when type = "equal" 
            # Need to define these for tuples.
            if (type == 'equal') {
              if (isTRUE(maxgap != 0L)) {
                stop(paste0("'maxgap' must be 0 when 'type = equal', ", 
                            "other values not yet supported"))
              } 
              if (isTRUE(minoverlap != 1L)) {
                stop(paste0("'minoverlap' must be 1 when 'type = equal', ", 
                            "other values not yet supported"))
              }
              # Second check is whether one, and only one, of query or subject 
              # have size NA.
              if ((isTRUE(size(query) != size(subject))) || 
                    (is.na(size(query)) + is.na(size(subject))) == 1) {
                stop("Cannot findOverlaps between '", class(query), "' and '", 
                     class(subject), "' with 'type = \"equal\"' if they have ",
                     "different 'size'.")
              }
            }
            
            if (isTRUE(size(query) >= 3) && type == 'equal') { 
              .findEqual.GTuples(query, subject, maxgap, minoverlap, 
                                 select, ignore.strand)
            } else{
              # TODO: Why doesn't callNextMethod() work?
              #callNextMethod()
              findOverlaps(query = as(query, "GRanges"), 
                           subject = as(subject, "GRanges"), 
                           maxgap = maxgap, 
                           minoverlap = minoverlap,
                           type = type,
                           select = select,
                           algorithm = algorithm,
                           ignore.strand = ignore.strand)
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
#' @export
setMethod("countOverlaps", signature = c("GTuples", "GTuples"),
    GenomicRanges:::countOverlaps.definition
)

# All others defined via inheritance.

### =========================================================================
### findOverlaps-based methods with GTuples/GTuplesList signatures 
### -------------------------------------------------------------------------
# These all inherit from the appropriate GRanges/GRangesList method
