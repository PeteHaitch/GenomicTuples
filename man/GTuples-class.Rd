\name{GTuples-class}
\docType{class}

% Class:
\alias{class:GTuples}
\alias{GTuples-class}
\alias{GTuples}

% Constructors:
\alias{GTuples}
\alias{updateObject,GTupless-method}

% Accessors:
\alias{seqnames,GTuples-method}
\alias{seqnames<-,GTuples-method}
\alias{ranges,GTuples-method}
\alias{ranges<-,GTuples-method}
\alias{strand,GTuples-method}
\alias{strand<-,GTuples-method}
\alias{elementMetadata<-,GTuples-method}
\alias{names,GTuples-method}
\alias{names<-,GTuples-method}
\alias{seqinfo,GTuples-method}
\alias{seqinfo<-,GTuples-method}
\alias{score,GTuples-method}
\alias{score<-,GTuples-method}

% Coercion:
\alias{coerce,GRanges,GTuples-method}
\alias{as.character,GTuples-method}
\alias{as.factor,GTuples-method}
\alias{as.data.frame,GTuples-method}
\alias{granges,GTuples-method}

% Tuples methods:
\alias{tuples}
\alias{tuples<-}
\alias{tuples,GTuples-method}
\alias{tuples<-,GTuples-method}
\alias{start,GTuples-method}
\alias{start<-,GTuples-method}
\alias{end,GTuples-method}
\alias{end<-,GTuples-method}
\alias{width,GTuples-method}
\alias{width<-,GTuples-method}
\alias{IPD,GTuples-method}
\alias{size,GTuples-method}

% Vector methods:
\alias{length,GTuples-method}
\alias{[,GTuples-method}
\alias{[<-,GTuples,ANY,ANY,ANY-method}
\alias{c,GTuples-method}
\alias{window,GTuples-method}

% $ and $<- methods:
\alias{$,GTuples-method}
\alias{$<-,GTuples,numeric-method}

% "show" method:
\alias{show,GTuples-method}

% Generics (required to prevent "Undocumented code objects:" warning)
\alias{IPD}
\alias{size}

\title{GTuples objects}

\description{
  The \code{GTuples} class is a container for the genomic tuples and their
  associated annotations.
}

\details{
  \code{GTuples} extends \code{\link[GenomicRanges]{GRanges}} as a container
  for genomic tuples rather than genomic ranges. \code{GTuples} is a vector of
  genomic locations and associated annotations. Each element in the vector is
  comprised of a sequence name, a tuple, a \code{\link[BiocGenerics]{strand}},
  and optional metadata columns (e.g. score, GC content, etc.). This
  information is stored in four components:
  \describe{
  \item{\code{seqnames}}{a 'factor' \code{\link[S4Vectors]{Rle}} object
   containing the sequence names.}

  \item{\code{tuples}}{externally, a matrix-link object containing the
  tuples. Internally, an \code{\link[IRanges]{IRanges}} object storing the
  first and last position of each tuple and, if required, a matrix storing
  the "internal" positions of each tuple (see description of
  \code{internalPos} below).}

  \item{\code{strand}}{a \code{\link[S4Vectors]{Rle}} Rle object containing
  the strand information.}

  \item{\code{mcols}}{a \code{\link[S4Vectors]{DataFrame}} object
  containing the metadata columns. Columns cannot be named "\code{seqnames}"
  "\code{ranges}", "\code{tuples}", "\code{internalPos}", "\code{size}", 
  "\code{strand}", "\code{seqlevels}", "\code{seqlengths}", 
  "\code{isCircular}", "\code{start}", "\code{end}", "\code{width}", or 
  "\code{element}".}

  \item{\code{seqinfo}}{a \code{\link[S4Vectors]{DataFrame}} object
  containing information about the set of genomic sequences present in the
  \code{GTuples} object.}
  }
}

\section{Slots}{
  Since the \code{GTuples} class extends the 
  \code{\link[GenomicRanges]{GRanges}} class it contains the \code{seqnames}, 
  \code{ranges}, \code{strand}, \code{elementMetadata}, \code{seqinfo} and 
  \code{metadata}. The \code{GTuples} class also contains two additional slots, 
  \code{size} and \code{internalPos}.

    \describe{
      \item{\code{size}}{An integer. The \code{size} of the genomic tuples 
      stored in the \code{GTuples} object.}

      \item{\code{internalPos}}{If the \code{size} of the genomic tuples is 
      greater than 2, \code{internalPos} is an integer 
      \code{matrix} storing the "internal" positions of each genomic tuple. 
      Otherwise \code{internalPos} is \code{NULL}.}
    }
}

\section{Constructor}{
 \describe{
   \item{}{\code{GTuples(seqnames = Rle(), tuples = matrix(),
     strand = Rle("*", length(seqnames)), ..., seqlengths = NULL,
     seqinfo = NULL)}: Creates a \code{GTuples} object.

     \describe{
       \item{\code{seqnames}}{\code{\link[S4Vectors]{Rle}} object, character
       vector, or factor containing the sequence names.}

       \item{\code{tuples}}{matrix object containing the positions of the
       tuples. The first column should refer to pos1, the second to pos2,
       etc.}

       \item{\code{strand}}{\code{\link[S4Vectors]{Rle}} object, character
       vector, or factor containing the strand information.}

       \item{\code{...}}{Optional metadata columns. These columns cannot be
       named "\code{start}", "\code{end}", "\code{width}", or
       "\code{element}". A named integer vector "\code{seqlength}" can be
       used instead of \code{seqinfo}.}

       \item{\code{seqlengths}}{an integer vector named with the sequence
       names and containing the lengths (or \code{NA}) for each
       \code{level(seqnames)}.}

       \item{\code{seqinfo}}{a \code{\link[S4Vectors]{DataFrame}} object
       containing allowed sequence names and lengths (or \code{NA}) for each
       \code{level(seqnames)}.}
       }
     }
   }
}

\section{Coercion}{
  In the code snippets below, \code{x} is a GTuples object.

  \describe{
  % TODO: Uncomment once implemented
%    \item{}{
%      \code{as(from, "GTuples")}: Creates a GTuples object from a character
%      vector, or a factor.
%
%      When \code{from} is a character vector (or a factor), each element
%      in it must represent a genomic tuples in format \code{chr1:100,109,115}
%      (unstranded tuple) or \code{chr1:100,109,115:+} (stranded range).
%      \code{..} is also supported as a separator between the start and end
%      positions. Strand can be \code{+}, \code{-}, \code{*}, or missing.
%      The names on \code{from} are propagated to the returned GTuples object.
%      See \code{as.character()} and \code{as.factor()} below for the
%      reverse transformations.
%
       % NOTE: There is no makeGTuplesFromDataFrame()
%      Coercing a data.frame or DataFrame into a GRanges object is also
%      supported. See \code{\link{makeGRangesFromDataFrame}} for the details.
%    }
    \item{}{
      \code{as.data.frame(x, row.names = NULL, optional = FALSE, ...)}:
      Creates a data.frame with columns \code{seqnames} (factor), 
      \code{tuples} (integer), \code{strand} (factor), as well as 
      the additional metadata columns stored in \code{mcols(x)}. Pass an 
      explicit \code{stringsAsFactors=TRUE/FALSE} argument via \code{\ldots} to
      override the default conversions for the metadata columns in 
      \code{mcols(x)}.
    }
    \item{}{
      \code{as.character(x, ignore.strand=FALSE)}:
      Turn GTuples object \code{x} into a character vector where each
      tuples in \code{x} is represented by a string in format
      \code{chr1:100,109,115:+}. If \code{ignore.strand} is TRUE or if
      \emph{all} the ranges in \code{x} are unstranded (i.e. their strand
      is set to \code{*}), then all the strings in the output are in
      format \code{chr1:100,109,115}.

      The names on \code{x} are propagated to the returned character vector.
      Its metadata (\code{metadata(x)}) and metadata columns (\code{mcols(x)})
      are ignored.

      % TODO: Uncomment when implemented. 
      % See \code{as(from, "GTuples")} above for the reverse transformation.
    }
    \item{}{
      \code{as.factor(x)}:
      Equivalent to
\preformatted{  factor(as.character(x), levels=as.character(sort(unique(x))))
}
    }
    \item{}{
      \code{as(x, "GRanges"), granges(x)}: Creates a 
      \code{\link[GenomicRanges]{GRanges}} object from a
      \code{GTuples} object. \strong{WARNING:} This is generally a 
      \emph{destructive} operation because all "internal" positions will be 
      dropped.
    }
  }
}


\section{Accessors}{
  In the following code snippets, x is a \code{GTuples} object.

 \describe{
   \item{}{
    \code{size(x)}: Get the size of the genomic tuples stored in
    \code{x}.
   }

   \item{}{
    \code{length(x)}: Get the number of elements.
   }

   \item{}{
    \code{seqnames(x)}, \code{seqnames(x) <- value}: Get or set the
    sequence names. \code{value} can be an \code{\link[S4Vectors]{Rle}}
    object, a character vector, or a factor.
   }
   
   \item{}{
    \code{tuples(x)}, \code{tuples(x) <- value}: Get the positions of the 
    tuples, which are returned as an integer matrix. \code{value} can be an
    integer matrix.
   }

   \item{}{
    \code{ranges(x, use.mcols = FALSE)}, \code{ranges(x) <- value}: Get or set 
    the ranges in the form of a CompressedIRangesList. \code{value} can be a 
    RangesList object.
    
    \strong{WARNING}: The use of \code{ranges} with \code{GTuples} objects is 
    \strong{strongly} discouraged. It will only get/set \eqn{pos_{1}} and 
    \eqn{pos_{m}} of the tuples, where \eqn{m} is the \code{size} of the 
    tuples, as these are what are stored in the "ranges" slot of a 
    \code{GTuples} object.
   }

   \item{}{
    \code{names(x)}, \code{names(x) <- value}: Get or set the names
    of the elements.
   }

   \item{}{
    \code{strand(x)}, \code{strand(x) <- value}: Get or set the
    strand. \code{value} can be an \code{\link[S4Vectors]{Rle}} object,
    character vector, or factor.
   }

   \item{}{
    \code{mcols(x, use.names=FALSE)}, \code{mcols(x) <- value}: Get
    or set the metadata columns. If \code{use.names=TRUE} and the metadata
    columns are not \code{NULL}, then the names of \code{x} are propagated as
    the row names of the returned \code{\link[S4Vectors]{DataFrame}}
    object. When setting the metadata columns, the supplied value must be
    \code{NULL} or a data.frame-like object (i.e.
    \code{\link[S4Vectors]{DataTable}} or \code{\link[base]{data.frame}})
    object holding element-wise metadata.
   }

   \item{}{
    \code{elementMetadata(x)}, \code{elementMetadata(x) <- value},
    \code{values(x)}, \code{values(x) <- value}: Alternatives to \code{mcols}
    functions. Their use is discouraged.
   }

   \item{}{
    \code{seqinfo(x)}, \code{seqinfo(x) <- value}: Get or set the
    information about the underlying sequences. \code{value} must be a
    \code{\link[S4Vectors]{DataFrame}} object.
   }

   \item{}{
    \code{seqlevels(x)}, \code{seqlevels(x, force=FALSE) <- value}:
    Get or set the sequence levels. \code{seqlevels(x)} is equivalent to
    \code{seqlevels(seqinfo(x))} or to \code{levels(seqnames(x))}, those 2
    expressions being guaranteed to return identical character vectors on a
    \code{GTuples} object. \code{value} must be a character vector with no
    \code{NA}s. See \code{?}\code{\link[GenomeInfoDb]{seqlevels}} for more
    information.
   }

   \item{}{
    \code{seqlengths(x)}, \code{seqlengths(x) <- value}: Get or set
    the sequence lengths. \code{seqlengths(x)} is equivalent to
    \code{seqlengths(seqinfo(x))}. \code{value} can be a named non-negative
    integer or numeric vector eventually with \code{NA}s.
   }

   \item{}{
    \code{isCircular(x)}, \code{isCircular(x) <- value}: Get or set
    the circularity flags. \code{isCircular(x)} is equivalent to
    \code{isCircular(seqinfo(x))}. \code{value} must be a named logical
    vector eventually with \code{NA}s.
   }

   \item{}{
    \code{genome(x)}, \code{genome(x) <- value}: Get or set the
    genome identifier or assembly name for each sequence. \code{genome(x)} is
    equivalent to \code{genome(seqinfo(x))}. \code{value} must be a named
    character vector eventually with \code{NA}s.
   }

   \item{}{
    \code{seqlevelsStyle(x)}, \code{seqlevelsStyle(x) <- value}: Get
    or set the seqname style for \code{x}. See the
    \code{\link[GenomeInfoDb]{seqlevelsStyle}} generic getter and setter in
    the \strong{GenomeInfoDb} package for more information.
   }

   \item{}{
    \code{score(x)}, \code{score(x) <- value}: Get or set the "score"
    column from the element metadata.
   }
 }
}

\section{Tuples methods}{
  In the following code snippets, x is a \code{GTuples} object. 
  \strong{WARNING}: The preferred setter is \code{tuples(x) <- value} and the 
  use of \code{start(x) <- value}, \code{end(x) <- value} and 
  \code{width(x) <- value is discouraged}.
    
  \describe{
    \item{}{
      \code{start(x)}, \code{start(x) <- value}:
      Get or set \eqn{pos_{1}} of the tuples. \strong{WARNING}: The use of 
      \code{width(x) <- value} is discouraged; instead, construct the tuples as 
      the appropriate integer matrix, \code{mvalue}, and use 
      \code{tuples(x) <- mvalue}.
    }
    
    \item{}{
      \code{end(x)}, \code{end(x) <- value}:
      Get or set \eqn{pos_{m}} of the tuples, where \eqn{m} is the \code{size} 
      of the tuples. \strong{WARNING}: The use of \code{end(x) <- value} is 
      discouraged; instead, construct the tuples as the appropriate integer
      matrix, \code{mvalue}, and use \code{tuples(x) <- mvalue}.
    }
    
    \item{}{
      \code{IPD(x)}: Get the intra-pair distances (\code{IPD}). \code{IPD} is 
      only defined for tuples with \code{size} > 1. The \code{IPD} of a tuple 
      with \code{size}\eqn{= m} is the vector of intra-pair distances, 
      \eqn{(pos_{2} - pos_{1}, \ldots, pos_{m} - pos_{m - 1})}.
    }
    
    \item{}{
      \code{width(x)}, \code{width(x) <- value}:
      Get or set \eqn{pos_{m} - pos_{1}} of the tuples, where \eqn{m} is the 
      \code{size} of the tuples. If using \code{width(x) <- value}, 
      \eqn{pos_{1}} is held fixed and \eqn{pos_{m}} is altered. 
      \strong{WARNING}: The use of \code{width(x) <- value} is discouraged; 
      instead, construct the tuples as the appropriate integer matrix, 
      \code{mvalue}, and use \code{tuples(x) <- mvalue}.
    }
  }
}

\section{Splitting and Combining}{
  In the following code snippets, x is a \code{GTuples} object.

 \describe{
   \item{}{
    \code{append(x, values, after = length(x))}: Inserts the values
    into \code{x} at the position given by after, where \code{x} and
    \code{values} are of the same class.
   }

   \item{}{
    \code{c(x, ...)}: Combines \code{x} and the \code{GTuples}
    objects in \code{...} together. Any object in \code{...} must belong to
    the same class as \code{x}, or to one of its subclasses, or must be
    \code{NULL}. The result is an object of the same class as \code{x}.
   }

   \item{}{
    \code{c(x, ..., ignore.mcols=FALSE)} If the \code{GTuples}
    objects have metadata columns (represented as one
    \code{\link[S4Vectors]{DataFrame}} per object), each such
    \code{\link[S4Vectors]{DataFrame}} must have the same columns in
    order to combine successfully. In order to circumvent this restraint, you
    can pass in an \code{ignore.mcols=TRUE} argument which will combine all
    the objects into one and drop all of their metadata columns.
   }

   \item{}{
    \code{split(x, f, drop=FALSE)}: Splits \code{x} according to
    \code{f} to create a \code{GTuplesList} object. If \code{f} is a
    list-like object then \code{drop} is ignored and \code{f} is treated as
    if it was \code{rep(seq_len(length(f)), sapply(f, length))}, so the
    returned object has the same shape as \code{f} (it also receives the
    names of \code{f}). Otherwise, if \code{f} is not a list-like object,
    empty list elements are removed from the returned object if \code{drop}
    is \code{TRUE}.
   }

 }
}

\section{Subsetting}{
  In the following code snippets, x is a \code{GTuples} object.

 \describe{
   \item{}{
    \code{x[i, j]}, \code{x[i, j] <- value}: Get or set elements
    \code{i} with optional metadata columns \code{mcols(x)[,j]}, where
    \code{i} can be missing; an \code{NA}-free logical, numeric, or character
    vector; or a 'logical' \code{\link[S4Vectors]{Rle}} object.
   }

   \item{}{
    \code{x[i,j] <- value}: Replaces elements \code{i} and optional
    metadata columns \code{j} with \code{value}.
   }

   \item{}{
    \code{head(x, n = 6L)}: If \code{n} is non-negative, returns the
    first \code{n} elements of the \code{GTuples} object. If \code{n} is
    negative, returns all but the last \code{abs(n)} elements of the
    \code{GTuples} object.
   }

   \item{}{
    \code{rep(x, times, length.out, each)}: Repeats the values in
    \code{x} through one of the following conventions:
      \describe{
        \item{\code{times}}{
          Vector giving the number of times to repeat each
          element if of length \code{length(x)}, or to repeat the whole vector
          if of length 1.
        }

        \item{\code{length.out}}{
          Non-negative integer. The desired length of
          the output vector.
        }

        \item{\code{each}}{
          Non-negative integer. Each element of \code{x} is
          repeated each times.
        }
     }
   }

   \item{}{
    \code{subset(x, subset)}: Returns a new object of the same class
    as \code{x} made of the subset using logical vector subset, where missing
    values are taken as \code{FALSE}.
   }

   \item{}{
    \code{tail(x, n = 6L)}: If \code{n} is non-negative, returns the
    last \code{n} elements of the \code{GTuples} object. If \code{n} is
    negative, returns all but the first \code{abs(n)} elements of the
    \code{GTuples} object.
   }

    \item{}{
      \code{window(x, start = NA, end = NA, width = NA, frequency = NULL, delta = NULL, ...)}:
      Extracts the subsequence window from the \code{GTuples} object using:
      \describe{
        \item{\code{start}, \code{end}, \code{width}}{The start, end, or width
          of the window. Two of the three are required.}
        \item{\code{frequency}, \code{delta}}{Optional arguments that specify
          the sampling frequency and increment within the window.}
      }
      In general, this is more efficient than using \code{"["} operator.
    }
    \item{}{
      \code{window(x, start = NA, end = NA, width = NA, keepLength = TRUE) <- value}:
      Replaces the subsequence window specified on the left (i.e. the subsequence
      in \code{x} specified by \code{start}, \code{end} and \code{width})
      by \code{value}.
      \code{value} must either be of class \code{class(x)}, belong to a subclass
      of \code{class(x)}, be coercible to \code{class(x)}, or be \code{NULL}.
      If \code{keepLength} is \code{TRUE}, the elements of \code{value} are
      repeated to create a \code{GTuples} object with the same number of elements
      as the width of the subsequence window it is replacing.
      If \code{keepLength} is \code{FALSE}, this replacement method can modify
      the length of \code{x}, depending on how the length of the left
      subsequence window compares to the length of \code{value}.
    }

   \item{}{
    \code{x$name}, \code{x$name <- value}: Shortcuts for
    \code{mcols(x)$name} and \code{mcols(x)$name <- value}, respectively.
    Provided as a convenience, from \code{\link[GenomicRanges]{GRanges}} as
    the result of strong popular demand. Note that those methods are not
    consistent with the other \code{$} and \code{$<-} methods in the
    \code{\link[IRanges]{IRanges}}/
    \code{\link[GenomicRanges]{GenomicRanges}} infrastructure, and might
    confuse some users by making them believe that a
    \code{\link[GenomicRanges]{GRanges}} object can be manipulated as a
    data.frame-like object. Therefore we recommend using them only
    interactively, and we discourage their use in scripts or packages. For
    the latter, use \code{mcols(x)$name} and \code{mcols(x)$name <- value},
    instead of \code{x$name} and \code{x$name <- value}, respectively.
   }
 }
}

\section{Other methods}{

 \describe{
   item{}{
    \code{show(x)}: By default the show method displays 5 head and 5
    tail elements. This can be changed by setting the global options
    \code{showHeadLines} and \code{showTailLines}. If the object length is
    less than (or equal to) the sum of these 2 options plus 1, then the
    full object is displayed. Note that these options also affect the
    display of \code{\link[GenomicRanges]{GRanges}},
    \code{\link[GenomicAlignments]{GAlignments}} and
    \code{\link[GenomicAlignments]{GAlignmentPairs}} objects (defined in
    the \strong{GenomicAlignments} package), as well as other objects
    defined in the \strong{IRanges} and \strong{Biostrings} packages (e.g.
    \code{\link[IRanges]{IRanges}} and \code{\link[Biostrings]{DNAStringSet}}
    objects).
   }
 }
}

\author{Peter Hickey}

\seealso{
  \code{\link{GTuplesList-class}},
  \code{\link[GenomeInfoDb]{seqinfo}},
  \code{\link[S4Vectors]{Vector-class}},
  \code{\link[S4Vectors]{Rle-class}},
  \code{\link[IRanges]{Ranges-class}},
  \code{\link[S4Vectors]{DataFrame}},
  \code{\link[GenomicRanges]{GRanges-class}},
  \code{\link{intra-tuple-methods}},
  \code{\link{findOverlaps-methods}},
  \code{\link{nearest-methods}},
}

\examples{
  ## Create example 4-tuples
  seqinfo <- Seqinfo(paste0("chr", 1:3), c(1000, 2000, 1500), NA, "mock1")
  gt4 <- GTuples(seqnames = Rle(c("chr1", "chr2", "chr1", "chr3"),
                                c(1, 3, 2, 4)),
                 tuples = matrix(c(1:10, 2:11, 3:12, 4:13), ncol = 4),
                 strand = Rle(strand(c("-", "+", "*", "+", "-")),
                              c(1, 2, 2, 3, 2)),
                 score = 1:10, GC = seq(1, 0, length = 10), seqinfo = seqinfo)
  gt4
  
  ## Summarizing elements
  table(seqnames(gt4))
  sum(width(gt4))
  summary(mcols(gt4)[,"score"])
  
  ## Renaming the underlying sequences
  seqlevels(gt4)
  seqlevels(gt4) <- sub("chr", "Chrom", seqlevels(gt4))
  gt4
  seqlevels(gt4) <- sub("Chrom", "chr", seqlevels(gt4)) # revert
  
  ## Combining objects
  gt4_a <- GTuples(seqnames = Rle(c("chr1", "chr2", "chr1", "chr3"),
                                c(1, 3, 2, 4)),
                 tuples = matrix(c(1:10, 21:30, 31:40, 41:50), ncol = 4),
                 strand = Rle(strand(c("-", "+", "*", "+", "-")),
                              c(1, 2, 2, 3, 2)),
                 score = 1:10, seqinfo = seqinfo)
                 
  gt4_b <- GTuples(seqnames = Rle(c("chr1", "chr2", "chr1", "chr3"),
                                c(1, 3, 2, 4)),
                 tuples = matrix(c(101:110, 121:130, 131:140, 141:150), 
                                 ncol = 4),
                 strand = Rle(strand(c("-", "+", "*", "+", "-")),
                              c(1, 2, 2, 3, 2)),
                 score = 1:10, seqinfo = seqinfo)
                 
  some_gt4 <- c(gt4_a, gt4_b)

  ## all_gt4 <- c(gt4, gt4_a, gt4_b) ## (This would fail)
  all_gt4 <- c(gt4, gt4_a, gt4_b, ignore.mcols=TRUE)
  
  ## The number of lines displayed in the 'show' method
  ## are controlled with two global options.
  options("showHeadLines" = 7)
  options("showTailLines" = 2)
  all_gt4
  
  ## Revert to default values
  options("showHeadLines"=NULL)
  options("showTailLines"=NULL)
  
  ## Get the size of the tuples stored in the GTuples object
  size(gt4)
  
  ## Get the tuples
  tuples(gt4)
  
  ## Get the matrix of intra-pair distances (IPD)
  IPD(all_gt4)  
  
  ## Can't combine genomic tuples of different sizes
  gt1 <- GTuples('chr1', matrix(30:40))
  gt1
  \dontrun{
  ## Returns error
  c(gt4, gt1)
  }
  
  
}
