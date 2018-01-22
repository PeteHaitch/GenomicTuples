\name{findOverlaps-methods}
\alias{findOverlaps-methods}

% Special treatment
\alias{findOverlaps}
\alias{findOverlaps,GTuples,GTuples-method}

\alias{countOverlaps}
% Special treatment
\alias{countOverlaps,GTuples,GTuples-method}

\alias{overlapsAny}
% Special treatment
\alias{overlapsAny,GTuples,GTuples-method}

\alias{subsetByOverlaps}
% Special treatment
\alias{subsetByOverlaps,GTuples,GTuples-method}

\title{Finding overlapping genomic tuples}

\description{
  Various methods for finding/counting overlaps between objects containing
  genomic tuples. This man page describes the methods that operate on
  \link{GTuples} and \link{GTuplesList} objects.
  
  NOTE: The \code{\link[IRanges]{findOverlaps}} generic function is defined and 
  documented in the \pkg{IRanges} package. The \code{findOverlaps} method for 
  \link[GenomicRanges]{GenomicRanges} and \link[GenomicRanges]{GRangesList} 
  objects are defined and documented in the \pkg{GenoicRanges} package.
  
  \link{GTuples} and \link{GTuplesList} objects also support
  \code{countOverlaps}, \code{overlapsAny}, and \code{subsetByOverlaps} 
  thanks to the default methods defined in the \pkg{IRanges} package and
  to the \code{findOverlaps} and \code{countOverlaps} methods defined in
  this package and documented below.
}

\usage{
\S4method{findOverlaps}{GTuples,GTuples}(query, subject,
    maxgap = -1L, minoverlap = 0L,
    type = c("any", "start", "end", "within", "equal"),
    select = c("all", "first", "last", "arbitrary"),
    ignore.strand = FALSE)

\S4method{countOverlaps}{GTuples,GTuples}(query, subject,
    maxgap = -1L, minoverlap = 0L,
    type = c("any", "start", "end", "within", "equal"),
    ignore.strand = FALSE)
}

\arguments{
  \item{query, subject}{
    A \code{\link{GTuples}} or \code{\link{GTuplesList}} object. 
  }
  \item{type}{
    See details below.
  }
  \item{maxgap, minoverlap}{
    See \code{?\link[IRanges]{findOverlaps}} in the \pkg{IRanges} package
    for a description of these arguments.
  }
  \item{select}{
    When \code{select} is \code{"all"} (the default), the results are
    returned as a \link[S4Vectors]{Hits} object.
    Otherwise the returned value is an integer vector parallel to \code{query}
    (i.e. same length) containing the first, last, or arbitrary overlapping
    interval in \code{subject}, with \code{NA} indicating intervals that did
    not overlap any intervals in \code{subject}.
  }
  \item{ignore.strand}{
    When set to \code{TRUE}, the strand information is ignored in the
    overlap calculations.
  }
}

\details{
  The \code{findOverlaps}-based methods involving genomic tuples, 
  either through \code{\link{GTuples}} or \code{\link{GTuplesList}} objects, 
  can search for \emph{tuple-tuple}, \emph{tuple-range} and \emph{range-tuple} 
  overlaps. Each of these are described below, with attention paid to 
  the important special case of finding "equal tuple-tuple overlaps".
      
  \describe{
    \item{Equal tuple-tuple overlaps}{
      When the \code{query} and the \code{subject} are both 
      \code{\link{GTuples}} objects and \code{type = "equal"}, 
      \code{findOverlaps} uses the seqnames (\code{\link{seqnames}}), positions 
      (\code{\link{tuples,GTuples-method}}) and strand (\code{\link{strand}}) 
      to determine which tuples from the \code{query} exactly match those in 
      the \code{subject}, where a \code{strand} value of \code{"*"} is treated 
      as occurring on both the \code{"+"} and \code{"-"} strand. An overlap is 
      recorded when a tuple in the \code{query} and a tuple in the
      \code{subject} have the same sequence name, have a compatible pairing of 
      strands (e.g. \code{"+"}/\code{"+"}, \code{"-"}/\code{"-"}, 
      \code{"*"}/\code{"+"}, \code{"*"}/\code{"-"}, etc.), and have 
      identical positions.
      
      \strong{NOTE}: Equal tuple-tuple overlaps can only be computed if 
      \code{size(query)} is equal to \code{size(subject)}.
    }
      
    \item{Other tuple-tuple overlaps}{
      When the \code{query} and the \code{subject} are \code{\link{GTuples}} or 
      \code{\link{GTuplesList}} objects and \code{type = "any"}, 
      \code{"start"}, \code{"end"} or \code{"within"}, \code{findOverlaps} 
      treats the tuples as if they were ranges, with ranges given by 
      \eqn{[pos_{1}, pos_{m}]} and where \eqn{m} is the 
      \code{\link{size,GTuples-method}} of 
      the tuples. This is done via inheritance so that a \code{\link{GTuples}} 
      (resp. \code{\link{GTuplesList}}) object is treated as a 
      \code{\link[GenomicRanges]{GRanges}} (resp. 
      \code{\link[GenomicRanges]{GRangesList}}) and the appropriate 
      \code{findOverlaps} method is dispatched upon.
      
      \strong{NOTE}: This is the only type of overlap finding available 
      when either the \code{query} and \code{subject} are 
      \code{\link{GTuplesList}} objects. This is following the behaviour of 
      \code{\link[GenomicRanges]{findOverlaps,GRangesList,GRangesList-method}} 
      that allows \code{type = "any"}, \code{"start"}, \code{"end"} or 
      \code{"within"} but does not allow \code{type = "equal"}.
    }
      
    \item{tuple-range and range-tuple overlaps}{
      When one of the \code{query} and the \code{subject} is not a 
      \code{\link{GTuples}} or \code{\link{GTuplesList}} objects, 
      \code{findOverlaps} treats the tuples as if they were ranges, with ranges 
      given by \eqn{[pos_{1}, pos_{m}]} and where \eqn{m} is the 
      \code{\link{size,GTuples-method}} of the tuples. This is done via 
      inheritance so that a \code{\link{GTuples}} (resp. 
      \code{\link{GTuplesList}}) object is treated as a 
      \code{\link[GenomicRanges]{GRanges}} (resp. 
      \code{\link[GenomicRanges]{GRangesList}}) and the appropriate 
      \code{findOverlaps} method is dispatched upon.
    }
  }

  In the context of \code{findOverlaps}, a feature is a collection of
  tuples/ranges that are treated as a single entity. For \code{\link{GTuples}} 
  objects, a feature is a single tuple; while for \code{\link{GTuplesList}} 
  objects, a feature is a list element containing a set of tuples. In the 
  results, the features are referred to by number, which run from 1 to 
  \code{length(query)}/\code{length(subject)}.
}

\value{
  For \code{findOverlaps}, either a \link[S4Vectors]{Hits} object when
  \code{select = "all"} or an integer vector otherwise. 

  For \code{countOverlaps}, an integer vector containing the tabulated
  query overlap hits.

  For \code{overlapsAny} a logical vector of length equal to the number of
  tuples/ranges in \code{query} indicating those that overlap any of the 
  tuples/ranges in \code{subject}.

  For \code{subsetByOverlaps} an object of the same class as \code{query}
  containing the subset that overlapped at least one entity in \code{subject}.
}

\author{Peter Hickey for methods involving \code{\link{GTuples}} and 
\code{\link{GTuplesList}}. P. Aboyoun, S. Falcon, M. Lawrence, 
N. Gopalakrishnan, H. Pages and H. Corrada Bravo for all the real work 
underlying the powerful \code{findOverlaps} functionality.}

\seealso{
  \itemize{
    \item Please see the package vignette for an extended discussion of 
    overlaps involving genomic tuples, which is available by typing 
    \code{vignette(topic = 'GenomicTuplesIntroduction', package = 'GenomicTuples')} 
    at the R prompt.
    \item \code{\link[GenomicRanges]{findOverlaps}}
    \item \code{\link[IRanges]{findOverlaps}}
    \item \code{\link[S4Vectors]{Hits-class}}
    \item \code{\link{GTuples-class}}
    \item \code{\link{GTuplesList-class}}
    \item \code{\link[GenomicRanges]{GRanges-class}}
    \item \code{\link[GenomicRanges]{GRangesList-class}}
  }
}

\examples{
## GTuples object containing 3-tuples:
gt3 <- GTuples(seqnames = c('chr1', 'chr1', 'chr1', 'chr1', 'chr2'), 
               tuples = matrix(c(10L, 10L, 10L, 10L, 10L, 20L, 20L, 20L, 25L, 
                                 20L, 30L, 30L, 35L, 30L, 30L), ncol = 3), 
               strand = c('+', '-', '*', '+', '+'))
               
## GTuplesList object
gtl3 <- GTuplesList(A = gt3[1:3], B = gt3[4:5])
               
## Find equal genomic tuples:
findOverlaps(gt3, gt3, type = 'equal')
## Note that this is different to the results if the tuples are treated as 
## ranges since this ignores the "internal positions" (pos2):
findOverlaps(granges(gt3), granges(gt3), type = 'equal')
               
## Scenarios where tuples are treated as ranges:
findOverlaps(gt3, gt3, type = 'any')
findOverlaps(gt3, gt3, type = 'start')
findOverlaps(gt3, gt3, type = 'end')
findOverlaps(gt3, gt3, type = 'within')

## Overlapping a GTuples and a GTuplesList object (tuples treated as ranges):
table(!is.na(findOverlaps(gtl3, gt3, select="first")))
countOverlaps(gtl3, gt3)
findOverlaps(gtl3, gt3)
subsetByOverlaps(gtl3, gt3)
countOverlaps(gtl3, gt3, type = "start")
findOverlaps(gtl3, gt3, type = "start")
subsetByOverlaps(gtl3, gt3, type = "start")
findOverlaps(gtl3, gt3, select = "first")
}

\keyword{methods}
\keyword{utilities}
