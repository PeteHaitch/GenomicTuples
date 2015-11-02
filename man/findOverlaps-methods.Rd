\name{findOverlaps-methods}
\alias{findOverlaps-methods}

% Special treatment
\alias{findOverlaps}
\alias{findOverlaps,GTuples,GTuples-method}
\alias{findOverlaps,GTuplesList,GTuples-method}
\alias{findOverlaps,GTuples,GTuplesList-method}
% GTuples treated as GenomicRanges (resp. GTuplesList as GRangesList)
\alias{findOverlaps,GTuples,GIntervalTree-method}
\alias{findOverlaps,GTuplesList,GTuplesList-method}
\alias{findOverlaps,RangesList,GTuples-method}
\alias{findOverlaps,RangesList,GTuplesList-method}
\alias{findOverlaps,GTuples,RangesList-method}
\alias{findOverlaps,GTuplesList,RangesList-method}
\alias{findOverlaps,RangedData,GTuples-method}
\alias{findOverlaps,RangedData,GTuplesList-method}
\alias{findOverlaps,GTuples,RangedData-method}
\alias{findOverlaps,GTuplesList,RangedData-method}
\alias{findOverlaps,GTuples,GenomicRanges-method}
\alias{findOverlaps,GenomicRanges,GTuples-method}
\alias{findOverlaps,GTuplesList,GenomicRanges-method}
\alias{findOverlaps,GenomicRanges,GTuplesList-method}
\alias{findOverlaps,GTuples,GRangesList-method}
\alias{findOverlaps,GRangesList,GTuples-method}
\alias{findOverlaps,GTuplesList,GRangesList-method}
\alias{findOverlaps,GRangesList,GTuplesList-method}

% Special treatment
\alias{countOverlaps}
\alias{countOverlaps,GTuples,GTuples-method}
\alias{countOverlaps,GTuplesList,GTuplesList-method}
\alias{countOverlaps,GTuples,GTuplesList-method}
\alias{countOverlaps,GTuplesList,GTuples-method}
% GTuples treated as GenomicRanges (resp. GTuplesList as GRangesList)
\alias{countOverlaps,GTuples,Vector-method}
\alias{countOverlaps,Vector,GTuples-method}
\alias{countOverlaps,GTuplesList,Vector-method}
\alias{countOverlaps,Vector,GTuplesList-method}
\alias{countOverlaps,GTuples,GenomicRanges-method}
\alias{countOverlaps,GenomicRanges,GTuples-method}
\alias{countOverlaps,GTuples,GenomicRangesList-method}
\alias{countOverlaps,GenomicRangesList,GTuples-method}
\alias{countOverlaps,GTuplesList,GenomicRanges-method}
\alias{countOverlaps,GenomicRanges,GTuplesList-method}
\alias{countOverlaps,GTuplesList,GenomicRangesList-method}
\alias{countOverlaps,GenomicRangesList,GTuplesList-method}

% Special treatment
\alias{overlapsAny}
\alias{overlapsAny,GTuples,GTuples-method}
\alias{overlapsAny,GTuplesList,GTuples-method}
\alias{overlapsAny,GTuples,GTuplesList-method}
\alias{overlapsAny,GTuplesList,GtuplesList-method}
% GTuples treated as GenomicRanges (resp. GTuplesList as GRangesList)
\alias{overlapsAny,RangesList,GTuples-method}
\alias{overlapsAny,RangesList,GTuplesList-method}
\alias{overlapsAny,GTuples,RangesList-method}
\alias{overlapsAny,GTuplesList,RangesList-method}
\alias{overlapsAny,RangedData,GTuples-method}
\alias{overlapsAny,RangedData,GTuplesList-method}
\alias{overlapsAny,GTuples,RangedData-method}
\alias{overlapsAny,GTuplesList,RangedData-method}
\alias{overlapsAny,GTuples,GRanges-method}
\alias{overlapsAny,GRanges,GTuples-method}
\alias{overlapsAny,GTuples,GRangesList-method}
\alias{overlapsAny,GRangesList,GTuples-method}
\alias{overlapsAny,GTuplesList,GRanges-method}
\alias{overlapsAny,GRanges,GTuplesList-method}
\alias{overlapsAny,GTuplesList,GRangesList-method}
\alias{overlapsAny,GRangesList,GTuplesList-method}

% Special treatment
\alias{subsetByOverlaps}
\alias{subsetByOverlaps,GTuples,GTuples-method}
\alias{subsetByOverlaps,GTuplesList,GTuples-method}
\alias{subsetByOverlaps,GTuples,GTuplesList-method}
\alias{subsetByOverlaps,GTuplesList,GTuplesList-method}
% GTuples treated as GenomicRanges (resp. GTuplesList as GRangesList)
\alias{subsetByOverlaps,RangesList,GTuples-method}
\alias{subsetByOverlaps,RangesList,GTuplesList-method}
\alias{subsetByOverlaps,GTuples,RangesList-method}
\alias{subsetByOverlaps,GTuplesList,RangesList-method}
\alias{subsetByOverlaps,RangedData,GTuples-method}
\alias{subsetByOverlaps,RangedData,GTuplesList-method}
\alias{subsetByOverlaps,GTuples,RangedData-method}
\alias{subsetByOverlaps,GTuplesList,RangedData-method}
\alias{subsetByOverlaps,GTuples,GRanges-method}
\alias{subsetByOverlaps,GRanges,GTuples-method}
\alias{subsetByOverlaps,GTuples,GRangesList-method}
\alias{subsetByOverlaps,GRangesList,GTuples-method}
\alias{subsetByOverlaps,GTuplesList,GRanges-method}
\alias{subsetByOverlaps,GRanges,GTuplesList-method}
\alias{subsetByOverlaps,GTuplesList,GRangesList-method}
\alias{subsetByOverlaps,GRangesList,GTuplesList-method}

\title{Finding overlapping genomic tuples}

\description{
  Finds tuple overlaps between a \link{GTuples} or \link{GTuplesList} object, 
  and another object containing tuples or ranges.
  
  NOTE: The \code{\link[IRanges]{findOverlaps}} generic function and 
  methods for \link[IRanges]{Ranges} and \link[IRanges]{RangesList} objects
  are defined and documented in the \pkg{IRanges} package.
  The methods for \link[GenomicRanges]{GenomicRanges},
  \link[GenomicRanges]{GRangesList}, and
  \link[GenomicRanges]{GIntervalTree} objects are defined and
  documented in the \pkg{GenomicRanges} package.
  The methods for \link[GenomicAlignments]{GAlignments},
  \link[GenomicAlignments]{GAlignmentPairs}, and
  \link[GenomicAlignments]{GAlignmentsList} objects are defined and
  documented in the \pkg{GenomicAlignments} package.
}

\usage{
\S4method{findOverlaps}{GTuples,GTuples}(query, subject,
    maxgap = 0L, minoverlap = 1L,
    type = c("any", "start", "end", "within", "equal"),
    select = c("all", "first", "last", "arbitrary"),
    algorithm = c("nclist", "intervaltree"),
    ignore.strand = FALSE)

\S4method{countOverlaps}{GTuples,GTuples}(query, subject,
    maxgap = 0L, minoverlap = 1L,
    type = c("any", "start", "end", "within", "equal"),
    algorithm = c("nclist", "intervaltree"),
    ignore.strand = FALSE)

\S4method{overlapsAny}{GTuples,GTuples}(query, subject,
    maxgap = 0L, minoverlap = 1L,
    type = c("any", "start", "end", "within", "equal"),
    algorithm = c("nclist", "intervaltree"),
    ignore.strand = FALSE)

\S4method{subsetByOverlaps}{GTuples,GTuples}(query, subject,
    maxgap = 0L, minoverlap = 1L,
    type = c("any", "start", "end", "within", "equal"),
    algorithm = c("nclist", "intervaltree"),
    ignore.strand = FALSE)
}

\arguments{
  \item{query, subject}{
    A \code{\link{GTuples}} or \code{\link{GTuplesList}} object. 
    \code{\link[GenomicRanges]{GRanges}}, 
    \code{\link[GenomicRanges]{GRangesList}}, \code{\link[IRanges]{RangesList}} 
    and \code{\link[IRanges]{RangedData}} are also accepted for one of 
    \code{query} or \code{subject}.
  }
  \item{type}{
    See details below.
  }
  \item{algorithm}{
    Please see \code{GenomicRanges::\link[GenomicRanges]{findOverlaps}}.
  }
  \item{maxgap, minoverlap}{
    See \code{\link[IRanges]{findOverlaps}} in the IRanges package for
    a description of these arguments. These arguments have no effect if both 
    \code{query} and \code{subject} are \code{\link{GTuples}} objects and 
    \code{type = "equal"}.
  }
  \item{select}{
    See \code{\link[IRanges]{findOverlaps}} in the IRanges package for
    a description of this argument.}
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
      as occuring on both the \code{"+"} and \code{"-"} strand. An overlap is 
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

  When the query is a \code{\link{GTuples}} or \code{\link{GTuplesList}} object 
  then the \code{subject} can be a \link{GIntervalTree} object. For repeated 
  queries against the same subject, it is more efficient to create a 
  \code{GIntervalTree} once for the subject using the \link{GIntervalTree} 
  constructor described below and then perform the queries against
  the \code{GIntervalTree} instance. \strong{NOTE}: \code{\link{GIntervalTree}}
  objects only support genomic ranges and not genomic tuples, and are not 
  supported for circular genomes.
}

\value{
  For \code{findOverlaps} either a \link[S4Vectors]{Hits} object when
  \code{select = "all"} or an integer vector otherwise. 

  For \code{countOverlaps} an integer vector containing the tabulated
  query overlap hits.

  For \code{overlapsAny} a logical vector of length equal to the number of
  tuples/ranges in \code{query} indicating those that overlap any of the 
  tuples/ranges in \code{subject}.

  For \code{subsetByOverlaps} an object of the same class as \code{query}
  containing the subset that overlapped at least one entity in \code{subject}.

  For \code{RangedData} and \code{RangesList}, with the exception of
  \code{subsetByOverlaps}, the results align to the unlisted
  form of the object. This turns out to be fairly convenient for
  \code{RangedData} (not so much for \code{RangesList}, but something
  has to give).
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
    \item \code{\link[GenomicRanges]{GIntervalTree-class}}
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
