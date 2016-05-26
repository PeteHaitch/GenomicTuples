\name{nearest-methods}

\alias{nearest-methods}

\alias{precede,GTuples,GTuples-method}
\alias{precede,GTuples,missing-method}
\alias{follow,GTuples,GTuples-method}
\alias{follow,GTuples,missing-method}
\alias{nearest,GTuples,GTuples-method}
\alias{nearest,GTuples,missing-method}
\alias{distance,GTuples,GTuples-method}
\alias{distanceToNearest,GTuples,GTuples-method}
\alias{distanceToNearest,GTuples,missing-method}

\title{Finding the nearest genomic tuple/range neighbour}

\description{
  The \code{nearest}, \code{precede}, \code{follow}, \code{distance}
  and \code{distanceToNearest} methods for \code{\linkS4class{GTuples}}
  objects and subclasses.
  
  \strong{NOTE}: These methods treat the tuples as if they were ranges, with 
  ranges given by \eqn{[pos_{1}, pos_{m}]} and where \eqn{m} is the 
  \code{\link{size,GTuples-method}} of the tuples. This is done via inheritance 
  so that a \code{\link{GTuples}} object is treated as a 
  \code{\link[GenomicRanges]{GRanges}} and the appropriate method is dispatched 
  upon.
}

\usage{
\S4method{precede}{GTuples,GTuples}(x, subject, select = c("arbitrary", "all"), 
        ignore.strand = FALSE, ...)
\S4method{precede}{GTuples,missing}(x, subject, select = c("arbitrary", "all"), 
        ignore.strand = FALSE, ...)

\S4method{follow}{GTuples,GTuples}(x, subject, select = c("arbitrary", "all"), 
       ignore.strand=FALSE, ...)
\S4method{follow}{GTuples,missing}(x, subject, select = c("arbitrary", "all"), 
       ignore.strand = FALSE, ...)

\S4method{nearest}{GTuples,GTuples}(x, subject, select = c("arbitrary", "all"), 
        ignore.strand = FALSE, ...)
\S4method{nearest}{GTuples,missing}(x, subject, select = c("arbitrary", "all"), 
        ignore.strand = FALSE, ...)

\S4method{distanceToNearest}{GTuples,GTuples}(x, subject, ignore.strand = FALSE, 
                  ...)
\S4method{distanceToNearest}{GTuples,missing}(x, subject, ignore.strand = FALSE, 
                  ...)

\S4method{distance}{GTuples,GTuples}(x, y, ignore.strand = FALSE, ...)
}

\arguments{
  \item{x}{The query \link{GTuples} instance.
  }
  \item{subject}{The subject \link{GTuples} instance
    within which the nearest neighbours are found. Can be missing,
    in which case \code{x} is also the subject. 
  }
  \item{y}{For the \code{distance} method, a \code{GTuples} or \code{GRanges} 
  instance. Cannot be missing. If \code{x} and \code{y} are not the same 
  length, the shortest will be recycled to match the length of the longest.
  }
  \item{select}{Logic for handling ties. By default, all methods
    select a single tuple/range (arbitrary for \code{nearest},
    the first by order in \code{subject} for \code{precede}, and the
    last for \code{follow}). 

    When \code{select = "all"} a \link[S4Vectors]{Hits} object is returned with 
    all matches for \code{x}. If \code{x} does not have a match in 
    \code{subject} the \code{x} is not included in the \link[S4Vectors]{Hits}
    object. 
  }
  \item{ignore.strand}{
    A \code{logical} indicating if the strand of the input tuples/ranges
    should be ignored. When \code{TRUE}, strand is set to \code{'+'}.
  }
  \item{...}{Additional arguments for methods.}
}

\details{
  \itemize{
    \item{nearest: }{
      Performs conventional nearest neighbour finding.
      Returns an integer vector containing the index of the nearest neighbour 
      tuple/range in \code{subject} for each range in \code{x}. If there is no 
      nearest neighbour \code{NA} is returned. For details of the algorithm
      see the man page in IRanges, ?\code{nearest}. 
    }
    \item{precede: }{
      For each range in \code{x}, \code{precede} returns
      the index of the tuple/range in \code{subject} that is directly 
      preceded by the tuple/range in \code{x}. Overlapping tuples/ranges are 
      excluded. \code{NA} is returned when there are no qualifying 
      tuples/ranges in \code{subject}.
    }
    \item{follow: }{
      The opposite of \code{precede}, \code{follow} returns
      the index of the tuple/range in \code{subject} that is directly followed 
      by the tuple/range in \code{x}. Overlapping tuples/ranges are excluded. 
      \code{NA} is returned when there are no qualifying tuples/ranges in 
      \code{subject}.
    }
    \item{Orientation and Strand: }{
      The relevant orientation for \code{precede} and \code{follow} 
      is 5' to 3', consistent with the direction of translation. 
      Because positional numbering along a chromosome is from left to 
      right and transcription takes place from 5' to 3', \code{precede} and 
      \code{follow} can appear to have `opposite' behaviour on the \code{+}
      and \code{-} strand. Using positions 5 and 6 as an example, 5 precedes 
      6 on the \code{+} strand but follows 6 on the \code{-} strand.

      A tuple/range with strand \code{*} can be compared to tuples/ranges on 
      either the \code{+} or \code{-} strand. Below we outline the priority when 
      tuples/ranges on multiple strands are compared. When 
      \code{ignore.strand=TRUE} all tuples/ranges are treated as if on the 
      \code{+} strand. 
      \itemize{
        \item{}{x on + strand can match to tuples/ranges on both + and 
          * strands. In the case of a tie the first tuple/range by order 
          is chosen.
        } 
        \item{}{x on - strand can match to tuples/ranges on both - and 
          * strands. In the case of a tie the first tuple/range by order 
          is chosen.
        } 
        \item{}{x on * strand can match to tuples/ranges on any of +, 
          - or * strands. In the case of a tie the first tuple/range by
          order is chosen.
        }
      }
    }
    \item{distanceToNearest: }{Returns the distance for each tuple/range in 
    \code{x} to its nearest neighbour in the \code{subject}. 
    }
    \item{distance: }{
      Returns the distance for each tuple/range in \code{x} to the range in 
      \code{y}. The behaviour of \code{distance} has changed in Bioconductor 
      2.12. See the man page \code{?distance} in IRanges for details.  
    }
  }
}

\value{
  For \code{nearest}, \code{precede} and \code{follow}, an integer
  vector of indices in \code{subject}, or a \link[S4Vectors]{Hits} if 
  \code{select = "all"}.

  For \code{distanceToNearest}, a \link[S4Vectors]{Hits} object with a
  column for the \code{query} index (from), \code{subject} index
  (to) and the \code{distance} between the pair.

  For \code{distance}, an integer vector of distances between the tuples/ranges
  in \code{x} and \code{y}.
}

\author{Peter Hickey for methods involving \code{\link{GTuples}}. P. Aboyoun 
and V. Obenchain <vobencha@fhcrc.org> for all the real work underlying the 
powerful nearest methods.}

\seealso{
  \itemize{
    \item The \link{GTuples} and \link{GRanges} classes.
    \item The \link[GenomicRanges]{GenomicRanges} and 
      \link[GenomicRanges]{GRanges} classes in the \pkg{GenomicRanges} package.
    \item The \link[IRanges]{Ranges} class in the \pkg{IRanges} package.
    \item The \link[S4Vectors]{Hits} class in the \pkg{S4Vectors} package.
    \item The \link[GenomicRanges]{nearest-methods} man page in the 
      \pkg{GenomicRanges} package.
    \item \link[GenomicTuples]{findOverlaps-methods} for finding just the
      overlapping ranges.
  }
}

\examples{
  ## -----------------------------------------------------------
  ## precede() and follow()
  ## -----------------------------------------------------------
  query <- GTuples("A", matrix(c(5L, 20L, 6L, 21L), ncol = 2), strand = "+")
  subject <- GTuples("A", matrix(c(rep(c(10L, 15L), 2), rep(c(11L, 16L), 2)), 
                                 ncol = 2),
                          strand = c("+", "+", "-", "-"))
  precede(query, subject)
  follow(query, subject)
 
  strand(query) <- "-"
  precede(query, subject)
  follow(query, subject)
 
  ## ties choose first in order
  query <- GTuples("A", matrix(c(10L, 11L), ncol = 2), c("+", "-", "*"))
  subject <- GTuples("A", matrix(c(rep(c(5L, 15L), each = 3), 
                                   rep(c(6L, 16L), each = 3)), ncol = 2),
                          rep(c("+", "-", "*"), 2))
  precede(query, subject)
  precede(query, rev(subject))
 
  ## ignore.strand = TRUE treats all ranges as '+'
  precede(query[1], subject[4:6], select="all", ignore.strand = FALSE)
  precede(query[1], subject[4:6], select="all", ignore.strand = TRUE)
  
  ## -----------------------------------------------------------
  ## nearest()
  ## -----------------------------------------------------------
  ## When multiple tuples overlap an "arbitrary" tuple is chosen
  query <- GTuples("A", matrix(c(5L, 15L), ncol = 2))
  subject <- GTuples("A", matrix(c(1L, 15L, 5L, 19L), ncol = 2))
  nearest(query, subject)
 
  ## select = "all" returns all hits
  nearest(query, subject, select = "all")
 
  ## Tuples in 'x' will self-select when 'subject' is present
  query <- GTuples("A", matrix(c(1L, 10L, 6L, 15L), ncol = 2))
  nearest(query, query)
 
  ## Tuples in 'x' will not self-select when 'subject' is missing
  nearest(query)
  
  ## -----------------------------------------------------------
  ## distance(), distanceToNearest()
  ## -----------------------------------------------------------
  ## Adjacent, overlap, separated by 1
  query <- GTuples("A", matrix(c(1L, 2L, 10L, 5L, 8L, 11L), ncol = 2))
  subject <- GTuples("A", matrix(c(6L, 5L, 13L, 10L, 10L, 15L), ncol = 2))
  distance(query, subject)

  ## recycling
  distance(query[1], subject)

  query <- GTuples(c("A", "B"), matrix(c(1L, 5L, 2L, 6L), ncol = 2))
  distanceToNearest(query, subject)
}
 
\keyword{utilities}