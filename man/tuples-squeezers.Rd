\name{tuples-squeezers}

\alias{tuples-squeezers}

\alias{gtuples}
\alias{gtlist}

\title{Squeeze the tuples out of a tuples-based object}

\description{
  S4 generic functions for squeezing the tuples out of a tuples-based object. 
  Similar to the S4 generic functions for squeezing the ranges out of a 
  ranged-based object, see
  \link[GenomicRanges:genomic-range-squeezers]{granges} and
  \link[GenomicRanges:genomic-range-squeezers]{grglist}.

  \code{gtuples} returns them as a \link{GTuples} object, and \code{gtlist} as 
  a \link{GTuplesList} object.
}

\usage{
gtuples(x, use.mcols=FALSE, ...)
gtlist(x, use.mcols=FALSE, ...)
}

\arguments{
  \item{x}{
    A tuples-based object.
    % TODO: Uncomment once MethylationTuples is part of Bioconductor.
    % e.g. a \link{SummarizedExperiment},
    % \link[MethylationTuples]{MethPat},
    % or \link[MethylationTuples]{SparseMethPat} object.
  }
  \item{use.mcols}{
    \code{TRUE} or \code{FALSE} (the default).
    Whether the metadata columns on \code{x} (accessible with \code{mcols(x)})
    should be propagated to the returned object or not.
  }
  \item{...}{
    Additional arguments, for use in specific methods.
  }
}

\details{
  The \pkg{MethylationTuples} 
  (\url{https://github.com/PeteHaitch/MethylationTuples}) package defines and
  document methods for various types of tuples-based objects.
  % TODO: Uncomment once MethylationTuples is part of Bioconductor.
  % (e.g. for
  % \link[MethylationTuples]{MethPat},
  % and \link[MethylationTuples]{SparseMethPat} objects).
  Other Bioconductor packages might as well.

  Note that these functions can be seen as a specific kind of \emph{object
  getters} as well as functions performing coercion.
}

\value{
  A \link{GTuples} object for \code{gtuples}.

  A \link{GTuplesList} object for \code{gtlist}.

  If \code{x} is a vector-like object, the returned object is expected
  to be \emph{parallel} to \code{x}, that is, the i-th element in the output
  corresponds to the i-th element in the input. If \code{x} has names on it,
  they're propagated to the returned object. If \code{use.mcols} is TRUE and
  \code{x} has metadata columns on it (accessible with \code{mcols(x)}),
  they're propagated to the returned object.
}

\author{Peter Hickey}

\seealso{
  \itemize{
    \item \link{GTuples} and \link{GTuplesList} objects.

    % TODO: Uncomment once MethylationTuples is part of Bioconductor.
%    \item \link[MethylationTuples]{MethPat} and 
%          \link[MethylationTuples]{SparseMethPat} objects.
  }
}


% TODO: Update comment once MethylationTuples is part of Bioconductor.
\examples{
## See ?MethPat in the MethylationTuples package (GitHub-only package) for some
## examples.
}

\keyword{methods}