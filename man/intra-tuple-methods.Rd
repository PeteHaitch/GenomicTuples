\name{intra-tuple-methods}

\alias{intra-tuple-methods}

\alias{shift}
\alias{shift,GTuples-method}
\alias{shift,GTuplesList-method}

\alias{trim,GTuples-method}

\title{Intra-tuple transformations of a GTuples or GTuplesLists object}

\description{
  This man page documents intra-tuple transformations of a
  \code{\link{GTuples}} or a \code{\link{GTuplesList}} object.
  
  \strong{WARNING}: These are not exactly the same as the intra-range methods 
  defined in the \pkg{GenomicRanges} package 
  (\code{?GenomicRanges::\link[GenomicRanges]{intra-range-methods}}) or in the 
  \pkg{IRanges} package (\code{?IRanges::\link[IRanges]{intra-range-methods}}).
}

\usage{
\S4method{shift}{GTuples}(x, shift = 0L, use.names = TRUE)
\S4method{shift}{GTuplesList}(x, shift = 0L, use.names = TRUE)

\S4method{trim}{GTuples}(x, use.names = TRUE)
}

\arguments{
  \item{x}{A \code{\link{GTuples}} or \code{\link{GTuplesList}} object.}
  \item{shift, use.names}{
    See \code{?`\link[IRanges]{intra-range-methods}`}.
  }
}

\details{
  \itemize{
    \item{}{\code{shift} behaves like the \code{shift} method for 
      \link[GenomicRanges]{GRanges} objects, except that any \code{internalPos} 
      are also shifted. See \code{?`\link[GenomicRanges]{intra-range-methods}`} 
      for further details of the \code{shift} method.
    }
    \item{}{\code{trim} trims out-of-bound tuples located on non-circular
      sequences whose length is not NA.
    }
  }
}

\author{Peter Hickey for methods involving \code{GTuples} and 
\code{GTuplesList}. P. Aboyoun and V. Obenchain <vobencha@fhcrc.org> for all 
the real work underlying the powerful intra-range methods.}

\value{
  See Details section above.
}

\seealso{
  \itemize{
    \item \code{\link{GTuples}} and \code{\link{GTuplesList}} objects.
    \item The \link[GenomicRanges]{intra-range-methods} man page in the
          \pkg{GenomicRanges} package.
  }
}

\examples{
## ---------------------------------------------------------------------
## A. ON A GTuples OBJECT
## ---------------------------------------------------------------------
gt3 <- GTuples(seqnames = c('chr1', 'chr1', 'chr1', 'chr1', 'chr2'), 
               tuples = matrix(c(10L, 10L, 10L, 10L, 10L, 20L, 20L, 20L, 25L, 
                                 20L, 30L, 30L, 35L, 30L, 30L), ncol = 3), 
               strand = c('+', '-', '*', '+', '+'))
gt3

shift(gt3, 10)

## ---------------------------------------------------------------------
## B. ON A GTuplesList OBJECT
## ---------------------------------------------------------------------
gtl3 <- GRangesList(A = gt3, B = rev(gt3))
gtl3

shift(gtl3, IntegerList(10, 100))

}

\keyword{methods}
\keyword{utilities}
