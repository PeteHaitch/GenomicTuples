% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllUtilities.R
\name{.zero_range}
\alias{.zero_range}
\title{Check whether all elements of a numeric vector are identical (within machine 
precision)}
\usage{
.zero_range(x, tol = .Machine$double.eps^0.5)
}
\arguments{
\item{x}{a numeric vector.}
}
\value{
TRUE if all elements of the vector are identical (within machine 
precision). FALSE in all other cases, including if the vector contains any 
NAs.
}
\description{
Check whether all elements of a numeric vector are identical (within machine 
precision)
}
\note{
This function is based on Hadley and John's answer to 
http://stackoverflow.com/q/4752275. No check is made that \code{x} is a 
numeric vector.
}
\keyword{internal}
