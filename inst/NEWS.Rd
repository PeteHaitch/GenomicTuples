\name{GenomicTuplesnews}
\title{GenomicTuples News}
\encoding{UTF-8}

\section{Version 1.5.x}{

\itemize{

\item New findOverlaps,GTuples,GTuples-method when type = "equal" gives 10-100x 
      speedup by using data.table.
\item After being deprecated from GenomicRanges in BioC 3.1, mapCoords() and 
      pmapCoords() are now defunct.
\item After being deprecated in BioC 3.1, the "intervaltree" algorithm in 
      findOverlaps() is now defunct.
}}

\section{Version 1.1.x}{

\itemize{

\item Switched to a html vignette from a pdf one.

}}

\section{Version 0.99.1}{

Addressing comments made in Bioconductor code review.

\itemize{

\item Added missing \code{Value} sections to \code{GTuples-comparison.Rd} and 
\code{intra-tuple-methods.Rd}.
\item Improved description of \code{findOverlaps,GTuples,GTuples-method} and 
what it means for two tuples to overlap. In particular, included a reference to 
the vignette which uses examples to explain these definitions.
\item Included in the vignette my motivation for creating the 
\code{GenomicTuples} package.
}}

\section{Version 0.99.0}{\itemize{

\item Initial version submitted to Bioconductor.

}}