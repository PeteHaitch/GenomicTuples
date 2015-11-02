\name{Undefined methods}

% AllUtilites
\alias{replaceROWS,NULL-method}

% GTuples-class methods
\alias{granges,GTuples-method}

% intra-tuple methods
\alias{flank,GTuples-method}
\alias{flank,GTuplesList-method}
\alias{promoters,GTuples-method}
\alias{promoters,GTuplesList-method}
\alias{resize,GTuples-method}
\alias{resize,GTuplesList-method}
\alias{restrict,GTuplesList-method}

% inter-tuple methods
\alias{disjoin,GTuples-method}
\alias{disjoin,GTuplesList-method}
\alias{disjointBins,GTuples-method}
\alias{gaps,GTuples-method}
\alias{isDisjoint,GTuples-method}
\alias{isDisjoint,GTuplesList-method}
\alias{range,GTuples-method}
\alias{range,GTuplesList-method}
\alias{reduce,GTuples-method}
\alias{reduce,GTuplesList-method}

% mapCoords methods
\alias{mapCoords,GTuples,GTuplesList-methods}

% setops methods
\alias{Ops,GTuples,numeric-method}
\alias{intersect,GTuples,GTuples-method}
\alias{pgap,GTuples,GTuples-method}
\alias{pintersect,GTuples,GTuples-method}
\alias{pintersect,GTuples,GTuplesList-method}
\alias{pintersect,GTuplesList,GTuples-method}
\alias{pintersect,GTuplesList,GTuplesList-method}
\alias{psetdiff,GTuples,GTuples-method}
\alias{psetdiff,GTuples,GTuplesList-method}
\alias{psetdiff,GTuplesList,GTuplesList-method}
\alias{punion,GTuples,GTuples-method}
\alias{punion,GTuples,GTuplesList-method}
\alias{punion,GTuplesList,GTuples-method}
\alias{setdiff,GTuples,GTuples-method}
\alias{union,GTuples,GTuples-method}

% tile methods
\alias{tile,GTuples-method}

\title{Undefined methods}

\description{
  These are methods defined for \code{\link[GenomicRanges]{GRanges}} and 
  \code{\link[GenomicRanges]{GRangesList}} objects that have no well-defined 
  equivalent for \code{\link{GTuples}} or \code{\link{GTuplesList}}. Therefore, 
  I have explicitly written methods for these that return errors when called.
}
