\name{GTuplesList-class}
\docType{class}

\encoding{utf8} 

% Class:
\alias{class:GTuplesList}
\alias{GTuplesList-class}
\alias{GTuplesList}

% Constructors:
\alias{GTuplesList}
\alias{updateObject,GTuplesList-method}

% Accessor methods:
\alias{seqnames,GTuplesList-method}
\alias{seqnames<-,GTuplesList-method}
\alias{ranges,GTuplesList-method}
\alias{ranges<-,GTuplesList-method}
\alias{strand,GTuplesList-method}
\alias{strand<-,GTuplesList,ANY-method}
\alias{strand<-,GTuplesList,character-method}
\alias{elementMetadata,GTuplesList-method}
\alias{elementMetadata<-,GTuplesList-method}
\alias{seqinfo,GTuplesList-method}
\alias{seqinfo<-,GTuplesList-method}
\alias{score,GTuplesList-method}
\alias{score<-,GTuplesList-method}

% Coercion:
\alias{coerce,GTuplesList,GRangesList-method}
\alias{coerce,GTuplesList,data.frame-method}
\alias{coerce,GTuplesList,list-method}

% Tuples methods:
\alias{tuples,GTuplesList-method}
\alias{tuples<-,GTuplesList-method}
\alias{start,GTuplesList-method}
\alias{start<-,GTuplesList-method}
\alias{end,GTuplesList-method}
\alias{end<-,GTuplesList-method}
\alias{width,GTuplesList-method}
\alias{width<-,GTuplesList-method}
\alias{IPD,GTuplesList-method}
\alias{size,GTuplesList-method}


% Subsetting:
\alias{[,GTuplesList,ANY-method}
\alias{[<-,GTuplesList,ANY,ANY,ANY-method}
\alias{[[<-,GTuplesList,ANY,ANY,ANY-method}

% extractList() and family:
\alias{relistToClass,GTuples-method}

\title{GTuplesList objects}

\description{
  The \code{GTuplesList} class is a container for storing a collection of
  \code{\link{GTuples}} objects. It is derived from 
  \code{\linkS4class{GRangesList}}.
  
}

\section{Constructor}{
  \describe{
    \item{\code{GTuplesList(...)}:}{
      Creates a \code{GTuplesList} object using \code{\link{GTuples}} objects
      supplied in \code{\dots}.
    }
  }
}

\section{Accessors}{
  In the following code snippets, \code{x} is a \code{\link{GTuplesList}} 
  object.
  
  \describe{
    \item{\code{length(x)}:}{
      Get the number of list elements.
    }
    \item{\code{names(x)}, \code{names(x) <- value}:}{
      Get or set the names on \code{x}.
    }
    \item{\code{elementNROWS(x)}:}{
      Get the \code{length} of each of the list elements.
    }
    \item{\code{isEmpty(x)}:}{
      Returns a logical indicating either if the \code{GTuplesList} has no
      elements or if all its elements are empty.
    }
    \item{\code{seqnames(x)}, \code{seqnames(x) <- value}:}{
      Get or set the sequence names in the form of an 
      \code{\linkS4class{RleList}}. \code{value} can be an 
      \code{\linkS4class{RleList}} or \code{\linkS4class{CharacterList}} 
      object.
    }
    \item{\code{tuples(x)}, \code{tuples(x) <- value}:}{
      Get or set the tuples in the form of a 
      \code{\linkS4class{SimpleList}} of integer matrices. \code{value} can 
      be a a single integer matrix.
    }
    \item{\code{ranges(x, use.mcols = FALSE)}, \code{ranges(x) <- value}:}{
      Get or set the ranges in the form of a 
      \code{\linkS4class{CompressedIRangesList}}. \code{value} can be a 
      \code{\linkS4class{IntegerRangesList}} object.
      
      \strong{WARNING}: The use of \code{ranges} with \code{GTuplesList} 
      objects is \strong{strongly} discouraged. It will only get/set 
      \eqn{pos_{1}} and \eqn{pos_{m}} of the tuples, where \eqn{m} is the 
      \code{size} of the tuples, as these are what are stored in the "ranges" 
      slot of the \code{GTuples} objects. 
    }
    \item{\code{strand(x)}, \code{strand(x) <- value}:}{
      Get or set the strand in the form of an \code{\linkS4class{RleList}}. 
      \code{value} can be an \code{\linkS4class{RleList}}, 
      \code{\linkS4class{CharacterList}} or single character. \code{value} as 
      a single character converts all ranges in \code{x} to the same
      \code{value}; for selective strand conversion (i.e., mixed \dQuote{+}
      and \dQuote{-}) use \code{\linkS4class{RleList}} or 
      \code{\linkS4class{CharacterList}}. 
    }
    \item{\code{mcols(x, use.names=FALSE)}, \code{mcols(x) <- value}:}{
      Get or set the metadata columns.
      \code{value} can be \code{NULL}, or a data.frame-like object (i.e.
      \code{\linkS4class{DataFrame}} or data.frame) holding 
      element-wise metadata.
    }
    \item{\code{elementMetadata(x)}, \code{elementMetadata(x) <- value},
       \code{values(x)}, \code{values(x) <- value}:}{
       Alternatives to \code{mcols} functions. Their use is discouraged.
    }
    \item{\code{seqinfo(x)}, \code{seqinfo(x) <- value}:}{
      Get or set the information about the underlying sequences.
      \code{value} must be a \link{Seqinfo} object.
    }
    \item{\code{seqlevels(x)}, \code{seqlevels(x, force=FALSE) <- value}:}{
      Get or set the sequence levels.
      \code{seqlevels(x)} is equivalent to \code{seqlevels(seqinfo(x))}
      or to \code{levels(seqnames(x))}, those 2 expressions being
      guaranteed to return identical character vectors on a \code{GTuplesList}
      object. \code{value} must be a character vector with no NAs.
      See \code{?\link[GenomeInfoDb:seqinfo]{seqlevels}} for more information.
    }
    \item{\code{seqlengths(x)}, \code{seqlengths(x) <- value}:}{
      Get or set the sequence lengths.
      \code{seqlengths(x)} is equivalent to \code{seqlengths(seqinfo(x))}.
      \code{value} can be a named non-negative integer or numeric vector
      eventually with NAs.
    }
    \item{\code{isCircular(x)}, \code{isCircular(x) <- value}:}{
      Get or set the circularity flags.
      \code{isCircular(x)} is equivalent to \code{isCircular(seqinfo(x))}.
      \code{value} must be a named logical vector eventually with NAs.
    }
    \item{\code{genome(x)}, \code{genome(x) <- value}:}{
      Get or set the genome identifier or assembly name for each sequence.
      \code{genome(x)} is equivalent to \code{genome(seqinfo(x))}.
      \code{value} must be a named character vector eventually with NAs.
    }
    \item{\code{seqlevelsStyle(x)}, \code{seqlevelsStyle(x) <- value}:}{
      Get or set the seqname style for \code{x}.
      See the \link[GenomeInfoDb]{seqlevelsStyle} generic getter and setter
      in the \pkg{GenomeInfoDb} package for more information.
    }
    \item{\code{score(x), score(x) <- value}: Get or set the \dQuote{score}
      metadata column.}{
    }
  }
}

\section{Tuples methods}{
  In the following code snippets, x is a \code{GTuplesList} object.
  
  \strong{WARNING}: The preferred setter is \code{tuples(x) <- value} and the 
  use of \code{start(x) <- value}, \code{end(x) <- value} and 
  \code{width(x) <- value is discouraged}.
  
  \describe{
    \item{\code{start(x)}, \code{start(x) <- value}:}{
      Get or set \eqn{pos_{1}} of the \code{tuples}. \strong{WARNING}: The use 
      of \code{start(x) <- value} is discouraged; instead, construct the 
      tuples as the appropriate \code{List} of integer matrices, \code{mvalue}, 
      and use \code{tuples(x) <- mvalue}.
    }
    
    \item{\code{end(x)}, \code{end(x) <- value}:}{
      Get or set \eqn{pos_{m}} of the tuples, where \eqn{m} is the \code{size} 
      of the tuples. \strong{WARNING}: The use of \code{end(x) <- value} is 
      discouraged; instead, construct the tuples as the appropriate \code{List} 
      of integer matrices, \code{mvalue}, and use \code{tuples(x) <- mvalue}.
    }
    
    \item{\code{IPD(x)}:}{
      Get the intra-pair distances (\code{IPD}) in the form of a 
      \code{\linkS4class{SimpleList}} of integer matrices. \code{IPD} is 
      only defined for tuples with \code{size} > 1. The \code{IPD} of a tuple 
      with \code{size}\eqn{= m} is the vector of intra-pair distances, 
      \eqn{(pos_{2} - pos_{1}, \ldots, pos_{m} - pos_{m - 1})}.
    }
    
    \item{\code{width(x)}, \code{width(x) <- value}:}{
      Get or set \eqn{pos_{m} - pos_{1}} of the tuples, where \eqn{m} is the 
      \code{size} of the tuples. If using \code{width(x) <- value}, 
      \eqn{pos_{1}} is held fixed and \eqn{pos_{m}} is altered. 
      \strong{WARNING}: The use of \code{width(x) <- value} is discouraged; 
      instead, instead, construct the tuples as the appropriate \code{List} 
      of integer matrices, \code{mvalue}, and use \code{tuples(x) <- mvalue}.
    }
  }
}

\section{Coercion}{
    In the code snippets below, \code{x} is a GTuplesList object.

  \describe{
    \item{\code{as.data.frame(x, row.names = NULL, optional = FALSE,
         ..., value.name = "value", use.outer.mcols = FALSE, 
         group_name.as.factor = FALSE)}:}{
      Coerces \code{x} to a \code{data.frame}. See as.data.frame on the 
      \code{List} man page for details (?\code{List}). 
    }
    \item{\code{as.list(x, use.names = TRUE)}:}{
      Creates a list containing the elements of \code{x}.
    }
  }
  \describe{
    \item{\code{as(x, "GRangesList")}:}{
      Creates a \code{\linkS4class{GRangesList}} object from a
      \code{GTuplesList} object. \strong{WARNING:} This is generally a 
      \emph{destructive} operation, as the original \code{GTuplesList} may not 
      be re-creatable.
    }
  }
}

\section{Subsetting}{
  In the following code snippets, \code{x} is a \code{GTuplesList} object.

  \describe{
    \item{\code{x[i, j]}, \code{x[i, j] <- value}:}{
      Get or set elements \code{i} with optional metadata columns
      \code{mcols(x)[,j]}, where \code{i} can be missing; an NA-free
      logical, numeric, or character vector; a 'logical' Rle object, or
      an AtomicList object.
    }
    \item{\code{x[[i]]}, \code{x[[i]] <- value}:}{
      Get or set element \code{i}, where \code{i} is a numeric or character
      vector of length 1.
    }
    \item{\code{x$name}, \code{x$name <- value}:}{
      Get or set element \code{name}, where \code{name} is a name or character
      vector of length 1.
    }
    \item{\code{head(x, n = 6L)}:}{
      If \code{n} is non-negative, returns the first n elements of the
      GTuplesList object.
      If \code{n} is negative, returns all but the last \code{abs(n)} elements
      of the GTuplesList object.
    }
    \item{\code{rep(x, times, length.out, each)}:}{
      Repeats the values in \code{x} through one of the following conventions:
      \describe{
        \item{\code{times}}{Vector giving the number of times to repeat each
          element if of length \code{length(x)}, or to repeat the whole vector
          if of length 1.}
        \item{\code{length.out}}{
          Non-negative integer. The desired length of the output vector.
        }
        \item{\code{each}}{
          Non-negative integer. Each element of \code{x} is repeated \code{each} 
          times.
        }
      }
    }
    \item{\code{subset(x, subset)}:}{
      Returns a new object of the same class as \code{x} made of the subset
      using logical vector \code{subset}, where missing values are taken as
      \code{FALSE}.
    }
    \item{\code{tail(x, n = 6L)}:}{
      If \code{n} is non-negative, returns the last n elements of the
      GTuples object.
      If \code{n} is negative, returns all but the first \code{abs(n)} elements
      of the GTuples object.
    }
  }
}

\section{Combining}{
  In the code snippets below, \code{x} is a GTuplesList object.

  \describe{
    \item{\code{c(x, ...)}:}{
      Combines \code{x} and the GTuplesList objects in \code{...}
      together. Any object in \code{...} must belong to the same class
      as \code{x}, or to one of its subclasses, or must be \code{NULL}.
      The result is an object of the same class as \code{x}.
    }
    \item{\code{append(x, values, after = length(x))}:}{
      Inserts the \code{values} into \code{x} at the position given by
      \code{after}, where \code{x} and \code{values} are of the same
      class.
    }
    \item{\code{unlist(x, recursive = TRUE, use.names = TRUE)}:}{
      Concatenates the elements of \code{x} into a single GTuples
      object.
    }
  }
}

\section{Looping}{
  In the code snippets below, \code{x} is a GTuplesList object.

  \describe{
    \item{\code{endoapply(X, FUN, ...)}:}{
      Similar to \code{\link[base]{lapply}}, but performs an endomorphism,
      i.e. returns an object of \code{class(X)}.
    }
    \item{\code{lapply(X, FUN, ...)}:}{
      Like the standard \code{\link[base]{lapply}} function defined in the
      base package, the \code{lapply} method for GTuplesList objects
      returns a list of the same length as \code{X}, with each element being
      the result of applying \code{FUN} to the corresponding element of
      \code{X}.
    }
    \item{\code{Map(f, ...)}:}{
      Applies a function to the corresponding elements of given
      GTuplesList objects.
    }
    \item{\code{mapply(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, 
                   USE.NAMES = TRUE)}:}{
      Like the standard \code{\link[base]{mapply}} function defined in the
      base package, the \code{mapply} method for GTuplesList objects is a
      multivariate version of \code{sapply}.
    }
    \item{\code{mendoapply(FUN, ..., MoreArgs = NULL)}:}{
      Similar to \code{\link[base]{mapply}}, but performs an endomorphism
      across multiple objects, i.e. returns an object of
      \code{class(list(...)[[1]])}.
    }
    \item{\code{Reduce(f, x, init, right = FALSE, accumulate = FALSE)}:}{
      Uses a binary function to successively combine the elements of \code{x}
      and a possibly given initial value.
      \describe{
        \item{f}{A binary argument function.}
        \item{init}{An R object of the same kind as the elements of \code{x}.}
        \item{right}{A logical indicating whether to proceed from left to right
          (default) or from right to left.}
        \item{nomatch}{The value to be returned in the case when "no match" (no
          element satisfying the predicate) is found.}
      }
    }
    \item{\code{sapply(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)}:}{
      Like the standard \code{\link[base:lapply]{sapply}} function defined in
      the base package, the \code{sapply} method for GTuplesList objects
      is a user-friendly version of \code{lapply} by default returning a vector
      or matrix if appropriate.
    }
  }
}

\author{Peter Hickey for \code{GTuplesList} definition and methods. P. Aboyoun 
& H. Pagès for all the real work underlying the powerful \code{GRangesList} 
class and methods.}

\seealso{
  \link{GTuples-class}
  \code{\link[GenomeInfoDb]{seqinfo}},
  \code{\linkS4class{GRangesList}},
  \code{\linkS4class{Vector}},
  \code{\linkS4class{IntegerRangesList}},
  \code{\linkS4class{RleList}},
  \code{\linkS4class{DataFrameList}},
  \link{findOverlaps-methods}
}

\examples{
  ## Construction of GTuplesList of 4-tuples with GTuplesList():
  seqinfo <- Seqinfo(paste0("chr", 1:3), c(1000, 2000, 1500), NA, "mock1")
  gt4 <- GTuples(seqnames = Rle(c("chr1", "chr2", "chr1", "chr3"),
                                c(1, 3, 2, 4)),
                 tuples = matrix(c(1:10, 2:11, 3:12, 4:13), ncol = 4),
                 strand = Rle(strand(c("-", "+", "*", "+", "-")),
                              c(1, 2, 2, 3, 2)),
                 score = 1:10, GC = seq(1, 0, length = 10), seqinfo = seqinfo)
  gtl4 <- GTuplesList(A = gt4[1:4], B = gt4[5:10])
  gtl4
  
  ## Summarizing elements:
  elementNROWS(gtl4)
  table(seqnames(gtl4))
  
  ## Extracting subsets:
  gtl4[seqnames(gtl4) == "chr1", ]
  gtl4[seqnames(gtl4) == "chr1" & strand(gtl4) == "+", ]
  
  ## Renaming the underlying sequences:
  seqlevels(gtl4)
  seqlevels(gtl4) <- sub("chr", "Chrom", seqlevels(gtl4))
  gtl4
  
  ## Coerce to GRangesList ("internal positions" information is lost):
  as(gtl4, "GRangesList")

  ## Get the size of the tuples stored in the GTuplesList object
  size(gtl4)
  
  ## Get the tuples
  tuples(gtl4)
  
  ## Get the matrix of intra-pair distances (IPD)
  IPD(gtl4)  
  
  ## Can't combine genomic tuples of different sizes
  gt1 <- GTuples('chr1', matrix(30:40))
  gt1
  \dontrun{
  ## Returns error
  GTuplesList(A = gt4, gt1)
  }
}
