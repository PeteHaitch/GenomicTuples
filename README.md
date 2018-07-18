
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Bioconductor devel build
status](http://bioconductor.org/shields/build/devel/bioc/GenomicTuples.svg)](http://bioconductor.org/checkResults/devel/bioc-LATEST/GenomicTuples/)
[![Linux Build
Status](https://travis-ci.org/PeteHaitch/GenomicTuples.svg?branch=master)](https://travis-ci.org/PeteHaitch/GenomicTuples)
[![Bioconductor
downloads](http://bioconductor.org/shields/downloads/GenomicTuples.svg)](http://bioconductor.org/packages/stats/bioc/GenomicTuples.html)
[![Coverage
Status](https://img.shields.io/codecov/c/github/PeteHaitch/GenomicTuples/master.svg)](https://codecov.io/github/PeteHaitch/GenomicTuples?branch=master)
[![JOSS](http://joss.theoj.org/papers/10.21105/joss.00020/status.svg)](http://dx.doi.org/10.21105/joss.00020)
[![DOI](https://zenodo.org/badge/22085/PeteHaitch/GenomicTuples.svg)](https://zenodo.org/badge/latestdoi/22085/PeteHaitch/GenomicTuples)

## R package: GenomicTuples

**GenomicTuples** is an R/Bioconductor package that defines general
purpose containers for storing and manipulating *genomic tuples*. A
genomic tuple of size `m` is of the form `chromosome:strand:{pos_1,
pos_2, ..., pos_m}` where `pos_1` \< `pos_2` \< `...` \< `pos_m` are
positions along the chromosome. **GenomicTuples** aims to provide
functionality for tuples of genomic co-ordinates that are analogous to
those available for genomic ranges in the
[**GenomicRanges**](http://bioconductor.org/packages/GenomicRanges/)
R/Bioconductor package.

## Installation

Most users will want to install **GenomicTuples** using the current
release of Bioconductor using:

``` r
install.packages("BiocManager")
BiocManager::install("GenomicTuples")
```

The master branch of this repository is the development version of the
package. The development version of **GenomicTuples** can only be
installed using the development version of Bioconductor. Please first
read these [instructions on installing the development version of
Bioconductor](http://www.bioconductor.org/developers/how-to/useDevel/);
**GenomicTuples** can then be installed by:

``` r
install.packages("BiocManager")
BiocManager::install("GenomicTuples", version = "devel")
```

## Quick demo

Here we use the **GenomicTuples** package to define two *GTuples*
objects, one containing 5 3-tuples and one containing 3 3-tuples,
demonstrate how to identify ‘equal’ genomic tuples, and how this
calculation would be incorrect if we were to mistakenly treat these
genomic tuples as genomic ranges:

``` r
# Load the package
library(GenomicTuples)

# Create a GTuples object containing 5 3-tuples
x <- GTuples(seqnames = c('chr1', 'chr1', 'chr1', 'chr1', 'chr2'), 
               tuples = matrix(c(10L, 10L, 10L, 10L, 10L, 20L, 20L, 20L, 25L, 
                                 20L, 30L, 30L, 35L, 30L, 30L), ncol = 3), 
               strand = c('+', '-', '*', '+', '+'))
x
#> GTuples object with 5 x 3-tuples and 0 metadata columns:
#>       seqnames pos1 pos2 pos3 strand
#>   [1]     chr1   10   20   30      +
#>   [2]     chr1   10   20   30      -
#>   [3]     chr1   10   20   35      *
#>   [4]     chr1   10   25   30      +
#>   [5]     chr2   10   20   30      +
#>   ---
#>   seqinfo: 2 sequences from an unspecified genome; no seqlengths
# Create a new GTuples object containing 3 3-tuples by subsetting x
y <- x[2:4]
y
#> GTuples object with 3 x 3-tuples and 0 metadata columns:
#>       seqnames pos1 pos2 pos3 strand
#>   [1]     chr1   10   20   30      -
#>   [2]     chr1   10   20   35      *
#>   [3]     chr1   10   25   30      +
#>   ---
#>   seqinfo: 2 sequences from an unspecified genome; no seqlengths

# Find equal genomic tuples
subsetByOverlaps(x, y, type = "equal")
#> GTuples object with 3 x 3-tuples and 0 metadata columns:
#>       seqnames pos1 pos2 pos3 strand
#>   [1]     chr1   10   20   30      -
#>   [2]     chr1   10   20   35      *
#>   [3]     chr1   10   25   30      +
#>   ---
#>   seqinfo: 2 sequences from an unspecified genome; no seqlengths
# Find equal genomic tuples when ignoring strand
subsetByOverlaps(x, y, type = "equal", ignore.strand = TRUE)
#> GTuples object with 4 x 3-tuples and 0 metadata columns:
#>       seqnames pos1 pos2 pos3 strand
#>   [1]     chr1   10   20   30      +
#>   [2]     chr1   10   20   30      -
#>   [3]     chr1   10   20   35      *
#>   [4]     chr1   10   25   30      +
#>   ---
#>   seqinfo: 2 sequences from an unspecified genome; no seqlengths

# Note that if were to mistakenly treat these as genomic ranges, then the set 
# of 'equal' genomic tuples would be incorrect since treating these tuples as 
# ranges ignores the "internal positions" (pos2).
# The set of overlaps when correctly treated as genomic tuples:
findOverlaps(x, y, type = "equal")
#> Hits object with 3 hits and 0 metadata columns:
#>       queryHits subjectHits
#>       <integer>   <integer>
#>   [1]         2           1
#>   [2]         3           2
#>   [3]         4           3
#>   -------
#>   queryLength: 5 / subjectLength: 3
# The set of overlaps when incorrected treated as genomic ranges:
findOverlaps(as(x, "GRanges"), as(y, "GRanges"), type = 'equal')
#> Hits object with 4 hits and 0 metadata columns:
#>       queryHits subjectHits
#>       <integer>   <integer>
#>   [1]         1           3
#>   [2]         2           1
#>   [3]         3           2
#>   [4]         4           3
#>   -------
#>   queryLength: 5 / subjectLength: 3
```

**GenomicTuples** includes extensive documentation available through the
R help system:

``` r
# See all documentation for the package
help(package = "GenomicTuples")
# See documentation for the GTuples class
?GTuples
```

The package also includes a comprehensive vignette that explains in
greater detail the different between a genomic tuple and a genomic
range, when genomic tuples may be useful, and common operations on
genomic tuples. The vignette can be viewed at
<http://bioconductor.org/packages/release/bioc/vignettes/GenomicTuples/inst/doc/GenomicTuplesIntroduction.html>
or accessed from R using:

``` r
vignette("GenomicTuplesIntroduction", package = "GenomicTuples")
```

## Need help or want to get involved?

I recommend that questions seeking support in using the software are
posted to the Bioconductor support forum -
<https://support.bioconductor.org/> - where they will attract not only
my attention but that of the wider Bioconductor community.

Code contributions, bug reports (and fixes\!), and feature requests are
most welcome. Please make any pull requests against the master branch at
<https://github.com/PeteHaitch/GenomicTuples> and file issues at
<https://github.com/PeteHaitch/GenomicTuples/issues>

## Citation

**GenomicTuples** has been published in The Journal of Open Source
Software, <http://joss.theoj.org/papers/10.21105/joss.00020>.

If you use **GenomicTuples**, please cite the paper and software
version. This can be done as follows:

``` r
citation("GenomicTuples")
#> 
#> To cite GenomicTuples in publications, please use:
#> 
#>   Peter F Hickey, (2016) Representation and Manipulation of
#>   Genomic Tuples in R. JOSS: doi:10.21105/joss.00020
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {Representation and Manipulation of Genomic Tuples in R},
#>     author = {Peter F Hickey},
#>     year = {2016},
#>     journal = {The Journal of Open Source Software},
#>     doi = {http://dx.doi.org/10.21105/joss.00020},
#>     url = {http://joss.theoj.org/papers/64b99f363d24b8a7e9025188183e9865},
#>   }
packageVersion("GenomicTuples")
#> [1] '1.15.1'
```

## License

**GenomicTuples** is licensed under
[Artistic 2.0](https://www.r-project.org/Licenses/Artistic-2.0).
