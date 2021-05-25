
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GenomicTuples

<!-- badges: start -->

[![BioC
status](http://www.bioconductor.org/shields/build/release/bioc/GenomicTuples.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/GenomicTuples)
[![R-CMD-check-bioc](https://github.com/PeteHaitch/GenomicTuples/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/PeteHaitch/GenomicTuples/actions)
<!-- badges: end -->

**GenomicTuples** is an R/Bioconductor package that defines general
purpose containers for storing and manipulating *genomic tuples*. A
genomic tuple of size `m` is of the form
`chromosome:strand:{pos_1, pos_2, ..., pos_m}` where `pos_1` &lt;
`pos_2` &lt; `...` &lt; `pos_m` are positions along the chromosome.
**GenomicTuples** aims to provide functionality for tuples of genomic
co-ordinates that are analogous to those available for genomic ranges in
the
***[GenomicRanges](https://bioconductor.org/packages/3.14/GenomicRanges)***
R/Bioconductor package.

## Installation instructions

Get the latest stable `R` release from
[CRAN](http://cran.r-project.org/). Then install **GenomicTuples** from
[Bioconductor](http://bioconductor.org/) using the following code:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("GenomicTuples")
```

And the development version from
[GitHub](https://github.com/PeteHaitch/GenomicTuples) with:

``` r
BiocManager::install("PeteHaitch/GenomicTuples")
```

## Example

Here we use the **GenomicTuples** package to define two *GTuples*
objects, one containing 5 3-tuples and one containing 3 3-tuples,
demonstrate how to identify ‘equal’ genomic tuples, and how this
calculation would be incorrect if we were to mistakenly treat these
genomic tuples as genomic ranges:

``` r
# Load the package
library(GenomicTuples)
# Create a GTuples object containing 5 3-tuples
x <- GTuples(
    seqnames = c('chr1', 'chr1', 'chr1', 'chr1', 'chr2'), 
    tuples = matrix(
        c(10, 10, 10, 10, 10, 20, 20, 20, 25, 20, 30, 30, 35, 30, 30), 
        ncol = 3), 
    strand = c('+', '-', '*', '+', '+'))
#> Warning in GTuples(seqnames = c("chr1", "chr1", "chr1", "chr1", "chr2"), :
#> Converting 'tuples' to integer mode
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
range, when genomic tuples may be usefu, and common operations on
genomic tuples. The vignette can be viewed at
<http://bioconductor.org/packages/release/bioc/vignettes/GenomicTuples/inst/doc/GenomicTuplesIntroduction.html>
or accessed from R using:

``` r
vignette("GenomicTuplesIntroduction", package = "GenomicTuples")
```

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
#>   Peter F Hickey, (2016) Representation and Manipulation of Genomic
#>   Tuples in R. JOSS: doi:10.21105/joss.00020
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
#> [1] '1.27.0'
```

## License

**GenomicTuples** is licensed under [Artistic
2.0](https://www.r-project.org/Licenses/Artistic-2.0).

## Code of Conduct

Please note that the **GenomicTuples** project is released with a
[Contributor Code of
Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.
