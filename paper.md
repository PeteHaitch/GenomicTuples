---
title: 'Representation and Manipulation of Genomic Tuples in R'
tags:
  - bioinformatics
  - computational biology
authors:
 - name: Peter F Hickey
   orcid: 0000-0002-8153-6258
   affiliation: Department of Biostatistics, Johns Hopkins Bloomberg School of Public Health
date: 17 May 2016
bibliography: paper.bib
---

# Summary

GenomicTuples is an R/Bioconductor package [@R;@Bioconductor] that defines general purpose containers for storing and manipulating _genomic tuples_. A genomic tuple of size `m` is of the form `chromosome:strand:{pos_1, pos_2, ..., pos_m}` where `pos_1` < `pos_2` < `...` < `pos_m`. The difference between a genomic tuple and a genomic range/interval is like that of a difference between an ordered set and an interval. For example, the genomic 2-tuple `chr3:+:{65, 77}` differs from the genomic range `chr3:+:[65, 77]` by not including any of the intervening loci, `chr3:+:66` to `chr3:+:76`.

GenomicTuples aims to provide functionality for manipulating tuples of genomic co-ordinates that are analogous to those available for genomic ranges in the popular GenomicRanges R/Bioconductor package [@GenomicRanges]. To that end, the GenomicTuples API mimics that of GenomicRanges. By extending classes defined in the GenomicRange package, objects from the GenomicTuples package may be used as drop-in replacements for objects from the GenomicRanges package. This ensures easy interoperability with other popular Bioconductor packages, such as SummarizedExperiment [@SummarizedExperiment], and the availability of common operations, such as finding overlaps between genomic tuples and genomic features of interest.

# References
