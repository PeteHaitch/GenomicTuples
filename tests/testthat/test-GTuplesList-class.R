# NB: Several objects used in testing are defined in 
# tests/testthat/helper-make-test-data.R

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###
context("GTuplesList validity")

test_that(".valid.GTuplesList.mcols works", {
  expect_error(GTuplesList(GTuples(seqnames = 'chr1', tuples = matrix(1:10), 
                                   strand = '+', seqnames = letters[1:10])), 
               "formal argument \"seqnames\" matched by multiple")
  expect_error(GTuplesList(GTuples(seqnames = 'chr1', tuples = matrix(1:10), 
                                   strand = '+', ranges = letters[1:10])), 
               "formal argument \"ranges\" matched by multiple")
  expect_error(GTuplesList(GTuples(seqnames = 'chr1', tuples = matrix(1:10), 
                                   strand = '+', strand = letters[1:10])), 
               "formal argument \"strand\" matched by multiple")
  expect_error(GTuplesList(GTuples(seqnames = 'chr1', tuples = matrix(1:10), 
                                   strand = '+', strand = letters[1:10])), 
               "formal argument \"strand\" matched by multiple")
  expect_error(GTuplesList(GTuples(seqnames = 'chr1', tuples = matrix(1:10), 
                                   strand = '+', seqlevels = letters[1:10])), 
               "names of metadata columns cannot be one of")
  expect_error(GTuplesList(GTuples(seqnames = 'chr1', tuples = matrix(1:10), 
                                   strand = '+', seqlengths = letters[1:10])), 
               "length of supplied 'seqlengths' must equal the number of")
  expect_error(GTuplesList(GTuples(seqnames = 'chr1', tuples = matrix(1:10), 
                                   strand = '+', isCircular = letters[1:10])), 
               "names of metadata columns cannot be one of")
  expect_error(GTuplesList(GTuples(seqnames = 'chr1', tuples = matrix(1:10), 
                                   strand = '+', start = letters[1:10])), 
               "names of metadata columns cannot be one of")
  expect_error(GTuplesList(GTuples(seqnames = 'chr1', tuples = matrix(1:10), 
                                   strand = '+', end = letters[1:10])), 
               "names of metadata columns cannot be one of")
  expect_error(GTuplesList(GTuples(seqnames = 'chr1', tuples = matrix(1:10), 
                                   strand = '+', width = letters[1:10])), 
               "names of metadata columns cannot be one of")
  expect_error(GTuplesList(GTuples(seqnames = 'chr1', tuples = matrix(1:10), 
                                   strand = '+', element = letters[1:10])), 
               "names of metadata columns cannot be one of")
  expect_error(GTuplesList(GTuples(seqnames = 'chr1', tuples = matrix(1:10), 
                                   strand = '+', tuples = letters[1:10])), 
               "formal argument \"tuples\" matched by multiple")
  expect_error(GTuplesList(GTuples(seqnames = 'chr1', tuples = matrix(1:10), 
                                   strand = '+', 
                                   internalPos = letters[1:10])), 
               "names of metadata columns cannot be one of")
  expect_error(GTuplesList(GTuples(seqnames = 'chr1', tuples = matrix(1:10), 
                                   strand = '+', size = letters[1:10])), 
               "names of metadata columns cannot be one of")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###
context("GTuplesList constructor")

test_that("GTuplesList constructor returns a valid object with no arguments", {
  expect_true(validObject(new("GTuplesList")))
})

test_that("GTuplesList constructor returns a valid object when m = 0", {
  expect_true(validObject(gtl0))
})

test_that("GTuplesList constructor returns a valid object when m = 1", {
  expect_true(validObject(gtl1))
})

test_that("GTuplesList constructor returns a valid object when m = 2", {
  expect_true(validObject(gtl2))
})

test_that("GTuplesList constructor returns a valid object when m >= 3", {
  expect_true(validObject(gt3))
  expect_true(validObject(gt4))
})

test_that("GTuplesList constructor returns errors on bad input", {
  expect_error(GTuplesList(gt1, gt2), 
               "all GTuples in '...' must have the same 'size'")
  gr <- granges(gt2)
  expect_error(GTuplesList(gt2, gr), 
               "all elements in '...' must be GTuples objects")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Accessors
###
context("GTuplesList accessors")

test_that("GRangesList inherited accessors work", {
  expect_identical(length(gtl0), 2L)
  expect_identical(length(gtl1), 2L)
  expect_identical(length(gtl2), 2L)
  expect_identical(length(gtl3), 2L)
  expect_identical(length(gtl4), 2L)
  expect_identical(names(gtl3), c("A", "B"))
  expect_identical(elementLengths(gtl3), c("A" = 5L, "B" = 5L))
  expect_true(isEmpty(gtl0))
  expect_false(isEmpty(gtl1))
  expect_false(isEmpty(gtl2))
  expect_false(isEmpty(gtl3))
  expect_false(isEmpty(gtl4))
  expect_identical(seqnames(gtl0), RleList("A" = seqnames(gtl0[[1]]), 
                                           "B" = seqnames(gtl0[[2]])))
  expect_identical(seqnames(gtl1), RleList("A" = seqnames(gtl1[[1]]), 
                                           "B" = seqnames(gtl1[[2]])))
  expect_identical(ranges(gtl0), IRangesList("A" = ranges(gtl0[[1]]), 
                                             "B" = ranges(gtl0[[2]])))
  expect_identical(ranges(gtl1), IRangesList("A" = ranges(gtl1[[1]]), 
                                             "B" = ranges(gtl1[[2]])))
  expect_identical(start(gtl0), IntegerList("A" = integer(), 
                                            "B" = integer()))
  expect_identical(start(gtl1), IntegerList("A" = start(gtl1[[1]]), 
                                            "B" = start(gtl1[[2]])))
  expect_identical(end(gtl0), IntegerList("A" = integer(), 
                                          "B" = integer()))
  expect_identical(end(gtl1), IntegerList("A" = end(gtl1[[1]]), 
                                          "B" = end(gtl1[[2]])))
  expect_identical(width(gtl0), IntegerList("A" = integer(), 
                                            "B" = integer()))
  expect_identical(width(gtl1), IntegerList("A" = width(gtl1[[1]]), 
                                            "B" = width(gtl1[[2]])))
  expect_identical(strand(gtl0), 
                   RleList("A" = Rle(factor(levels = c("+", "-", "*"))), 
                           "B" = Rle(factor(levels = c("+", "-", "*")))))
  expect_identical(strand(gtl1), 
                   RleList("A" = Rle(S4Vectors:::decodeRle(strand(gtl1[[1]]))), 
                           "B" = Rle(S4Vectors:::decodeRle(strand(gtl1[[2]])))))
  DF0 <- DataFrame()
  DF0@nrows <- 2L
  expect_identical(mcols(gtl0), DF0)
  expect_identical(mcols(gtl1), DF0)
  expect_identical(elementMetadata(gtl0), DF0)
  expect_identical(elementMetadata(gtl1), DF0)
  expect_identical(seqinfo(gtl0), seqinfo(gtl0[[1]]))
  expect_identical(seqinfo(gtl1), seqinfo(gtl1[[1]]))
  expect_identical(seqlevels(gtl0), seqlevels(gtl0[[1]]))
  expect_identical(seqlevels(gtl1), seqlevels(gtl1))
  expect_identical(seqlengths(gtl0), seqlengths(gtl0[[1]]))
  expect_identical(seqlengths(gtl1), seqlengths(gtl1[[1]]))
  expect_identical(isCircular(gtl0), isCircular(gtl0[[1]]))
  expect_identical(isCircular(gtl1), isCircular(gtl1[[1]]))
  expect_identical(genome(gtl0), genome(gtl0[[1]]))
  expect_identical(genome(gtl1), genome(gtl1[[1]]))
  expect_identical(seqlevelsStyle(gtl0), seqlevelsStyle(gtl0[[1]]))
  expect_identical(seqlevelsStyle(gtl1), seqlevelsStyle(gtl1[[1]]))
  # 'score' can be set at GTuples- or GTuplesList-level.
  # In both gtl0 and gtl1 it is set at the GTuples-level
  expect_identical(score(gtl0), NULL)
  expect_identical(score(gtl1), NULL)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Tuples methods
###
context("GTuplesList tuples methods")

test_that("size works", {
  expect_identical(size(gtl0), NA_integer_)
  expect_identical(size(gtl1), 1L)
  expect_identical(size(gtl2), 2L)
  expect_identical(size(gtl3), 3L)
  expect_identical(size(gtl4), 4L)
})

test_that("IPD works", {
  expect_error(IPD(gtl0), "Cannot compute IPD from an empty 'GTuples'.")
  expect_error(IPD(gtl1), 
               "It does not make sense to compute IPD when 'size' = 1.")
  expect_identical(IPD(gtl2), List(A = matrix(1L, nrow = 5, ncol = 1),
                                   B = matrix(1L, nrow = 5, ncol = 1)))
  expect_identical(IPD(gtl3), List(A = matrix(1L, nrow = 5, ncol = 2), 
                                   B = matrix(1L, nrow = 5, ncol = 2)))
  expect_identical(IPD(gtl4), List(A = matrix(1L, nrow = 5, ncol = 3), 
                                   B = matrix(1L, nrow = 5, ncol = 3)))
})

test_that("tuples works", {
  expect_identical(tuples(gtl0), List(A = matrix(), B = matrix()))
  expect_identical(tuples(gtl1), 
                   List(A = matrix(1:5, ncol = 1, 
                                   dimnames = list(NULL, 'pos1')), 
                        B = matrix(6:10, ncol = 1, 
                                   dimnames = list(NULL, 'pos1'))))
  expect_identical(tuples(gtl2), 
                   List(A = matrix(c(1:5, 2:6), ncol = 2, 
                                   dimnames = list(NULL, c('pos1', 'pos2'))), 
                        B = matrix(c(6:10, 7:11), ncol = 2, 
                                   dimnames = list(NULL, c('pos1', 'pos2')))))
  expect_identical(tuples(gtl3), 
                   List(A = matrix(c(1:5, 2:6, 3:7), ncol = 3, 
                                   dimnames = list(NULL, 
                                                   c('pos1', 'pos2', 'pos3'))), 
                        B = matrix(c(6:10, 7:11, 8:12), ncol = 3, 
                                   dimnames = list(NULL, 
                                                   c('pos1', 'pos2', 'pos3')))))
  expect_identical(
    tuples(gtl4), List(A = matrix(c(1:5, 2:6, 3:7, 4:8), ncol = 4, 
                                  dimnames = list(NULL, c('pos1', 'pos2', 
                                                          'pos3', 'pos4'))), 
                       B = matrix(c(6:10, 7:11, 8:12, 9:13), ncol = 4, 
                                  dimnames = list(NULL, c('pos1', 'pos2', 
                                                          'pos3', 'pos4')))))
})

test_that("tuples<- works", {
  tuples(gtl1) <- matrix(101:110, ncol = 1)
  expect_identical(tuples(gtl1), 
                   List(A = matrix(101:105, ncol = 1, 
                                   dimnames = list(NULL, 'pos1')), 
                        B = matrix(106:110, ncol = 1, 
                                   dimnames = list(NULL, 'pos1'))))
  tuples(gtl2) <- matrix(c(101:110, 102:111), ncol = 2)
  expect_identical(tuples(gtl2), 
                   List(A = matrix(c(101:105, 102:106), ncol = 2, 
                                   dimnames = list(NULL, c('pos1', 'pos2'))), 
                        B = matrix(c(106:110, 107:111), ncol = 2, 
                                   dimnames = list(NULL, c('pos1', 'pos2')))))
  tuples(gtl3) <- matrix(c(101:110, 102:111, 103:112), ncol = 3)
  expect_identical(tuples(gtl3), 
                   List(A = matrix(c(101:105, 102:106, 103:107), ncol = 3, 
                                   dimnames = list(NULL, c('pos1', 'pos2', 
                                                           'pos3'))), 
                        B = matrix(c(106:110, 107:111, 108:112), ncol = 3, 
                                   dimnames = list(NULL, c('pos1', 'pos2', 
                                                           'pos3')))))
  tuples(gtl4) <- matrix(c(101:110, 102:111, 103:112, 104:113), ncol = 4)
  expect_identical(tuples(gtl4), 
                   List(A = matrix(c(101:105, 102:106, 103:107, 104:108), 
                                   ncol = 4, dimnames = 
                                     list(NULL, c('pos1', 'pos2', 'pos3', 
                                                  'pos4'))), 
                        B = matrix(c(106:110, 107:111, 108:112, 109:113), 
                                   ncol = 4, dimnames = 
                                     list(NULL, c('pos1', 'pos2', 'pos3', 
                                                  'pos4')))))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###
context("GTuplesList coercion")

test_that("as.data.frame works", {
  expect_identical(as.data.frame(gtl0), 
                   data.frame(group = integer(0),
                              group_name = character(0),
                              seqnames = factor(levels = paste0("chr", 1:3)),
                              strand = factor(levels = c("+", "-", "*")),
                              stringsAsFactors = FALSE))
  expect_identical(as.data.frame(gtl1), 
                   cbind(data.frame(group = c(rep(1L, 5), rep(2L, 5)),
                                    group_name = c(rep("A", 5), rep("B", 5)), 
                                    stringsAsFactors = FALSE),
                         as.data.frame(gt1)))
  expect_identical(as.data.frame(gtl2), 
                   cbind(data.frame(group = c(rep(1L, 5), rep(2L, 5)),
                                    group_name = c(rep("A", 5), rep("B", 5)),
                                    stringsAsFactors = FALSE),
                         as.data.frame(gt2)))
  expect_identical(as.data.frame(gtl3), 
                   cbind(data.frame(group = c(rep(1L, 5), rep(2L, 5)),
                                    group_name = c(rep("A", 5), rep("B", 5)),
                                    stringsAsFactors = FALSE),
                         as.data.frame(gt3)))
  expect_identical(as.data.frame(gtl4), 
                   cbind(data.frame(group = c(rep(1L, 5), rep(2L, 5)),
                                    group_name = c(rep("A", 5), rep("B", 5)),
                                    stringsAsFactors = FALSE),
                         as.data.frame(gt4)))
})

test_that("as.list works", {
  expect_identical(as.list(gtl0), list(A = gt0, B = gt0))
  expect_identical(as.list(gtl1), list(A = gt1[1:5], B = gt1[6:10]))
  expect_identical(as.list(gtl2), list(A = gt2[1:5], B = gt2[6:10]))
  expect_identical(as.list(gtl3), list(A = gt3[1:5], B = gt3[6:10]))
  expect_identical(as.list(gtl4), list(A = gt4[1:5], B = gt4[6:10]))
  
})

test_that("Coercion to GRangesList works", {
  expect_identical(as(gtl0, "GRangesList"), 
                   GRangesList(A = GRanges(seqinfo = seqinfo), 
                               B = GRanges(seqinfo = seqinfo)))
  expect_identical(as(gtl1, "GRangesList"), 
                   GRangesList(A = gr1[1:5], B = gr1[6:10]))
  expect_identical(as(gtl2, "GRangesList"), 
                   GRangesList(A = gr2[1:5], B = gr2[6:10]))
  expect_identical(as(gtl3, "GRangesList"), 
                   GRangesList(A = gr3[1:5], B = gr3[6:10]))
  expect_identical(as(gtl4, "GRangesList"), 
                   GRangesList(A = gr4[1:5], B = gr4[6:10]))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Subsetting
###
context("GTuplesList subsetting")

test_that("[ works", {
  expect_identical(gtl0[1], GTuplesList(A = gt0))
  expect_identical(gtl1[1], GTuplesList(A = gt1[1:5]))
  expect_identical(gtl2[1], GTuplesList(A = gt2[1:5]))
  expect_identical(gtl3[1], GTuplesList(A = gt3[1:5]))
  expect_identical(gtl4[1], GTuplesList(A = gt4[1:5]))
})

test_that("[<- works", {
  gtl0[1] <- gtl0[2]
  expect_identical(gtl0, GTuplesList(A = gt0, B = gt0))
  gtl1[1] <- gtl1[2]
  expect_identical(gtl1, GTuplesList(A = gt1[6:10], B = gt1[6:10]))
  gtl2[1] <- gtl2[2]
  expect_identical(gtl2, GTuplesList(A = gt2[6:10], B = gt2[6:10]))
  gtl3[1] <- gtl3[2]
  expect_identical(gtl3, GTuplesList(A = gt3[6:10], B = gt3[6:10]))
  gtl4[1] <- gtl4[2]
  expect_identical(gtl4, GTuplesList(A = gt4[6:10], B = gt4[6:10]))
})

test_that("[[<- works", {
  gtl0[[1]] <- gt0
  expect_identical(gtl0, GTuplesList(A = gt0, B = gt0))
  gtl1[[1]] <- gt1
  expect_identical(gtl1, GTuplesList(A = gt1, B = gt1[6:10]))
  gtl2[[1]] <- gt2
  expect_identical(gtl2, GTuplesList(A = gt2, B = gt2[6:10]))
  gtl3[[1]] <- gt3
  expect_identical(gtl3, GTuplesList(A = gt3, B = gt3[6:10]))
  gtl4[[1]] <- gt4
  expect_identical(gtl4, GTuplesList(A = gt4, B = gt4[6:10]))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Deconstruction/reconstruction of a GTuplesList into/from a GTuples
### object.
###
### For internal use only (not exported).
###
context("Desconstruction/reconstruction of GTuplesList")
test_that("Returns identical", {
  expect_identical(GenomicRanges:::reconstructGRLfromGR(
    GenomicRanges:::deconstructGRLintoGR(gtl0), gtl0), gtl0)
  expect_identical(GenomicRanges:::reconstructGRLfromGR(
    GenomicRanges:::deconstructGRLintoGR(gtl1), gtl1), gtl1)
  expect_identical(GenomicRanges:::reconstructGRLfromGR(
    GenomicRanges:::deconstructGRLintoGR(gtl2), gtl2), gtl2)
  expect_identical(GenomicRanges:::reconstructGRLfromGR(
    GenomicRanges:::deconstructGRLintoGR(gtl3), gtl3), gtl3)
  expect_identical(GenomicRanges:::reconstructGRLfromGR(
    GenomicRanges:::deconstructGRLintoGR(gtl4), gtl4), gtl4)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### show method.
###

# TODO: Not sure how to test this. However, these tests are necessary due 
# to the showGTuples function being rather fragile.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Going from GTuples to GTuplesList with extractList() and family.
###
context("GTuplesList relistToClass")

test_that("relistToClass works", {
  expect_identical(relistToClass(gt0), "GTuplesList")
  expect_identical(relistToClass(gt1), "GTuplesList")
  expect_identical(relistToClass(gtl0), "SimpleList")
  expect_identical(relistToClass(gtl1), "SimpleList")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### apply methods
###
context("GTuplesList apply methods")

test_that("GTuplesList endoapply works", {
  expect_identical(gtl0, endoapply(gtl0, function(x) {x}))
  expect_identical(gtl1, endoapply(gtl1, function(x) {x}))
  expect_identical(gtl2, endoapply(gtl2, function(x) {x}))
  expect_identical(gtl4, endoapply(gtl4, function(x) {x}))
})
