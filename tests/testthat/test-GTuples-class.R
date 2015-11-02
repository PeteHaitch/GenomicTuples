# NB: Several objects used in testing are defined in 
# tests/testthat/helper-make-test-data.R

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity
###
context("GTuples validity methods")

test_that(".valid.GTuples.pos works for 1-tuples", {
  expect_that(GTuples('chr1', tuples = matrix(12:1, ncol = 1)), 
              not(throws_error()))
})
test_that(".valid.GTuples.pos works for 2-tuples", {
  expect_error(GTuples('chr1', tuples = matrix(12:1, ncol = 2)), 
               "negative widths are not allowed")
  expect_error(GTuples('chr1', tuples = cbind(11:20, 11:20)), 
               "positions in each tuple must be sorted")
  expect_error(GTuples('chr1', tuples = cbind(11:20, c(12:20, 20L))), 
               "positions in each tuple must be sorted")
})
test_that(".valid.GTuples.pos works for m-tuples (m > 2)", {
  expect_error(GTuples('chr1', tuples = matrix(12:1, ncol = 3)), 
               "negative widths are not allowed")
  expect_error(GTuples('chr1', tuples = cbind(11:20, 1:10, 31:40)), 
               "positions in each tuple must be sorted")
  expect_error(GTuples('chr1', tuples = cbind(11:20, c(12:20, 20L), 31:40)), 
               "positions in each tuple must be sorted")
  expect_error(GTuples('chr1', tuples = matrix(12:1, ncol = 4)), 
               "negative widths are not allowed")
  expect_error(GTuples('chr1', tuples = cbind(11:20, 1:10, 31:40, 41:50)), 
               "positions in each tuple must be sorted")
  expect_error(GTuples('chr1', tuples = cbind(11:20, 31:40, 1:10, 41:50)), 
               "positions in each tuple must be sorted")
  expect_error(GTuples('chr1', tuples = cbind(11:20, c(12:20, 20L), 31:40, 
                                              41:50)), 
               "positions in each tuple must be sorted")
})

test_that(".valid.GTuples.mcols works", {
  expect_error(GTuples(seqnames = 'chr1', tuples = matrix(1:10), strand = '+',
                       seqnames = letters[1:10]), 
               "formal argument \"seqnames\" matched by multiple")
  expect_error(GTuples(seqnames = 'chr1', tuples = matrix(1:10), strand = '+',
                       ranges = letters[1:10]), 
               "formal argument \"ranges\" matched by multiple")
  expect_error(GTuples(seqnames = 'chr1', tuples = matrix(1:10), strand = '+',
                       strand = letters[1:10]), 
               "formal argument \"strand\" matched by multiple")
  expect_error(GTuples(seqnames = 'chr1', tuples = matrix(1:10), strand = '+',
                       strand = letters[1:10]), 
               "formal argument \"strand\" matched by multiple")
  expect_error(GTuples(seqnames = 'chr1', tuples = matrix(1:10), strand = '+',
                       seqlevels = letters[1:10]), 
               "names of metadata columns cannot be one of")
  expect_error(GTuples(seqnames = 'chr1', tuples = matrix(1:10), strand = '+',
                       seqlengths = letters[1:10]), 
               "length of supplied 'seqlengths' must equal the number of")
  expect_error(GTuples(seqnames = 'chr1', tuples = matrix(1:10), strand = '+',
                       isCircular = letters[1:10]), 
               "names of metadata columns cannot be one of")
  expect_error(GTuples(seqnames = 'chr1', tuples = matrix(1:10), strand = '+',
                       start = letters[1:10]), 
               "names of metadata columns cannot be one of")
  expect_error(GTuples(seqnames = 'chr1', tuples = matrix(1:10), strand = '+',
                       end = letters[1:10]), 
               "names of metadata columns cannot be one of")
  expect_error(GTuples(seqnames = 'chr1', tuples = matrix(1:10), strand = '+',
                       width = letters[1:10]), 
               "names of metadata columns cannot be one of")
  expect_error(GTuples(seqnames = 'chr1', tuples = matrix(1:10), strand = '+',
                       element = letters[1:10]), 
               "names of metadata columns cannot be one of")
  expect_error(GTuples(seqnames = 'chr1', tuples = matrix(1:10), strand = '+',
                       tuples = letters[1:10]), 
               "formal argument \"tuples\" matched by multiple")
  expect_error(GTuples(seqnames = 'chr1', tuples = matrix(1:10), strand = '+',
                       internalPos = letters[1:10]), 
               "names of metadata columns cannot be one of")
  expect_error(GTuples(seqnames = 'chr1', tuples = matrix(1:10), strand = '+',
                       size = letters[1:10]), 
               "names of metadata columns cannot be one of")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###
context("GTuples constructor")

test_that("GTuples constructor returns a valid GTuples object when m = 0", {
  expect_true(validObject(new("GTuples")))
  expect_true(validObject(gt0))
})

test_that("GTuples constructor returns a valid object when m = 1", {
  expect_true(validObject(gt1))
})

test_that("GTuples constructor returns a valid object when m = 2", {
  expect_true(validObject(gt2))
})

test_that("GTuples constructor returns a valid object when m >= 3", {
  expect_true(validObject(gt3))
  expect_true(validObject(gt4))
})

test_that("GTuples constructor returns warnings on unexpected input", {
  expect_warning(GTuples('chr1', tuples = matrix(c(1.1, 2, 3), ncol = 1)), 
                 "Converting 'tuples' to integer mode")
})

test_that("GTuples constructor returns errors on bad input", {
  expect_error(GTuples('chr1', tuples = 1:10), 
               "'tuples' must be an integer matrix")
  expect_error(GTuples('chr1', tuples = as.matrix(letters)), 
               "'tuples' must be an integer matrix")
  expect_error(GTuples('chr1', tuples = matrix(c(1, NA), ncol = 1)), 
               "'NA' detected in 'tuples'")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Coercion
###
context("GTuples coercion")

test_that("as.data.frame works", {
  expect_identical(as.data.frame(gt0), data.frame())
  expect_identical(as.data.frame(gt1), 
                   data.frame(seqnames = as.factor(seqnames(gt1)),
                              pos1 = start(gt1),
                              strand = as.factor(strand(gt1)),
                              score = mcols(gt1)$score
                   )
  )
  expect_identical(as.data.frame(gt2), 
                   data.frame(seqnames = as.factor(seqnames(gt4)),
                              pos1 = start(gt1),
                              pos2 = end(gt2),
                              strand = as.factor(strand(gt2)),
                              score = mcols(gt2)$score
                   )
  )
  expect_identical(as.data.frame(gt3), 
                   data.frame(seqnames = as.factor(seqnames(gt3)),
                              pos1 = start(gt3),
                              pos2 = gt3@internalPos,
                              pos3 = end(gt3),
                              strand = as.factor(strand(gt3)),
                              score = mcols(gt1)$score
                   )
  )
  expect_identical(as.data.frame(gt4), 
                   data.frame(seqnames = as.factor(seqnames(gt4)),
                              pos1 = start(gt4),
                              pos2 = gt4@internalPos[, 1],
                              pos3 = gt4@internalPos[, 2],
                              pos4 = end(gt4),
                              strand = as.factor(strand(gt4)),
                              score = mcols(gt4)$score
                   )
  )
})

test_that("granges works", {
  expect_identical(granges(gt0), GRanges(seqinfo = seqinfo(gt0)))
  expect_identical(granges(gt1, use.mcols = FALSE), 
                   GRanges(seqnames(gt1), IRanges(start(gt1), end(gt1)),
                           strand(gt1), 
                           seqinfo = seqinfo(gt1)))
  expect_identical(granges(gt1, use.mcols = TRUE), 
                   GRanges(seqnames(gt1), IRanges(start(gt1), end(gt1)),
                           strand(gt1), score = mcols(gt1)$score, 
                           seqinfo = seqinfo(gt1)))
  expect_identical(granges(gt4, use.mcols = FALSE), 
                   GRanges(seqnames(gt4), IRanges(start(gt4), end(gt4)),
                           strand(gt4), 
                           seqinfo = seqinfo(gt4)))
  expect_identical(granges(gt4, use.mcols = TRUE), 
                   GRanges(seqnames(gt4), IRanges(start(gt4), end(gt4)),
                           strand(gt4), score = mcols(gt4)$score, 
                           seqinfo = seqinfo(gt4)))
  expect_identical(granges(gt3, use.mcols = FALSE), 
                   GRanges(seqnames(gt3), IRanges(start(gt3), end(gt3)),
                           strand(gt3), 
                           seqinfo = seqinfo(gt3)))
  expect_identical(granges(gt3, use.mcols = TRUE), 
                   GRanges(seqnames(gt3), IRanges(start(gt3), end(gt3)),
                           strand(gt3), score = mcols(gt3)$score, 
                           seqinfo = seqinfo(gt3)))
  expect_identical(granges(gt4, use.mcols = FALSE), 
                   GRanges(seqnames(gt4), IRanges(start(gt4), end(gt4)),
                           strand(gt4), 
                           seqinfo = seqinfo(gt4)))
  expect_identical(granges(gt4, use.mcols = TRUE), 
                   GRanges(seqnames(gt4), IRanges(start(gt4), end(gt4)),
                           strand(gt4), score = mcols(gt4)$score, 
                           seqinfo = seqinfo(gt4)))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Updating and cloning
###
context("GTuples updating and cloning")

test_that("update works on all relevant slots", {
  gt1_update <- update(gt1, seqnames = rev(seqnames(gt1)))
  expect_identical(gt1_update, GTuples(rev(seqnames(gt1)), tuples(gt1), 
                                       strand(gt1), score = mcols(gt1)$score, 
                                       seqinfo = seqinfo(gt1)))
  gt1_update <- update(gt1, ranges = rev(ranges(gt1)))
  expect_identical(gt1_update, GTuples(seqnames(gt1), 
                                       as.matrix(rev(tuples(gt1))), 
                                       strand(gt1), score = mcols(gt1)$score, 
                                       seqinfo = seqinfo(gt1)))
  gt1_update <- update(gt1, strand = rev(strand(gt1)))
  expect_identical(gt1_update, GTuples(seqnames(gt1), tuples(gt1), 
                                       rev(strand(gt1)),
                                       score = mcols(gt1)$score, 
                                       seqinfo = seqinfo(gt1)))
  gt1_update <- update(gt1, elementMetadata = DataFrame(score = Rle(0L, 10)))
  expect_identical(gt1_update, GTuples(seqnames(gt1), tuples(gt1), strand(gt1),
                                       score = Rle(0L, 10), 
                                       seqinfo = seqinfo(gt1)))
  seqinfo <- Seqinfo(seqnames = c("chr1", "chr2", "chr3"), 
                     seqlengths = c(10000L, 20000L, 15000L), 
                     isCircular = c(NA, NA, NA), 
                     genome = c("mock1", "mock1", "mock1"))
  gt1_update <- update(gt1, seqinfo = seqinfo)
  expect_identical(gt1_update, GTuples(seqnames(gt1), tuples(gt1), strand(gt1),
                                       score = mcols(gt1)$score, 
                                       seqinfo = seqinfo))
  # metadata(gt1) is not the same as setting the metadata in the GTuples() 
  # constructor. This (somewhat confusing) behaviour is inherited from GRanges()
  gt1_update <- update(gt1, metadata = list("foo" = "bar"))
  gt1_metadata <- gt1
  metadata(gt1_metadata) <- list("foo" = "bar")
  expect_identical(gt1_update, gt1_metadata)
  gt3_update <- update(gt3, ranges = IRanges(start(gt3) + 10L, end(gt3) + 10L), 
                       internalPos = gt3@internalPos + 10L)
  expect_identical(gt3_update, GTuples(seqnames(gt3), unname(tuples(gt3)) + 10L, 
                                       strand(gt3), score = mcols(gt3)$score, 
                                       seqinfo = seqinfo(gt3)))
  gt4_update <- update(gt4, ranges = IRanges(start(gt4) + 10L, end(gt4) + 10L), 
                       internalPos = gt4@internalPos + 10L)
  expect_identical(gt4_update, GTuples(seqnames(gt4), unname(tuples(gt4)) + 10L, 
                                       strand(gt4), score = mcols(gt4)$score, 
                                       seqinfo = seqinfo(gt4)))
})

test_that("clone works", {
  gt1_clone <- GenomicRanges:::clone(gt1, seqnames = rev(seqnames(gt1)))
  expect_identical(gt1_clone, GTuples(rev(seqnames(gt1)), tuples(gt1), 
                                      strand(gt1), score = mcols(gt1)$score, 
                                      seqinfo = seqinfo(gt1)))
  gt1_clone <- GenomicRanges:::clone(gt1, ranges = rev(ranges(gt1)))
  expect_identical(gt1_clone, GTuples(seqnames(gt1), 
                                      as.matrix(rev(tuples(gt1))), strand(gt1), 
                                      score = mcols(gt1)$score, 
                                      seqinfo = seqinfo(gt1)))
  gt1_clone <- GenomicRanges:::clone(gt1, strand = rev(strand(gt1)))
  expect_identical(gt1_clone, GTuples(seqnames(gt1), tuples(gt1), 
                                      rev(strand(gt1)), 
                                      score = mcols(gt1)$score, 
                                      seqinfo = seqinfo(gt1)))
  gt1_clone <- GenomicRanges:::clone(gt1, elementMetadata = 
                                       DataFrame(score = Rle(0L, 10)))
  expect_identical(gt1_clone, GTuples(seqnames(gt1), tuples(gt1), strand(gt1),
                                      score = Rle(0L, 10), 
                                      seqinfo = seqinfo(gt1)))
  seqinfo <- Seqinfo(seqnames = c("chr1", "chr2", "chr3"), 
                     seqlengths = c(10000L, 20000L, 15000L), 
                     isCircular = c(NA, NA, NA), 
                     genome = c("mock1", "mock1", "mock1"))
  gt1_clone <- GenomicRanges:::clone(gt1, seqinfo = seqinfo)
  expect_identical(gt1_clone, GTuples(seqnames(gt1), tuples(gt1), strand(gt1),
                                      score = mcols(gt1)$score, 
                                      seqinfo = seqinfo))
  # metadata(gt1) is not the same as setting the metadata in the GTuples() 
  # constructor. This (somewhat confusing) behaviour is inherited from GRanges()
  gt1_clone <- GenomicRanges:::clone(gt1, metadata = list("foo" = "bar"))
  gt1_metadata <- gt1
  metadata(gt1_metadata) <- list("foo" = "bar")
  expect_identical(gt1_clone, gt1_metadata)
  gt3_clone <- GenomicRanges:::clone(gt3, ranges = IRanges(start(gt3) + 10L, 
                                                           end(gt3) + 10L), 
                                     internalPos = gt3@internalPos + 10L)
  expect_identical(gt3_clone, GTuples(seqnames(gt3), unname(tuples(gt3)) + 10L, 
                                      strand(gt3), score = mcols(gt3)$score, 
                                      seqinfo = seqinfo(gt3)))
  gt4_clone <- GenomicRanges:::clone(gt4, ranges = IRanges(start(gt4) + 10L, 
                                                           end(gt4) + 10L), 
                                     internalPos = gt4@internalPos + 10L)
  expect_identical(gt4_clone, GTuples(seqnames(gt4), unname(tuples(gt4)) + 10L, 
                                      strand(gt4), score = mcols(gt4)$score, 
                                      seqinfo = seqinfo(gt4)))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Combining
###
context("Combining GTuples")

test_that(".unlist_list_of_GTuples works", {
  expect_identical(.unlist_list_of_GTuples(list(gt0)), gt0)
  expect_identical(.unlist_list_of_GTuples(list(gt1)), gt1)
  expect_identical(.unlist_list_of_GTuples(list(gt2)), gt2)
  expect_identical(.unlist_list_of_GTuples(list(gt3)), gt3)
  expect_identical(.unlist_list_of_GTuples(list(gt4)), gt4)
})

test_that("c works", {
  expect_identical(c(gt1[1:5], gt1[6:10]), gt1)
  expect_identical(c(gt2[1:5], gt2[6:10]), gt2)
  expect_identical(c(gt3[1:5], gt3[6:10]), gt3)
  expect_identical(c(gt4[1:5], gt4[6:10]), gt4)
  expect_error(c(gt3, granges(gt3)), 
               paste0("unable to find an inherited method for function ", 
                      sQuote('size'), " for signature ", sQuote('"GRanges"')))
  expect_error(c(gt3, gt4), 
               paste0("Cannot combine GTuples containing tuples of ", 
                      "different 'size'"))
  setClass(Class = "MTuples", contains = "GTuples")
  x <- new("MTuples", gt2)
  expect_is(c(x, gt2), "GTuples")
  expect_is(c(gt2, x), "GTuples")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Getters
###
context("GTuples getters")

test_that("GRanges inherited getters work", {
  expect_identical(length(gt0), 0L)
  expect_identical(length(gt1), 10L)
  expect_identical(length(gt2), 10L)
  expect_identical(length(gt3), 10L)
  expect_identical(length(gt4), 10L)
  expect_identical(seqnames(gt1), gt1@seqnames)
  expect_identical(ranges(gt2), gt2@ranges)
  expect_identical(strand(gt3), gt3@strand)
  expect_identical(mcols(gt3), gt3@elementMetadata)
  expect_identical(elementMetadata(gt3), gt3@elementMetadata)
  expect_identical(seqinfo(gt3), gt3@seqinfo)
  expect_identical(seqlevels(gt3), seqlevels(gt3@seqinfo))
  expect_identical(seqlengths(gt3), seqlengths(gt3@seqinfo))
  expect_identical(isCircular(gt3), isCircular(gt3@seqinfo))
  expect_identical(genome(gt3), genome(gt3@seqinfo))
  expect_identical(seqlevelsStyle(gt3), seqlevelsStyle(gt3@seqinfo))
  expect_identical(score(gt3), gt3@elementMetadata$score)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Splitting
###
context("GTuples splitting")

test_that("inherited split works", {
  ## by integer
  gt2_s <- split(gt2, 1:10)
  expect_identical(length(gt2_s), 10L)
  expect_is(gt2_s, "GTuplesList")
  ## by Rle
  gt2_s <- split(gt2, seqnames(gt2))
  expect_identical(length(gt2_s), 3L)
  expect_is(gt2_s, "GTuplesList")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Setters
###
context("GTuples setters")

test_that("GRanges inherited getters work", {
  gt1_ <- gt1
  seqnames(gt1_) <- rev(seqnames(gt1))
  expect_identical(seqnames(gt1_), rev(seqnames(gt1)))
  gt1_ <- gt1
  ranges(gt1_) <- rev(ranges(gt1))
  expect_identical(ranges(gt1_), rev(ranges(gt1)))
  gt1_ <- gt1
  strand(gt1_) <- rev(strand(gt1))
  expect_identical(strand(gt1_), rev(strand(gt1)))
  gt1_ <- gt1
  mcols(gt1_) <- DataFrame(score = rev(mcols(gt1)$score))
  expect_identical(mcols(gt1_), DataFrame(score = rev(mcols(gt1)$score)))
  gt1_ <- gt1
  seqinfo(gt1_) <- Seqinfo(seqnames = c("chr1", "chr2", "chr3"), 
                           seqlengths = c(10000L, 20000L, 15000L), 
                           isCircular = c(NA, NA, NA), 
                           genome = c("mock1", "mock1", "mock1"))
  expect_identical(seqinfo(gt1_), Seqinfo(seqnames = c("chr1", "chr2", "chr3"), 
                                          seqlengths = c(10000L, 20000L, 
                                                         15000L), 
                                          isCircular = c(NA, NA, NA), 
                                          genome = c("mock1", "mock1", 
                                                     "mock1")))
  gt1_ <- gt1
  seqlevels(gt1_) <- c('chrI', 'chrII', 'chrIII')
  expect_identical(seqlevels(gt1_), c('chrI', 'chrII', 'chrIII'))
  gt1_ <- gt1
  seqlengths(gt1_) <- c(10000L, 20000L, 15000L)
  expect_identical(seqlengths(gt1_), c('chr1' = 10000L, 'chr2' = 20000L, 
                                       'chr3' = 15000L))
  gt1_ <- gt1
  isCircular(gt1_) <- c('chr1' = TRUE, 'chr2' = FALSE, 'chr3' = FALSE)
  expect_identical(isCircular(gt1_), c('chr1' = TRUE, 'chr2' = FALSE, 
                                       'chr3' = FALSE))
  gt1_ <- gt1
  genome(gt1_) <- 'foo'
  expect_identical(genome(gt1_), c('chr1' = 'foo', 'chr2' = 'foo', 
                                   'chr3' = 'foo'))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Tuples methods
###
context("GTuples tuples methods")

test_that("size works", {
  expect_identical(size(gt0), NA_integer_)
  expect_identical(size(gt1), 1L)
  expect_identical(size(gt2), 2L)
  expect_identical(size(gt3), 3L)
  expect_identical(size(gt4), 4L)
})

test_that("IPD works", {
  expect_error(IPD(gt0), "Cannot compute IPD from an empty 'GTuples'.")
  expect_error(IPD(gt1), 
               "It does not make sense to compute IPD when 'size' = 1.")
  expect_identical(IPD(gt2), matrix(1L, nrow = 10, ncol = 1))
  expect_identical(IPD(gt3), matrix(1L, nrow = 10, ncol = 2))
  expect_identical(IPD(gt4), matrix(1L, nrow = 10, ncol = 3))
})

test_that("tuples works", {
  expect_identical(tuples(gt0), matrix())
  expect_identical(tuples(gt1), matrix(1:10, ncol = 1, 
                                       dimnames = list(NULL, 'pos1')))
  expect_identical(tuples(gt2), 
                   matrix(c(1:10, 2:11), ncol = 2, 
                          dimnames = list(NULL, c('pos1', 'pos2'))))
  expect_identical(tuples(gt3), 
                   matrix(c(1:10, 2:11, 3:12), ncol = 3, 
                          dimnames = list(NULL, c('pos1', 'pos2', 'pos3'))))
  expect_identical(tuples(gt4), 
                   matrix(c(1:10, 2:11, 3:12, 4:13), ncol = 4, 
                          dimnames = 
                            list(NULL, c('pos1', 'pos2', 'pos3', 'pos4'))))
})

test_that("tuples<- works", {
  tuples(gt1) <- matrix(101:110, ncol = 1)
  expect_identical(tuples(gt1), 
                   matrix(101:110, ncol = 1, dimnames = list(NULL, 'pos1')))
  tuples(gt2) <- matrix(c(101:110, 102:111), ncol = 2)
  expect_identical(tuples(gt2), 
                   matrix(c(101:110, 102:111), ncol = 2, 
                          dimnames = list(NULL, c('pos1', 'pos2'))))
  tuples(gt3) <- matrix(c(101:110, 102:111, 103:112), ncol = 3)
  expect_identical(tuples(gt3), 
                   matrix(c(101:110, 102:111, 103:112), ncol = 3, 
                          dimnames = list(NULL, c('pos1', 'pos2', 'pos3'))))
  tuples(gt4) <- matrix(c(101:110, 102:111, 103:112, 104:113), ncol = 4)
  expect_identical(tuples(gt4), 
                   matrix(c(101:110, 102:111, 103:112, 104:113), ncol = 4, 
                          dimnames = 
                            list(NULL, c('pos1', 'pos2', 'pos3', 'pos4'))))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### IPD
###
context("GTuples subsetting")

test_that("extractROWS works", {
  expect_identical(extractROWS(gt1, 1), 
                   GTuples('chr1', matrix(1L), strand = '-', score = 1L, 
                           seqinfo = seqinfo(gt1)))
  expect_identical(extractROWS(gt1, 1:2), 
                   GTuples(c('chr1', 'chr2'), matrix(1:2), strand = c('-', '+'), 
                           score = 1:2, seqinfo = seqinfo(gt2)))
  expect_identical(extractROWS(gt2, 1), 
                   GTuples('chr1', matrix(1:2, ncol = 2), strand = '-', 
                           score = 1L, seqinfo = seqinfo(gt1)))
  expect_identical(extractROWS(gt2, 1:2), 
                   GTuples(c('chr1', 'chr2'), matrix(c(1:2, 2:3), ncol = 2), 
                           strand = c('-', '+'), score = 1:2, 
                           seqinfo = seqinfo(gt2)))
  expect_identical(extractROWS(gt3, 1), 
                   GTuples('chr1', matrix(1:3, ncol = 3), strand = '-', 
                           score = 1L, seqinfo = seqinfo(gt3)))
  expect_identical(extractROWS(gt3, 1:2), 
                   GTuples(c('chr1', 'chr2'), matrix(c(1:2, 2:3, 3:4), 
                                                     ncol = 3), 
                           strand = c('-', '+'), score = 1:2, 
                           seqinfo = seqinfo(gt2)))
  expect_identical(extractROWS(gt4, 1), 
                   GTuples('chr1', matrix(1:4, ncol = 4), strand = '-', 
                           score = 1L, seqinfo = seqinfo(gt4)))
  expect_identical(extractROWS(gt4, 1:2), 
                   GTuples(c('chr1', 'chr2'), matrix(c(1:2, 2:3, 3:4, 4:5), 
                                                     ncol = 4), 
                           strand = c('-', '+'), score = 1:2, 
                           seqinfo = seqinfo(gt4)))
})

test_that("[ works", {
  expect_identical(gt1[1], 
                   GTuples('chr1', matrix(1L), strand = '-', score = 1L,
                           seqinfo = seqinfo(gt1)))
  expect_identical(gt1[1:2], 
                   GTuples(c('chr1', 'chr2'), matrix(1:2), strand = c('-', '+'), 
                           score = 1:2, seqinfo = seqinfo(gt2)))
  expect_identical(gt2[1], 
                   GTuples('chr1', matrix(1:2, ncol = 2), strand = '-', 
                           score = 1L, seqinfo = seqinfo(gt1)))
  expect_identical(gt2[1:2], 
                   GTuples(c('chr1', 'chr2'), 
                           matrix(c(1:2, 2:3), ncol = 2), strand = c('-', '+'), 
                           score = 1:2, seqinfo = seqinfo(gt2)))
  expect_identical(gt3[1], 
                   GTuples('chr1', matrix(1:3, ncol = 3), strand = '-', 
                           score = 1L, seqinfo = seqinfo(gt3)))
  expect_identical(gt3[1:2], 
                   GTuples(c('chr1', 'chr2'), 
                           matrix(c(1:2, 2:3, 3:4), ncol = 3), 
                           strand = c('-', '+'), score = 1:2, 
                           seqinfo = seqinfo(gt2)))
  expect_identical(gt4[1], 
                   GTuples('chr1', matrix(1:4, ncol = 4), strand = '-', 
                           score = 1L, seqinfo = seqinfo(gt4)))
  expect_identical(gt4[1:2], 
                   GTuples(c('chr1', 'chr2'), 
                           matrix(c(1:2, 2:3, 3:4, 4:5), ncol = 4), 
                           strand = c('-', '+'), 
                           score = 1:2, seqinfo = seqinfo(gt4)))
})

test_that("replaceROWS works", {
  gt1_ <- replaceROWS(gt1, 1, GTuples('chr3', matrix(20L), score = 20L, 
                                      seqinfo = seqinfo(gt1)))
  expect_identical(gt1_, GTuples(c('chr3', as.vector(seqnames(gt1)[2:10])), 
                                 matrix(c(20L, 2:10)), 
                                 strand = c('*', as.vector(strand(gt1)[2:10])), 
                                 score = c(20L, 2:10), 
                                 seqinfo = seqinfo(gt1)))
  gt1_ <- replaceROWS(gt1, 1:2, GTuples('chr3', matrix(20:21), score = 20:21, 
                                        seqinfo = seqinfo(gt1)))
  expect_identical(gt1_, GTuples(c(rep('chr3', 2), 
                                   as.vector(seqnames(gt1)[3:10])), 
                                 matrix(c(20:21, 3:10)), 
                                 strand = c(rep('*', 2), 
                                            as.vector(strand(gt1)[3:10])), 
                                 score = c(20:21, 3:10), 
                                 seqinfo = seqinfo(gt1)))
  gt2_ <- replaceROWS(gt2, 1, GTuples('chr3', matrix(20:21, ncol = 2), 
                                      score = 20L, seqinfo = seqinfo(gt2)))
  expect_identical(gt2_, GTuples(c('chr3', as.vector(seqnames(gt2)[2:10])), 
                                 rbind(20:21, unname(tuples(gt2)[2:10,])), 
                                 strand = c('*', as.vector(strand(gt2)[2:10])), 
                                 score = c(20L, 2:10), 
                                 seqinfo = seqinfo(gt2)))
  gt2_ <- replaceROWS(gt2, 1:2, GTuples('chr3', matrix(c(20:21, 21:22), 
                                                       ncol = 2), 
                                        score = 20:21, seqinfo = seqinfo(gt2)))
  expect_identical(gt2_, GTuples(c(rep('chr3', 2), 
                                   as.vector(seqnames(gt2)[3:10])), 
                                 rbind(matrix(c(20:21, 21:22), ncol = 2), 
                                       unname(tuples(gt2)[3:10,])), 
                                 strand = c(rep('*', 2), 
                                            as.vector(strand(gt2)[3:10])), 
                                 score = c(20:21, 3:10), 
                                 seqinfo = seqinfo(gt2)))
  gt3_ <- replaceROWS(gt3, 1, GTuples('chr3', matrix(20:22, ncol = 3), 
                                      score = 20L, seqinfo = seqinfo(gt3)))
  expect_identical(gt3_, GTuples(c('chr3', as.vector(seqnames(gt3)[2:10])), 
                                 rbind(20:22, unname(tuples(gt3)[2:10,])), 
                                 strand = c('*', as.vector(strand(gt3)[2:10])), 
                                 score = c(20L, 2:10), 
                                 seqinfo = seqinfo(gt3)))
  gt3_ <- replaceROWS(gt3, 1:2, GTuples('chr3', matrix(c(20:21, 21:22, 22:23), 
                                                       ncol = 3), 
                                        score = 20:21, seqinfo = seqinfo(gt3)))
  expect_identical(gt3_, GTuples(c(rep('chr3', 2), 
                                   as.vector(seqnames(gt3)[3:10])), 
                                 rbind(matrix(c(20:21, 21:22, 22:23), 
                                              ncol = 3),
                                       unname(tuples(gt3)[3:10,])), 
                                 strand = c(rep('*', 2), 
                                            as.vector(strand(gt3)[3:10])), 
                                 score = c(20:21, 3:10), 
                                 seqinfo = seqinfo(gt3)))
  gt4_ <- replaceROWS(gt4, 1, GTuples('chr3', matrix(20:23, ncol = 4), 
                                      score = 20L, seqinfo = seqinfo(gt4)))
  expect_identical(gt4_, GTuples(c('chr3', as.vector(seqnames(gt4)[2:10])), 
                                 rbind(20:23, unname(tuples(gt4)[2:10,])), 
                                 strand = c('*', as.vector(strand(gt4)[2:10])), 
                                 score = c(20L, 2:10), 
                                 seqinfo = seqinfo(gt4)))
  gt4_ <- replaceROWS(gt4, 1:2, GTuples('chr3', 
                                        matrix(c(20:21, 21:22, 22:23, 23:24), 
                                               ncol = 4), 
                                        score = 20:21, seqinfo = seqinfo(gt4)))
  expect_identical(gt4_, GTuples(c(rep('chr3', 2), 
                                   as.vector(seqnames(gt4)[3:10])), 
                                 rbind(matrix(c(20:21, 21:22, 22:23, 23:24), 
                                              ncol = 4),
                                       unname(tuples(gt4)[3:10,])), 
                                 strand = c(rep('*', 2), 
                                            as.vector(strand(gt4)[3:10])), 
                                 score = c(20:21, 3:10), 
                                 seqinfo = seqinfo(gt4)))
})

test_that("[<- works", {
  gt1_ <- gt1
  gt1_[1] <- GTuples('chr3', matrix(20L), score = 20L, seqinfo = seqinfo(gt1))
  expect_identical(gt1_, GTuples(c('chr3', as.vector(seqnames(gt1)[2:10])), 
                                 matrix(c(20L, 2:10)), 
                                 strand = c('*', as.vector(strand(gt1)[2:10])), 
                                 score = c(20L, 2:10), 
                                 seqinfo = seqinfo(gt1)))
  
  gt1_ <- gt1
  gt1_[1:2] <- GTuples('chr3', matrix(20:21), score = 20:21,
                       seqinfo = seqinfo(gt1))
  expect_identical(gt1_, GTuples(c(rep('chr3', 2), 
                                   as.vector(seqnames(gt1)[3:10])), 
                                 matrix(c(20:21, 3:10)), 
                                 strand = c(rep('*', 2), 
                                            as.vector(strand(gt1)[3:10])), 
                                 score = c(20:21, 3:10), 
                                 seqinfo = seqinfo(gt1)))
  
  gt2_ <- gt2
  gt2_[1] <- GTuples('chr3', matrix(20:21, ncol = 2), score = 20L, 
                     seqinfo = seqinfo(gt2))
  expect_identical(gt2_, GTuples(c('chr3', as.vector(seqnames(gt2)[2:10])), 
                                 rbind(20:21, unname(tuples(gt2)[2:10,])), 
                                 strand = c('*', as.vector(strand(gt2)[2:10])), 
                                 score = c(20L, 2:10), 
                                 seqinfo = seqinfo(gt2)))
  gt2_ <- gt2
  gt2_[1:2] <- GTuples('chr3', matrix(c(20:21, 21:22), ncol = 2), 
                       score = 20:21, seqinfo = seqinfo(gt2))
  expect_identical(gt2_, GTuples(c(rep('chr3', 2), 
                                   as.vector(seqnames(gt2)[3:10])), 
                                 rbind(matrix(c(20:21, 21:22), ncol = 2), 
                                       unname(tuples(gt2)[3:10,])), 
                                 strand = c(rep('*', 2), 
                                            as.vector(strand(gt2)[3:10])), 
                                 score = c(20:21, 3:10), 
                                 seqinfo = seqinfo(gt2)))
  gt3_ <- gt3
  gt3_[1] <- GTuples('chr3', matrix(20:22, ncol = 3), score = 20L, 
                     seqinfo = seqinfo(gt3))
  expect_identical(gt3_, GTuples(c('chr3', as.vector(seqnames(gt3)[2:10])), 
                                 rbind(20:22, unname(tuples(gt3)[2:10,])), 
                                 strand = c('*', as.vector(strand(gt3)[2:10])), 
                                 score = c(20L, 2:10), 
                                 seqinfo = seqinfo(gt3)))
  gt3_ <- gt3
  gt3_[1:2] <- GTuples('chr3', matrix(c(20:21, 21:22, 22:23), ncol = 3), 
                       score = 20:21, seqinfo = seqinfo(gt3))
  expect_identical(gt3_, GTuples(c(rep('chr3', 2), 
                                   as.vector(seqnames(gt3)[3:10])), 
                                 rbind(matrix(c(20:21, 21:22, 22:23), 
                                              ncol = 3),
                                       unname(tuples(gt3)[3:10,])), 
                                 strand = c(rep('*', 2), 
                                            as.vector(strand(gt3)[3:10])), 
                                 score = c(20:21, 3:10), 
                                 seqinfo = seqinfo(gt3)))
  gt4_ <- gt4
  gt4_[1] <- GTuples('chr3', matrix(20:23, ncol = 4), score = 20L, 
                     seqinfo = seqinfo(gt4))
  expect_identical(gt4_, GTuples(c('chr3', as.vector(seqnames(gt4)[2:10])), 
                                 rbind(20:23, unname(tuples(gt4)[2:10,])), 
                                 strand = c('*', as.vector(strand(gt4)[2:10])), 
                                 score = c(20L, 2:10), 
                                 seqinfo = seqinfo(gt4)))
  gt4_[1:2] <- GTuples('chr3', matrix(c(20:21, 21:22, 22:23, 23:24), ncol = 4), 
                       score = 20:21, seqinfo = seqinfo(gt4))
  expect_identical(gt4_, GTuples(c(rep('chr3', 2), 
                                   as.vector(seqnames(gt4)[3:10])), 
                                 rbind(matrix(c(20:21, 21:22, 22:23, 23:24), 
                                              ncol = 4),
                                       unname(tuples(gt4)[3:10,])), 
                                 strand = c(rep('*', 2), 
                                            as.vector(strand(gt4)[3:10])), 
                                 score = c(20:21, 3:10), 
                                 seqinfo = seqinfo(gt4)))
  expect_error(gt2[1:2] <- gt3[1:2], 
               "Cannot replace with tuples of a different 'size'")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Show
###

# TODO: Not sure how to test this. However, these tests are necessary due 
# to the showGTuples function being rather fragile.
