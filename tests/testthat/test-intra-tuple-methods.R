### =========================================================================
### Intra-tuple methods
### -------------------------------------------------------------------------

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### shift()
###

context("shift,GTuples-method")

test_that("Works on empty GTuples", {
  expect_identical(shift(gt0, 0L), gt0)
  expect_identical(shift(gt0, 10L), gt0)
  expect_identical(shift(gt0, -10L), gt0)  
})

test_that("Works on 1-tuples", {
  expect_identical(shift(gt1, 0L), gt1)
  expect_identical(shift(gt1, 10L), 
                   GTuples(seqnames = seqnames(gt1), 
                           tuples = unname(tuples(gt1)) + 10L, 
                           strand = strand(gt1), 
                           seqinfo = seqinfo(gt1), 
                           score = score(gt1)))
  expect_identical(shift(gt1[-1], -1L), 
                   GTuples(seqnames = seqnames(gt1[-1]), 
                           tuples = unname(tuples(gt1[-1])) - 1L, 
                           strand = strand(gt1[-1]), 
                           seqinfo = seqinfo(gt1[-1]), 
                           score = score(gt1[-1])))  
})

test_that("Works on 2-tuples", {
  expect_identical(shift(gt2, 0L), gt2)
  expect_identical(shift(gt2, 10L), 
                   GTuples(seqnames = seqnames(gt2), 
                           tuples = unname(tuples(gt2)) + 10L, 
                           strand = strand(gt2), 
                           seqinfo = seqinfo(gt2), 
                           score = score(gt2)))
  expect_identical(shift(gt2[-1], -1L), 
                   GTuples(seqnames = seqnames(gt2[-1]), 
                           tuples = unname(tuples(gt2[-1])) - 1L, 
                           strand = strand(gt2[-1]), 
                           seqinfo = seqinfo(gt2[-1]), 
                           score = score(gt2[-1])))  
})

test_that("Works on 3-tuples", {
  expect_identical(shift(gt3, 0L), gt3)
  expect_identical(shift(gt3, 10L), 
                   GTuples(seqnames = seqnames(gt3), 
                           tuples = unname(tuples(gt3)) + 10L, 
                           strand = strand(gt3), 
                           seqinfo = seqinfo(gt3), 
                           score = score(gt3)))
  expect_identical(shift(gt3[-1], -1L), 
                   GTuples(seqnames = seqnames(gt3[-1]), 
                           tuples = unname(tuples(gt3[-1])) - 1L, 
                           strand = strand(gt3[-1]), 
                           seqinfo = seqinfo(gt3[-1]), 
                           score = score(gt3[-1])))  
})

test_that("Works on m-tuples, m > 3", {
  expect_identical(shift(gt4, 0L), gt4)
  expect_identical(shift(gt4, 10L), 
                   GTuples(seqnames = seqnames(gt4), 
                           tuples = unname(tuples(gt4)) + 10L, 
                           strand = strand(gt4), 
                           seqinfo = seqinfo(gt4), 
                           score = score(gt4)))
  expect_identical(shift(gt4[-1], -1L), 
                   GTuples(seqnames = seqnames(gt4[-1]), 
                           tuples = unname(tuples(gt4[-1])) - 1L, 
                           strand = strand(gt4[-1]), 
                           seqinfo = seqinfo(gt4[-1]), 
                           score = score(gt4[-1])))  
})

context("shift,GTuplesList-method")

test_that("Works on empty GTuples", {
  expect_identical(shift(gtl0, 0L), gtl0)
  expect_identical(shift(gtl0, IntegerList(0L, 0L)), gtl0)
  expect_identical(shift(gtl0, 10L), gtl0)
  expect_identical(shift(gtl0, IntegerList(10L, 20L)), gtl0)
  expect_identical(shift(gtl0, -10L), gtl0)
  expect_identical(shift(gtl0, IntegerList(-10L, -20L)), gtl0)
})

test_that("Works on 1-tuples", {
  expect_identical(shift(gtl1, 0L), gtl1)
  expect_identical(shift(gtl1, IntegerList(0L, 0L)), gtl1)
  expect_identical(shift(gtl1, 10L), 
                   GTuplesList(A = shift(gt1[1:5], 10L), 
                               B = shift(gt1[6:10], 10L)))
  expect_identical(shift(gtl1, IntegerList(10L, 20L)), 
                   GTuplesList(A = shift(gt1[1:5], 10L), 
                               B = shift(gt1[6:10], 20L)))
  expect_identical(shift(gtl1[2], -1L), 
                   GTuplesList(B = shift(gt1[6:10], -1L)))
  expect_identical(shift(gtl1, IntegerList(0L, -2L)), 
                   GTuplesList(A = gt1[1:5], 
                               B = shift(gt1[6:10], -2L)))
})

test_that("Works on 2-tuples", {
  expect_identical(shift(gtl2, 0L), gtl2)
  expect_identical(shift(gtl2, IntegerList(0L, 0L)), gtl2)
  expect_identical(shift(gtl2, 10L), 
                   GTuplesList(A = shift(gt2[1:5], 10L), 
                               B = shift(gt2[6:10], 10L)))
  expect_identical(shift(gtl2, IntegerList(10L, 20L)), 
                   GTuplesList(A = shift(gt2[1:5], 10L), 
                               B = shift(gt2[6:10], 20L)))
  expect_identical(shift(gtl2[2], -1L), 
                   GTuplesList(B = shift(gt2[6:10], -1L)))
  expect_identical(shift(gtl2, IntegerList(0L, -2L)), 
                   GTuplesList(A = gt2[1:5], 
                               B = shift(gt2[6:10], -2L)))
})

test_that("Works on 3-tuples", {
  expect_identical(shift(gtl3, 0L), gtl3)
  expect_identical(shift(gtl3, IntegerList(0L, 0L)), gtl3)
  expect_identical(shift(gtl3, 10L), 
                   GTuplesList(A = shift(gt3[1:5], 10L), 
                               B = shift(gt3[6:10], 10L)))
  expect_identical(shift(gtl3, IntegerList(10L, 20L)), 
                   GTuplesList(A = shift(gt3[1:5], 10L), 
                               B = shift(gt3[6:10], 20L)))
  expect_identical(shift(gtl3[2], -1L), 
                   GTuplesList(B = shift(gt3[6:10], -1L)))
  expect_identical(shift(gtl3, IntegerList(0L, -2L)), 
                   GTuplesList(A = gt3[1:5], 
                               B = shift(gt3[6:10], -2L)))
})

test_that("Works on m-tuples, m > 3", {
  expect_identical(shift(gtl4, 0L), gtl4)
  expect_identical(shift(gtl4, IntegerList(0L, 0L)), gtl4)
  expect_identical(shift(gtl4, 10L), 
                   GTuplesList(A = shift(gt4[1:5], 10L), 
                               B = shift(gt4[6:10], 10L)))
  expect_identical(shift(gtl4, IntegerList(10L, 20L)), 
                   GTuplesList(A = shift(gt4[1:5], 10L), 
                               B = shift(gt4[6:10], 20L)))
  expect_identical(shift(gtl4[2], -1L), 
                   GTuplesList(B = shift(gt4[6:10], -1L)))
  expect_identical(shift(gtl4, IntegerList(0L, -2L)), 
                   GTuplesList(A = gt4[1:5], 
                               B = shift(gt4[6:10], -2L)))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### narrow()
###
context("narrow,GTuples-method")

test_that("Returns error", {
  expect_error(narrow(gt2), 
               "GTuples do not currently support the 'narrow' method.")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### flank()
###
context("flank,GTuples-method")

test_that("Returns error", {
  expect_error(flank(gt2), 
               "GTuples do not currently support the 'flank' method.")
})

context("flank,GTuplesList-method")

test_that("Returns error", {
  expect_error(flank(gtl2), 
               "GTuplesList do not currently support the 'flank' method.")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### promoters()
###
context("promoters,GTuples-method")

test_that("Returns error", {
  expect_error(promoters(gt2), 
               "GTuples do not currently support the 'promoters' method.")
})

context("promoters,GTuplesList-method")

test_that("Returns error", {
  expect_error(promoters(gtl2), 
               "GTuplesList do not currently support the 'promoters' method.")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### resize()
###
context("resize,GTuples-method")

test_that("Returns error", {
  expect_error(resize(gt2), 
               "GTuples do not currently support the 'resize' method.")
})

context("resize,GTuplesList-method")

test_that("Returns error", {
  expect_error(resize(gtl2), 
               "GTuplesList do not currently support the 'resize' method.")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### restrict()
###
context("restrict,GTuplesList-method")

test_that("Returns error", {
  expect_error(restrict(gtl2), 
               "GTuplesList do not currently support the 'restrict' method.")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### trim()
###
context("trim,GTuples-method")

test_that("Works on empty GTuples", {
  expect_identical(trim(gt0), gt0)
})

test_that("Works on 1-tuples", {
  x <- suppressWarnings(GTuples('chr1', matrix(10L), 
                                seqinfo = Seqinfo('chr1', 5)))
  tmp <- suppressWarnings(GTuples('chr1', matrix(6L), 
                                  seqinfo = Seqinfo('chr1', 5)))
  width(tmp) <- 0
  expect_identical(trim(x), tmp)
})

test_that("Works on 2-tuples", {
  x <- suppressWarnings(GTuples('chr1', matrix(c(4L, 10L), ncol = 2), 
                                seqinfo = Seqinfo('chr1', 5)))
  tmp <- GTuples('chr1', matrix(c(4L, 5L), ncol = 2), 
                 seqinfo = Seqinfo('chr1', 5))
  expect_identical(trim(x), tmp)
  x <- suppressWarnings(GTuples('chr1', matrix(c(10L, 20L), ncol = 2), 
                                seqinfo = Seqinfo('chr1', 5)))
  expect_error(trim(x))
})

test_that("Works on 3-tuples", {
  x <- suppressWarnings(GTuples('chr1', matrix(c(2L, 4L, 10L), ncol = 3), 
                                seqinfo = Seqinfo('chr1', 5)))
  tmp <- GTuples('chr1', matrix(c(2L, 4L, 5L), ncol = 3), 
                 seqinfo = Seqinfo('chr1', 5))
  expect_identical(trim(x), tmp)
  x <- suppressWarnings(GTuples('chr1', matrix(c(2L, 10L, 20L), ncol = 3), 
                                seqinfo = Seqinfo('chr1', 5)))
  expect_error(trim(x))
})

test_that("Works on m-tuples, m > 3", {
  x <- suppressWarnings(GTuples('chr1', matrix(c(1L, 2L, 4L, 10L), ncol = 4), 
                                seqinfo = Seqinfo('chr1', 5)))
  tmp <- GTuples('chr1', matrix(c(1L, 2L, 4L, 5L), ncol = 4), 
                 seqinfo = Seqinfo('chr1', 5))
  expect_identical(trim(x), tmp)
  x <- suppressWarnings(GTuples('chr1', matrix(c(1L, 2L, 10L, 20L), ncol = 4), 
                                seqinfo = Seqinfo('chr1', 5)))
  expect_error(trim(x))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Zooming (symmetrically scales the width).
###
context("Ops,GTuples-method")

test_that("Returns error", {
  expect_error(Ops(gt2, 2), 
               "GTuples do not currently support the 'zoom' method.")
})
