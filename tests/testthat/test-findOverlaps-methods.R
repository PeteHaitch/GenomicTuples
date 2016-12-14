# NOTE: Several objects used in testing are defined in 
#       tests/testthat/helper-make-test-data.R
# NOTE: There are a large number of calls to suppressWarnings() in these tests 
#       because findOverlaps() produces a warning whenever a GTuples object is 
#       treated as a GRanges object.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### findOverlaps
###
context("findOverlaps,GTuples,GTuples-method")

test_that("Works on empty GTuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  hits <- findOverlaps(gt0, gt0, type = "equal")
  expect_true(inherits(hits, "Hits"))
  expect_identical(length(hits), 0L)
  expect_identical(findOverlaps(gt0, gt0, type = 'any'), 
                   findOverlaps(gr0, gr0, 
                                type = 'any'))
  expect_identical(findOverlaps(gt0, gt0, type = 'start'), 
                   findOverlaps(gr0, gr0, 
                                type = 'start'))
  expect_identical(findOverlaps(gt0, gt0, type = 'end'), 
                   findOverlaps(gr0, gr0, 
                                type = 'end'))
  expect_identical(findOverlaps(gt0, gt0, type = 'within'), 
                   findOverlaps(gr0, gr0, 
                                type = 'within'))
  expect_identical(findOverlaps(gt0, gt0, type = 'equal'), 
                   findOverlaps(gr0, gr0, 
                                type = 'equal'))
})

test_that("Works on 1-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  hits <- findOverlaps(gt1, gt1, type = "equal")
  expect_true(inherits(hits, "Hits"))
  expect_identical(length(hits), 10L)
  expect_identical(queryHits(hits), 1:10)
  expect_identical(subjectHits(hits), 1:10)
  expect_identical(findOverlaps(gt1, gt1, type = 'any'), 
                   findOverlaps(gr1, gr1, 
                                type = 'any'))
  expect_identical(findOverlaps(gt1, gt1, type = 'start'), 
                   findOverlaps(gr1, gr1, 
                                type = 'start'))
  expect_identical(findOverlaps(gt1, gt1, type = 'end'), 
                   findOverlaps(gr1, gr1, 
                                type = 'end'))
  expect_identical(findOverlaps(gt1, gt1, type = 'within'), 
                   findOverlaps(gr1, gr1, 
                                type = 'within'))
  expect_identical(findOverlaps(gt1, gt1, type = 'equal'), 
                   findOverlaps(gr1, gr1, 
                                type = 'equal'))
})

test_that("Works on 2-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  tmp2 <- GTuples(seqnames = c("chr1", "chr1"),
                  tuples = matrix(1:4, ncol = 2),
                  strand = "+")
  hits <- findOverlaps(tmp2, tmp2, type = "equal")
  expect_identical(length(hits), 2L)
  expect_identical(queryHits(hits), 1:2)
  hits <- findOverlaps(tmp2, tmp2, type = "any")
  expect_identical(length(hits), 4L)
  expect_identical(queryHits(hits), c(1L, 1L, 2L, 2L))
  expect_identical(subjectHits(hits), c(1L, 2L, 1L, 2L))
  expect_identical(findOverlaps(gt2, gt2, type = 'any'), 
                   findOverlaps(gr2, gr2, 
                                type = 'any'))
  expect_identical(findOverlaps(gt2, gt2, type = 'start'), 
                   findOverlaps(gr2, gr2, 
                                type = 'start'))
  expect_identical(findOverlaps(gt2, gt2, type = 'end'), 
                   findOverlaps(gr2, gr2, 
                                type = 'end'))
  expect_identical(findOverlaps(gt2, gt2, type = 'within'), 
                   findOverlaps(gr2, gr2, 
                                type = 'within'))
  expect_identical(findOverlaps(gt2, gt2, type = 'equal'), 
                   findOverlaps(gr2, gr2, 
                                type = 'equal'))
})

test_that("Works on 3-tuples", {
  # More complicated than 1- or 2-tuples. 
  # Specifically, need specialised tests if type = 'equal'
  tmp3 <- GTuples(seqnames = c("chr1", "chr1"),
                  tuples = matrix(1:6, ncol = 3),
                  strand = "+")
  hits <- findOverlaps(tmp3, tmp3, type = "equal")
  expect_identical(length(hits), 2L)
  expect_identical(queryHits(hits), 1:2)
  hits <- suppressWarnings(findOverlaps(tmp3, tmp3, type = "any"))
  expect_identical(length(hits), 4L)
  expect_identical(queryHits(hits), c(1L, 1L, 2L, 2L))
  expect_identical(subjectHits(hits), c(1L, 2L, 1L, 2L))
  expect_identical(findOverlaps(gt3, gt3, type = 'equal'), 
                   Hits(1:10, 1:10, 10L, 10L, sort.by.query = TRUE))
  expect_identical(suppressWarnings(findOverlaps(q3, q3, type = 'any')),
                   findOverlaps(q3_gr, q3_gr, type = 'any'))
  expect_identical(suppressWarnings(findOverlaps(q3, q3, type = 'start')),
                   findOverlaps(q3_gr, q3_gr, type = 'start'))
  expect_identical(suppressWarnings(findOverlaps(q3, q3, type = 'end')),
                   findOverlaps(q3_gr, q3_gr, type = 'end'))
  expect_identical(suppressWarnings(findOverlaps(q3, q3, type = 'within')),
                   findOverlaps(q3_gr, q3_gr, type = 'within'))
  expect_that(findOverlaps(q3, q3, type = 'equal'),
              negate(is_identical_to(findOverlaps(q3_gr, q3_gr, 
                                               type = 'equal'))))
  expect_identical(findOverlaps(q3, q3, type = 'equal', select = 'all'),
                   Hits(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 5L, 5L, 
                          6L, 6L, 7L, 7L, 8L, 8L, 9L, 9L), 
                        c(1L, 4L, 7L, 2L, 5L, 8L, 3L, 6L, 9L, 1L, 4L, 2L, 5L, 
                          3L, 6L, 1L, 7L, 2L, 8L, 3L, 9L), 
                        9L, 9L,
                        sort.by.query = TRUE))
  expect_identical(findOverlaps(q3, q3, type = 'equal', select = 'first'), 
                   rep(1:3, 3))
  expect_identical(findOverlaps(q3, q3, type = 'equal', select = 'last'), 
                   c(7:9, 4:6, 7:9))
  expect_identical(findOverlaps(q3, q3, type = 'equal', select = 'arbitrary'), 
                   c(7:9, 4:6, 7:9))
  expect_identical(findOverlaps(q3, q3, type = 'equal', select = 'all', 
                                ignore.strand = TRUE), 
                   Hits(c(rep(1:9, each = 3)), 
                        rep(c(1L, 4L, 7L, 2L, 5L, 8L, 3L, 6L, 9L), times = 3),
                        9L, 9L,
                        sort.by.query = TRUE))
  q3c <- q3
  seqinfo(q3c) <- Seqinfo('chr1', 100, isCircular = TRUE)
  expect_identical(findOverlaps(q3c, q3c, type = 'equal'), 
                   findOverlaps(q3c, shift(q3c, 100L), type = 'equal'))
  expect_error(findOverlaps(q3, q3, type = 'equal', minoverlap = 10L), 
               paste0("'minoverlap' must be 1 when 'type = equal', other ", 
                      "values not yet supported"))
  expect_error(findOverlaps(q3, q3, type = 'equal', maxgap = 10L), 
               paste0("'maxgap' must be 0 when 'type = equal', other ", 
                      "values not yet supported"))
})

test_that("Works on m-tuples, m > 3", {
  tmp4 <- GTuples(seqnames = c("chr1", "chr1"),
                  tuples = matrix(1:8, ncol = 4),
                  strand = "+")
  hits <- findOverlaps(tmp4, tmp4, type = "equal")
  expect_identical(length(hits), 2L)
  expect_identical(queryHits(hits), 1:2)
  hits <- suppressWarnings(findOverlaps(tmp4, tmp4, type = "any"))
  expect_identical(length(hits), 4L)
  expect_identical(queryHits(hits), c(1L, 1L, 2L, 2L))
  expect_identical(subjectHits(hits), c(1L, 2L, 1L, 2L))
  expect_identical(findOverlaps(gt4, gt4, type = 'equal'), 
                   Hits(1:10, 1:10, 10L, 10L, sort.by.query = TRUE))
  expect_identical(suppressWarnings(findOverlaps(q4, q4, type = 'any')),
                   findOverlaps(q4_gr, q4_gr, type = 'any'))
  expect_identical(suppressWarnings(findOverlaps(q4, q4, type = 'start')),
                   findOverlaps(q4_gr, q4_gr, type = 'start'))
  expect_identical(suppressWarnings(findOverlaps(q4, q4, type = 'end')),
                   findOverlaps(q4_gr, q4_gr, type = 'end'))
  expect_identical(suppressWarnings(findOverlaps(q4, q4, type = 'within')),
                   findOverlaps(q4_gr, q4_gr, type = 'within'))
  expect_that(findOverlaps(q4, q4, type = 'equal'),
              negate(is_identical_to(findOverlaps(q4_gr, q4_gr, 
                                               type = 'equal'))))
  expect_identical(findOverlaps(q4, q4, type = 'equal', select = 'all'),
                   Hits(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 
                          5L, 5L, 6L, 6L, 7L, 7L, 8L, 8L, 9L, 9L, 10L, 10L, 
                          11L, 11L, 12L, 12L), 
                        c(1L, 5L, 9L, 2L, 6L, 10L, 3L, 7L, 11L, 4L, 8L, 12L, 
                          1L, 5L, 2L, 6L, 3L, 7L, 4L, 8L, 1L, 9L, 2L, 10L, 
                          3L, 11L, 4L, 12L), 
                        12L, 12L,
                        sort.by.query = TRUE))
  expect_identical(findOverlaps(q4, q4, type = 'equal', select = 'first'), 
                   rep(1:4, 3))
  expect_identical(findOverlaps(q4, q4, type = 'equal', select = 'last'), 
                   c(9:12, 5:8, 9:12))
  expect_identical(findOverlaps(q4, q4, type = 'equal', select = 'arbitrary'), 
                   c(9:12, 5:8, 9:12))
  expect_identical(findOverlaps(q4, q4, type = 'equal', select = 'all', 
                                ignore.strand = TRUE), 
                   Hits(rep(1:12, each = 3), 
                        rep(c(1L, 5L, 9L, 2L, 6L, 10L, 3L, 7L, 11L, 
                              4L, 8L, 12L), times = 3), 
                        12L, 12L,
                        sort.by.query = TRUE))
  q4c <- q4
  seqinfo(q4c) <- Seqinfo('chr1', 100, isCircular = TRUE)
  expect_identical(findOverlaps(q4c, q4c, type = 'equal'), 
                   findOverlaps(q4c, shift(q4c, 100L), type = 'equal'))
  expect_error(findOverlaps(q4, q4, type = 'equal', minoverlap = 10L), 
               paste0("'minoverlap' must be 1 when 'type = equal', other ", 
                      "values not yet supported"))
  expect_error(findOverlaps(q4, q4, type = 'equal', maxgap = 10L), 
               paste0("'maxgap' must be 0 when 'type = equal', other ", 
                      "values not yet supported"))
})

test_that("Errors on different sized tuples with type = equal", {
  expect_error(findOverlaps(gt2, gt3, type = 'equal'), 
               paste0("Cannot findOverlaps between 'GTuples' and 'GTuples' ", 
                      "with 'type = \"equal\"' if they have different 'size'"))
})

context("findOverlaps,GTuples,GTuplesList-method")

test_that("Works on empty GTuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  hits <- findOverlaps(gt0, gtl0, type = "start")
  expect_true(inherits(hits, "Hits"))
  expect_identical(length(hits), 0L)
  expect_identical(findOverlaps(gt0, gtl0, type = 'any'), 
                   findOverlaps(gr0, grl0, 
                                type = 'any'))
  expect_identical(findOverlaps(gt0, gtl0, type = 'start'), 
                   findOverlaps(gr0, grl0, 
                                type = 'start'))
  expect_identical(findOverlaps(gt0, gtl0, type = 'end'), 
                   findOverlaps(gr0, grl0, 
                                type = 'end'))
  expect_identical(findOverlaps(gt0, gtl0, type = 'within'), 
                   findOverlaps(gr0, grl0, 
                                type = 'within'))
})

test_that("Works on 1-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  hits <- findOverlaps(gt1, gtl1, type = "start")
  expect_true(inherits(hits, "Hits"))
  expect_identical(length(hits), 2L)
  expect_identical(queryHits(hits), c(1L, 6L))
  expect_identical(subjectHits(hits), 1:2)
  expect_identical(findOverlaps(gt1, gtl1, type = 'any'), 
                   findOverlaps(gr1, grl1, 
                                type = 'any'))
  expect_identical(findOverlaps(gt1, gtl1, type = 'start'), 
                   findOverlaps(gr1, grl1, 
                                type = 'start'))
  expect_identical(findOverlaps(gt1, gtl1, type = 'end'), 
                   findOverlaps(gr1, grl1, 
                                type = 'end'))
  expect_identical(findOverlaps(gt1, gtl1, type = 'within'), 
                   findOverlaps(gr1, grl1, 
                                type = 'within'))
})

test_that("Works on 2-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  hits <- findOverlaps(gt2, gtl2, type = "start")
  expect_identical(length(hits), 2L)
  expect_identical(queryHits(hits), c(1L, 6L))
  expect_identical(subjectHits(hits), 1:2)
  expect_identical(findOverlaps(gt2, gtl2, type = 'any'), 
                   findOverlaps(gr2, grl2, 
                                type = 'any'))
  expect_identical(findOverlaps(gt2, gtl2, type = 'start'), 
                   findOverlaps(gr2, grl2, 
                                type = 'start'))
  expect_identical(findOverlaps(gt2, gtl2, type = 'end'), 
                   findOverlaps(gr2, grl2, 
                                type = 'end'))
  expect_identical(findOverlaps(gt2, gtl2, type = 'within'), 
                   findOverlaps(gr2, grl2, 
                                type = 'within'))
})

test_that("Works on 3-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  hits <- suppressWarnings(findOverlaps(gt3, gtl3, type = "start"))
  expect_identical(length(hits), 2L)
  expect_identical(queryHits(hits), c(1L, 6L))
  expect_identical(subjectHits(hits), 1:2)
  expect_identical(suppressWarnings(findOverlaps(gt3, gtl3, type = 'any')), 
                   findOverlaps(gr3, grl3, 
                                type = 'any'))
  expect_identical(suppressWarnings(findOverlaps(gt3, gtl3, type = 'start')), 
                   findOverlaps(gr3, grl3, 
                                type = 'start'))
  expect_identical(suppressWarnings(findOverlaps(gt3, gtl3, type = 'end')), 
                   findOverlaps(gr3, grl3, 
                                type = 'end'))
  expect_identical(suppressWarnings(findOverlaps(gt3, gtl3, type = 'within')), 
                   findOverlaps(gr3, grl3, 
                                type = 'within'))
})

test_that("Works on m-tuples, m > 3", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  hits <- suppressWarnings(findOverlaps(gt4, gtl4, type = "start"))
  expect_identical(length(hits), 2L)
  expect_identical(queryHits(hits), c(1L, 6L))
  expect_identical(subjectHits(hits), 1:2)
  expect_identical(suppressWarnings(findOverlaps(gt4, gtl4, type = 'any')), 
                   findOverlaps(gr4, grl4, 
                                type = 'any'))
  expect_identical(suppressWarnings(findOverlaps(gt4, gtl4, type = 'start')), 
                   findOverlaps(gr4, grl4, 
                                type = 'start'))
  expect_identical(suppressWarnings(findOverlaps(gt4, gtl4, type = 'end')), 
                   findOverlaps(gr4, grl4, 
                                type = 'end'))
  expect_identical(suppressWarnings(findOverlaps(gt4, gtl4, type = 'within')), 
                   findOverlaps(gr4, grl4, 
                                type = 'within'))
})

context("findOverlaps,GTuplesList,GTuples-method")

test_that("Works on empty GTuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  hits <- findOverlaps(gtl0, gt0, type = "start")
  expect_true(inherits(hits, "Hits"))
  expect_identical(length(hits), 0L)
  expect_identical(findOverlaps(gtl0, gt0, type = 'any'), 
                   findOverlaps(grl0, gr0, 
                                type = 'any'))
  expect_identical(findOverlaps(gtl0, gt0, type = 'start'), 
                   findOverlaps(grl0, gr0, 
                                type = 'start'))
  expect_identical(findOverlaps(gtl0, gt0, type = 'end'), 
                   findOverlaps(grl0, gr0, 
                                type = 'end'))
  expect_identical(findOverlaps(gtl0, gt0, type = 'within'), 
                   findOverlaps(grl0, gr0, 
                                type = 'within'))
})

test_that("Works on 1-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  hits <- findOverlaps(gtl1, gt1, type = "start")
  expect_true(inherits(hits, "Hits"))
  expect_identical(length(hits), 10L)
  expect_identical(queryHits(hits), rep(1:2, each = 5))
  expect_identical(subjectHits(hits), 1:10)
  expect_identical(findOverlaps(gtl1, gt1, type = 'any'), 
                   findOverlaps(grl1, gr1, 
                                type = 'any'))
  expect_identical(findOverlaps(gtl1, gt1, type = 'start'), 
                   findOverlaps(grl1, gr1, 
                                type = 'start'))
  expect_identical(findOverlaps(gtl1, gt1, type = 'end'), 
                   findOverlaps(grl1, gr1, 
                                type = 'end'))
  expect_identical(findOverlaps(gtl1, gt1, type = 'within'), 
                   findOverlaps(grl1, gr1, 
                                type = 'within'))
})

test_that("Works on 2-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  hits <- findOverlaps(gtl2, gt2, type = "start")
  expect_identical(length(hits), 10L)
  expect_identical(queryHits(hits), rep(1:2, each = 5))
  expect_identical(subjectHits(hits), 1:10)
  expect_identical(findOverlaps(gtl2, gt2, type = 'any'), 
                   findOverlaps(grl2, gr2, 
                                type = 'any'))
  expect_identical(findOverlaps(gtl2, gt2, type = 'start'), 
                   findOverlaps(grl2, gr2, 
                                type = 'start'))
  expect_identical(findOverlaps(gtl2, gt2, type = 'end'), 
                   findOverlaps(grl2, gr2, 
                                type = 'end'))
  expect_identical(findOverlaps(gtl2, gt2, type = 'within'), 
                   findOverlaps(grl2, gr2, 
                                type = 'within'))
})

test_that("Works on 3-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  hits <- suppressWarnings(findOverlaps(gtl3, gt3, type = "start"))
  expect_identical(length(hits), 10L)
  expect_identical(queryHits(hits), rep(1:2, each = 5))
  expect_identical(subjectHits(hits), 1:10)
  expect_identical(suppressWarnings(findOverlaps(gtl3, gt3, type = 'any')), 
                   findOverlaps(grl3, gr3, 
                                type = 'any'))
  expect_identical(suppressWarnings(findOverlaps(gtl3, gt3, type = 'start')), 
                   findOverlaps(grl3, gr3, 
                                type = 'start'))
  expect_identical(suppressWarnings(findOverlaps(gtl3, gt3, type = 'end')), 
                   findOverlaps(grl3, gr3, 
                                type = 'end'))
  expect_identical(suppressWarnings(findOverlaps(gtl3, gt3, type = 'within')), 
                   findOverlaps(grl3, gr3, 
                                type = 'within'))
})

test_that("Works on m-tuples, m > 3", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  hits <- suppressWarnings(findOverlaps(gtl4, gt4, type = "start"))
  expect_identical(length(hits), 10L)
  expect_identical(queryHits(hits), rep(1:2, each = 5))
  expect_identical(subjectHits(hits), 1:10)
  expect_identical(suppressWarnings(findOverlaps(gtl4, gt4, type = 'any')), 
                   findOverlaps(grl4, gr4, 
                                type = 'any'))
  expect_identical(suppressWarnings(findOverlaps(gtl4, gt4, type = 'start')), 
                   findOverlaps(grl4, gr4, 
                                type = 'start'))
  expect_identical(suppressWarnings(findOverlaps(gtl4, gt4, type = 'end')), 
                   findOverlaps(grl4, gr4, 
                                type = 'end'))
  expect_identical(suppressWarnings(findOverlaps(gtl4, gt4, type = 'within')), 
                   findOverlaps(grl4, gr4, 
                                type = 'within'))
})

context("findOverlaps,GTuplesList,GTuplesList-method")

test_that("Works on empty GTuples", {
  # Just need to test that output is the same as if the GTuplesList were 
  # GRangesList
  hits <- findOverlaps(gtl0, gtl0, type = "start")
  expect_true(inherits(hits, "Hits"))
  expect_identical(length(hits), 0L)
  expect_identical(findOverlaps(gtl0, gtl0, type = 'any'), 
                   findOverlaps(grl0, 
                                grl0, 
                                type = 'any'))
  expect_identical(findOverlaps(gtl0, gtl0, type = 'start'), 
                   findOverlaps(grl0, 
                                grl0, 
                                type = 'start'))
  expect_identical(findOverlaps(gtl0, gtl0, type = 'end'), 
                   findOverlaps(grl0, 
                                grl0, 
                                type = 'end'))
  expect_identical(findOverlaps(gtl0, gtl0, type = 'within'), 
                   findOverlaps(grl0, 
                                grl0, 
                                type = 'within'))
})

test_that("Works on 1-tuples", {
  # Just need to test that output is the same as if the GTuplesList were 
  # GRangesList
  hits <- suppressWarnings(findOverlaps(gtl1, gtl1, type = "start"))
  expect_true(inherits(hits, "Hits"))
  expect_identical(length(hits), 2L)
  expect_identical(queryHits(hits), c(1L, 2L))
  expect_identical(subjectHits(hits), c(1L, 2L))
  expect_identical(findOverlaps(gtl1, gtl1, type = 'any'), 
                   findOverlaps(grl1, 
                                grl1, 
                                type = 'any'))
  expect_identical(findOverlaps(gtl1, gtl1, type = 'start'), 
                   findOverlaps(grl1, 
                                grl1, 
                                type = 'start'))
  expect_identical(findOverlaps(gtl1, gtl1, type = 'end'), 
                   findOverlaps(grl1, 
                                grl1, 
                                type = 'end'))
  expect_identical(findOverlaps(gtl1, gtl1, type = 'within'), 
                   findOverlaps(grl1, 
                                grl1, 
                                type = 'within'))
})

test_that("Works on 2-tuples", {
  # Just need to test that output is the same as if the GTuplesList were 
  # GRangesList
  hits <- suppressWarnings(findOverlaps(gtl2, gtl2, type = "start"))
  expect_identical(length(hits), 2L)
  expect_identical(queryHits(hits), c(1L, 2L))
  expect_identical(subjectHits(hits), c(1L, 2L))
  expect_identical(findOverlaps(gtl2, gtl2, type = 'any'), 
                   findOverlaps(grl2, 
                                grl2, 
                                type = 'any'))
  expect_identical(findOverlaps(gtl2, gtl2, type = 'start'), 
                   findOverlaps(grl2, 
                                grl2, 
                                type = 'start'))
  expect_identical(findOverlaps(gtl2, gtl2, type = 'end'), 
                   findOverlaps(grl2, 
                                grl2, 
                                type = 'end'))
  expect_identical(findOverlaps(gtl2, gtl2, type = 'within'), 
                   findOverlaps(grl2, 
                                grl2, 
                                type = 'within'))
})

test_that("Works on 3-tuples", {
  # Just need to test that output is the same as if the GTuplesList were 
  # GRangesList
  hits <- suppressWarnings(findOverlaps(gtl3, gtl3, type = "start"))
  expect_identical(length(hits), 2L)
  expect_identical(queryHits(hits), c(1L, 2L))
  expect_identical(subjectHits(hits), c(1L, 2L))
  expect_identical(suppressWarnings(findOverlaps(gtl3, gtl3, type = 'any')), 
                   findOverlaps(grl3, 
                                grl3, 
                                type = 'any'))
  expect_identical(suppressWarnings(findOverlaps(gtl3, gtl3, type = 'start')), 
                   findOverlaps(grl3, 
                                grl3, 
                                type = 'start'))
  expect_identical(suppressWarnings(findOverlaps(gtl3, gtl3, type = 'end')), 
                   findOverlaps(grl3, 
                                grl3, 
                                type = 'end'))
  expect_identical(suppressWarnings(findOverlaps(gtl3, gtl3, type = 'within')), 
                   findOverlaps(grl3, 
                                grl3, 
                                type = 'within'))
})

test_that("Works on m-tuples, m > 3", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  hits <- suppressWarnings(findOverlaps(gtl4, gtl4, type = "start"))
  expect_identical(length(hits), 2L)
  expect_identical(queryHits(hits), c(1L, 2L))
  expect_identical(subjectHits(hits), c(1L, 2L))
  expect_identical(suppressWarnings(findOverlaps(gtl4, gtl4, type = 'any')), 
                   findOverlaps(grl4, 
                                grl4, 
                                type = 'any'))
  expect_identical(suppressWarnings(findOverlaps(gtl4, gtl4, type = 'start')), 
                   findOverlaps(grl4, 
                                grl4, 
                                type = 'start'))
  expect_identical(suppressWarnings(findOverlaps(gtl4, gtl4, type = 'end')), 
                   findOverlaps(grl4, 
                                grl4, 
                                type = 'end'))
  expect_identical(suppressWarnings(findOverlaps(gtl4, gtl4, type = 'within')), 
                   findOverlaps(grl4, 
                                grl4, 
                                type = 'within'))
})

context("countOverlaps,GTuples,GTuples-method")

test_that("Works on empty GTuples", {
 # Just need to test that output is the same as if the GTuples were GRanges
 expect_identical(countOverlaps(gt0, gt0, type = 'any'), 
                  countOverlaps(gr0, gr0, type = 'any'))
 expect_identical(countOverlaps(gt0, gt0, type = 'start'), 
                  countOverlaps(gr0, gr0, type = 'start'))
 expect_identical(countOverlaps(gt0, gt0, type = 'end'), 
                  countOverlaps(gr0, gr0, type = 'end'))
 expect_identical(countOverlaps(gt0, gt0, type = 'within'), 
                  countOverlaps(gr0, gr0, type = 'within'))
 expect_identical(countOverlaps(gt0, gt0, type = 'equal'), 
                  countOverlaps(gr0, gr0, type = 'equal'))
})

test_that("Works on 1-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(countOverlaps(gt1, gt1, type = 'any'), 
                   countOverlaps(gr1, gr1, 
                                 type = 'any'))
  expect_identical(countOverlaps(gt1, gt1, type = 'start'), 
                   countOverlaps(gr1, gr1, 
                                 type = 'start'))
  expect_identical(countOverlaps(gt1, gt1, type = 'end'), 
                   countOverlaps(gr1, gr1, 
                                 type = 'end'))
  expect_identical(countOverlaps(gt1, gt1, type = 'within'), 
                   countOverlaps(gr1, gr1, 
                                 type = 'within'))
  expect_identical(countOverlaps(gt1, gt1, type = 'equal'), 
                   countOverlaps(gr1, gr1, 
                                 type = 'equal'))
})

test_that("Works on 2-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(countOverlaps(gt2, gt2, type = 'any'), 
                   countOverlaps(gr2, gr2, 
                                 type = 'any'))
  expect_identical(countOverlaps(gt2, gt2, type = 'start'), 
                   countOverlaps(gr2, gr2, 
                                 type = 'start'))
  expect_identical(countOverlaps(gt2, gt2, type = 'end'), 
                   countOverlaps(gr2, gr2, 
                                 type = 'end'))
  expect_identical(countOverlaps(gt2, gt2, type = 'within'), 
                   countOverlaps(gr2, gr2, 
                                 type = 'within'))
  expect_identical(countOverlaps(gt2, gt2, type = 'equal'), 
                   countOverlaps(gr2, gr2, 
                                 type = 'equal'))
})

test_that("Works on 3-tuples", {
  # More complicated than 1- or 2-tuples. 
  # Specifically, need specialised tests if type = 'equal'
  expect_identical(suppressWarnings(countOverlaps(gt3, gt3, type = 'any')), 
                   countOverlaps(gr3, gr3, type = 'any'))
  expect_identical(suppressWarnings(countOverlaps(gt3, gt3, type = 'start')), 
                   countOverlaps(gr3, gr3, type = 'start'))
  expect_identical(suppressWarnings(countOverlaps(gt3, gt3, type = 'end')), 
                   countOverlaps(gr3, gr3, type = 'end'))
  expect_identical(suppressWarnings(countOverlaps(gt3, gt3, type = 'within')), 
                   countOverlaps(gr3, gr3, type = 'within'))
  expect_identical(countOverlaps(q3, q3, type = 'equal'), 
                   c(rep(3L, 3), rep(2L, 6)))
  expect_identical(countOverlaps(q3, q3, type = 'equal', ignore.strand = TRUE), 
                   rep(3L, 9))
})

test_that("Works on m-tuples, m > 3", {
  # More complicated than 1- or 2-tuples. 
  # Specifically, need specialised tests if type = 'equal'
  expect_identical(suppressWarnings(countOverlaps(gt4, gt4, type = 'any')), 
                   countOverlaps(gr4, gr4, type = 'any'))
  expect_identical(suppressWarnings(countOverlaps(gt4, gt4, type = 'start')), 
                   countOverlaps(gr4, gr4, type = 'start'))
  expect_identical(suppressWarnings(countOverlaps(gt4, gt4, type = 'end')), 
                   countOverlaps(gr4, gr4, type = 'end'))
  expect_identical(suppressWarnings(countOverlaps(gt4, gt4, type = 'within')), 
                   countOverlaps(gr4, gr4, type = 'within'))
  expect_identical(countOverlaps(q4, q4, type = 'equal'), 
                   c(rep(3L, 4), rep(2L, 8)))
  expect_identical(countOverlaps(q4, q4, type = 'equal', ignore.strand = TRUE), 
                   rep(3L, 12))
})

context("countOverlaps,GTuples,GTuplesList-method")

test_that("Works on empty GTuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(countOverlaps(gt0, gtl0, type = 'any'), 
                   countOverlaps(gr0, grl0, type = 'any'))
  expect_identical(countOverlaps(gt0, gtl0, type = 'start'), 
                   countOverlaps(gr0, grl0, type = 'start'))
  expect_identical(countOverlaps(gt0, gtl0, type = 'end'), 
                   countOverlaps(gr0, grl0, type = 'end'))
  expect_identical(countOverlaps(gt0, gtl0, type = 'within'), 
                   countOverlaps(gr0, grl0, type = 'within'))
})

test_that("Works on 1-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(countOverlaps(gt1, gtl1, type = 'any'), 
                   countOverlaps(gr1, grl1, type = 'any'))
  expect_identical(countOverlaps(gt1, gtl1, type = 'start'), 
                   countOverlaps(gr1, grl1, type = 'start'))
  expect_identical(countOverlaps(gt1, gtl1, type = 'end'), 
                   countOverlaps(gr1, grl1, type = 'end'))
  expect_identical(countOverlaps(gt1, gtl1, type = 'within'), 
                   countOverlaps(gr1, grl1, type = 'within'))
})

test_that("Works on 2-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(countOverlaps(gt2, gtl2, type = 'any'), 
                   countOverlaps(gr2, grl2, type = 'any'))
  expect_identical(countOverlaps(gt2, gtl2, type = 'start'), 
                   countOverlaps(gr2, grl2, type = 'start'))
  expect_identical(countOverlaps(gt2, gtl2, type = 'end'), 
                   countOverlaps(gr2, grl2, type = 'end'))
  expect_identical(countOverlaps(gt2, gtl2, type = 'within'), 
                   countOverlaps(gr2, grl2, type = 'within'))
})

test_that("Works on 3-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(suppressWarnings(countOverlaps(gt3, gtl3, type = 'any')), 
                   countOverlaps(gr3, grl3, type = 'any'))
  expect_identical(suppressWarnings(countOverlaps(gt3, gtl3, type = 'start')), 
                   countOverlaps(gr3, grl3, type = 'start'))
  expect_identical(suppressWarnings(countOverlaps(gt3, gtl3, type = 'end')), 
                   countOverlaps(gr3, grl3, type = 'end'))
  expect_identical(suppressWarnings(countOverlaps(gt3, gtl3, type = 'within')), 
                   countOverlaps(gr3, grl3, type = 'within'))
})

test_that("Works on m-tuples, m > 3", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(suppressWarnings(countOverlaps(gt4, gtl4, type = 'any')), 
                   countOverlaps(gr4, grl4, type = 'any'))
  expect_identical(suppressWarnings(countOverlaps(gt4, gtl4, type = 'start')), 
                   countOverlaps(gr4, grl4, type = 'start'))
  expect_identical(suppressWarnings(countOverlaps(gt4, gtl4, type = 'end')), 
                   countOverlaps(gr4, grl4, type = 'end'))
  expect_identical(suppressWarnings(countOverlaps(gt4, gtl4, type = 'within')), 
                   countOverlaps(gr4, grl4, type = 'within'))
})

context("countOverlaps,GTuplesList,GTuples-method")

test_that("Works on empty GTuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(countOverlaps(gtl0, gt0, type = 'any'), 
                   countOverlaps(grl0, gr0, type = 'any'))
  expect_identical(countOverlaps(gtl0, gt0, type = 'start'), 
                   countOverlaps(grl0, gr0, type = 'start'))
  expect_identical(countOverlaps(gtl0, gt0, type = 'end'), 
                   countOverlaps(grl0, gr0, type = 'end'))
  expect_identical(countOverlaps(gtl0, gt0, type = 'within'), 
                   countOverlaps(grl0, gr0, type = 'within'))
})

test_that("Works on 1-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(countOverlaps(gtl1, gt1, type = 'any'), 
                   countOverlaps(grl1, gr1, type = 'any'))
  expect_identical(countOverlaps(gtl1, gt1, type = 'start'), 
                   countOverlaps(grl1, gr1, type = 'start'))
  expect_identical(countOverlaps(gtl1, gt1, type = 'end'), 
                   countOverlaps(grl1, gr1, type = 'end'))
  expect_identical(countOverlaps(gtl1, gt1, type = 'within'), 
                   countOverlaps(grl1, gr1, type = 'within'))
})

test_that("Works on 2-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(countOverlaps(gtl2, gt2, type = 'any'), 
                   countOverlaps(grl2, gr2, type = 'any'))
  expect_identical(countOverlaps(gtl2, gt2, type = 'start'), 
                   countOverlaps(grl2, gr2, type = 'start'))
  expect_identical(countOverlaps(gtl2, gt2, type = 'end'), 
                   countOverlaps(grl2, gr2, type = 'end'))
  expect_identical(countOverlaps(gtl2, gt2, type = 'within'), 
                   countOverlaps(grl2, gr2, type = 'within'))
})

test_that("Works on 3-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(suppressWarnings(countOverlaps(gtl3, gt3, type = 'any')), 
                   countOverlaps(grl3, gr3, type = 'any'))
  expect_identical(suppressWarnings(countOverlaps(gtl3, gt3, type = 'start')), 
                   countOverlaps(grl3, gr3, type = 'start'))
  expect_identical(suppressWarnings(countOverlaps(gtl3, gt3, type = 'end')), 
                   countOverlaps(grl3, gr3, type = 'end'))
  expect_identical(suppressWarnings(countOverlaps(gtl3, gt3, type = 'within')), 
                   countOverlaps(grl3, gr3, type = 'within'))
})

test_that("Works on m-tuples, m > 3", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(suppressWarnings(countOverlaps(gtl4, gt4, type = 'any')), 
                   countOverlaps(grl4, gr4, type = 'any'))
  expect_identical(suppressWarnings(countOverlaps(gtl4, gt4, type = 'start')), 
                   countOverlaps(grl4, gr4, type = 'start'))
  expect_identical(suppressWarnings(countOverlaps(gtl4, gt4, type = 'end')), 
                   countOverlaps(grl4, gr4, type = 'end'))
  expect_identical(suppressWarnings(countOverlaps(gtl4, gt4, type = 'within')), 
                   countOverlaps(grl4, gr4, type = 'within'))
})

context("countOverlaps,GTuplesList,GTuplesList-method")

test_that("Works on empty GTuples", {
  # Just need to test that output is the same as if the GTuplesList were 
  # GRangesList 
  expect_identical(countOverlaps(gtl0, gtl0, type = 'any'), 
                   countOverlaps(grl0, grl0, type = 'any'))
  expect_identical(countOverlaps(gtl0, gtl0, type = 'start'), 
                   countOverlaps(grl0, grl0, type = 'start'))
  expect_identical(countOverlaps(gtl0, gtl0, type = 'end'), 
                   countOverlaps(grl0, grl0, type = 'end'))
  expect_identical(countOverlaps(gtl0, gtl0, type = 'within'), 
                   countOverlaps(grl0, grl0, type = 'within'))
})

test_that("Works on 1-tuples", {
  # Just need to test that output is the same as if the GTuplesList were 
  # GRangesList 
  expect_identical(countOverlaps(gtl1, gtl1, type = 'any'), 
                   countOverlaps(grl1, grl1, type = 'any'))
  expect_identical(countOverlaps(gtl1, gtl1, type = 'start'), 
                   countOverlaps(grl1, grl1, type = 'start'))
  expect_identical(countOverlaps(gtl1, gtl1, type = 'end'), 
                   countOverlaps(grl1, grl1, type = 'end'))
  expect_identical(countOverlaps(gtl1, gtl1, type = 'within'), 
                   countOverlaps(grl1, grl1, type = 'within'))
})

test_that("Works on 2-tuples", {
  # Just need to test that output is the same as if the GTuplesList were 
  # GRangesList 
  expect_identical(countOverlaps(gtl2, gtl2, type = 'any'), 
                   countOverlaps(grl2, grl2, type = 'any'))
  expect_identical(countOverlaps(gtl2, gtl2, type = 'start'), 
                   countOverlaps(grl2, grl2, type = 'start'))
  expect_identical(countOverlaps(gtl2, gtl2, type = 'end'), 
                   countOverlaps(grl2, grl2, type = 'end'))
  expect_identical(countOverlaps(gtl2, gtl2, type = 'within'), 
                   countOverlaps(grl2, grl2, type = 'within'))
})

test_that("Works on 3-tuples", {
  # Just need to test that output is the same as if the GTuplesList were 
  # GRangesList 
  expect_identical(suppressWarnings(countOverlaps(gtl3, gtl3, type = 'any')), 
                   countOverlaps(grl3, grl3, type = 'any'))
  expect_identical(suppressWarnings(countOverlaps(gtl3, gtl3, type = 'start')), 
                   countOverlaps(grl3, grl3, type = 'start'))
  expect_identical(suppressWarnings(countOverlaps(gtl3, gtl3, type = 'end')), 
                   countOverlaps(grl3, grl3, type = 'end'))
  expect_identical(suppressWarnings(countOverlaps(gtl3, gtl3, type = 'within')), 
                   countOverlaps(grl3, grl3, type = 'within'))
})

test_that("Works on m-tuples, m > 3", {
  # Just need to test that output is the same as if the GTuplesList were 
  # GRangesList 
  expect_identical(suppressWarnings(countOverlaps(gtl4, gtl4, type = 'any')), 
                   countOverlaps(grl4, grl4, type = 'any'))
  expect_identical(suppressWarnings(countOverlaps(gtl4, gtl4, type = 'start')), 
                   countOverlaps(grl4, grl4, type = 'start'))
  expect_identical(suppressWarnings(countOverlaps(gtl4, gtl4, type = 'end')), 
                   countOverlaps(grl4, grl4, type = 'end'))
  expect_identical(suppressWarnings(countOverlaps(gtl4, gtl4, type = 'within')), 
                   countOverlaps(grl4, grl4, type = 'within'))
})


context("overlapsAny,GTuples,GTuples-method")

test_that("Works on empty GTuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(overlapsAny(gt0, gt0, type = 'any'), 
                   overlapsAny(gr0, gr0, type = 'any'))
  expect_identical(overlapsAny(gt0, gt0, type = 'start'), 
                   overlapsAny(gr0, gr0, type = 'start'))
  expect_identical(overlapsAny(gt0, gt0, type = 'end'), 
                   overlapsAny(gr0, gr0, type = 'end'))
  expect_identical(overlapsAny(gt0, gt0, type = 'within'), 
                   overlapsAny(gr0, gr0, type = 'within'))
  expect_identical(overlapsAny(gt0, gt0, type = 'equal'), 
                   overlapsAny(gr0, gr0, type = 'equal'))
})

test_that("Works on 1-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(overlapsAny(gt1, gt1, type = 'any'), 
                   overlapsAny(gr1, gr1, type = 'any'))
  expect_identical(overlapsAny(gt1, gt1, type = 'start'), 
                   overlapsAny(gr1, gr1, type = 'start'))
  expect_identical(overlapsAny(gt1, gt1, type = 'end'), 
                   overlapsAny(gr1, gr1, type = 'end'))
  expect_identical(overlapsAny(gt1, gt1, type = 'within'), 
                   overlapsAny(gr1, gr1, type = 'within'))
  expect_identical(overlapsAny(gt1, gt1, type = 'equal'), 
                   overlapsAny(gr1, gr1, type = 'equal'))
})

test_that("Works on 2-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(overlapsAny(gt2, gt2, type = 'any'), 
                   overlapsAny(gr2, gr2, type = 'any'))
  expect_identical(overlapsAny(gt2, gt2, type = 'start'), 
                   overlapsAny(gr2, gr2, type = 'start'))
  expect_identical(overlapsAny(gt2, gt2, type = 'end'), 
                   overlapsAny(gr2, gr2, type = 'end'))
  expect_identical(overlapsAny(gt2, gt2, type = 'within'), 
                   overlapsAny(gr2, gr2, type = 'within'))
  expect_identical(overlapsAny(gt2, gt2, type = 'equal'), 
                   overlapsAny(gr2, gr2, type = 'equal'))
})

test_that("Works on 3-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(suppressWarnings(overlapsAny(gt3, gt3, type = 'any')), 
                   overlapsAny(gr3, gr3, type = 'any'))
  expect_identical(suppressWarnings(overlapsAny(gt3, gt3, type = 'start')), 
                   overlapsAny(gr3, gr3, type = 'start'))
  expect_identical(suppressWarnings(overlapsAny(gt3, gt3, type = 'end')), 
                   overlapsAny(gr3, gr3, type = 'end'))
  expect_identical(suppressWarnings(overlapsAny(gt3, gt3, type = 'within')), 
                   overlapsAny(gr3, gr3, type = 'within'))
  expect_identical(overlapsAny(q3, q3, type = 'equal'), 
                   rep(TRUE, 9))
  expect_identical(overlapsAny(q3, q3, type = 'equal', ignore.strand = TRUE), 
                   rep(TRUE, 9))
})

test_that("Works on m-tuples, m > 3", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(suppressWarnings(overlapsAny(gt4, gt4, type = 'any')), 
                   overlapsAny(gr4, gr4, type = 'any'))
  expect_identical(suppressWarnings(overlapsAny(gt4, gt4, type = 'start')), 
                   overlapsAny(gr4, gr4, type = 'start'))
  expect_identical(suppressWarnings(overlapsAny(gt4, gt4, type = 'end')), 
                   overlapsAny(gr4, gr4, type = 'end'))
  expect_identical(suppressWarnings(overlapsAny(gt4, gt4, type = 'within')), 
                   overlapsAny(gr4, gr4, type = 'within'))
  expect_identical(overlapsAny(q4, q4, type = 'equal'), 
                   rep(TRUE, 12))
  expect_identical(overlapsAny(q4, q4, type = 'equal', ignore.strand = TRUE), 
                   rep(TRUE, 12))
})

context("overlapsAny,GTuples,GTuplesList-method")

test_that("Works on empty GTuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(overlapsAny(gt0, gtl0, type = 'any'), 
                   overlapsAny(gr0, grl0, type = 'any'))
  expect_identical(overlapsAny(gt0, gtl0, type = 'start'), 
                   overlapsAny(gr0, grl0, type = 'start'))
  expect_identical(overlapsAny(gt0, gtl0, type = 'end'), 
                   overlapsAny(gr0, grl0, type = 'end'))
  expect_identical(overlapsAny(gt0, gtl0, type = 'within'), 
                   overlapsAny(gr0, grl0, type = 'within'))
})

test_that("Works on 1-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(overlapsAny(gt1, gtl1, type = 'any'), 
                   overlapsAny(gr1, grl1, type = 'any'))
  expect_identical(overlapsAny(gt1, gtl1, type = 'start'), 
                   overlapsAny(gr1, grl1, type = 'start'))
  expect_identical(overlapsAny(gt1, gtl1, type = 'end'), 
                   overlapsAny(gr1, grl1, type = 'end'))
  expect_identical(overlapsAny(gt1, gtl1, type = 'within'), 
                   overlapsAny(gr1, grl1, type = 'within'))
})

test_that("Works on 2-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(overlapsAny(gt2, gtl2, type = 'any'), 
                   overlapsAny(gr2, grl2, type = 'any'))
  expect_identical(overlapsAny(gt2, gtl2, type = 'start'), 
                   overlapsAny(gr2, grl2, type = 'start'))
  expect_identical(overlapsAny(gt2, gtl2, type = 'end'), 
                   overlapsAny(gr2, grl2, type = 'end'))
  expect_identical(overlapsAny(gt2, gtl2, type = 'within'), 
                   overlapsAny(gr2, grl2, type = 'within'))
})

test_that("Works on 3-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(suppressWarnings(overlapsAny(gt3, gtl3, type = 'any')), 
                   overlapsAny(gr3, grl3, type = 'any'))
  expect_identical(suppressWarnings(overlapsAny(gt3, gtl3, type = 'start')), 
                   overlapsAny(gr3, grl3, type = 'start'))
  expect_identical(suppressWarnings(overlapsAny(gt3, gtl3, type = 'end')), 
                   overlapsAny(gr3, grl3, type = 'end'))
  expect_identical(suppressWarnings(overlapsAny(gt3, gtl3, type = 'within')), 
                   overlapsAny(gr3, grl3, type = 'within'))
})

test_that("Works on m-tuples, m > 3", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(suppressWarnings(overlapsAny(gt4, gtl4, type = 'any')), 
                   overlapsAny(gr4, grl4, type = 'any'))
  expect_identical(suppressWarnings(overlapsAny(gt4, gtl4, type = 'start')), 
                   overlapsAny(gr4, grl4, type = 'start'))
  expect_identical(suppressWarnings(overlapsAny(gt4, gtl4, type = 'end')), 
                   overlapsAny(gr4, grl4, type = 'end'))
  expect_identical(suppressWarnings(overlapsAny(gt4, gtl4, type = 'within')), 
                   overlapsAny(gr4, grl4, type = 'within'))
})

context("overlapsAny,GTuplesList,GTuples-method")

test_that("Works on empty GTuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(overlapsAny(gtl0, gt0, type = 'any'), 
                   overlapsAny(grl0, gr0, type = 'any'))
  expect_identical(overlapsAny(gtl0, gt0, type = 'start'), 
                   overlapsAny(grl0, gr0, type = 'start'))
  expect_identical(overlapsAny(gtl0, gt0, type = 'end'), 
                   overlapsAny(grl0, gr0, type = 'end'))
  expect_identical(overlapsAny(gtl0, gt0, type = 'within'), 
                   overlapsAny(grl0, gr0, type = 'within'))
})

test_that("Works on 1-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(overlapsAny(gtl1, gt1, type = 'any'), 
                   overlapsAny(grl1, gr1, type = 'any'))
  expect_identical(overlapsAny(gtl1, gt1, type = 'start'), 
                   overlapsAny(grl1, gr1, type = 'start'))
  expect_identical(overlapsAny(gtl1, gt1, type = 'end'), 
                   overlapsAny(grl1, gr1, type = 'end'))
  expect_identical(overlapsAny(gtl1, gt1, type = 'within'), 
                   overlapsAny(grl1, gr1, type = 'within'))
})

test_that("Works on 2-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(overlapsAny(gtl2, gt2, type = 'any'), 
                   overlapsAny(grl2, gr2, type = 'any'))
  expect_identical(overlapsAny(gtl2, gt2, type = 'start'), 
                   overlapsAny(grl2, gr2, type = 'start'))
  expect_identical(overlapsAny(gtl2, gt2, type = 'end'), 
                   overlapsAny(grl2, gr2, type = 'end'))
  expect_identical(overlapsAny(gtl2, gt2, type = 'within'), 
                   overlapsAny(grl2, gr2, type = 'within'))
})

test_that("Works on 3-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(suppressWarnings(overlapsAny(gtl3, gt3, type = 'any')), 
                   overlapsAny(grl3, gr3, type = 'any'))
  expect_identical(suppressWarnings(overlapsAny(gtl3, gt3, type = 'start')), 
                   overlapsAny(grl3, gr3, type = 'start'))
  expect_identical(suppressWarnings(overlapsAny(gtl3, gt3, type = 'end')), 
                   overlapsAny(grl3, gr3, type = 'end'))
  expect_identical(suppressWarnings(overlapsAny(gtl3, gt3, type = 'within')), 
                   overlapsAny(grl3, gr3, type = 'within'))
})

test_that("Works on m-tuples, m > 3", {
  # Just need to test that output is the same as if the GTuples were GRanges 
  # and GTuplesList were GRangesList
  expect_identical(suppressWarnings(overlapsAny(gtl4, gt4, type = 'any')), 
                   overlapsAny(grl4, gr4, type = 'any'))
  expect_identical(suppressWarnings(overlapsAny(gtl4, gt4, type = 'start')), 
                   overlapsAny(grl4, gr4, type = 'start'))
  expect_identical(suppressWarnings(overlapsAny(gtl4, gt4, type = 'end')), 
                   overlapsAny(grl4, gr4, type = 'end'))
  expect_identical(suppressWarnings(overlapsAny(gtl4, gt4, type = 'within')), 
                   overlapsAny(grl4, gr4, type = 'within'))
})

context("overlapsAny,GTuplesList,GTuplesList-method")

test_that("Works on empty GTuples", {
  # Just need to test that output is the same as if the GTuplesList were 
  # GRangesList 
  expect_identical(overlapsAny(gtl0, gtl0, type = 'any'), 
                   overlapsAny(grl0, grl0, type = 'any'))
  expect_identical(overlapsAny(gtl0, gtl0, type = 'start'), 
                   overlapsAny(grl0, grl0, type = 'start'))
  expect_identical(overlapsAny(gtl0, gtl0, type = 'end'), 
                   overlapsAny(grl0, grl0, type = 'end'))
  expect_identical(overlapsAny(gtl0, gtl0, type = 'within'), 
                   overlapsAny(grl0, grl0, type = 'within'))
})

test_that("Works on 1-tuples", {
  # Just need to test that output is the same as if the GTuplesList were 
  # GRangesList 
  expect_identical(overlapsAny(gtl1, gtl1, type = 'any'), 
                   overlapsAny(grl1, grl1, type = 'any'))
  expect_identical(overlapsAny(gtl1, gtl1, type = 'start'), 
                   overlapsAny(grl1, grl1, type = 'start'))
  expect_identical(overlapsAny(gtl1, gtl1, type = 'end'), 
                   overlapsAny(grl1, grl1, type = 'end'))
  expect_identical(overlapsAny(gtl1, gtl1, type = 'within'), 
                   overlapsAny(grl1, grl1, type = 'within'))
})

test_that("Works on 2-tuples", {
  # Just need to test that output is the same as if the GTuplesList were 
  # GRangesList 
  expect_identical(overlapsAny(gtl2, gtl2, type = 'any'), 
                   overlapsAny(grl2, grl2, type = 'any'))
  expect_identical(overlapsAny(gtl2, gtl2, type = 'start'), 
                   overlapsAny(grl2, grl2, type = 'start'))
  expect_identical(overlapsAny(gtl2, gtl2, type = 'end'), 
                   overlapsAny(grl2, grl2, type = 'end'))
  expect_identical(overlapsAny(gtl2, gtl2, type = 'within'), 
                   overlapsAny(grl2, grl2, type = 'within'))
})

test_that("Works on 3-tuples", {
  # Just need to test that output is the same as if the GTuplesList were 
  # GRangesList 
  expect_identical(suppressWarnings(overlapsAny(gtl3, gtl3, type = 'any')), 
                   overlapsAny(grl3, grl3, type = 'any'))
  expect_identical(suppressWarnings(overlapsAny(gtl3, gtl3, type = 'start')), 
                   overlapsAny(grl3, grl3, type = 'start'))
  expect_identical(suppressWarnings(overlapsAny(gtl3, gtl3, type = 'end')), 
                   overlapsAny(grl3, grl3, type = 'end'))
  expect_identical(suppressWarnings(overlapsAny(gtl3, gtl3, type = 'within')), 
                   overlapsAny(grl3, grl3, type = 'within'))
})

test_that("Works on m-tuples, m > 3", {
  # Just need to test that output is the same as if the GTuplesList were 
  # GRangesList 
  expect_identical(suppressWarnings(overlapsAny(gtl4, gtl4, type = 'any')), 
                   overlapsAny(grl4, grl4, type = 'any'))
  expect_identical(suppressWarnings(overlapsAny(gtl4, gtl4, type = 'start')), 
                   overlapsAny(grl4, grl4, type = 'start'))
  expect_identical(suppressWarnings(overlapsAny(gtl4, gtl4, type = 'end')), 
                   overlapsAny(grl4, grl4, type = 'end'))
  expect_identical(suppressWarnings(overlapsAny(gtl4, gtl4, type = 'within')), 
                   overlapsAny(grl4, grl4, type = 'within'))
})

context("subsetByOverlaps,GTuples,GTuples-method")
test_that("Works on empty GTuples", {
  expect_identical(subsetByOverlaps(gt0, gt0, type = 'any'), 
                   gt0)
  expect_identical(subsetByOverlaps(gt0, gt0, type = 'start'), 
                   gt0)
  expect_identical(subsetByOverlaps(gt0, gt0, type = 'end'), 
                   gt0)
  expect_identical(subsetByOverlaps(gt0, gt0, type = 'within'), 
                   gt0)
  expect_identical(subsetByOverlaps(gt0, gt0, type = 'equal'), 
                   gt0)
})

test_that("Works on 1-tuples", {
  expect_identical(subsetByOverlaps(gt1, gt1[1:3], type = 'any'), 
                   gt1[1:3])
  expect_identical(subsetByOverlaps(gt1, gt1[1:3], type = 'start'), 
                   gt1[1:3])
  expect_identical(subsetByOverlaps(gt1, gt1[1:3], type = 'end'), 
                   gt1[1:3])
  expect_identical(subsetByOverlaps(gt1, gt1[1:3], type = 'within'), 
                   gt1[1:3])
  expect_identical(subsetByOverlaps(gt1, gt1[1:3], type = 'equal'), 
                   gt1[1:3])
})

test_that("Works on 2-tuples", {
  expect_identical(subsetByOverlaps(gt2, gt2[1:3], type = 'any'), 
                   gt2[1:4])
  expect_identical(subsetByOverlaps(gt2, gt2[1:3], type = 'start'), 
                   gt2[1:3])
  expect_identical(subsetByOverlaps(gt2, gt2[1:3], type = 'end'), 
                   gt2[1:3])
  expect_identical(subsetByOverlaps(gt2, gt2[1:3], type = 'within'), 
                   gt2[1:3])
  expect_identical(subsetByOverlaps(gt2, gt2[1:3], type = 'equal'), 
                   gt2[1:3])
})

test_that("Works on 3-tuples", {
  expect_identical(suppressWarnings(subsetByOverlaps(gt3, gt3[1:3], type = 'any')), 
                   gt3[1:4])
  expect_identical(suppressWarnings(subsetByOverlaps(gt3, gt3[1:3], type = 'start')), 
                   gt3[1:3])
  expect_identical(suppressWarnings(subsetByOverlaps(gt3, gt3[1:3], type = 'end')), 
                   gt3[1:3])
  expect_identical(suppressWarnings(subsetByOverlaps(gt3, gt3[1:3], type = 'within')), 
                   gt3[1:3])
  expect_identical(subsetByOverlaps(q3, q3[4:6], type = 'equal'), 
                   q3[1:6])
  expect_identical(subsetByOverlaps(q3, q3[4:6], type = 'equal', 
                                    ignore.strand = TRUE), 
                   q3)
})

test_that("Works on m-tuples, m > 3", {
  expect_identical(suppressWarnings(subsetByOverlaps(gt4, gt4[1:3], type = 'any')), 
                   gt4[1:4])
  expect_identical(suppressWarnings(subsetByOverlaps(gt4, gt4[1:3], type = 'start')), 
                   gt4[1:3])
  expect_identical(suppressWarnings(subsetByOverlaps(gt4, gt4[1:3], type = 'end')), 
                   gt4[1:3])
  expect_identical(suppressWarnings(subsetByOverlaps(gt4, gt4[1:3], type = 'within')), 
                   gt4[1:3])
  expect_identical(subsetByOverlaps(q4, q4[5:8], type = 'equal'), 
                   q4[1:8])
  expect_identical(subsetByOverlaps(q4, q4[5:8], type = 'equal', 
                                    ignore.strand = TRUE), 
                   q4)
})

context("subsetByOverlaps,GTuples,GTuplesList-method")

test_that("Works on empty GTuples", {
  expect_identical(subsetByOverlaps(gt0, gtl0, type = 'any'), 
                   gt0)
  expect_identical(subsetByOverlaps(gt0, gtl0, type = 'start'), 
                   gt0)
  expect_identical(subsetByOverlaps(gt0, gtl0, type = 'end'), 
                   gt0)
  expect_identical(subsetByOverlaps(gt0, gtl0, type = 'within'), 
                   gt0)
})

test_that("Works on 1-tuples", {
  expect_identical(subsetByOverlaps(gt1, gtl1[1], type = 'any'), 
                   gt1[1:5])
  # Slightly unexpected behaviour but consistent with GRanges/GRangesList
  expect_identical(subsetByOverlaps(gt1, gtl1, type = 'start'), 
                   gt1[c(1, 6)])
  # Slightly unexpected behaviour but consistent with GRanges/GRangesList
  expect_identical(subsetByOverlaps(gt1, gtl1, type = 'end'), 
                   gt1[c(5, 10)])
  expect_identical(subsetByOverlaps(gt1, gtl1, type = 'within'), 
                   gt1)
})

test_that("Works on 2-tuples", {
  expect_identical(subsetByOverlaps(gt2, gtl2[1], type = 'any'), 
                   gt2[1:6])
  # Slightly unexpected behaviour but consistent with GRanges/GRangesList
  expect_identical(subsetByOverlaps(gt2, gtl2, type = 'start'), 
                   gt2[c(1, 6)])
  # Slightly unexpected behaviour but consistent with GRanges/GRangesList
  expect_identical(subsetByOverlaps(gt2, gtl2, type = 'end'), 
                   gt2[c(5, 10)])
  expect_identical(subsetByOverlaps(gt2, gtl2, type = 'within'), 
                   gt2)
})

test_that("Works on 3-tuples", {
  expect_identical(suppressWarnings(subsetByOverlaps(gt3, gtl3[1], type = 'any')), 
                   gt3[1:6])
  expect_identical(suppressWarnings(subsetByOverlaps(gt3, gtl3, type = 'start')), 
                   gt3[c(1, 6)])
  expect_identical(suppressWarnings(subsetByOverlaps(gt3, gtl3, type = 'end')), 
                   gt3[c(5, 10)])
  expect_identical(suppressWarnings(subsetByOverlaps(gt3, gtl3, type = 'within')), 
                   gt3)
})

test_that("Works on m-tuples, m > 3", {
  expect_identical(suppressWarnings(subsetByOverlaps(gt4, gtl4[1], type = 'any')), 
                   gt4[1:6])
  expect_identical(suppressWarnings(subsetByOverlaps(gt4, gtl4, type = 'start')), 
                   gt4[c(1, 6)])
  expect_identical(suppressWarnings(subsetByOverlaps(gt4, gtl4, type = 'end')), 
                   gt4[c(5, 10)])
  expect_identical(suppressWarnings(subsetByOverlaps(gt4, gtl4, type = 'within')), 
                   gt4)
})

context("subsetByOverlaps,GTuplesList,GTuples-method")

test_that("Works on empty GTuples", {
#   Results of expect_equal(subsetByOverlaps(gtl0, gt0, type = 'any'), 
#                GTuplesList()) is FALSE due to minor and irrelevant difference 
#   in some attributes. Therefore, create a modified version with the 
#   "corrected" attributes and compare against this.
  tmp <- GTuplesList()
  tmp@partitioning@NAMES <- character(0)
  seqinfo(tmp) <- seqinfo(gtl0)
  expect_equal(subsetByOverlaps(gtl0, gt0, type = 'any'), 
                   tmp)
  expect_identical(subsetByOverlaps(gtl0, gt0, type = 'start'), 
                   tmp)
  expect_identical(subsetByOverlaps(gtl0, gt0, type = 'end'), 
                   tmp)
  expect_identical(subsetByOverlaps(gtl0, gt0, type = 'within'), 
                   tmp)
})

test_that("Works on 1-tuples", {
  expect_identical(subsetByOverlaps(gtl1, gt1[1:4], type = 'any'), 
                   gtl1[1])
  expect_identical(subsetByOverlaps(gtl1, gt1, type = 'start'), 
                   gtl1)
  expect_identical(subsetByOverlaps(gtl1, gt1, type = 'end'), 
                   gtl1)
  # Results of expect_equal(subsetByOverlaps(gtl1, gt1, type = 'within'),
  # GTuplesList()) is FALSE due to minor and (mostly) irrelevant 
  # differences in some attributes. Therefore, create a modified version with 
  # the "corrected" attributes and compare against this.
  tmp <- GTuplesList()
  seqinfo(tmp) <- seqinfo(gtl1)
  tmp@partitioning@NAMES <- character(0)
  tmp@unlistData@elementMetadata <- DataFrame(score = integer(0))
  # size(gtl1) is 1, so the subset version of it should have the same size
  tmp@unlistData@size <- 1L
  expect_identical(subsetByOverlaps(gtl1, gt1, type = 'within'), 
                   tmp)
})

test_that("Works on 2-tuples", {
  expect_identical(subsetByOverlaps(gtl2, gt2[1:4], type = 'any'), 
                   gtl2[1])
  expect_identical(subsetByOverlaps(gtl2, gt2, type = 'start'), 
                   gtl2)
  expect_identical(subsetByOverlaps(gtl2, gt2, type = 'end'), 
                   gtl2)
  # Results of expect_equal(subsetByOverlaps(gtl2, gt2, type = 'within'),
  # GTuplesList()) is FALSE due to minor and (mostly) irrelevant 
  # differences in some attributes. Therefore, create a modified version with 
  # the "corrected" attributes and compare against this.
  tmp <- GTuplesList()
  seqinfo(tmp) <- seqinfo(gtl2)
  tmp@partitioning@NAMES <- character(0)
  tmp@unlistData@elementMetadata <- DataFrame(score = integer(0))
  # size(gtl1) is 1, so the subset version of it should have the same size
  tmp@unlistData@size <- 2L
  expect_identical(subsetByOverlaps(gtl2, gt2, type = 'within'), 
                   tmp)
})

test_that("Works on 3-tuples", {
  expect_identical(suppressWarnings(subsetByOverlaps(gtl3, gt3[1:4], type = 'any')), 
                   gtl3[1])
  expect_identical(suppressWarnings(subsetByOverlaps(gtl3, gt3, type = 'start')), 
                   gtl3)
  expect_identical(suppressWarnings(subsetByOverlaps(gtl3, gt3, type = 'end')), 
                   gtl3)
  # Results of expect_equal(subsetByOverlaps(gtl3, gt3, type = 'within'),
  # GTuplesList()) is FALSE due to minor and (mostly) irrelevant 
  # differences in some attributes. Therefore, create a modified version with 
  # the "corrected" attributes and compare against this.
  tmp <- GTuplesList()
  seqinfo(tmp) <- seqinfo(gtl2)
  tmp@partitioning@NAMES <- character(0)
  tmp@unlistData@elementMetadata <- DataFrame(score = integer(0))
  # size(gtl1) is 1, so the subset version of it should have the same size
  tmp@unlistData@size <- 3L
  tmp@unlistData@internalPos <- matrix(integer(0))
  expect_identical(suppressWarnings(subsetByOverlaps(gtl3, gt3, type = 'within')), 
                   tmp)
})

test_that("Works on m-tuples, m > 3", {
  expect_identical(suppressWarnings(subsetByOverlaps(gtl4, gt4[1:4], type = 'any')), 
                   gtl4[1])
  expect_identical(suppressWarnings(subsetByOverlaps(gtl4, gt4, type = 'start')), 
                   gtl4)
  expect_identical(suppressWarnings(subsetByOverlaps(gtl4, gt4, type = 'end')), 
                   gtl4)
  # Results of expect_equal(subsetByOverlaps(gtl4, gt4, type = 'within'),
  # GTuplesList()) is FALSE due to minor and (mostly) irrelevant 
  # differences in some attributes. Therefore, create a modified version with 
  # the "corrected" attributes and compare against this.
  tmp <- GTuplesList()
  seqinfo(tmp) <- seqinfo(gtl2)
  tmp@partitioning@NAMES <- character(0)
  tmp@unlistData@elementMetadata <- DataFrame(score = integer(0))
  # size(gtl1) is 1, so the subset version of it should have the same size
  tmp@unlistData@size <- 4L
  tmp@unlistData@internalPos <- matrix(integer(0), ncol = 2)
  expect_identical(suppressWarnings(subsetByOverlaps(gtl4, gt4, type = 'within')), 
                   tmp)
})

context("subsetByOverlaps,GTuplesList,GTuplesList-method")

test_that("Works on empty GTuples", {
  #   Results of expect_equal(subsetByOverlaps(gtl0, gtl0, type = 'any'), 
  #                GTuplesList()) is FALSE due to minor and irrelevant difference 
  #   in some attributes. Therefore, create a modified version with the 
  #   "corrected" attributes and compare against this.
  tmp <- GTuplesList()
  tmp@partitioning@NAMES <- character(0)
  seqinfo(tmp) <- seqinfo(gtl0)
  expect_identical(subsetByOverlaps(gtl0, gtl0, type = 'any'), 
                   tmp)
  expect_identical(subsetByOverlaps(gtl0, gtl0, type = 'start'), 
                   tmp)
  expect_identical(subsetByOverlaps(gtl0, gtl0, type = 'end'), 
                   tmp)
  expect_identical(subsetByOverlaps(gtl0, gtl0, type = 'within'), 
                   tmp)
})

test_that("Works on 1-tuples", {
  expect_identical(subsetByOverlaps(gtl1, gtl1[1], type = 'any'), 
                   gtl1[1])
  expect_identical(subsetByOverlaps(gtl1, gtl1, type = 'start'), 
                   gtl1)
  expect_identical(subsetByOverlaps(gtl1, gtl1, type = 'end'), 
                   gtl1)
  expect_identical(subsetByOverlaps(gtl1, gtl1, type = 'within'), 
                   gtl1)
})

test_that("Works on 2-tuples", {
  expect_identical(subsetByOverlaps(gtl2, gtl2[1], type = 'any'), 
                   gtl2)
  expect_identical(subsetByOverlaps(gtl2, gtl2, type = 'start'), 
                   gtl2)
  expect_identical(subsetByOverlaps(gtl2, gtl2, type = 'end'), 
                   gtl2)
  expect_identical(subsetByOverlaps(gtl2, gtl2, type = 'within'), 
                   gtl2)
})

test_that("Works on 3-tuples", {
  expect_identical(suppressWarnings(subsetByOverlaps(gtl3, gtl3[1], type = 'any')), 
                   gtl3)
  expect_identical(suppressWarnings(subsetByOverlaps(gtl3, gtl3, type = 'start')), 
                   gtl3)
  expect_identical(suppressWarnings(subsetByOverlaps(gtl3, gtl3, type = 'end')), 
                   gtl3)
  expect_identical(suppressWarnings(subsetByOverlaps(gtl3, gtl3, type = 'within')), 
                   gtl3)
})

test_that("Works on m-tuples, m > 3", {
  expect_identical(suppressWarnings(subsetByOverlaps(gtl4, gtl4[1], type = 'any')), 
                   gtl4)
  expect_identical(suppressWarnings(subsetByOverlaps(gtl4, gtl4, type = 'start')), 
                   gtl4)
  expect_identical(suppressWarnings(subsetByOverlaps(gtl4, gtl4, type = 'end')), 
                   gtl4)
  expect_identical(suppressWarnings(subsetByOverlaps(gtl4, gtl4, type = 'within')), 
                   gtl4)
})

test_that("Returns error if algorithm argument is not 'nclist'", {
  expect_error(findOverlaps(gt1, gt1, type = "equal", 
                            algorithm = "intervaltree"), 
               "unused argument \\(algorithm = \"intervaltree\"\\)")
  expect_error(findOverlaps(gt3, gt3, type = "equal", 
                            algorithm = "intervaltree"), 
               "unused argument \\(algorithm = \"intervaltree\"\\)")
  error_1 <- try(findOverlaps(gt3, gt3, type = "start", 
                              algorithm = "intervaltree"), silent = TRUE)
  error_3 <- try(findOverlaps(gt3, gt3, type = "equal", 
                              algorithm = "intervaltree"), silent = TRUE)
  expect_identical(error_1, error_3)
})

test_that("Warnings if 'type' is not 'equal' and size > 2", {
  expect_warning(findOverlaps(gt3, gt3), 
                 paste0("'type' is not 'equal' so coercing 'query' and ", 
                        "'subject' to 'GRanges' objects"))
  expect_warning(findOverlaps(gt3, gt4), 
                 paste0("'type' is not 'equal' so coercing 'query' and ", 
                        "'subject' to 'GRanges' objects"))
})
