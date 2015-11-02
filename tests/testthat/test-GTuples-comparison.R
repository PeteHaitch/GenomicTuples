# NB: Several objects used in testing are defined in 
# tests/testthat/helper-make-test-data.R

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### compare() and related methods.
###
context("compare,GTuples,GTuples-method and related methods")

test_that("Returns errors on bad input", {
    # empty fails
    expect_error(compare(gt0, gt0), "Cannot compare empty 'GTuples'.")
    expect_error(gt0 == gt0, "Cannot compare empty 'GTuples'.")
    expect_error(compare(gt0, gt1), "Cannot compare empty 'GTuples'.")
    expect_error(compare(gt1, gt2), 
                 "Cannot compare 'GTuples' objects of different 'size'.")
    expect_error(compare(gt2, gt3), 
                 "Cannot compare 'GTuples' objects of different 'size'.")
    expect_error(compare(gt3, gt4), 
                 "Cannot compare 'GTuples' objects of different 'size'.")
    expect_error(compare(gt3[1], gt4[1]), 
                 "Cannot compare 'GTuples' objects of different 'size'.")

    # switching chromosome names
    seqinfo <- Seqinfo(paste0("chr", 3:1), c(1000, 2000, 1500), NA, "mock1")
    gt3_fake <- GTuples(seqnames = Rle(c("chr1", "chr2", "chr1", "chr3"),
                     c(1, 3, 2, 4)),
                   tuples = matrix(c(1:10, 2:11, 3:12), ncol = 3),
                   strand = Rle(strand(c("-", "+", "*", "+", "-")),
                     c(1, 2, 2, 3, 2)),
                   score = 1:10, seqinfo = seqinfo)
    expect_error(gt3 == gt3_fake, 
                 "sequences chr1, chr3 have incompatible seqlengths")
    expect_error(granges(gt3) == granges(gt3_fake), 
                 "sequences chr1, chr3 have incompatible seqlengths")
    # GRanges fails the same way
    expect_equal(
      tryCatch(gt3 == gt3_fake, error=function(e) as.character(e)),
      tryCatch(granges(gt3) == granges(gt3_fake), error=function(e) as.character(e))
    )   
})

test_that("compare works", {
  expect_identical(compare(gt1, gt1), rep(0L, length(gt1)))
  expect_identical(compare(gt1, gt1[1]), 
                   c(0L, 31L, 32L, 58L, 19L, -7L, 32L, 32L, 45L, 45L))
  expect_identical(compare(gt2, gt2), rep(0L, length(gt2)))
  expect_identical(compare(gt2, gt2[1]), 
                   c(0L, 30L, 31L, 58L, 19L, -7L, 32L, 32L, 45L, 45L))
  expect_identical(compare(q3, q3), rep(0L, length(q3)))
  expect_identical(compare(q3, q3[1]), 
                   c(0L, 2L, 2L, -2L, -2L, -2L, -1L, -1L, -1L))
  expect_identical(compare(q4, q4), rep(0L, length(q4)))
  expect_identical(compare(q4, q4[1]), 
                   c(0L, 2L, 2L, 4L, -2L, -2L, -2L, -2L, -1L, -1L, -1L, -1L))
})

test_that("<= works", {
  expect_identical(gt1 <= gt1, rep(TRUE, length(gt1)))
  expect_identical(gt1 <= gt1[1], 
                   c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, 
                     FALSE, FALSE))
  expect_identical(gt2 <= gt2, rep(TRUE, length(gt2)))
  expect_identical(gt2 <= gt2[1], c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, 
                                    FALSE, FALSE, FALSE, FALSE))
  expect_identical(q3 <= q3, rep(TRUE, length(q3)))
  expect_identical(q3 <= q3[1], 
                   c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_true(q3[4] <= q3[1])
  expect_true(q3[7] <= q3[1])
  expect_true(q3[4] <= q3[7])
  expect_identical(q4 <= q4, rep(TRUE, length(q4)))
  expect_identical(q4 <= q4[1], 
                   c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                     TRUE, TRUE, TRUE))
  expect_true(q4[5] <= q4[1])
  expect_true(q4[9] <= q4[1])
  expect_true(q4[5] <= q4[9])
})

test_that("== works", {
  expect_identical(gt1 == gt1, rep(TRUE, length(gt1)))
  expect_identical(gt1 == gt1[1], 
                   c(TRUE, rep(FALSE, length(gt1) - 1)))
  expect_identical(gt2 == gt2, rep(TRUE, length(gt2)))
  expect_identical(gt2 == gt2[1], c(TRUE, rep(FALSE, length(gt2) - 1)))
  expect_identical(q3 == q3, rep(TRUE, length(q3)))
  expect_identical(q3 == q3[1], c(TRUE, rep(FALSE, length(q3) - 1)))
  expect_false(q3[4] == q3[1])
  expect_false(q3[7] == q3[1])
  expect_false(q3[4] == q3[7])
  expect_identical(q4 == q4, rep(TRUE, length(q4)))
  expect_identical(q4 == q4[1], 
                   c(TRUE, rep(FALSE, length(q4) - 1)))
  expect_false(q4[5] == q4[1])
  expect_false(q4[9] == q4[1])
  expect_false(q4[5] == q4[9])
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### duplicated()  
###
context("duplicated,GTuples-method and related methods")

test_that("duplicated works", {
  expect_identical(duplicated(gt0), logical(0))
  expect_identical(duplicated(c(gt1, rev(gt1))), 
                   c(rep(FALSE, length(gt1)), rep(TRUE, length(gt1))))
  expect_identical(duplicated(c(gt1, rev(gt1)), fromLast = TRUE), 
                   c(rep(TRUE, length(gt1)), rep(FALSE, length(gt1))))
  expect_identical(duplicated(c(gt2, rev(gt2))), 
                   c(rep(FALSE, length(gt2)), rep(TRUE, length(gt2))))
  expect_identical(duplicated(c(gt2, rev(gt2)), fromLast = TRUE), 
                   c(rep(TRUE, length(gt2)), rep(FALSE, length(gt2))))
  expect_identical(duplicated(c(q3, rev(q3))), 
                   c(rep(FALSE, length(q3)), rep(TRUE, length(q3))))
  expect_identical(duplicated(c(q3, rev(q3)), fromLast = TRUE), 
                   c(rep(TRUE, length(q3)), rep(FALSE, length(q3))))
  expect_identical(duplicated(c(q4, rev(q4))), 
                   c(rep(FALSE, length(q4)), rep(TRUE, length(q4))))
  expect_identical(duplicated(c(q4, rev(q4)), fromLast = TRUE), 
                   c(rep(TRUE, length(q4)), rep(FALSE, length(q4))))
  expect_error(duplicated(gt3, incomparables = TRUE), 
               paste0("\"duplicated\" method for 'GTuples' objects only ", 
                      "accepts 'incomparables = FALSE'"))
})

test_that("unique works", {
  expect_identical(unique(gt0), gt0)
  expect_identical(unique(c(gt1, rev(gt1))), gt1)
  expect_identical(unique(c(gt2, rev(gt2))), gt2)
  expect_identical(unique(c(q3, rev(q3))), q3)
  expect_identical(unique(c(q4, rev(q4))), q4)
})


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### match()
###
context("match,GTuples-method and related methods")

test_that("match works", {
  expect_identical(match(gt0, gt0), integer(0))
  expect_identical(match(gt1, gt1), seq_len(length(gt1)))
  expect_identical(match(gt2, gt2), seq_len(length(gt2)))
  expect_identical(match(q3, q3), rep(1:3, times = 3))
  expect_identical(match(q4, q4), rep(1:4, times = 3))
  expect_error(match(gt1, gt2), 
               paste0("Cannot findOverlaps between 'GTuples' and 'GTuples' ", 
                      "with 'type = \"equal\"' if they have different 'size'."))
  expect_error(match(gt2, gt2, incomparables = TRUE), 
               paste0("\"match\" method for GenomicRanges objects only ", 
                      "accepts 'incomparables = NULL'"))
  table <- q3[4:6]
  expect_identical(match(q3, table), c(rep(1:3, times = 2), rep(NA, 3)))
  expect_identical(match(q3, table, ignore.strand = TRUE), rep(1:3, times = 3))
})

test_that("findMatches works", {
  expect_identical(findMatches(gt0, gt0), 
                   Hits())
  table <- gt1[4:6]
  strand(table) <- '-'
  expect_identical(findMatches(gt1, table), 
                   Hits(queryHits = 4:5,
                        subjectHits = 1:2,
                        queryLength = 10L,
                        subjectLength = 3L))
  expect_identical(findMatches(gt1, table, ignore.strand = TRUE), 
                   Hits(queryHits = 4:6,
                        subjectHits = 1:3,
                        queryLength = 10L,
                        subjectLength = 3L))
  table <- gt2[4:6]
  strand(table) <- '-'
  expect_identical(findMatches(gt2, table), 
                   Hits(queryHits = 4:5,
                        subjectHits = 1:2,
                        queryLength = 10L,
                        subjectLength = 3L))
  expect_identical(findMatches(gt2, table, ignore.strand = TRUE), 
                   Hits(queryHits = 4:6,
                        subjectHits = 1:3,
                        queryLength = 10L,
                        subjectLength = 3L))
  table <- q3[4:6]
  expect_identical(findMatches(q3, table), 
                   Hits(queryHits = 1:6,
                        subjectHits = rep(1:3, times = 2),
                        queryLength = 9L,
                        subjectLength = 3L))
  expect_identical(findMatches(q3, table, ignore.strand = TRUE), 
                   Hits(queryHits = 1:9,
                        subjectHits = rep(1:3, times = 3),
                        queryLength = 9L,
                        subjectLength = 3L))
  table <- q4[5:8]
  expect_identical(findMatches(q4, table), 
                   Hits(queryHits = 1:8, 
                        subjectHits = rep(1:4, times = 2), 
                        queryLength = 12L,
                        subjectLength = 4L))
  expect_identical(findMatches(q4, table, ignore.strand = TRUE), 
                   Hits(queryHits = 1:12, 
                        subjectHits = rep(1:4, times = 3), 
                        queryLength = 12L,
                        subjectLength = 4L))
})

test_that("countMatches works", {
  expect_identical(countMatches(gt0, gt0), 
                   integer(0))
  table <- gt1[4:6]
  strand(table) <- '-'
  expect_identical(countMatches(gt1, table), 
                   c(rep(0L, 3), rep(1L, 2), rep(0L, 5)))
  expect_identical(countMatches(gt1, table, ignore.strand = TRUE), 
                   c(rep(0L, 3), rep(1L, 3), rep(0L, 4)))
  table <- gt2[4:6]
  strand(table) <- '-'
  expect_identical(countMatches(gt2, table), 
                   c(rep(0L, 3), rep(1L, 2), rep(0L, 5)))
  expect_identical(countMatches(gt2, table, ignore.strand = TRUE), 
                   c(rep(0L, 3), rep(1L, 3), rep(0L, 4)))
  table <- q3[4:6]
  expect_identical(countMatches(q3, table), 
                   c(rep(1L, 6), rep(0L, 3)))
  expect_identical(countMatches(q3, table, ignore.strand = TRUE), 
                   rep(1L, 9))
  table <- q4[5:8]
  expect_identical(countMatches(q4, table), 
                   c(rep(1L, 8), rep(0L, 4)))
  expect_identical(countMatches(q4, table, ignore.strand = TRUE), 
                   rep(1L, 12))
})

test_that("%in% works", {
  expect_identical(gt0 %in% gt0, logical(0))
  expect_identical(gt1 %in% gt1[-1], c(FALSE, rep(TRUE, length(gt1) - 1)))
  expect_identical(gt2 %in% gt2[-1], c(FALSE, rep(TRUE, length(gt2) - 1)))
  expect_identical(q3 %in% q3[-c(1:6)], 
                   c(rep(TRUE, 3), rep(FALSE, 3), rep(TRUE, 3)))
  expect_identical(q4 %in% q4[-c(1:8)], 
                   c(rep(TRUE, 4), rep(FALSE, 4), rep(TRUE, 4)))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### order() and related methods.
###
context("order,GTuples-method and related methods")

test_that("order works with single argument", {
  expect_identical(order(gt0), order(gr0))
  expect_identical(order(gt1), 
                   order(gr1))
  expect_identical(order(gt2), 
                   order(gr2))
  expect_identical(order(q3), c(4:9, 1:3))
  expect_identical(order(q4), c(5:12, 1:4))
  expect_identical(order(gt0, decreasing = TRUE), order(gr0, decreasing = TRUE))
  expect_identical(order(gt1, decreasing = TRUE), 
                   order(gr1, decreasing = TRUE))
  expect_identical(order(gt2, decreasing = TRUE), 
                   order(gr2, decreasing = TRUE))
  expect_identical(order(q3, decreasing = TRUE), rev(c(4:9, 1:3)))
  expect_identical(order(q4, decreasing = TRUE), rev(c(5:12, 1:4)))
})

test_that("order works with multiple arguments", {
  expect_identical(order(gt0, gt0), order(gr0, gr0))
  expect_identical(order(gt1, gt1), order(gr1, gr1))
  expect_identical(order(gt2, gt2), order(gr2, gr2))
  expect_identical(order(q3, q3), c(4:9, 1:3))
  expect_identical(order(q4, q4), c(5:12, 1:4))
  expect_identical(order(gt0, gt0, decreasing = TRUE), 
                   order(gr0, gr0, decreasing = TRUE))
  expect_identical(order(gt1, gt1, decreasing = TRUE), 
                   order(gr1, gr1, decreasing = TRUE))
  expect_identical(order(gt2, gt2, decreasing = TRUE), 
                   order(gr2, gr2, decreasing = TRUE))
  expect_identical(order(q3, q3, decreasing = TRUE), 
                   rev(c(4:9, 1:3)))
  expect_identical(order(q4, q4, decreasing = TRUE), 
                   rev(c(5:12, 1:4)))
})

test_that("rank works", {
  expect_identical(rank(gt0), rank(gr0))
  expect_identical(rank(sort(gt0)), seq_along(gt0))
  expect_identical(rank(gt1), rank(gr1))
  expect_identical(rank(sort(gt1)), seq_along(gt1))
  expect_identical(rank(gt2), rank(gr2))
  expect_identical(rank(sort(gt2)), seq_along(gt2))
  expect_identical(rank(q3), c(7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L))
  expect_identical(rank(sort(q3)), seq_len(length(q3)))
  expect_identical(rank(q4), 
                   c(9L, 10L, 11L, 12L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L))
  expect_identical(rank(sort(q4)), seq_len(length(q4)))
})

test_that("sort works", {
  expect_identical(gt0, sort(gt0))
  expect_identical(sort(gt1), gt1[order(gt1)])
  expect_identical(sort(gt2), gt2[order(gt2)])
  expect_identical(sort(q3), q3[order(q3)])
  expect_identical(sort(q4), q4[order(q4)])
})

# TODO: More tests