# NB: Several objects used in testing are defined in 
# tests/testthat/helper-make-test-data.R

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### pcompare() and related methods.
###
context("pcompare,GTuples,GTuples-method and related methods")

test_that("Returns errors on bad input", {
    # empty fails
    expect_error(pcompare(gt0, gt0), "Cannot pcompare empty 'GTuples'.")
    expect_error(gt0 == gt0, "Cannot pcompare empty 'GTuples'.")
    expect_error(pcompare(gt0, gt1), "Cannot pcompare empty 'GTuples'.")
    expect_error(pcompare(gt1, gt2), 
                 "Cannot pcompare 'GTuples' objects of different 'size'.")
    expect_error(pcompare(gt2, gt3), 
                 "Cannot pcompare 'GTuples' objects of different 'size'.")
    expect_error(pcompare(gt3, gt4), 
                 "Cannot pcompare 'GTuples' objects of different 'size'.")
    expect_error(pcompare(gt3[1], gt4[1]), 
                 "Cannot pcompare 'GTuples' objects of different 'size'.")

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
      tryCatch(gt3 == gt3_fake, error = function(e) as.character(e)),
      tryCatch(granges(gt3) == granges(gt3_fake), 
               error = function(e) as.character(e))
    )   
})

test_that("pcompare works", {
  expect_identical(pcompare(gt1, gt1), rep(0L, length(gt1)))
  expect_identical(pcompare(gt1, gt1[1]), 
                   c(0L, 31L, 32L, 58L, 19L, -7L, 32L, 32L, 45L, 45L))
  expect_identical(pcompare(gt2, gt2), rep(0L, length(gt2)))
  expect_identical(pcompare(gt2, gt2[1]), 
                   c(0L, 30L, 31L, 58L, 19L, -7L, 32L, 32L, 45L, 45L))
  expect_identical(pcompare(q3, q3), rep(0L, length(q3)))
  expect_identical(pcompare(q3, q3[1]), 
                   c(0L, 2L, 2L, -2L, -2L, -2L, -1L, -1L, -1L))
  expect_identical(pcompare(q4, q4), rep(0L, length(q4)))
  expect_identical(pcompare(q4, q4[1]), 
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
  expect_identical(match(q3, q3), 1:9)
  expect_identical(match(q3, q3, ignore.strand = TRUE), rep(1:3, times = 3))
  expect_identical(match(q4, q4), 1:12)
  expect_identical(match(q4, q4, ignore.strand = TRUE), rep(1:4, times = 3))
  expect_error(match(gt1, gt2), 
               paste0("Cannot findOverlaps between 'GTuples' and 'GTuples' ", 
                      "with 'type = \"equal\"' if they have different 'size'."))
  expect_error(match(gt2, gt2, incomparables = TRUE), 
               paste0("\"match\" method for GenomicRanges objects only ", 
                      "accepts 'incomparables = NULL'"))
  table <- q3[4:6]
  expect_identical(match(q3, table), 
                   c(NA, NA, NA, 1L, 2L, 3L, NA, NA, NA))
  expect_identical(match(q3, table, ignore.strand = TRUE), rep(1:3, times = 3))
})

test_that("findMatches works", {
  expect_identical(findMatches(gt0, gt0), 
                   Hits(sort.by.query = TRUE))
  table <- gt1[4:6]
  strand(table) <- '-'
  expect_identical(findMatches(gt1, table), 
                   Hits(from = 4:5,
                        to = 1:2,
                        nLnode = 10L,
                        nRnode = 3L,
                        sort.by.query = TRUE))
  expect_identical(findMatches(gt1, table, ignore.strand = TRUE), 
                   Hits(from = 4:6,
                        to = 1:3,
                        nLnode = 10L,
                        nRnode = 3L,
                        sort.by.query = TRUE))
  table <- gt2[4:6]
  strand(table) <- '-'
  expect_identical(findMatches(gt2, table), 
                   Hits(from = 4:5,
                        to = 1:2,
                        nLnode = 10L,
                        nRnode = 3L,
                        sort.by.query = TRUE))
  expect_identical(findMatches(gt2, table, ignore.strand = TRUE), 
                   Hits(from = 4:6,
                        to = 1:3,
                        nLnode = 10L,
                        nRnode = 3L,
                        sort.by.query = TRUE))
  table <- q3[4:6]
  expect_identical(findMatches(q3, table), 
                   Hits(from = 1:6,
                        to = rep(1:3, times = 2),
                        nLnode = 9L,
                        nRnode = 3L,
                        sort.by.query = TRUE))
  expect_identical(findMatches(q3, table, ignore.strand = TRUE), 
                   Hits(from = 1:9,
                        to = rep(1:3, times = 3),
                        nLnode = 9L,
                        nRnode = 3L,
                        sort.by.query = TRUE))
  table <- q4[5:8]
  expect_identical(findMatches(q4, table), 
                   Hits(from = 1:8, 
                        to = rep(1:4, times = 2), 
                        nLnode = 12L,
                        nRnode = 4L,
                        sort.by.query = TRUE))
  expect_identical(findMatches(q4, table, ignore.strand = TRUE), 
                   Hits(from = 1:12, 
                        to = rep(1:4, times = 3), 
                        nLnode = 12L,
                        nRnode = 4L,
                        sort.by.query = TRUE))
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

test_that("is.unsorted works", {
  expect_false(is.unsorted(gt0))
  expect_true(is.unsorted(gt1))
  expect_false(is.unsorted(sort(gt1)))
  expect_true(is.unsorted(gt2))
  expect_false(is.unsorted(sort(gt2)))
  expect_true(is.unsorted(gt3))
  expect_false(is.unsorted(sort(gt3)))
  expect_true(is.unsorted(gt4))
  expect_false(is.unsorted(sort(gt4)))
  q3 <- c(gt3[1], gt3[1])
  strand(q3) <- c("-", "+")
  expect_true(is.unsorted(q3))
  expect_false(is.unsorted(q3, ignore.strand = TRUE))
  strand(q3) <- "*"
  expect_false(is.unsorted(q3))
  expect_true(is.unsorted(q3, strictly = TRUE))
  expect_warning(is.unsorted(gt2, na.rm = TRUE),
                 paste0("\"is.unsorted\" method for 'GTuples' objects ignores ", 
                        "the 'na.rm' argument"))
  q3 <- GTuples(seqnames = c('chr1', 'chr1', 'chr1', 'chr1', 'chr2'), 
                tuples = matrix(c(10L, 10L, 10L, 10L, 10L, 20L, 20L, 20L, 25L, 
                                  20L, 30L, 30L, 35L, 30L, 30L), ncol = 3), 
                strand = c('+', '-', '*', '+', '+'))
  q3 <- c(q3, rev(q3[3:5]))
  expect_true(is.unsorted(q3, ignore.strand = TRUE))
  expect_true(is.unsorted(sort(q3, ignore.strand = TRUE)))
  expect_false(is.unsorted(sort(q3, ignore.strand = TRUE), 
                           ignore.strand = TRUE))
})

test_that("order signature has ignore.strand", {
  # NOTE: see https://github.com/PeteHaitch/GenomicTuples/issues/31
  generic <- getGeneric(order)
  method <- getMethod(order, "GTuples")
  expect_identical(formals(generic), formals(method))
})

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

test_that("rank(..., ties.method = 'average') is the default", {
  expect_identical(rank(q3), rank(q3, ties.method = "average"))
})

test_that("rank(sort(x)) returns seq_along(x)", {
  expect_equal(rank(sort(gt0)), seq_along(gt0))
  expect_equal(rank(sort(gt1)), seq_along(gt1))
  expect_equal(rank(sort(gt2)), seq_along(gt2))
  expect_equal(rank(sort(gt3)), seq_along(gt3))
  expect_equal(rank(sort(gt4)), seq_along(gt4))
})

test_that("rank() works for different sized tuples and `ties.method` values", {
  # NOTE: GTuples with size < 3 are basically GRanges
  Map(function(gt, gr) {
    lapply(c("average", "first", "last", "random", "max", "min"), function(tm) {
      expect_identical(rank(gt, ties.method = tm), rank(gr, ties.method = tm))
    })
  }, gt = list(gt0, gt1, gt2), gr = list(gr0, gr1, gr2))
  # 3-tuples
  expect_identical(rank(q3), c(2, 5, 8, 2, 5, 8, 2, 5, 8))
  expect_identical(rank(q3, ties.method = "first"), 
                   c(7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L))
  expect_identical(rank(q3, ties.method = "last"),
                   c(3L, 6L, 9L, 2L, 5L, 8L, 1L, 4L, 7L))
  set.seed(666)
  expect_identical(rank(q3, ties.method = "random"), 
                   c(2L, 4L, 9L, 1L, 5L, 8L, 3L, 6L, 7L))
  expect_identical(rank(q3, ties.method = "max"), 
                   c(3L, 6L, 9L, 3L, 6L, 9L, 3L, 6L, 9L))
  expect_identical(rank(q3, ties.method = "min"), 
                   c(7L, 8L, 9L, 7L, 8L, 9L, 7L, 8L, 9L))
  expect_identical(rank(sort(q3), ties.method = "first"), seq_len(length(q3)))
  # 4-tuples
  expect_identical(rank(q4, ties.method = "average"),
                   c(2, 5, 8, 11, 2, 5, 8, 11, 2, 5, 8, 11))
  expect_identical(rank(q4, ties.method = "first"), 
                   c(9L, 10L, 11L, 12L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L))
  expect_identical(rank(q4, ties.method = "last"),
                   c(3L, 6L, 9L, 12L, 2L, 5L, 8L, 11L, 1L, 4L, 7L, 10L))
  set.seed(666)
  expect_identical(rank(q4, ties.method = "random"),
                   c(3L, 4L, 8L, 11L, 2L, 6L, 9L, 12L, 1L, 5L, 7L, 10L))
  expect_identical(rank(q4, ties.method = "max"),
                   c(3L, 6L, 9L, 12L, 3L, 6L, 9L, 12L, 3L, 6L, 9L, 12L))
  expect_identical(rank(q4, ties.method = "min"),
                   c(9L, 10L, 11L, 12L, 9L, 10L, 11L, 12L, 9L, 10L, 11L, 12L))
  expect_identical(rank(sort(q4), ties.method = "first"), seq_len(length(q4)))
})

test_that("rank() works when ties are present", {
  q2 <- c(gt2[1], gt2[1], gt2[1])
  expect_identical(rank(q2), c(2, 2, 2))
  expect_identical(rank(q2, ties.method = "first"), c(1L, 2L, 3L))
  expect_identical(rank(q2, ties.method = "last"), c(3L, 2L, 1L))
  set.seed(666)
  expect_identical(rank(q2, ties.method = "random"), c(2L, 1L, 3L))
  expect_identical(rank(q2, ties.method = "max"), c(3L, 3L, 3L))
  expect_identical(rank(q2, ties.method = "min"), c(1L, 1L, 1L))
})

test_that("rank(..., ignore.strand = TRUE) works", {
  q2 <- c(gt2[1], gt2[1], gt2[1])
  strand(q2) <- rev(levels(strand()))
  expect_identical(rank(q2, ties.method = "average", ignore.strand = FALSE),
                   c(3, 2, 1))
  expect_identical(rank(q2, ties.method = "average", ignore.strand = TRUE), 
                   c(2, 2, 2))
  expect_identical(rank(q2, ties.method = "first", ignore.strand = FALSE),
                   c(3L, 2L, 1L))
  expect_identical(rank(q2, ties.method = "first", ignore.strand = TRUE), 
                   c(1L, 2L, 3L))
  expect_identical(rank(q2, ties.method = "last", ignore.strand = FALSE),
                   c(3L, 2L, 1L))
  expect_identical(rank(q2, ties.method = "last", ignore.strand = TRUE), 
                   c(3L, 2L, 1L))
  set.seed(666)
  expect_identical(rank(q2, ties.method = "random", ignore.strand = FALSE),
                   c(3L, 2L, 1L))
  set.seed(666)
  expect_identical(rank(q2, ties.method = "random", ignore.strand = TRUE), 
                   c(2L, 1L, 3L))
  expect_identical(rank(q2, ties.method = "max", ignore.strand = FALSE),
                   c(3L, 2L, 1L))
  expect_identical(rank(q2, ties.method = "max", ignore.strand = TRUE), 
                   c(3L, 3L, 3L))
  expect_identical(rank(q2, ties.method = "min", ignore.strand = FALSE),
                   c(3L, 2L, 1L))
  expect_identical(rank(q2, ties.method = "min", ignore.strand = TRUE), 
                   c(1L, 1L, 1L))
})

test_that("sort works", {
  expect_identical(gt0, sort(gt0))
  expect_identical(sort(gt1), gt1[order(gt1)])
  expect_identical(sort(gt2), gt2[order(gt2)])
  expect_identical(sort(q3), q3[order(q3)])
  expect_identical(sort(q4), q4[order(q4)])
  expect_identical(gt0, sort(gt0, decreasing = TRUE))
  expect_identical(sort(gt1, decreasing = TRUE), 
                   gt1[order(gt1, decreasing = TRUE)])
  expect_identical(sort(gt2, decreasing = TRUE), 
                   gt2[order(gt2, decreasing = TRUE)])
  expect_identical(sort(q3, decreasing = TRUE), 
                   q3[order(q3, decreasing = TRUE)])
  expect_identical(sort(q4, decreasing = TRUE), 
                   q4[order(q4, decreasing = TRUE)])
})
