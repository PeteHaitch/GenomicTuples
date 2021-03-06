### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### .zero_range
###

context(".zero_range helper function")

test_that("Works on zero length arguments", {
  expect_warning(val <- .zero_range(c()), "length\\(x\\) == 0L")
  expect_true(val)
})

test_that("Works on length one arguments", {
  expect_true(.zero_range(3.4))
  expect_true(.zero_range(10L))
})

test_that("Works on numeric vector arguments", {
  expect_true(.zero_range(rep(3.4, 10)))
  expect_false(.zero_range(c(3.4, 3.41)))
  expect_true(.zero_range(rep(10L, 10)))
  expect_false(.zero_range(c(10L, 9L)))
})

context('.matrixDiffWithRecycling helper function')

test_that("Matches matrix arithmetic when matrices have same number of rows", {
  x <- matrix(1:10, ncol = 2)
  y <- matrix(10:1, ncol = 2)
  expect_identical(.matrixDiffWithRecycling(x, y), x - y)
})

test_that("Recycles correctly when matrices have difference number of rows", {
  x <- matrix(1:10, ncol = 2)
  y1 <- matrix(1:2, ncol = 2)
  y2 <- matrix(1:4, ncol = 2)
  expect_identical(.matrixDiffWithRecycling(x, y1), 
                   cbind(0:4, 4:8))
  expect_warning(val <- .matrixDiffWithRecycling(x, y2))
  expect_identical(val, 
                   matrix(c(0L, 0L, 2L, 2L, 4L, 3L, 3L, 5L, 5L, 7L), ncol = 2))
})

context(".GT2DT helper function")

test_that("Works on different size tuples", {
  expect_identical(.GT2DT(gt0), data.table::data.table())
  expect_identical(.GT2DT(gt1[1]), 
                   data.table::data.table(
                     seqnames = factor("chr1", paste0("chr", 1:3)),
                     strand = factor("-", c("+", "-", "*")),
                     pos1 = 1L))
  expect_identical(.GT2DT(gt1[1], ignore.strand = TRUE), 
                   data.table::data.table(
                     seqnames = factor("chr1", paste0("chr", 1:3)),
                     strand = factor("*", c("+", "-", "*")),
                     pos1 = 1L))
  expect_identical(.GT2DT(gt2[1]), 
                   data.table::data.table(
                     seqnames = factor("chr1", paste0("chr", 1:3)),
                     strand = factor("-", c("+", "-", "*")),
                     pos1 = 1L,
                     pos2 = 2L))
  expect_identical(.GT2DT(gt2[1], ignore.strand = TRUE), 
                   data.table::data.table(
                     seqnames = factor("chr1", paste0("chr", 1:3)),
                     strand = factor("*", c("+", "-", "*")),
                     pos1 = 1L,
                     pos2 = 2L))
  expect_identical(.GT2DT(gt3[1]), 
                   data.table::data.table(
                     seqnames = factor("chr1", paste0("chr", 1:3)),
                     strand = factor("-", c("+", "-", "*")),
                     pos1 = 1L,
                     pos2 = 2L,
                     pos3 = 3L))
  expect_identical(.GT2DT(gt3[1], ignore.strand = TRUE), 
                   data.table::data.table(
                     seqnames = factor("chr1", paste0("chr", 1:3)),
                     strand = factor("*", c("+", "-", "*")),
                     pos1 = 1L,
                     pos2 = 2L,
                     pos3 = 3L))
  expect_identical(.GT2DT(gt4[1]), 
                   data.table::data.table(
                     seqnames = factor("chr1", paste0("chr", 1:3)),
                     strand = factor("-", c("+", "-", "*")),
                     pos1 = 1L,
                     pos2 = 2L,
                     pos3 = 3L,
                     pos4 = 4L))
  expect_identical(.GT2DT(gt4[1], ignore.strand = TRUE), 
                   data.table::data.table(
                     seqnames = factor("chr1", paste0("chr", 1:3)),
                     strand = factor("*", c("+", "-", "*")),
                     pos1 = 1L,
                     pos2 = 2L,
                     pos3 = 3L,
                     pos4 = 4L))
})
