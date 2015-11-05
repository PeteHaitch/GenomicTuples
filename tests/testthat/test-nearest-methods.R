### =========================================================================
### nearest (and related) methods
### -------------------------------------------------------------------------
###

# NOTE: There are a number of calls to suppressWarnings() in these tests 
#       because nearest() and distanceToNearest() produce a warning whenever a 
#       GTuples object is treated as a GRanges object.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### precede() and follow()
###
context("precede,GTuples-method")

test_that("Works on empty GTuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  suppressWarnings(expect_identical(precede(gt0), precede(gr0)))
  suppressWarnings(expect_identical(precede(gt0, gt1), precede(gr0, gr1)))
})

test_that("Works on 1-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(precede(gt1), precede(gr1))
  expect_identical(precede(gt1, gt2), precede(gr1, gr2))
})

test_that("Works on 2-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(precede(gt2), precede(gr2))
  expect_identical(precede(gt2, gt3), precede(gr2, gr3))
})

test_that("Works on 3-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(precede(gt3), precede(gr3))
  expect_identical(precede(gt3, gt4), precede(gr3, gr4))
})

test_that("Works on m-tuples, m > 3", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(precede(gt4), precede(gr4))
  expect_identical(precede(gt4, gt3), precede(gr4, gr3))
})

context("follow,GTuples-method")

test_that("Works on empty GTuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  suppressWarnings(expect_identical(follow(gt0), follow(gr0)))
  suppressWarnings(expect_identical(follow(gt0, gt1), follow(gr0, gr1)))
})

test_that("Works on 1-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(follow(gt1), follow(gr1))
  expect_identical(follow(gt1, gt2), follow(gr1, gr2))
})

test_that("Works on 2-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(follow(gt2), follow(gr2))
  expect_identical(follow(gt2, gt3), follow(gr2, gr3))
})

test_that("Works on 3-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(follow(gt3), follow(gr3))
  expect_identical(follow(gt3, gt4), follow(gr3, gr4))
})

test_that("Works on m-tuples, m > 3", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(follow(gt4), follow(gr4))
  expect_identical(follow(gt4, gt3), follow(gr4, gr3))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### nearest()
###
context("nearest,GTuples-method")

test_that("Works on empty GTuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(nearest(gt0), nearest(gr0))
  expect_identical(nearest(gt0, gt1), nearest(gr0, gr1))
})

test_that("Works on 1-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(nearest(gt1), nearest(gr1))
  expect_identical(nearest(gt1, gt2), nearest(gr1, gr2))
})

test_that("Works on 2-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(nearest(gt2), nearest(gr2))
  expect_identical(nearest(gt2, gt3), nearest(gr2, gr3))
})

test_that("Works on 3-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(suppressWarnings(nearest(gt3)), nearest(gr3))
  expect_identical(suppressWarnings(nearest(gt3, gt4)), nearest(gr3, gr4))
})

test_that("Works on m-tuples, m > 3", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(suppressWarnings(nearest(gt4)), nearest(gr4))
  expect_identical(suppressWarnings(nearest(gt4, gt3)), nearest(gr4, gr3))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### distance()
###
context("distance,GTuples-method")

test_that("Works on empty GTuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(distance(gt0, gt0), distance(gr0, gr0))
})

test_that("Works on 1-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(distance(gt1, gt1), distance(gr1, gr1))
})

test_that("Works on 2-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(distance(gt2, gt2), distance(gr2, gr2))
})

test_that("Works on 3-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(distance(gt3, gt3), distance(gr3, gr3))
})

test_that("Works on m-tuples, m > 3", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(distance(gt4, gt4), distance(gr4, gr4))
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### distanceToNearest()
###
context("distanceToNearest,GTuples-method")

test_that("Works on empty GTuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(distanceToNearest(gt0), distanceToNearest(gr0))
  expect_identical(distanceToNearest(gt0, gt1), distanceToNearest(gr0, gr1))
})

test_that("Works on 1-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(distanceToNearest(gt1), distanceToNearest(gr1))
  expect_identical(distanceToNearest(gt1, gt2), distanceToNearest(gr1, gr2))
})

test_that("Works on 2-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(distanceToNearest(gt2), distanceToNearest(gr2))
  expect_identical(distanceToNearest(gt2, gt3), distanceToNearest(gr2, gr3))
})

test_that("Works on 3-tuples", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(suppressWarnings(distanceToNearest(gt3)), 
                   distanceToNearest(gr3))
  expect_identical(suppressWarnings(distanceToNearest(gt3, gt4)), 
                   distanceToNearest(gr3, gr4))
})

test_that("Works on m-tuples, m > 3", {
  # Just need to test that output is the same as if the GTuples were GRanges
  expect_identical(suppressWarnings(distanceToNearest(gt4)), 
                   distanceToNearest(gr4))
  expect_identical(suppressWarnings(distanceToNearest(gt4, gt3)), 
                   distanceToNearest(gr4, gr3))
})
