### =========================================================================
### Inter-tuple methods
### -------------------------------------------------------------------------
###

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### range()
###
context("range,GTuples-method")

test_that("Returns error", {
  expect_error(range(gt2), 
               "GTuples do not currently support the 'range' method.")
})

context("range,GTuplesList-method")

test_that("Returns error", {
  expect_error(range(gtl2), 
               "GTuplesList do not currently support the 'range' method.")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### reduce()
###
context("reduce,GTuples-method")

test_that("Returns error", {
  expect_error(reduce(gt2), 
               "GTuples do not currently support the 'reduce' method.")
})

context("reduce,GTuplesList-method")

test_that("Returns error", {
  expect_error(reduce(gtl2), 
               "GTuplesList do not currently support the 'reduce' method.")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### gaps()
###
context("gaps,GTuples-method")

test_that("Returns error", {
  expect_error(gaps(gt2), 
               "GTuples do not currently support the 'gaps' method.")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### disjoin()
###
context("disjoin,GTuples-method")

test_that("Returns error", {
  expect_error(disjoin(gt2), 
               "GTuples do not currently support the 'disjoin' method.")
})

context("disjoin,GTuplesList-method")

test_that("Returns error", {
  expect_error(disjoin(gtl2), 
               "GTuplesList do not currently support the 'disjoin' method.")
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### disjointBins()
###
context("disjointBins,GTuples-method")

test_that("Returns error", {
  expect_error(disjointBins(gt2), 
               "GTuples do not currently support the 'disjointBins' method.")
})
