### =========================================================================
### "coverage" methods
### -------------------------------------------------------------------------
###
context("coverage,GTuples-method")

test_that("Returns error", {
  expect_error(coverage(gt2), 
               "GTuples do not currently support the 'coverage' method.")
})

test_that("coverage,GTuplesList-method", {
  expect_error(coverage(gtl2), 
               "GTuplesList do not currently support the 'coverage' method.")
})
