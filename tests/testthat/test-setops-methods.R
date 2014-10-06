### =========================================================================
### Set operations
### -------------------------------------------------------------------------

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### union(), intersect(), setdiff()
###
context("union,GTuples-method")

test_that("Returns error", {
  expect_error(union(gt2, gt2), 
               "GTuples do not currently support the 'union' method.")
})

test_that("Returns error", {
  expect_error(intersect(gt2, gt2), 
               "GTuples do not currently support the 'intersect' method.")
})

test_that("Returns error", {
  expect_error(setdiff(gt2, gt2), 
               "GTuples do not currently support the 'setdiff' method.")
})

### =========================================================================
### Parallel set operations
### -------------------------------------------------------------------------
context("punion")

test_that("Returns error", {
  expect_error(punion(gt2, gt2), 
               paste0("GTuples and GTuples do not currently support the ", 
                      "'punion' method."))
  expect_error(punion(gt2, gtl2), 
               paste0("GTuples and GTuplesList do not currently support the ", 
                      "'punion' method."))
  expect_error(punion(gtl2, gt2), 
               paste0("GTuplesList and GTuples do not currently support the ", 
                      "'punion' method."))
})

context("psetdiff")

test_that("Returns error", {
  expect_error(psetdiff(gt2, gt2), 
               paste0("GTuples and GTuples do not currently support the ", 
                      "'psetdiff' method."))
  expect_error(psetdiff(gt2, gtl2), 
               paste0("GTuples and GTuplesList do not currently support the ", 
                      "'psetdiff' method."))
  expect_error(psetdiff(gtl2, gtl2), 
               paste0("GTuplesList and GTuplesList do not currently support ", 
                      "the 'psetdiff' method."))
})

context("psetdiff")

test_that("Returns error", {
  expect_error(psetdiff(gt2, gt2), 
               paste0("GTuples and GTuples do not currently support the ", 
                      "'psetdiff' method."))
  expect_error(psetdiff(gt2, gtl2), 
               paste0("GTuples and GTuplesList do not currently support the ", 
                      "'psetdiff' method."))
  expect_error(psetdiff(gtl2, gtl2), 
               paste0("GTuplesList and GTuplesList do not currently support ", 
                      "the 'psetdiff' method."))
})

context("pgap")

test_that("Returns error", {
  expect_error(pgap(gt2, gt2), 
               paste0("GTuples and GTuples do not currently support the ", 
                      "'pgap' method."))
})
