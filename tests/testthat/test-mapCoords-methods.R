### =========================================================================
### mapCoords methods
### -------------------------------------------------------------------------
###
context("mapCoords,GTuples-method")

test_that("Returns error", {
  expect_error(mapCoords(gt2, gtl2), 
               "GTuples do not currently support the 'mapCoords' method.")
})