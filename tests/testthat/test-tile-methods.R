### =========================================================================
### "tile" methods
### -------------------------------------------------------------------------

context("tile,GTuples-method")

test_that("Returns error", {
  expect_error(tile(gt2), 
               "GTuples do not currently support the 'tile' method.")
})
