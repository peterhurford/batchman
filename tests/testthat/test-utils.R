context("utils")

test_that("as.ordinal", {
  checkr::quickcheck(as.ordinal)
})
