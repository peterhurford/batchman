context("utils")

test_that("as.ordinal", {
  checkr::quickcheck(as.ordinal)
})

describe("printing calculates preconditions, postconditions, and the before_fn", {
  called_before <- FALSE
  called_body <- FALSE
  with_mock(
    `batchman::get_before_fn` = function(...) { called_before <<- TRUE },
    `body` = function(...) { called_body <<- TRUE },
      expect_false(called_before)
      expect_false(called_body)
      print(batchman::batch(identity, "x"))
      expect_true(called_before)
      expect_true(called_body)
    })
})
