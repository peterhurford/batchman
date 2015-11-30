context("progress")

test_that("it stores partial progress on error", {
  fn1 <- function() 1
  b_fn <- get_expect_error_fn(trycatch = TRUE, stop = TRUE)
  rbomb$reset()
  expect_error(b_fn(c(fn1, fn1, fn1, fn1, rbomb$detonate)))
  expect_equal(c(1, 1, 1, 1), progress())
})

test_that("it clears progress with clear", {
  partial_progress$set("hello")
  expect_equal(progress(), "hello")
  expect_equal(partial_progress$clear(), list())
})
