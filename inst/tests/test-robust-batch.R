context('robust_batch')

test_that('Robust batch can batch an R Bomb without erroring.', {
  fn1 <- function() 1
  b_fn <- get_expect_error_fn(trycatch = TRUE, stop = TRUE)
  rbomb$reset()
  expect_equal(
    robust_batch(b_fn, c(fn1, fn1, fn1, fn1, rbomb$detonate)),
    c(1, 1, 1, 1, 1)
  )
})
