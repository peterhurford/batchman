context('robust_batch')

test_that('Robust batch can batch an R Bomb without erroring.', {
  b_fn <- get_expect_error_fn(trycatch = TRUE, stop = TRUE)
  rbomb$reset()
  expect_equal(
    robust_batch(b_fn, c(fn1, fn1, fn1, fn1, rbomb$detonate), verbose = FALSE),
    c(1, 1, 1, 1, 1)
  )
})

test_that('It produces the same output as a regular batch would', {
  b_fn <- get_expect_error_fn(trycatch = TRUE, stop = TRUE)
  rbomb$reset()
  expect_equal(
    robust_batch(b_fn, c(fn1, rbomb$detonate), verbose = FALSE),
    b_fn(c(fn1, fn1))
  ) # True as long as rbomb$detonate() == fn1
})

test_that('it still errors with fundamental problems (like no matching keys)', {
  batched_reverse <- batch(reverse, 'w',
    combination_strategy = c, size = 1, verbose = FALSE)
  expect_error(robust_batch(
    batched_reverse,
    c(1, 2, 3), c(4, 5, 6),
    verbose = FALSE),
  'Bad keys')
})

test_that('it works with two keys', {
  batchman:::partial_progress$clear()
  rbomb$reset()
  double_fn_caller <- function(x, y) { x[[1]](); y[[1]](); }
  batched_fn <- batch(
    double_fn_caller,
    c('x', 'y'),
    combination_strategy = function(x,y) unlist(c(x,y)),
    size = 1,
    verbose = FALSE
  )
  o <- robust_batch(
    batched_fn,
    c(fn1, rbomb$detonate, fn1),
    c(fn1, fn1, rbomb$detonate),
    verbose = FALSE
  )
  expect_equal(o, c(1, 1, 1))
})

test_that('it can work with a splat', {
  batchman:::partial_progress$clear()
  rbomb$reset()
  splat_caller <- function(...) {
    lapply(list(...), function(l) do.call(l[[1]], list()))
  }
  batched_fn <- batch(
    splat_caller,
    '...',
    combination_strategy = c,
    size = 1,
    verbose = FALSE
  )
  o <- robust_batch(batched_fn, list(fn1), list(fn1), list(rbomb$detonate), verbose = FALSE)
  expect_equal(o, list(1, 1, 1))
})

#TODO: Batch multiple, etc.
#TODO: Robust batch with different classes.
