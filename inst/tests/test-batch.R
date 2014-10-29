context('batch')

test_that('it stores partial progress on error', {
  batchman:::partial_progress$clear()
  expect_equal(list(), batchman::progress())
  fn1 <- function() 1
  fncaller <- function(list_fn) list_fn[[1]]()
  print_ex <- batch(fncaller, 'list_fn', combination_strategy = function(x,y) unlist(c(x,y)), size = 1)
  expect_error(print_ex(c(fn1, fn1, fn1, fn1, identity)))  # Will error because identity doesn't have x arg
  expect_equal(c(1, 1, 1, 1), batchman::progress())
})
