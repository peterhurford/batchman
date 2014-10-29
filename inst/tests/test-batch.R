context('batch')

test_that('it stores partial progress on error', {
  batchman:::partial_progress$clear()
  expect_equal(list(), batchman::progress())
  fn1() <- function() 1
  fncaller <- function(fn) fn()
  print_ex <- batch(fncaller, fn, combination_strategy = function(x,y) unlist(c(x,y)), size = 3)
  print_ex(c(fn1, fn1, fn1, fn1, identity))  # Will error because identity doesn't have an x arg
  expect_equal(list(c(1, 1, 1)), batchman::progress())
})
