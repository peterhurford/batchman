context('batch')

check_for_batch_length_of <- function(len) {
  batch_length <- 0
  batch_check <- function(x)
    if (batch_length == 0) batch_length <<- length(x)
  batch_check(seq(1:10))
  expect_equal(10, batch_length)
  batch_length <- 0
  batch_run <- batch(batch_check, 'x',
    combination_strategy = paste0, size = len, verbose = FALSE
  )
  batch_run(seq(1:10))
  expect_equal(len, batch_length)
}

for (i in seq(1:5)) {
  test_that(paste('it sends things in batches of size', i), {
    check_for_batch_length_of(i)
  })
}

test_that('it can recombine', {
  batched_toupper <- batch(toupper, 'x',
    combination_strategy = paste, size = 1, verbose = FALSE)
  o <- batched_toupper(c('hi', 'hello', 'how are you'))
  expect_equal('HI HELLO HOW ARE YOU', o)
})

test_that('it can batch twice by two keys', {
  reverse <- function(x, y) c(y, x)
  batched_reverse <- batch(reverse, c('x', 'y'),
    combination_strategy = function(x, y) c(x, y),
    size = 1, verbose = FALSE)
  o <- batched_reverse(c(1, 2, 3), c(4, 5, 6))
  expect_equal(c(4, 1, 5, 2, 6, 3), o)
})

test_that('it can batch by two keys and include two nonbatched params', {
  pending()
})

test_that('it can batch by two keys and include a nonbatched param as the first param', {
  pending()
})

test_that('it can batch by an existant key and a nonexistant key', {
  pending()
})

test_that('it can batch by either one or the other key provided', {
  pending()
})

test_that('it stores partial progress on error', {
  batchman:::partial_progress$clear()
  expect_equal(list(), batchman::progress())
  fn1 <- function() 1
  fncaller <- function(list_fn) list_fn[[1]]()
  print_ex <- batch(fncaller, 'list_fn',
    combination_strategy = function(x,y) unlist(c(x,y)),
    size = 1, verbose = FALSE
  ) # Will error because identity doesn't have x arg
  expect_error(print_ex(c(fn1, fn1, fn1, fn1, identity)))  
  expect_equal(c(1, 1, 1, 1), batchman::progress())
})
