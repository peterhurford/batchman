context("combine")

test_that("it errors if input is not a list", {
  expect_error(combine_by_list(1, 2, 3))
})

test_that("it combines characters via paste0", {
  expect_equal(combine("a", "b", "c"), "abc")
})

test_that("it combines numeric via c", {
  expect_equal(combine(1, 2, 3), c(1, 2, 3))
})

test_that("it combines list via c", {
  expect_equal(combine(list(1), list(2), list(3)), list(1, 2, 3))
})

test_that("it combines logical via c", {
  expect_equal(combine(TRUE, FALSE, TRUE), c(TRUE, FALSE, TRUE))
})

test_that("it combines NULL via c", {
  expect_equal(combine(NULL, NULL), c(NULL, NULL))
})

test_that("it combines data frames via plyr::rbind.fill", {
  expect_equal(combine(iris, iris), plyr::rbind.fill(iris, iris))
})

test_that("it combines matricies via merge", {
  mat1 <- matrix(1, 2, 3)
  mat2 <- matrix(2, 4, 2)
  expect_equal(combine(mat1, mat2), merge(mat1, mat2))
})

test_that("it errors if class is not recognized", {
  weird1 <- 1
  weird2 <- 2
  class(weird1) <- "weird"
  class(weird2) <- "weird"
  expect_error(combine(weird1, weird2))
})

test_that("it simply returns if the length is 1", {
  weird <- 0
  class(weird) <- "weird"
  expect_equal(combine(weird), weird)
})
