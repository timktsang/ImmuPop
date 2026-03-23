test_that("fill_matrix pads smaller matrix into larger one", {
  orig <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  colnames(orig) <- c("1", "2")

  result <- fill_matrix(orig, n_row = 3, n_col = 4)

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4)
  # Original values preserved in matching columns
  expect_equal(result[1, 1], 1, ignore_attr = TRUE)
  expect_equal(result[2, 1], 2, ignore_attr = TRUE)
  expect_equal(result[1, 2], 3, ignore_attr = TRUE)
  expect_equal(result[2, 2], 4, ignore_attr = TRUE)
  # Padded values are zero
  expect_equal(result[3, 1], 0, ignore_attr = TRUE)
  expect_equal(result[3, 2], 0, ignore_attr = TRUE)
  expect_equal(result[1, 3], 0, ignore_attr = TRUE)
  expect_equal(result[1, 4], 0, ignore_attr = TRUE)
})

test_that("fill_matrix matches columns by name", {
  orig <- matrix(c(10, 20), nrow = 1, ncol = 2)
  colnames(orig) <- c("2", "4")

  result <- fill_matrix(orig, n_row = 2, n_col = 5)

  # Column "2" should be filled, column "4" should be filled
  expect_equal(result[1, 2], 10, ignore_attr = TRUE)
  expect_equal(result[1, 4], 20, ignore_attr = TRUE)
  # Other columns should be zero
  expect_equal(result[1, 1], 0, ignore_attr = TRUE)
  expect_equal(result[1, 3], 0, ignore_attr = TRUE)
  expect_equal(result[1, 5], 0, ignore_attr = TRUE)
})

test_that("fill_matrix errors when original exceeds new dimensions", {
  orig <- matrix(1:6, nrow = 2, ncol = 3)
  colnames(orig) <- c("1", "2", "3")

  # Too few columns
  expect_error(fill_matrix(orig, n_row = 2, n_col = 2))

  # Too few rows
  expect_error(fill_matrix(orig, n_row = 1, n_col = 5))
})

test_that("fill_matrix works with same-size matrix", {
  orig <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  colnames(orig) <- c("1", "2")

  result <- fill_matrix(orig, n_row = 2, n_col = 2)
  expect_equal(result, orig)
})

test_that("fill_matrix handles single-row matrix (no dimension drop)", {
  orig <- matrix(c(5, 10, 15), nrow = 1, ncol = 3)
  colnames(orig) <- c("1", "2", "3")

  result <- fill_matrix(orig, n_row = 2, n_col = 5)
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 5)
  expect_equal(result[1, 1], 5, ignore_attr = TRUE)
  expect_equal(result[1, 2], 10, ignore_attr = TRUE)
  expect_equal(result[1, 3], 15, ignore_attr = TRUE)
  expect_equal(result[2, 1], 0, ignore_attr = TRUE)
})
