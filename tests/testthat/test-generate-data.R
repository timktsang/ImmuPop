test_that("generate_data creates correct age groups", {
  raw_data <- data.frame(
    age = c(5, 15, 25, 40, 60),
    raw_titer = c(10, 50, 80, 100, 200)
  )
  result <- generate_data(raw_data, cut_age = c(0, 18, 60, 100))

  expect_true("agegp1" %in% names(result))
  expect_true("titer_level" %in% names(result))
  expect_equal(nrow(result), 5)

  # Ages 5 and 15 should be in [0,18)
  expect_equal(as.character(result$agegp1[1]), "[0,18)")
  expect_equal(as.character(result$agegp1[2]), "[0,18)")

  # Ages 25 and 40 should be in [18,60)
  expect_equal(as.character(result$agegp1[3]), "[18,60)")
  expect_equal(as.character(result$agegp1[4]), "[18,60)")

  # Age 60 should be in [60,100)
  expect_equal(as.character(result$agegp1[5]), "[60,100)")
})

test_that("generate_data calculates titer_level correctly", {
  raw_data <- data.frame(
    age = c(30, 30, 30, 30),
    raw_titer = c(5, 10, 20, 40)
  )
  result <- generate_data(raw_data, cut_age = c(0, 100))

  # log2(5/5) + 1 = 1, log2(10/5) + 1 = 2, log2(20/5) + 1 = 3, log2(40/5) + 1 = 4
  expect_equal(result$titer_level, c(1, 2, 3, 4))
})

test_that("generate_data preserves original columns", {
  raw_data <- data.frame(
    uid = 1:3,
    age = c(10, 30, 50),
    raw_titer = c(10, 20, 40),
    baseline = c("yes", "no", "yes")
  )
  result <- generate_data(raw_data, cut_age = c(0, 18, 100))

  expect_true(all(c("uid", "age", "raw_titer", "baseline", "agegp1", "titer_level") %in% names(result)))
})

test_that("generate_data handles boundary ages correctly", {
  raw_data <- data.frame(
    age = c(0, 18, 100),
    raw_titer = c(10, 10, 10)
  )
  # right = FALSE means [0,18), [18,100)
  # age 100 is outside [18,100) so should be NA
  result <- generate_data(raw_data, cut_age = c(0, 18, 100))

  expect_equal(as.character(result$agegp1[1]), "[0,18)")
  expect_equal(as.character(result$agegp1[2]), "[18,100)")
})
