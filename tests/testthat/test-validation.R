test_that("validation rejects non-data.frame input", {
  expect_error(
    validate_estimation_inputs("not a df", c(0.5), c(0.5), c(1), matrix(1), 100),
    "must be a data frame"
  )
})

test_that("validation rejects empty data frame", {
  df_empty <- data.frame(agegp1 = factor(character(0)),
                         raw_titer = numeric(0),
                         titer_level = numeric(0))
  expect_error(
    validate_estimation_inputs(df_empty, c(0.5), c(0.5), c(1), matrix(1), 100),
    "0 rows"
  )
})

test_that("validation catches missing columns", {
  df_bad <- data.frame(x = 1:3)
  expect_error(
    validate_estimation_inputs(df_bad, c(0.5), c(0.5), c(1), matrix(1), 100),
    "missing required columns.*generate_data"
  )
})

test_that("validation catches non-numeric age_prop", {
  df <- data.frame(agegp1 = factor("[0,100)"), raw_titer = 10, titer_level = 2)
  expect_error(
    validate_estimation_inputs(df, c(0.5), c(0.5), "bad", matrix(1), 100),
    "age_prop.*numeric"
  )
})

test_that("validation catches age_prop not summing to 1", {
  df <- data.frame(agegp1 = factor("[0,100)"), raw_titer = 10, titer_level = 2)
  expect_error(
    validate_estimation_inputs(df, c(0.5), c(0.5), c(0.3, 0.3), matrix(1, 2, 2), 100),
    "sum to approximately 1"
  )
})

test_that("validation catches non-matrix contact_matrix", {
  df <- data.frame(agegp1 = factor("[0,100)"), raw_titer = 10, titer_level = 2)
  expect_error(
    validate_estimation_inputs(df, c(0.5), c(0.5), c(1), data.frame(a = 1), 100),
    "must be a matrix"
  )
})

test_that("validation catches non-square contact_matrix", {
  df <- data.frame(agegp1 = factor("[0,100)"), raw_titer = 10, titer_level = 2)
  expect_error(
    validate_estimation_inputs(df, c(0.5), c(0.5), c(1), matrix(1, 1, 2), 100),
    "must be square"
  )
})

test_that("validation catches dimension mismatch between contact_matrix and age_prop", {
  df <- data.frame(agegp1 = factor("[0,100)"), raw_titer = 10, titer_level = 2)
  expect_error(
    validate_estimation_inputs(df, c(0.5), c(0.5), c(0.5, 0.5), matrix(1, 1, 1), 100),
    "dimension.*must match"
  )
})

test_that("validation catches different length protect vectors", {
  df <- data.frame(agegp1 = factor("[0,100)"), raw_titer = 10, titer_level = 2)
  expect_error(
    validate_estimation_inputs(df, c(0.5, 0.6), c(0.5), c(1), matrix(1), 100),
    "same length"
  )
})

test_that("validation catches invalid sim_num", {
  df <- data.frame(agegp1 = factor("[0,100)"), raw_titer = 10, titer_level = 2)
  expect_error(
    validate_estimation_inputs(df, c(0.5), c(0.5), c(1), matrix(1), -1),
    "positive integer"
  )
  expect_error(
    validate_estimation_inputs(df, c(0.5), c(0.5), c(1), matrix(1), "bad"),
    "positive integer"
  )
})

test_that("validation errors on age group count mismatch", {
  df <- data.frame(
    agegp1 = factor(c("[0,18)", "[18,100)")),
    raw_titer = c(10, 20),
    titer_level = c(2, 3)
  )
  # 2 age groups in data but 3 in age_prop — should now be an error
  expect_error(
    validate_estimation_inputs(df, c(0.5), c(0.5), c(0.3, 0.3, 0.4),
                               matrix(1, 3, 3), 100),
    "2 age group.*3 elements"
  )
})

test_that("validation passes with valid inputs", {
  df <- data.frame(agegp1 = factor("[0,100)"), raw_titer = 10, titer_level = 2)
  expect_true(
    validate_estimation_inputs(df, c(0.5), c(0.5), c(1), matrix(1), 100)
  )
})
