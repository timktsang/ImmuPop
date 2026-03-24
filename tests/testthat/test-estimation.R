## Helper: shared test parameters ------------------------------------------------
.test_params <- list(
  protect_c      = c(0.1, 0.2, 0.3, 0.4, 0.5),
  protect_a      = c(0.1, 0.2, 0.3, 0.4, 0.5),
  age_prop       = c(0.2, 0.8),
  contact_matrix = matrix(c(22, 16, 28, 120), nrow = 2, byrow = TRUE),
  sim_num        = 50,
  seed           = 42
)

.get_data <- function() {
  # Use explicit envir to ensure we load from data/ (.rda), not the objects.R definition
  e <- new.env(parent = emptyenv())
  data("ImmuPop_raw_data", package = "ImmuPop", envir = e)
  generate_data(e$ImmuPop_raw_data, cut_age = c(0, 18, 100))
}

## time == 2 is the earliest time point with both age groups in the real shipped data
.get_slice <- function(data_example, t = 2) {
  data_example[data_example$time == t, ]
}

## ImmuPop_est_timepoint -------------------------------------------------------

test_that("ImmuPop_est_timepoint returns correct structure on shipped data", {
  skip_if_not_installed("MCMCpack")

  data_example <- .get_data()
  data_t <- .get_slice(data_example, t = 2)
  expect_gt(nrow(data_t), 0)

  result <- ImmuPop_est_timepoint(
    df             = data_t,
    protect_c      = .test_params$protect_c,
    protect_a      = .test_params$protect_a,
    age_prop       = .test_params$age_prop,
    contact_matrix = .test_params$contact_matrix,
    sim_num        = .test_params$sim_num,
    seed           = .test_params$seed
  )

  expect_equal(nrow(result), 4)
  expect_equal(sort(result$estimator), c("GMT", "RR_R0", "pop_immun", "prop_5"))
  expect_true(all(c("estimator", "value", "CI_lwr", "CI_upr") %in% names(result)))
  expect_true(all(is.finite(result$value)))
  expect_true(all(is.finite(result$CI_lwr)))
  expect_true(all(is.finite(result$CI_upr)))
  expect_true(all(result$CI_lwr <= result$value + 1e-10))
  expect_true(all(result$value <= result$CI_upr + 1e-10))
})

test_that("ImmuPop_est_timepoint is reproducible with seed", {
  skip_if_not_installed("MCMCpack")

  data_example <- .get_data()
  data_t <- .get_slice(data_example, t = 2)

  args <- c(list(df = data_t), .test_params)
  result1 <- do.call(ImmuPop_est_timepoint, args)
  result2 <- do.call(ImmuPop_est_timepoint, args)
  expect_equal(result1, result2)
})

test_that("ImmuPop_est_timepoint values are in reasonable ranges", {
  skip_if_not_installed("MCMCpack")

  data_example <- .get_data()
  data_t <- .get_slice(data_example, t = 2)

  result <- ImmuPop_est_timepoint(
    df             = data_t,
    protect_c      = .test_params$protect_c,
    protect_a      = .test_params$protect_a,
    age_prop       = .test_params$age_prop,
    contact_matrix = .test_params$contact_matrix,
    sim_num        = .test_params$sim_num,
    seed           = .test_params$seed
  )

  pop_immun <- result[result$estimator == "pop_immun", ]
  expect_gte(pop_immun$value, 0)
  expect_lte(pop_immun$value, 1)

  rr_r0 <- result[result$estimator == "RR_R0", ]
  expect_gte(rr_r0$value, 0)
  expect_lte(rr_r0$value, 1)

  gmt <- result[result$estimator == "GMT", ]
  expect_gt(gmt$value, 0)

  prop5 <- result[result$estimator == "prop_5", ]
  expect_gte(prop5$value, 0)
  expect_lte(prop5$value, 1)
})

test_that("ImmuPop_est_timepoint handles equal age-group sizes (regression: sapply matrix collapse)", {
  skip_if_not_installed("MCMCpack")

  # time == 19 in the real data has equal group sizes — this triggered the original sapply bug
  data_example <- .get_data()
  data_t <- .get_slice(data_example, t = 19)
  skip_if(nrow(data_t) == 0, "time == 19 not present in this dataset version")

  result <- ImmuPop_est_timepoint(
    df             = data_t,
    protect_c      = .test_params$protect_c,
    protect_a      = .test_params$protect_a,
    age_prop       = .test_params$age_prop,
    contact_matrix = .test_params$contact_matrix,
    sim_num        = .test_params$sim_num,
    seed           = .test_params$seed
  )
  expect_equal(nrow(result), 4)
  expect_true(all(is.finite(result$value)))
})

## ImmuPop_est_baseline ---------------------------------------------------------

test_that("ImmuPop_est_baseline returns results for each epi group", {
  skip_if_not_installed("MCMCpack")

  data_example <- .get_data()
  df_bsl <- data_example[data_example$baseline == "yes", ]

  result <- ImmuPop_est_baseline(
    df_baseline    = df_bsl,
    protect_c      = .test_params$protect_c,
    protect_a      = .test_params$protect_a,
    age_prop       = .test_params$age_prop,
    contact_matrix = .test_params$contact_matrix,
    sim_num        = .test_params$sim_num,
    seed           = .test_params$seed
  )

  expect_true(all(c("estimator", "epi", "value", "CI_lwr", "CI_upr") %in% names(result)))
  n_epi <- length(unique(df_bsl$epi))
  expect_equal(nrow(result), n_epi * 4)
  expect_true(all(is.finite(result$value)))
})

## ImmuPop_est_timeseries --------------------------------------------------------

test_that("ImmuPop_est_timeseries returns results for multiple time points on shipped data", {
  skip_if_not_installed("MCMCpack")

  data_example <- .get_data()

  result <- ImmuPop_est_timeseries(
    df_long        = data_example,
    protect_c      = .test_params$protect_c,
    protect_a      = .test_params$protect_a,
    age_prop       = .test_params$age_prop,
    contact_matrix = .test_params$contact_matrix,
    sim_num        = .test_params$sim_num,
    seed           = .test_params$seed
  )

  expect_true(all(c("estimator", "time", "value", "CI_lwr", "CI_upr") %in% names(result)))
  expect_gt(length(unique(result$time)), 1)
  n_times <- length(unique(result$time))
  expect_equal(nrow(result), n_times * 4)
  expect_true(all(is.finite(result$value)))
})
