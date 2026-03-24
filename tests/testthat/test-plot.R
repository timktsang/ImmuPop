## Helper shared with test-estimation.R ----------------------------------------
.get_plot_data <- function() {
  e <- new.env(parent = emptyenv())
  data("ImmuPop_raw_data", package = "ImmuPop", envir = e)
  generate_data(e$ImmuPop_raw_data, cut_age = c(0, 18, 100))
}

.plot_params <- list(
  protect_c      = c(0.1, 0.2, 0.3, 0.4, 0.5),
  protect_a      = c(0.1, 0.2, 0.3, 0.4, 0.5),
  age_prop       = c(0.2, 0.8),
  contact_matrix = matrix(c(22, 16, 28, 120), nrow = 2, byrow = TRUE),
  sim_num        = 50,
  seed           = 42
)

# Helper: run a plotting expression inside a temporary PDF device
# to prevent Rplots.pdf from being created in the working directory
.with_temp_dev <- function(expr) {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) })
  force(expr)
}

## plot_titer_dist ------------------------------------------------------------

test_that("plot_titer_dist runs without error on processed data", {
  df <- .get_plot_data()
  .with_temp_dev(
    expect_silent(
      withCallingHandlers(
        plot_titer_dist(df),
        warning = function(w) invokeRestart("muffleWarning")
      )
    )
  )
})

test_that("plot_titer_dist returns a proportion matrix", {
  df  <- .get_plot_data()
  mat <- .with_temp_dev(plot_titer_dist(df))
  expect_true(is.matrix(mat))
  # All rows sum to 1
  expect_true(all(abs(rowSums(mat) - 1) < 1e-10))
  # All values in [0, 1]
  expect_true(all(mat >= 0 & mat <= 1))
})

test_that("plot_titer_dist errors on missing columns", {
  df_bad <- data.frame(x = 1:3)
  expect_error(plot_titer_dist(df_bad), "raw_titer")
})

## plot_estimates -------------------------------------------------------------

test_that("plot_estimates runs without error on timet result", {
  skip_if_not_installed("MCMCpack")

  df     <- .get_plot_data()
  data_t <- df[df$time == 2, ]
  result <- do.call(ImmuPop_est_timepoint, c(list(df = data_t), .plot_params))

  .with_temp_dev(
    expect_silent(
      withCallingHandlers(
        plot_estimates(result),
        warning = function(w) invokeRestart("muffleWarning")
      )
    )
  )
})

test_that("plot_estimates errors on wrong input", {
  expect_error(plot_estimates(data.frame(x = 1)), "estimator")
})

## plot_estimates with grouped data (baseline) ---------------------------------

test_that("plot_estimates runs without error on baseline result (grouped by epi)", {
  skip_if_not_installed("MCMCpack")

  df     <- .get_plot_data()
  df_bl  <- df[df$baseline == "yes", ]
  result <- do.call(ImmuPop_est_baseline, c(list(df_baseline = df_bl), .plot_params))

  .with_temp_dev(
    expect_silent(
      withCallingHandlers(
        plot_estimates(result),
        warning = function(w) invokeRestart("muffleWarning")
      )
    )
  )
})

## plot_titer_jitter -----------------------------------------------------------

test_that("plot_titer_jitter runs and returns GMT table", {
  df  <- .get_plot_data()
  res <- .with_temp_dev(plot_titer_jitter(df, seed = 42))
  expect_true(is.data.frame(res))
  expect_true(all(c("agegp", "n", "GMT", "CI_lwr", "CI_upr") %in% names(res)))
  expect_true(all(res$GMT > 0))
  expect_true(all(res$CI_lwr <= res$GMT))
  expect_true(all(res$GMT <= res$CI_upr))
})

test_that("plot_titer_jitter errors on wrong input", {
  expect_error(plot_titer_jitter(data.frame(x = 1)), "raw_titer")
})
