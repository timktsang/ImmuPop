#' Validate inputs for estimation functions
#'
#' Internal function that checks all inputs before running estimation.
#' Called at the start of \code{\link{ImmuPop_timet_est}}.
#'
#' @param df Data frame with individual data.
#' @param protect_c Numeric vector of protection effects for children.
#' @param protect_a Numeric vector of protection effects for adults.
#' @param age_prop Numeric vector of age group proportions.
#' @param contact_matrix Numeric contact matrix.
#' @param sim_num Number of simulations.
#' @return Invisible TRUE if all checks pass.
#' @keywords internal
validate_estimation_inputs <- function(df, protect_c, protect_a, age_prop, contact_matrix, sim_num) {
  # df must be a data frame
  if (!is.data.frame(df))
    stop("'df' must be a data frame.", call. = FALSE)

  # df must not be empty
  if (nrow(df) == 0)
    stop("'df' has 0 rows. Did you filter to a time point with no data?", call. = FALSE)

  # Required columns (from generate_data output)
  required_cols <- c("agegp1", "raw_titer", "titer_level")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0)
    stop(sprintf("'df' is missing required columns: %s. Did you run generate_data() first?",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)

  # age_prop must be numeric and sum to ~1
  if (!is.numeric(age_prop))
    stop("'age_prop' must be a numeric vector.", call. = FALSE)
  if (abs(sum(age_prop) - 1) > 0.01)
    stop(sprintf("'age_prop' must sum to approximately 1, got %.4f.", sum(age_prop)),
         call. = FALSE)

  # contact_matrix must be square with dimension matching age_prop
  if (!is.matrix(contact_matrix))
    stop("'contact_matrix' must be a matrix.", call. = FALSE)
  if (nrow(contact_matrix) != ncol(contact_matrix))
    stop("'contact_matrix' must be square.", call. = FALSE)
  if (nrow(contact_matrix) != length(age_prop))
    stop(sprintf("'contact_matrix' dimension (%d) must match length of 'age_prop' (%d).",
                 nrow(contact_matrix), length(age_prop)), call. = FALSE)

  # protect_c and protect_a must be numeric, same length
  if (!is.numeric(protect_c))
    stop("'protect_c' must be a numeric vector.", call. = FALSE)
  if (!is.numeric(protect_a))
    stop("'protect_a' must be a numeric vector.", call. = FALSE)
  if (length(protect_c) != length(protect_a))
    stop("'protect_c' and 'protect_a' must have the same length.", call. = FALSE)

  # sim_num must be a positive number
  if (!is.numeric(sim_num) || length(sim_num) != 1 || sim_num < 1)
    stop("'sim_num' must be a positive integer.", call. = FALSE)

  # Number of age groups in data must match age_prop
  n_agegp_data <- length(unique(df$agegp1))
  if (n_agegp_data != length(age_prop))
    stop(sprintf("Data has %d age group(s) but 'age_prop' has %d elements. Ensure all age groups are present in the data slice.",
                 n_agegp_data, length(age_prop)), call. = FALSE)

  invisible(TRUE)
}
