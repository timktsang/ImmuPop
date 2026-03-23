#' Estimate Immunity at the Baseline Period
#'
#' This function estimates immunity at the baseline period for different
#' epidemic (`epi`) and circulating virus groups.
#'
#' @name ImmuPop_bsl_est
#' @param df_long_bsl A dataframe containing baseline data (pre-epidemic),
#'   including epidemic information as `epi`. Must have been processed by
#'   \code{\link{generate_data}} first.
#' @param protect_c Numeric vector indicating the protection effect for children.
#' @param protect_a Numeric vector indicating the protection effect for adults.
#' @param age_prop Numeric vector of age group proportions in the population.
#' @param contact_matrix Numeric matrix of contact rates between age groups.
#' @param sim_num The number of bootstrap simulations to run (default = 500).
#' @param seed Optional integer seed for reproducibility (default = NULL).
#' @return A dataframe with baseline immunity estimates, including the median and 95% CI.
#' @export
ImmuPop_bsl_est <- function(df_long_bsl, protect_c, protect_a, age_prop, contact_matrix, sim_num = 500, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Split dataframe by epidemic group (`epi`)
  epi_list <- split(df_long_bsl, df_long_bsl$epi)

  # Loop through each epidemic group to estimate immunity
  result_all <- lapply(epi_list, function(df) {
    myepi <- unique(df$epi)

    # Estimate immunity for each epidemic
    estimate_res <- ImmuPop_timet_est(df,
      protect_c = protect_c, protect_a = protect_a, age_prop = age_prop,
      contact_matrix = contact_matrix, sim_num = sim_num
    )

    # Organize and reshape results into a dataframe
    result_epi <- estimate_res %>%
      mutate(epi = myepi) %>%
      dplyr::arrange(estimator) %>%
      dplyr::select(estimator, epi, value, CI_lwr, CI_upr)

    return(result_epi)
  })

  # Combine results from all epidemics
  result <- bind_rows(result_all) %>%
    dplyr::arrange(estimator, epi)

  return(result)
}
