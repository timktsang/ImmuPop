#' Estimate Immunity Across All Time Points
#'
#' This function estimates immunity at each time point using
#' \code{\link{ImmuPop_timet_est}} and returns the combined results.
#' Only time points that contain all age groups are included.
#'
#' @name ImmuPop_allt_est
#' @param df_long A dataframe containing the data for all time points.
#'   Must have been processed by \code{\link{generate_data}} first.
#' @param protect_c Numeric vector indicating the protection effect for children.
#' @param protect_a Numeric vector indicating the protection effect for adults.
#' @param age_prop Numeric vector of age group proportions in the population.
#' @param contact_matrix Numeric matrix of contact rates between age groups.
#' @param sim_num The number of bootstrap simulations to run (default = 500).
#' @param seed Optional integer seed for reproducibility (default = NULL).
#' @return A dataframe with immunity estimates for each time point, including
#'   the median and 95% CI.
#' @export
ImmuPop_allt_est <- function(df_long, protect_c, protect_a, age_prop, contact_matrix, sim_num = 500, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  count_agegps_num <- df_long %>%
    group_by(time) %>%
    dplyr::summarise(count = length(unique(agegp1)))
  time_filter <- count_agegps_num$time[count_agegps_num$count >= length(age_prop)]
  time_vec <- sort(time_filter)

  # Loop through each time point
  result_all <- lapply(time_vec, function(t) {
    df <- df_long %>% filter(time == t)

    # Estimate immunity at each time point
    result_t <- ImmuPop_timet_est(df,
      protect_c = protect_c, protect_a = protect_a,
      age_prop = age_prop, contact_matrix = contact_matrix,
      sim_num = sim_num
    ) %>%
      mutate(time = t)

    return(result_t)
  })

  # Combine results from all time points
  result <- bind_rows(result_all) %>%
    dplyr::arrange(estimator, time) %>%
    dplyr::select(estimator, time, value, CI_lwr, CI_upr)

  return(result)
}
