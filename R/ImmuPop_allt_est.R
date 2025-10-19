#' Estimate Immunity Across All Time Points
#'
#' This function estimates immunity at each time point using `ImmuPop_timet_est` and returns the results.
#'
#' @name ImmuPop_allt_est
#' @param df_long A dataframe containing the data for all time points.
#' @param protect_c Numeric vector indicating the protection effect for children.
#' @param protect_a Numeric vector indicating the protection effect for adults.
#' @param agep_prop Numeric vector of age group proportions in the population.
#' @param contact_matrix Numeric matrix of contact rates between age groups.
#' @param sim_num The number of bootstrap simulations to run (default = 500).
#' @return A dataframe with immunity estimates for each time point, including the median and 95% CI.
#' @import dplyr rlist
#' @export

# Global variables are defined after the function header
utils::globalVariables(c("time", "estimator", "CI_lwr", "CI_upr", "epi", "value", "agegp1"))

ImmuPop_allt_est <- function(df_long, protect_c, protect_a, age_prop, contact_matrix, sim_num = 500) {
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
    mutate(titer_strain = unique(df_long$titer_strain)) %>%
    dplyr::arrange(estimator, time) %>%
    dplyr::select(estimator, time, everything())

  return(result)
}
