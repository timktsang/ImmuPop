#' Calculate the geometric mean of titer values
#'
#' This function calculates the geometric mean for raw titer values.
#'
#' @param raw_titer A numeric vector of raw titer values
#' @return The geometric mean of the titer values
#' @export
geo_mean <- function(raw_titer) {
  return(exp(mean(log(raw_titer), na.rm = TRUE)))
}


#' Calculate the weighted geometric mean for the population
#'
#' This function calculates the weighted geometric mean across different age groups.
#'
#' @param resampled_df A dataframe containing raw titer values and age group information
#' @param age_prop A vector of age group proportions
#' @return The weighted geometric mean for the population
#' @export
weighted_gmt <- function(resampled_df, age_prop) {
  geo_means <- sapply(unique(resampled_df$agegp1), function(agegp) {
    agegp_resampled_titer <- resampled_df$raw_titer[resampled_df$agegp1 == agegp]
    geo_mean(agegp_resampled_titer)
  })

  weighted_gmt_value <- exp(sum(age_prop * log(geo_means)))
  return(weighted_gmt_value)
}

#' Calculate the weighted proportion of individuals with titer more than 5
#'
#' This function calculates the weighted proportion of individuals with a titer value greater than or equal to 10.
#'
#' @param resampled_df A dataframe containing raw titer values and age group information
#' @param age_prop A vector of age group proportions
#' @return The weighted proportion for the population
#' @export
weighted_prop_HImorethan5 <- function(resampled_df, age_prop) {
  prop_eachage <- sapply(unique(resampled_df$agegp1), function(agegp) {
    agegp_resampled_titer <- resampled_df$raw_titer[resampled_df$agegp1 == agegp]
    prop_morethan5 <- sum(agegp_resampled_titer >= 10) / length(agegp_resampled_titer)
    return(prop_morethan5)
  })

  weighted_prop_value <- sum(prop_eachage * age_prop)
  return(weighted_prop_value)
}
