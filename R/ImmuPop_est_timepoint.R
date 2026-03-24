#' Estimate Population Immunity, Reduction in R0, GMT, and Proportion of Seropositive Individuals
#'
#' This function estimates population immunity, the relative reduction in transmissibility (R0),
#' the geometric mean titer (GMT), and the proportion of individuals with detectable antibody
#' (raw_titer >= 10) using bootstrap sampling and MCMC-based Dirichlet estimation.
#' It considers multiple age groups and various parameters related to immunity.
#'
#' @param df Data frame containing individual data, including age group and raw titer values.
#'   Must have been processed by \code{\link{generate_data}} first (requires \code{agegp1},
#'   \code{raw_titer}, and \code{titer_level} columns).
#' @param protect_c Numeric vector of protection effect for children (one value per titer level).
#' @param protect_a Numeric vector of protection effect for adults (one value per titer level).
#' @param age_prop Numeric vector representing age group proportions in the population (must sum to 1).
#' @param contact_matrix Numeric square matrix of contact frequencies between age groups.
#'   Dimensions must match the length of \code{age_prop}.
#' @param sim_num Number of bootstrap simulations (default = 500).
#' @param seed Optional integer seed for reproducibility (default = NULL).
#' @return A dataframe with immunity estimates, including the median and 95% CI for
#'   population immunity, R0 reduction, GMT, and seropositive proportion.
#' @export
#' @examples
#' data("ImmuPop_raw_data")
#' data_example <- generate_data(ImmuPop_raw_data, cut_age = c(0, 18, 50, 100))
#'
#' # Define age group proportions
#' age_prop <- c(0.2, 0.4, 0.4)
#'
#' # Define contact matrix
#' contact_matrix <- matrix(c(22, 16, 15, 24, 28, 30, 18, 32, 35), nrow = 3, byrow = TRUE)
#'
#' # Define protection effects for children and adults
#' protect_c <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
#' protect_a <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
#'
#' # Data at a specific time point (e.g., time == 15)
#' data_example_t <- data_example[data_example$time == 15, ]
#'
#' # Run the estimation function
#' result <- ImmuPop_est_timepoint(
#'   df = data_example_t,
#'   protect_c = protect_c,
#'   protect_a = protect_a,
#'   age_prop = age_prop,
#'   contact_matrix = contact_matrix,
#'   sim_num = 100,
#'   seed = 42
#' )
#'
#' print(result)
ImmuPop_est_timepoint <- function(df, protect_c, protect_a, age_prop, contact_matrix, sim_num = 500, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Input validation
  validate_estimation_inputs(df, protect_c, protect_a, age_prop, contact_matrix, sim_num)

  # Perform bootstrap sampling and calculate weighted GMT and proportion seropositive
  bootstrap_res <- replicate(sim_num, {
    resampled_list <- sapply(sort(unique(df$agegp1)), function(myagegp) {
      agegp1_titer <- df$raw_titer[df$agegp1 == myagegp]
      sample(agegp1_titer, size = length(agegp1_titer), replace = TRUE)
    }, simplify = FALSE)

    # Reshape the sampled data back into a data frame
    resampled_df <- data.frame(raw_titer = unlist(resampled_list),
                               agegp1 = rep(sort(unique(df$agegp1)), as.numeric(table(df$agegp1))))

    gmt_value <- weighted_gmt(resampled_df, age_prop)
    prop_morethan5_value <- weighted_prop_HImorethan5(resampled_df, age_prop)

    c(gmt_value, prop_morethan5_value)
  }) %>%
    t() %>%
    as.data.frame()

  colnames(bootstrap_res) <- c("GMT", "Prop_5")

  # Calculate confidence intervals (95%) for GMT and proportion seropositive
  gmt_ci <- quantile(bootstrap_res$GMT, c(0.025, 0.5, 0.975))
  prop5_ci <- quantile(bootstrap_res$Prop_5, c(0.025, 0.5, 0.975))

  # Protection effects
  z1 <- protect_c
  z2 <- protect_a
  k <- seq_len(length(z1))

  agegp_num <- length(age_prop)

  imp_my_virus <- matrix(NA, ncol = agegp_num, nrow = sim_num)
  table_matrix <- table(df$agegp1, df$titer_level) %>% as.matrix()
  myvirus_age_titer_Freq <- fill_matrix(table_matrix, n_row = agegp_num, n_col = length(z1))

  # MCMC-based immune proportion estimation
  for (i in 1:sim_num) {
    for (j in 1:agegp_num) {
      my_virus_freq_age_j <- rdirichlet(sim_num, myvirus_age_titer_Freq[j, ] + 0.01)
      if (j <= 1) {
        imp_my_virus[i, j] <- sum(my_virus_freq_age_j[i, k] * z1[k])
      } else {
        imp_my_virus[i, j] <- sum(my_virus_freq_age_j[i, k] * z2[k])
      }
    }
  }

  # Compute population immunity and R0 reduction
  imp_my_virus_pop <- rowSums(imp_my_virus * age_prop)
  sus_my_virus <- 1 - imp_my_virus

  r0 <- max(eigen(contact_matrix)$values)
  my_virus_re <- apply(sus_my_virus, 1, function(x) max(Re(eigen(x * contact_matrix)$values)))
  my_virus_reduct_trans <- 1 - my_virus_re / r0

  # Store results
  my_virus_results <- data.frame(
    "pop_immun" = imp_my_virus_pop,
    "RR_R0" = my_virus_reduct_trans
  )

  # Compute confidence intervals for population immunity and R0 reduction
  pop_immune_prop <- quantile(my_virus_results$pop_immun, c(0.5, 0.025, 0.975))
  RR_R0 <- quantile(my_virus_results$RR_R0, c(0.5, 0.025, 0.975))

  # Create final result data frame
  estimate_res <- data.frame(
    estimator = c("pop_immun", "RR_R0", "GMT", "prop_5"),
    value = c(pop_immune_prop[1], RR_R0[1], gmt_ci[2], prop5_ci[2]),
    CI_lwr = c(pop_immune_prop[2], RR_R0[2], gmt_ci[1], prop5_ci[1]),
    CI_upr = c(pop_immune_prop[3], RR_R0[3], gmt_ci[3], prop5_ci[3])
  )

  .new_ImmuPop_result(estimate_res, type = "timepoint")
}
