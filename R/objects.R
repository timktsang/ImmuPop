#' Example Data for Baseline Immunity
#'
#' A data frame that contains example data for baseline immunity estimation.
#' It includes information about time points, baseline status, age groups, and raw titers.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{uid}{Numeric, subject id.}
#'   \item{time}{Numeric, representing the time point.}
#'   \item{epi}{Character, epidemics}
#'   \item{baseline}{Character, indicating baseline status ("yes" or "no").}
#'   \item{raw_titer}{Numeric, raw titer values for the immunity estimation.}
#'   \item{age}{Numeric, age of subject.}
#' }
#' @export
ImmuPop_raw_data <- data.frame(
  uid = c(1001, 1001, 1002, 1002, 1003, 1003, 1004, 1004, 1005, 1005, 1006, 1006),
  time = c(1, 15, 1, 15, 1, 15, 801, 815, 801, 815, 801, 815),
  epi = factor(c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2)),
  baseline = c("yes", "no", "yes", "no", "yes", "no", "yes", "no", "yes", "no", "yes", "no"),
  raw_titer = c(5, 40, 10, 160, 10, 20, 5, 80, 10, 160, 20, 80),
  age = c(10, 10, 25, 25, 55, 55, 14, 14, 40, 40, 64, 64)
)


#' Age Proportions in the Population
#'
#' A vector representing the proportion of individuals in each age group for the population.
#' This is used for calculating immunity estimates for children and adults.
#'
#' @format A numeric vector of length p, representing the age group proportions.
#' @export
age_prop <- c(0.2, 0.8)

#' Contact Matrix Between Age Groups
#'
#' A matrix representing the contact rates between different age groups in the population.
#' This matrix is used to estimate population immunity and disease transmission dynamics.
#'
#' @format A numeric matrix with dimensions [p x p] where rows and columns represent age groups.
#' @export
contact_matrix <- matrix(c(22, 16, 28, 120), nrow = 2, byrow = TRUE)



#' Protection Effect in the Population
#'
#' A vector representing the protection effect for children and adults.
#' This is used for immunity estimation.
#'
#' @format A numeric vector of length k, each represent protection effect from k-th antibody titer level,
#' for children.
#' @export
protect_c <- c(0.1, 0.2, 0.3, 0.4, 0.5)



#' Protection Effect in the Population
#'
#' A vector representing the protection effect for children and adults.
#' This is used for immunity estimation.
#'
#' @format A numeric vector of length k, each represent protection effect from k-th antibody titer level,
#' for adult.
#' @export
protect_a <- c(0.1, 0.2, 0.3, 0.4, 0.5)
