#' Generate Example Data from Raw Immunity Data
#'
#' This function takes raw immunity data (including individual age and raw titer values),
#' and generates a data frame with additional columns: age group and titer level.
#' The age group is categorized based on the provided `cut_age` vector.
#' The titer level is calculated using the formula `log2(raw_titer/5) + 1`.
#' @name generate_data
#' @param ImmuPop_raw_data A data frame containing individual raw data, which must have:
#'   - `age`: individual age
#'   - `raw_titer`: raw titer values.
#' @param cut_age A numeric vector indicating the breakpoints to categorize ages.
#'   For example, `cut_age = c(0, 18, 60, 100)` would create the following age groups:
#'   - [0, 18), [18, 60), [60, 100).
#'
#' @return A data frame containing the following columns:
#'   - `agegp1`: age group based on the cut points from `cut_age`.
#'   - `titer_level`: log2 transformed titer level calculated as `log2(raw_titer / 5) + 1`.
#'
#' @export
#' @examples
#' # Example usage of generate_data with a vector for cut_age
#' raw_data <- data.frame(
#'   age = c(5, 15, 25, 40, 60),
#'   raw_titer = c(10, 50, 80, 100, 200)
#' )
#' processed_data <- generate_data(raw_data, cut_age = c(0, 18, 60, 100))
#' print(processed_data)
#'
generate_data <- function(ImmuPop_raw_data, cut_age) {
  data_example <- ImmuPop_raw_data %>%
    mutate(
      agegp1 = cut(age, breaks = c(cut_age), right = FALSE),
      titer_level = log2(raw_titer / 5) + 1
    )
  return(data_example)
}
