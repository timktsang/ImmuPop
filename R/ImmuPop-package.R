#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr mutate filter group_by summarise arrange select bind_rows %>%
#' @importFrom MCMCpack rdirichlet
#' @importFrom stats quantile density
#' @importFrom graphics par plot axis barplot legend polygon segments points abline
#' @importFrom grDevices adjustcolor rainbow pdf dev.off
## usethis namespace: end
NULL

# Suppress R CMD check NOTEs for dplyr column references
utils::globalVariables(c("time", "estimator", "CI_lwr", "CI_upr", "epi",
                         "value", "agegp1", "age", "raw_titer"))
