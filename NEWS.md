# ImmuPop 0.1.1

## New features

* Four new plot functions for visualizing estimation results:
  - `plot_estimates()`: forest plot (timepoint/baseline) or multi-panel line
    plot (timeseries) with 95% CI, auto-detected from input type.
  - `plot_titer_dist()`: grouped bar chart of raw titer distributions by age
    group (HAI doubling dilutions).
  - `plot_titer_jitter()`: jitter plot of individual titers by age group with
    GMT overlay and seropositive/seroprotection threshold lines.

* Estimation functions renamed for clarity:
  - `ImmuPop_timet_est()` → `ImmuPop_est_timepoint()`
  - `ImmuPop_bsl_est()` → `ImmuPop_est_baseline()`
  - `ImmuPop_timeseries_est()` → `ImmuPop_est_timeseries()`
  Old names are retained as deprecated aliases.

* Added `seed` parameter to all three estimation functions for reproducible
  bootstrap/MCMC sampling.

* Added input validation with informative error messages (empty data, age-group
  mismatches).

## Bug fixes

* Fixed `sapply()` matrix-collapse bug in `ImmuPop_est_timepoint()` when age
  groups had equal sample sizes (`simplify = FALSE`).

* Fixed toy `ImmuPop_raw_data` fixture to use realistic time values aligned
  with bundled `.rda` data.

## Infrastructure

* Removed unused `rlist` and `Matrix` dependencies.
* Modernized `DESCRIPTION` with `Authors@R` format.
* Added comprehensive test suite (100 tests via testthat).
* Added `R/validation.R` for shared input-checking helpers.

# ImmuPop 0.1.0

* Initial release accompanying Xiong et al. (accepted, *The Lancet Infectious
  Diseases*).
* Core estimation functions for population immunity, relative reduction in R0,
  geometric mean titer, and proportion seropositive.
* Bootstrap and MCMC-based Dirichlet estimation with age-structured contact
  matrices.
