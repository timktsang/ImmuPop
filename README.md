<!-- This README is maintained directly. Do not regenerate from README.Rmd. -->

# ImmuPop

<!-- badges: start -->
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

**ImmuPop** estimates population immunity from individual serology data using a Bayesian simulation framework. Given individual antibody titers, age-specific protection curves, population age structure, and a contact matrix, it produces four key metrics: geometric mean titer (GMT), proportion seropositive, population immunity, and relative reduction in R0. The MCMC backend uses MCMCpack for Dirichlet-multinomial sampling of titer distributions.

## Features

- **Four immunity estimators** — GMT, proportion seropositive (titer >= 10), population immunity, and relative reduction in R0
- **Three estimation modes** — single time point (`ImmuPop_est_timepoint`), time series (`ImmuPop_est_timeseries`), and pre-epidemic baseline by epidemic group (`ImmuPop_est_baseline`)
- **Age-structured framework** — age-specific protection curves, population proportions, and contact matrices
- **Publication-ready plots** — titer jitter plots with GMT/CI, titer distributions, immunity dot-and-whisker charts, baseline comparisons
- **Flexible age grouping** — user-defined age cuts via `generate_data()`
- **Reproducible** — optional `seed` parameter on all estimation functions

## Installation

```r
# install.packages("devtools")
devtools::install_github("timktsang/ImmuPop")
```

## Interactive web app

No R experience needed — run ImmuPop directly in your browser:

```r
ImmuPop::launch_app()
```

Upload a CSV, configure parameters via the web interface, and download results. See [Input data format](#input-data-format) for the required CSV columns. Install extra dependencies first: `install.packages(c("shiny", "DT"))`.

## Quick start

### Step 1: Load your data

Replace `ImmuPop_raw_data` with your own data frame or CSV file. Your data must have at minimum `age` (numeric, years) and `raw_titer` (numeric, HAI titer) columns. See [Input data format](#input-data-format) for optional columns that enable baseline comparison and time series analysis.

```r
library(ImmuPop)

# Using the bundled example dataset:
data("ImmuPop_raw_data")

# Or load your own CSV:
# my_data <- read.csv("my_serology_data.csv")
```

### Step 2: Define age groups

`generate_data()` adds age group labels and titer levels to your data. Choose breakpoints that match the age groups in your population parameters. For example, `c(0, 18, 50, 100)` creates three groups: children [0,18), adults [18,50), and older adults [50,100).

```r
df <- generate_data(ImmuPop_raw_data, cut_age = c(0, 18, 50, 100))
```

### Step 3: Set population parameters

These parameters describe the **target population**, not your sample:

- **`age_prop`** — Fraction of the total population in each age group (from census data; must sum to 1).
- **`contact_matrix`** — Average daily contacts between age groups (from social contact surveys such as [POLYMOD](https://doi.org/10.1371/journal.pmed.0050074) or the [socialmixr](https://cran.r-project.org/package=socialmixr) R package). Row *i*, column *j* = contacts a person in group *i* has with people in group *j*.
- **`protect_c`, `protect_a`** — Probability of protection at each antibody titer level, for the first age group (children) and remaining groups (adults), respectively. Values should increase from low to high titer. The number of values must match the number of distinct titer levels in your data.

```r
age_prop       <- c(0.2, 0.4, 0.4)
contact_matrix <- matrix(c(22, 16, 15, 24, 28, 30, 18, 32, 35),
                         nrow = 3, byrow = TRUE)
protect_c      <- c(0.1, 0.2, 0.3, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75)
protect_a      <- c(0.1, 0.2, 0.3, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75)
```

### Step 4: Estimate immunity

Select a single time point from your data and run the estimation. `sim_num` controls the number of bootstrap simulations (higher = more precise CIs). Use `seed` for reproducible results.

```r
data_t <- df[df$time == 2, ]
result <- ImmuPop_est_timepoint(data_t, protect_c, protect_a,
                            age_prop, contact_matrix,
                            sim_num = 1000, seed = 42)
result
#>   estimator     value    CI_lwr     CI_upr
#> 1 pop_immun 0.2180654 0.1598600  0.3537494
#> 2     RR_R0 0.2311639 0.1555847  0.3595227
#> 3       GMT 6.1849899 3.0924949 10.2891499
#> 4    prop_5 0.3000000 0.1000000  0.6000000
```

## Visualization

### Titer jitter plot (by age group)

Replicates the standard HAI titer-by-age plot used in influenza seroprevalence publications. Individual titers shown as jittered points, with GMT (black dot) and 95% CI. Dashed lines mark seropositive (red, 1:10) and seroprotection (green, 1:40) thresholds.

```r
plot_titer_jitter(df, main = "HAI titer by age group", seed = 42)
```

<img src="man/figures/titer_jitter.png" width="700"/>

For multi-panel layouts (e.g. one panel per antigen), use `par(mfrow)` and call once per panel:

```r
par(mfrow = c(3, 1))
plot_titer_jitter(df_antigen1, main = "A. Antigen 1", seed = 42)
plot_titer_jitter(df_antigen2, main = "B. Antigen 2", seed = 42)
plot_titer_jitter(df_antigen3, main = "C. Antigen 3", seed = 42)
```

### Titer distribution

```r
plot_titer_dist(df, main = "Titer distribution by age group")
```

<img src="man/figures/titer_dist.png" width="700"/>

### Immunity estimates (single time point)

Estimate population immunity from a single survey round — e.g. a cross-sectional serosurvey at one point in time. This gives a snapshot of how immune the population is right now.

```r
data_t <- df[df$time == 2, ]
result <- ImmuPop_est_timepoint(data_t, protect_c, protect_a,
                            age_prop, contact_matrix,
                            sim_num = 1000, seed = 42)
plot_estimates(result)
```

<img src="man/figures/estimates.png" width="650"/>

### Baseline comparison across epidemics

Compare pre-epidemic immunity across multiple epidemic waves — e.g. to assess whether the population started each flu season with different levels of protection. `ImmuPop_est_baseline()` runs the same estimation separately for each epidemic group, so you can see how starting immunity changed from one wave to the next.

```r
df_bl  <- df[df$baseline == "yes", ]
res_bl <- ImmuPop_est_baseline(df_bl, protect_c, protect_a,
                           age_prop, contact_matrix,
                           sim_num = 1000, seed = 42)
plot_estimates(res_bl)
```

<img src="man/figures/baseline.png" width="650"/>

### Timeseries

Track how population immunity evolves over time across multiple survey rounds. For time series results (many time points), `plot_estimates()` automatically switches to a multi-panel line plot with 95% CI ribbon. Gaps between survey rounds are detected and drawn as separate segments.

```r
res_ts <- ImmuPop_est_timeseries(df, protect_c, protect_a,
                                  age_prop, contact_matrix,
                                  sim_num = 1000, seed = 42)
plot_estimates(res_ts)
```

<img src="man/figures/timeseries.png" width="700"/>

## Estimation modes

| Function | Use case | Input | Groups by |
|----------|----------|-------|-----------|
| `ImmuPop_est_timepoint()` | Snapshot from one survey round | Single time point subset | — (ungrouped) |
| `ImmuPop_est_baseline()` | Compare pre-epidemic immunity across waves | Baseline samples (`baseline == "yes"`) | Epidemic (`epi`) |
| `ImmuPop_est_timeseries()` | Track immunity over time | Full longitudinal data | Time point (`time`) |

All three return a data frame with columns `estimator`, `value`, `CI_lwr`, `CI_upr` (plus `epi` or `time` for grouped modes), and all work with `plot_estimates()`.

## Input data format

The input data frame must contain these columns:

| Column | Description |
|--------|-------------|
| `uid` | Individual identifier |
| `baseline` | Baseline indicator (`"yes"` / `"no"`) |
| `epi` | Epidemic identifier (integer) |
| `age` | Age in years |
| `time` | Time point identifier |
| `raw_titer` | HAI titer value (e.g. 5, 10, 20, 40, ...) |

Use `generate_data(raw_data, cut_age = c(0, 18, 50, 100))` to add age group and titer level columns.

## Population parameters

These describe the **target population** (not your study sample) and must be obtained from external sources:

| Parameter | Description | Source |
|-----------|-------------|--------|
| `age_prop` | Population proportion in each age group (must sum to 1) | Census / demographic data |
| `contact_matrix` | Square matrix of average daily contacts between age groups | Social contact surveys (e.g. [POLYMOD](https://doi.org/10.1371/journal.pmed.0050074), [socialmixr](https://cran.r-project.org/package=socialmixr)) |
| `protect_c` | Protection probability at each titer level for the first age group (children) | Literature / dose-response curves |
| `protect_a` | Protection probability at each titer level for remaining age groups (adults) | Literature / dose-response curves |

The number of values in `protect_c` and `protect_a` must match the number of distinct titer levels in your data (determined by the HAI doubling dilution scale).

## Citation

Xiong W, et al. A Framework for Measuring Population Immunity Against Influenza Using Individual Antibody Titers. *(in preparation)*

## Development

Code development assisted by AI tools (Claude, Anthropic; Codex, OpenAI).
