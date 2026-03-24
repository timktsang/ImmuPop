# S3 class for ImmuPop estimation results
# Wraps the data frame returned by estimation functions with pretty printing.

# Internal labels used by print/summary/table methods
.result_labels <- c(
  pop_immun = "Population immunity",
  RR_R0     = "Relative reduction in R0",
  GMT       = "Geometric mean titer",
  prop_5    = "Proportion seropositive"
)

# Constructor (internal) — attaches class + type attribute
.new_ImmuPop_result <- function(df, type = "timepoint") {
  structure(df, class = c("ImmuPop_result", "data.frame"), type = type)
}

#' Print ImmuPop estimation results
#'
#' Formats estimation results with human-readable labels and aligned columns.
#'
#' @param x An \code{ImmuPop_result} object returned by
#'   \code{\link{ImmuPop_est_timepoint}}, \code{\link{ImmuPop_est_baseline}},
#'   or \code{\link{ImmuPop_est_timeseries}}.
#' @param digits Number of significant digits (default 3).
#' @param ... Ignored.
#' @return Invisible \code{x}.
#' @export
print.ImmuPop_result <- function(x, digits = 3, ...) {
  type <- attr(x, "type") %||% "timepoint"
  type_label <- switch(type,
    timepoint  = "Immunity Estimates (single time point)",
    baseline   = "Baseline Immunity Comparison",
    timeseries = "Immunity Time Series",
    "Immunity Estimates"
  )

  cat(type_label, "\n")
  cat(strrep("-", nchar(type_label)), "\n")

  if (type == "timeseries") {
    # Compact summary for timeseries (too many rows to print individually)
    estimators <- unique(x$estimator)
    n_times <- length(unique(x$time))
    cat(sprintf("  %d time points, %d estimators\n\n", n_times, length(estimators)))
    for (est in estimators) {
      sub <- x[x$estimator == est, ]
      lbl <- .result_labels[est]
      if (is.na(lbl)) lbl <- est
      cat(sprintf("  %s:\n", lbl))
      cat(sprintf("    range: %s to %s (times %s to %s)\n",
                  formatC(min(sub$value), digits = digits, format = "f"),
                  formatC(max(sub$value), digits = digits, format = "f"),
                  min(sub$time), max(sub$time)))
    }
  } else if ("epi" %in% names(x)) {
    # Grouped (baseline) — show per estimator per epidemic
    estimators <- unique(x$estimator)
    for (est in estimators) {
      sub <- x[x$estimator == est, ]
      lbl <- .result_labels[est]
      if (is.na(lbl)) lbl <- est
      fmt <- if (est == "GMT") "f" else "f"
      dg  <- if (est == "GMT") 1 else digits
      cat(sprintf("\n  %s:\n", lbl))
      for (i in seq_len(nrow(sub))) {
        cat(sprintf("    Epidemic %s: %s (%s, %s)\n",
                    sub$epi[i],
                    formatC(sub$value[i], digits = dg, format = fmt),
                    formatC(sub$CI_lwr[i], digits = dg, format = fmt),
                    formatC(sub$CI_upr[i], digits = dg, format = fmt)))
      }
    }
  } else {
    # Ungrouped (timepoint) — one line per estimator
    cat("\n")
    for (i in seq_len(nrow(x))) {
      est <- x$estimator[i]
      lbl <- .result_labels[est]
      if (is.na(lbl)) lbl <- est
      dg  <- if (est == "GMT") 1 else digits
      cat(sprintf("  %-30s %s (%s, %s)\n",
                  lbl,
                  formatC(x$value[i], digits = dg, format = "f"),
                  formatC(x$CI_lwr[i], digits = dg, format = "f"),
                  formatC(x$CI_upr[i], digits = dg, format = "f")))
    }
  }
  cat("\n")
  invisible(x)
}

#' Summarize ImmuPop estimation results
#'
#' Returns a summary data frame with human-readable labels, formatted for
#' console display. The returned object prints as a clean table.
#'
#' @param object An \code{ImmuPop_result} object.
#' @param digits Number of significant digits (default 3).
#' @param ... Ignored.
#' @return A data frame with columns \code{Estimator}, \code{Estimate},
#'   \code{CI_lower}, \code{CI_upper}, and \code{Formatted} (a character
#'   column with \code{"estimate (lower, upper)"}).
#'   For grouped results, additional columns \code{Epidemic} or \code{Time}.
#' @export
summary.ImmuPop_result <- function(object, digits = 3, ...) {
  df <- as.data.frame(object)

  # Add readable labels
  df$Estimator <- vapply(df$estimator, function(e) {
    lbl <- .result_labels[e]
    if (is.na(lbl)) e else lbl
  }, character(1))

  # Format estimate string
  df$Formatted <- vapply(seq_len(nrow(df)), function(i) {
    dg <- if (df$estimator[i] == "GMT") 1 else digits
    sprintf("%s (%s, %s)",
            formatC(df$value[i], digits = dg, format = "f"),
            formatC(df$CI_lwr[i], digits = dg, format = "f"),
            formatC(df$CI_upr[i], digits = dg, format = "f"))
  }, character(1))

  # Build clean output
  if ("epi" %in% names(df)) {
    out <- data.frame(
      Estimator = df$Estimator,
      Epidemic  = df$epi,
      Estimate  = df$value,
      CI_lower  = df$CI_lwr,
      CI_upper  = df$CI_upr,
      Formatted = df$Formatted,
      stringsAsFactors = FALSE
    )
  } else if ("time" %in% names(df)) {
    out <- data.frame(
      Estimator = df$Estimator,
      Time      = df$time,
      Estimate  = df$value,
      CI_lower  = df$CI_lwr,
      CI_upper  = df$CI_upr,
      Formatted = df$Formatted,
      stringsAsFactors = FALSE
    )
  } else {
    out <- data.frame(
      Estimator = df$Estimator,
      Estimate  = df$value,
      CI_lower  = df$CI_lwr,
      CI_upper  = df$CI_upr,
      Formatted = df$Formatted,
      stringsAsFactors = FALSE
    )
  }
  rownames(out) <- NULL
  out
}

#' Format estimates as a publication-ready table
#'
#' Produces a formatted character table suitable for inclusion in manuscripts.
#' Each estimator is shown with its point estimate and 95\% CI in the format
#' \code{"estimate (lower, upper)"}. For grouped results (baseline or
#' timeseries), groups appear as columns.
#'
#' @param result An \code{ImmuPop_result} object, or a data frame returned
#'   by any of the three estimation functions.
#' @param digits Number of decimal places for proportion-scale estimates
#'   (default 3). GMT uses \code{digits - 2} (minimum 1).
#' @param caption Optional table caption (printed above the table).
#' @return Invisible character matrix. The table is printed to the console
#'   as a side effect.
#' @examples
#' \donttest{
#' data("ImmuPop_raw_data")
#' df <- generate_data(ImmuPop_raw_data, cut_age = c(0, 18, 100))
#' data_t <- df[df$time == 2, ]
#' result <- ImmuPop_est_timepoint(data_t,
#'   protect_c = c(0.1, 0.2, 0.3, 0.4, 0.5),
#'   protect_a = c(0.1, 0.2, 0.3, 0.4, 0.5),
#'   age_prop = c(0.2, 0.8),
#'   contact_matrix = matrix(c(22, 16, 28, 120), nrow = 2, byrow = TRUE),
#'   sim_num = 100, seed = 42)
#' summary_table(result)
#' }
#' @export
summary_table <- function(result, digits = 3, caption = NULL) {
  est_order <- c("pop_immun", "RR_R0", "GMT", "prop_5")
  labels <- .result_labels

  gmt_digits <- max(digits - 2, 1)

  .fmt <- function(est, val, lwr, upr) {
    dg <- if (est == "GMT") gmt_digits else digits
    sprintf("%s (%s, %s)",
            formatC(val, digits = dg, format = "f"),
            formatC(lwr, digits = dg, format = "f"),
            formatC(upr, digits = dg, format = "f"))
  }

  df <- as.data.frame(result)
  estimators <- intersect(est_order, unique(df$estimator))

  if ("epi" %in% names(df)) {
    # Wide format: rows = estimators, columns = epidemics
    groups <- sort(unique(df$epi))
    mat <- matrix("", nrow = length(estimators), ncol = length(groups) + 1)
    colnames(mat) <- c("Estimator", paste("Epidemic", groups))
    for (i in seq_along(estimators)) {
      est <- estimators[i]
      mat[i, 1] <- labels[est]
      for (j in seq_along(groups)) {
        row <- df[df$estimator == est & df$epi == groups[j], ]
        if (nrow(row) > 0)
          mat[i, j + 1] <- .fmt(est, row$value[1], row$CI_lwr[1], row$CI_upr[1])
      }
    }
  } else if ("time" %in% names(df)) {
    # Wide format: rows = estimators, columns = time points
    times <- sort(unique(df$time))
    mat <- matrix("", nrow = length(estimators), ncol = length(times) + 1)
    colnames(mat) <- c("Estimator", paste("Time", times))
    for (i in seq_along(estimators)) {
      est <- estimators[i]
      mat[i, 1] <- labels[est]
      for (j in seq_along(times)) {
        row <- df[df$estimator == est & df$time == times[j], ]
        if (nrow(row) > 0)
          mat[i, j + 1] <- .fmt(est, row$value[1], row$CI_lwr[1], row$CI_upr[1])
      }
    }
  } else {
    # Simple: rows = estimators, one Estimate column
    mat <- matrix("", nrow = length(estimators), ncol = 2)
    colnames(mat) <- c("Estimator", "Estimate (95% CI)")
    for (i in seq_along(estimators)) {
      est <- estimators[i]
      row <- df[df$estimator == est, ]
      mat[i, 1] <- labels[est]
      if (nrow(row) > 0)
        mat[i, 2] <- .fmt(est, row$value[1], row$CI_lwr[1], row$CI_upr[1])
    }
  }

  if (!is.null(caption)) cat(caption, "\n\n")

  # Print aligned table
  widths <- apply(rbind(colnames(mat), mat), 2, function(col) max(nchar(col)))
  header <- paste(mapply(function(h, w) formatC(h, width = w, flag = "-"), colnames(mat), widths), collapse = "  ")
  cat(header, "\n")
  cat(strrep("-", nchar(header)), "\n")
  for (i in seq_len(nrow(mat))) {
    row_str <- paste(mapply(function(v, w) formatC(v, width = w, flag = "-"), mat[i, ], widths), collapse = "  ")
    cat(row_str, "\n")
  }

  invisible(mat)
}

# %||% helper (base R doesn't have it before 4.x)
`%||%` <- function(a, b) if (is.null(a)) b else a
