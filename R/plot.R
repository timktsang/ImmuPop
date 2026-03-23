# Plot functions for ImmuPop

# Internal: display labels for the four estimators
.estimator_labels <- c(
  pop_immun = "Population immunity",
  RR_R0     = "Reduction in R0",
  GMT       = "Geometric mean titer",
  prop_5    = "Proportion seropositive (titer >= 10)"
)

#' Plot single time-point immunity estimates (forest plot)
#'
#' Visualizes the four estimates returned by \code{\link{ImmuPop_timet_est}} as
#' a forest plot. The three proportion-scale metrics (\code{pop_immun},
#' \code{RR_R0}, \code{prop_5}) share a 0--1 x-axis; the geometric mean titer
#' (\code{GMT}) is shown in a separate section with its own x-axis. Labels
#' appear on the left, point estimates with 95\% CI bars in the center, and
#' formatted values on the right.
#'
#' @param result Data frame returned by \code{\link{ImmuPop_timet_est}}. Must
#'   contain columns \code{estimator}, \code{value}, \code{CI_lwr},
#'   \code{CI_upr}.
#' @param file Optional file path (e.g. \code{"estimates.pdf"}). When
#'   specified, the plot is written to this file and the device is closed
#'   automatically.
#' @param width PDF width in inches (default 10).
#' @param height PDF height in inches. If \code{NULL}, auto-sized from the
#'   number of rows.
#' @param cex Base character expansion factor (default 0.85).
#' @param ... Additional graphical parameters passed to \code{plot()}.
#' @return Invisible \code{NULL}. Called for its side effect of producing plots.
#' @examples
#' \donttest{
#' data("ImmuPop_raw_data")
#' df   <- generate_data(ImmuPop_raw_data, cut_age = c(0, 18, 100))
#' data_t <- df[df$time == 2, ]
#' result <- ImmuPop_timet_est(
#'   df             = data_t,
#'   protect_c      = c(0.1, 0.2, 0.3, 0.4, 0.5),
#'   protect_a      = c(0.1, 0.2, 0.3, 0.4, 0.5),
#'   age_prop       = c(0.2, 0.8),
#'   contact_matrix = matrix(c(22, 16, 28, 120), nrow = 2, byrow = TRUE),
#'   sim_num        = 100, seed = 42
#' )
#' plot_estimates(result)
#' }
#' @export
plot_estimates <- function(result,
                           file = NULL, width = 10, height = NULL,
                           cex = 0.85, ...) {
  if (!all(c("estimator", "value", "CI_lwr", "CI_upr") %in% names(result)))
    stop("'result' must have columns: estimator, value, CI_lwr, CI_upr. ",
         "Use the output of ImmuPop_timet_est().", call. = FALSE)

  # Estimator display labels
  prop_ests <- c("pop_immun", "RR_R0", "prop_5")
  est_labels <- c(
    pop_immun = "Population immunity",
    RR_R0     = "Relative reduction in R0",
    prop_5    = "Proportion seropositive (>= 1:10)"
  )

  # Split into proportion vs titer groups
  prop_data <- result[result$estimator %in% prop_ests, ]
  prop_data <- prop_data[match(intersect(prop_ests, prop_data$estimator),
                               prop_data$estimator), ]
  gmt_data  <- result[result$estimator == "GMT", ]
  has_prop  <- nrow(prop_data) > 0
  has_gmt   <- nrow(gmt_data) > 0

  # Build row structure: list of list(type, label, ...)
  rows <- list()
  if (has_prop) {
    rows[[length(rows) + 1]] <- list(type = "section",
                                     label = "Proportion Estimates:")
    for (i in seq_len(nrow(prop_data))) {
      est <- prop_data$estimator[i]
      rows[[length(rows) + 1]] <- list(
        type = "estimate", group = "prop",
        label = est_labels[est],
        value = prop_data$value[i],
        lower = prop_data$CI_lwr[i],
        upper = prop_data$CI_upr[i]
      )
    }
  }
  if (has_gmt) {
    rows[[length(rows) + 1]] <- list(type = "section", label = "Titer:")
    rows[[length(rows) + 1]] <- list(
      type = "estimate", group = "gmt",
      label = "Geometric mean titer",
      value = gmt_data$value[1],
      lower = gmt_data$CI_lwr[1],
      upper = gmt_data$CI_upr[1]
    )
  }
  n_rows <- length(rows)

  # Axis ranges
  prop_xlim <- c(0, 1)
  if (has_gmt) {
    gmt_upper <- max(gmt_data$CI_upr[1] * 1.15,
                     gmt_data$value[1] * 1.3)
    gmt_xlim  <- c(0, gmt_upper)
  }

  # Layout constants (user coordinates)
  forest_left  <- 0
  forest_right <- 8
  label_x      <- forest_left - 8
  right_edge   <- forest_right + 6

  # Map value to forest-area x-coordinate
  .val_to_x <- function(val, group) {
    lims <- if (group == "prop") prop_xlim else gmt_xlim
    forest_left + (val - lims[1]) / diff(lims) *
      (forest_right - forest_left)
  }

  # Vertical layout: rows are 1 unit; axes take 1.5 units
  n_sections <- has_prop + has_gmt
  total_units <- n_rows + n_sections * 1.5

  # Open PDF device if file specified
  opened_device <- FALSE
  if (!is.null(file)) {
    if (is.null(height)) height <- 0.55 * total_units + 2
    grDevices::pdf(file, width = width, height = height)
    opened_device <- TRUE
  }

  op <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(op)
    if (opened_device) grDevices::dev.off()
  })
  graphics::par(mar = c(0, 0, 0, 0))

  # Assign y-positions top-down, inserting axis gaps after each section's
  # last estimate row
  y_pos  <- numeric(n_rows)
  axis_y <- list()
  cur_y  <- total_units

  for (i in seq_len(n_rows)) {
    cur_y <- cur_y - 1
    y_pos[i] <- cur_y
    row <- rows[[i]]
    if (row$type == "estimate") {
      is_last <- (i == n_rows) || (rows[[i + 1]]$type == "section")
      if (is_last) {
        axis_y[[row$group]] <- cur_y - 0.7
        cur_y <- cur_y - 1.5
      }
    }
  }

  y_top    <- total_units + 1.5
  y_bottom <- min(unlist(axis_y)) - 1.2

  graphics::plot(0, 0, type = "n",
                 xlim = c(label_x, right_edge),
                 ylim = c(y_bottom, y_top),
                 axes = FALSE, xlab = "", ylab = "", ...)

  # Column headers
  header_y <- total_units + 0.8
  graphics::text(label_x + 0.3, header_y, "Estimator",
                 adj = 0, font = 2, cex = cex)
  graphics::text(right_edge - 0.3, header_y, "Estimate",
                 adj = 1, font = 2, cex = cex)
  graphics::text(right_edge - 0.3, header_y - 0.5, "(95% CI)",
                 adj = 1, cex = cex * 0.85)

  # Draw rows
  for (i in seq_len(n_rows)) {
    row <- rows[[i]]
    y   <- y_pos[i]

    # Alternating row shading
    if (i %% 2 == 0) {
      graphics::rect(label_x, y - 0.5, right_edge, y + 0.5,
                     col = grDevices::rgb(0, 0, 0, 0.05), border = NA)
    }

    if (row$type == "section") {
      graphics::text(label_x + 0.3, y, row$label,
                     adj = 0, font = 2, cex = cex)

    } else if (row$type == "estimate") {
      # Label (indented)
      graphics::text(label_x + 1.5, y, row$label,
                     adj = 0, font = 1, cex = cex)

      # Point estimate and CI bar
      x_val <- .val_to_x(row$value, row$group)
      x_lo  <- .val_to_x(row$lower, row$group)
      x_hi  <- .val_to_x(row$upper, row$group)
      graphics::segments(x_lo, y, x_hi, y, lwd = 1.5)
      graphics::points(x_val, y, pch = 16, cex = 0.8)

      # Formatted estimate text (right-aligned)
      if (row$group == "prop") {
        est_text <- sprintf("%.3f (%.3f, %.3f)",
                            row$value, row$lower, row$upper)
      } else {
        est_text <- sprintf("%.1f (%.1f, %.1f)",
                            row$value, row$lower, row$upper)
      }
      graphics::text(right_edge - 0.3, y, est_text,
                     adj = 1, cex = cex * 0.9)
    }
  }

  # Draw x-axes for each section
  if (has_prop && !is.null(axis_y[["prop"]])) {
    ticks_val <- seq(0, 1, by = 0.25)
    ticks_x   <- vapply(ticks_val, function(v) .val_to_x(v, "prop"),
                        numeric(1))
    graphics::axis(1, at = ticks_x, labels = ticks_val,
                   pos = axis_y[["prop"]], cex.axis = cex * 0.85)
  }
  if (has_gmt && !is.null(axis_y[["gmt"]])) {
    ticks_val <- pretty(gmt_xlim, n = 5)
    ticks_val <- ticks_val[ticks_val >= gmt_xlim[1] &
                             ticks_val <= gmt_xlim[2]]
    ticks_x   <- vapply(ticks_val, function(v) .val_to_x(v, "gmt"),
                        numeric(1))
    graphics::axis(1, at = ticks_x, labels = ticks_val,
                   pos = axis_y[["gmt"]], cex.axis = cex * 0.85)
  }

  invisible(NULL)
}

#' Plot baseline estimates by epidemic group
#'
#' Visualizes estimates from \code{\link{ImmuPop_bsl_est}} as a dot-and-whisker
#' chart, comparing epidemic groups for each selected estimator.
#'
#' @param result Data frame returned by \code{\link{ImmuPop_bsl_est}}. Must
#'   contain columns \code{estimator}, \code{epi}, \code{value},
#'   \code{CI_lwr}, \code{CI_upr}.
#' @param estimators Character vector of estimators to plot. Allowed values:
#'   \code{"pop_immun"}, \code{"RR_R0"}, \code{"GMT"}, \code{"prop_5"}.
#'   Default: all estimators present in \code{result}.
#' @param col Colors for epi groups. Default: auto-assigned via
#'   \code{rainbow()}.
#' @param ... Additional graphical parameters passed to \code{plot()}.
#' @return Invisible \code{NULL}. Called for its side effect of producing plots.
#' @examples
#' \donttest{
#' data("ImmuPop_raw_data")
#' df     <- generate_data(ImmuPop_raw_data, cut_age = c(0, 18, 100))
#' df_bsl <- df[df$bsl == "yes", ]
#' result <- ImmuPop_bsl_est(
#'   df_long_bsl    = df_bsl,
#'   protect_c      = c(0.1, 0.2, 0.3, 0.4, 0.5),
#'   protect_a      = c(0.1, 0.2, 0.3, 0.4, 0.5),
#'   age_prop       = c(0.2, 0.8),
#'   contact_matrix = matrix(c(22, 16, 28, 120), nrow = 2, byrow = TRUE),
#'   sim_num        = 50, seed = 42
#' )
#' plot_baseline(result)
#' }
#' @export
plot_baseline <- function(result, estimators = NULL, col = NULL, ...) {
  if (!all(c("estimator", "epi", "value", "CI_lwr", "CI_upr") %in% names(result)))
    stop("'result' must have columns: estimator, epi, value, CI_lwr, CI_upr. ",
         "Use the output of ImmuPop_bsl_est().", call. = FALSE)

  all_ests <- c("pop_immun", "RR_R0", "GMT", "prop_5")
  if (is.null(estimators)) {
    estimators <- intersect(all_ests, result$estimator)
  } else {
    estimators <- match.arg(estimators, all_ests, several.ok = TRUE)
  }

  epi_groups <- sort(unique(result$epi))
  n_epi      <- length(epi_groups)

  if (is.null(col))
    col <- grDevices::rainbow(n_epi, s = 0.7, v = 0.85)

  n_panels <- length(estimators)
  n_cols   <- if (n_panels <= 2) n_panels else 2
  n_rows   <- ceiling(n_panels / n_cols)

  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op))
  graphics::par(mfrow = c(n_rows, n_cols),
                mar   = c(4.5, 4.5, 3, 1),
                mgp   = c(2.5, 0.7, 0))

  for (est in estimators) {
    sub    <- result[result$estimator == est, ]
    sub    <- sub[match(epi_groups, sub$epi), ]
    ylab   <- .estimator_labels[est]
    y_range <- range(c(sub$CI_lwr, sub$CI_upr), na.rm = TRUE)
    y_pad  <- diff(y_range) * 0.12
    ylim   <- c(max(0, y_range[1] - y_pad), y_range[2] + y_pad)

    x <- seq_len(n_epi)
    graphics::plot(x, sub$value,
                   pch = 16, col = col, cex = 1.5,
                   xlim = c(0.5, n_epi + 0.5), ylim = ylim,
                   xaxt = "n", xlab = "Epidemic", ylab = ylab,
                   main = ylab, ...)
    graphics::axis(1, at = x, labels = as.character(epi_groups), las = 2)
    for (i in seq_len(n_epi)) {
      graphics::segments(x[i], sub$CI_lwr[i], x[i], sub$CI_upr[i],
                         col = col[i], lwd = 2)
    }
    graphics::abline(h = 0, col = "grey80", lty = 2)
  }

  invisible(NULL)
}

#' Plot titer distribution by age group
#'
#' Shows the distribution of raw antibody titers by age group as a grouped bar
#' chart. Each bar represents the proportion of individuals in a titer
#' category (standard HAI doubling dilutions: \eqn{<10, 10, 20, \ldots,
#' \geq 640}) for one age group.
#'
#' For multi-panel layouts (e.g. one panel per antigen), subset the data
#' externally and call this function once per panel inside
#' \code{par(mfrow = ...)}.
#'
#' @param df Data frame output of \code{\link{generate_data}}. Must contain
#'   columns \code{raw_titer} and \code{agegp1}.
#' @param main Plot title. Default: \code{"Titer distribution"}.
#' @param col Colors for age groups. Default: auto-assigned via
#'   \code{rainbow()}.
#' @param ... Additional graphical parameters passed to \code{barplot()}.
#' @return Invisible proportion matrix with age groups as rows and titer
#'   categories as columns.
#' @examples
#' \donttest{
#' data("ImmuPop_raw_data")
#' df <- generate_data(ImmuPop_raw_data, cut_age = c(0, 18, 100))
#' plot_titer_dist(df)
#' }
#' @export
plot_titer_dist <- function(df, main = "Titer distribution", col = NULL, ...) {
  if (!all(c("raw_titer", "agegp1") %in% names(df)))
    stop("'df' must have columns: raw_titer, agegp1. ",
         "Use the output of generate_data().", call. = FALSE)

  age_groups <- sort(unique(df$agegp1))
  n_groups   <- length(age_groups)

  if (is.null(col))
    col <- grDevices::rainbow(n_groups, s = 0.6, v = 0.9)

  # Standard HAI doubling-dilution categories
  breaks <- c(0, 10, 20, 40, 80, 160, 320, 640, Inf)
  labels <- c("<10", "10", "20", "40", "80", "160", "320", ">=640")

  prop_matrix <- matrix(0, nrow = n_groups, ncol = length(labels),
                        dimnames = list(as.character(age_groups), labels))
  for (i in seq_len(n_groups)) {
    titers <- df$raw_titer[df$agegp1 == age_groups[i]]
    counts <- table(cut(titers, breaks = breaks, labels = labels,
                        right = FALSE, include.lowest = TRUE))
    prop_matrix[i, ] <- as.numeric(counts) / sum(as.numeric(counts))
  }

  y_max <- max(prop_matrix, na.rm = TRUE)

  # Only save/restore mar and mgp so caller's mfrow is preserved
  old_mar <- graphics::par("mar")
  old_mgp <- graphics::par("mgp")
  on.exit(graphics::par(mar = old_mar, mgp = old_mgp))
  graphics::par(mar = c(4.5, 4.5, 3, 7),
                mgp = c(2.5, 0.7, 0))

  graphics::barplot(prop_matrix,
                    beside    = TRUE,
                    col       = col,
                    names.arg = labels,
                    xlab      = "Raw titer",
                    ylab      = "Proportion",
                    main      = main,
                    ylim      = c(0, y_max * 1.25),
                    ...)

  graphics::abline(h = 0, lwd = 1)

  # Place legend in the right margin to avoid overlapping bars
  graphics::legend("topright",
                   legend = as.character(age_groups),
                   fill   = col,
                   title  = "Age group",
                   bty    = "n",
                   cex    = 0.8,
                   inset  = c(-0.18, 0),
                   xpd    = TRUE)

  invisible(prop_matrix)
}

#' Jitter plot of individual titers by age group
#'
#' Shows individual HAI titers as jittered points by age group, overlaid with
#' the geometric mean titer (GMT, black dot) and 95\% log-normal confidence
#' interval (vertical bar). Dashed reference lines mark the seropositive
#' threshold (red, default 1:10) and seroprotection threshold (green, default
#' 1:40). GMT values are printed above each age group.
#'
#' This replicates the standard titer-by-age jitter plot used in influenza
#' seroprevalence publications. For multi-panel layouts (e.g. one panel per
#' antigen), subset the data externally and call this function once per panel
#' inside \code{par(mfrow = ...)}.
#'
#' @param df Data frame output of \code{\link{generate_data}}. Must contain
#'   columns \code{raw_titer} and \code{agegp1}.
#' @param main Plot title. Default: \code{""}.
#' @param threshold_low Raw titer value for the lower reference line (red
#'   dashed). Default: 10 (seropositive, 1:10). Set to \code{NULL} to hide.
#' @param threshold_high Raw titer value for the upper reference line (green
#'   dashed). Default: 40 (seroprotection, 1:40). Set to \code{NULL} to hide.
#' @param show_gmt Logical; whether to print GMT values above each age group.
#'   Default: \code{TRUE}.
#' @param col_points Color for individual titer points. Default:
#'   semi-transparent blue.
#' @param seed Optional integer seed for reproducible jitter. Default:
#'   \code{NULL}.
#' @param ... Additional graphical parameters.
#' @return Invisible data frame with columns \code{agegp}, \code{n},
#'   \code{GMT}, \code{CI_lwr}, \code{CI_upr}.
#' @examples
#' \donttest{
#' data("ImmuPop_raw_data")
#' df <- generate_data(ImmuPop_raw_data, cut_age = c(0, 18, 100))
#' plot_titer_jitter(df)
#' }
#' @export
plot_titer_jitter <- function(df, main = "",
                              threshold_low = 10, threshold_high = 40,
                              show_gmt = TRUE, col_points = NULL,
                              seed = NULL, ...) {
  if (!all(c("raw_titer", "agegp1") %in% names(df)))
    stop("'df' must have columns: raw_titer, agegp1. ",
         "Use the output of generate_data().", call. = FALSE)

  if (!is.null(seed)) set.seed(seed)

  if (is.null(col_points))
    col_points <- grDevices::rgb(0, 0, 1, 0.35)

  # Titer-to-y transform (log2 scale matching HAI doubling dilutions)
  titer_to_y <- function(t) log2(t / 5) + 1

  # Standard HAI titer levels for y-axis
  titer_vals   <- c(5, 10, 20, 40, 80, 160, 320, 640, 1280)
  titer_labels <- c("<1:10", "1:10", "1:20", "1:40", "1:80",
                    "1:160", "1:320", "1:640", "1:1280")
  titer_y      <- titer_to_y(titer_vals)  # 1 through 9

  age_groups <- sort(unique(df$agegp1))
  n_groups   <- length(age_groups)

  # Y range: accommodate data range + room for GMT text
  y_data_max <- max(titer_to_y(df$raw_titer), na.rm = TRUE)
  y_max      <- max(y_data_max + 1.5, 10)

  # Only save/restore mar and mgp so caller's mfrow is preserved
  old_mar <- graphics::par("mar")
  old_mgp <- graphics::par("mgp")
  on.exit(graphics::par(mar = old_mar, mgp = old_mgp))
  graphics::par(mar = c(2.5, 2.7, 2, 0.5),
                mgp = c(1.5, 0.5, 0))

  # Empty plot frame
  graphics::plot(0, 0, type = "n", axes = FALSE,
                 xlim = c(0, n_groups + 1), ylim = c(0, y_max),
                 xlab = "", ylab = "", ...)

  # Title
  graphics::title(main, cex.main = 0.9, adj = 0)

  # X-axis: group labels
  graphics::axis(1, at = 0:(n_groups + 1), labels = NA, pos = 0, tck = 0)
  graphics::axis(1, at = seq_len(n_groups),
                 labels = as.character(age_groups),
                 cex.axis = 0.95, pos = 0, tick = FALSE)
  graphics::mtext("Age group", side = 1, cex = 0.8, line = 1.3,
                  at = (n_groups + 1) / 2)

  # Y-axis: HAI titer levels
  y_show <- titer_y[titer_y <= y_max]
  l_show <- titer_labels[titer_y <= y_max]
  graphics::axis(2, at = y_show, labels = NA, pos = 0, las = 1, tck = 0)
  graphics::axis(2, at = y_show, labels = l_show,
                 cex.axis = 0.95, pos = 0, las = 1, tick = FALSE)
  graphics::mtext("HAI titre", side = 2, cex = 0.8, line = 1.5,
                  at = mean(range(y_show)))

  # Per-age-group: jitter points + GMT + CI
  gmt_df <- data.frame(agegp  = character(n_groups),
                       n      = integer(n_groups),
                       GMT    = numeric(n_groups),
                       CI_lwr = numeric(n_groups),
                       CI_upr = numeric(n_groups),
                       stringsAsFactors = FALSE)

  for (j in seq_len(n_groups)) {
    titers <- df$raw_titer[df$agegp1 == age_groups[j]]
    n_j    <- length(titers)

    # Jittered individual points
    jx <- jitter(rep(j, n_j), amount = 0.2)
    jy <- jitter(titer_to_y(titers), amount = 0.2)
    graphics::points(jx, jy, pch = 16, col = col_points, cex = 0.6)

    # GMT and log-normal 95% CI
    log_t    <- log(titers)
    gmt_val  <- exp(mean(log_t))
    log_se   <- stats::sd(log_t) / sqrt(n_j)
    ci_lwr   <- exp(mean(log_t) - 1.96 * log_se)
    ci_upr   <- exp(mean(log_t) + 1.96 * log_se)

    # GMT point + CI bar
    graphics::points(j, titer_to_y(gmt_val),
                     pch = 16, col = "black", cex = 0.75)
    graphics::segments(j, titer_to_y(ci_lwr), j, titer_to_y(ci_upr),
                       col = "black", lwd = 1)

    gmt_df[j, ] <- list(as.character(age_groups[j]), n_j,
                         gmt_val, ci_lwr, ci_upr)
  }

  # Reference lines
  if (!is.null(threshold_low))
    graphics::lines(c(0, n_groups + 1),
                    rep(titer_to_y(threshold_low), 2),
                    col = "red", lty = 2)
  if (!is.null(threshold_high))
    graphics::lines(c(0, n_groups + 1),
                    rep(titer_to_y(threshold_high), 2),
                    col = "green", lty = 2)

  # GMT values printed above each group
  if (show_gmt) {
    gmt_text_y <- y_max - 0.3
    for (j in seq_len(n_groups)) {
      graphics::text(j, gmt_text_y,
                     format(round(gmt_df$GMT[j], 1), nsmall = 1),
                     cex = 0.9)
    }
    graphics::text(n_groups + 0.7, gmt_text_y, "(GMT)", cex = 0.9)
  }

  invisible(gmt_df)
}
