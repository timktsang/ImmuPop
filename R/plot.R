# Plot functions for ImmuPop

# Internal: display labels for the four estimators
.estimator_labels <- c(
  pop_immun = "Population immunity",
  RR_R0     = "Reduction in R0",
  GMT       = "Geometric mean titer",
  prop_5    = "Proportion seropositive (titer >= 10)"
)

#' Plot immunity estimates
#'
#' Visualizes estimates returned by any of the three estimation functions
#' (\code{\link{ImmuPop_est_timepoint}}, \code{\link{ImmuPop_est_baseline}},
#' \code{\link{ImmuPop_est_timeseries}}).
#'
#' The function auto-detects the input type:
#' \itemize{
#'   \item \strong{Timepoint / baseline} (no \code{time} column, or few groups):
#'     forest plot. The three proportion-scale metrics share a 0--1 x-axis;
#'     GMT is shown in a separate section with its own x-axis.
#'   \item \strong{Timeseries} (\code{time} column with many levels):
#'     multi-panel line plot with shaded 95\% CI ribbon, one panel per estimator.
#' }
#'
#' @param result Data frame returned by \code{\link{ImmuPop_est_timepoint}},
#'   \code{\link{ImmuPop_est_baseline}}, or \code{\link{ImmuPop_est_timeseries}}. Must
#'   contain columns \code{estimator}, \code{value}, \code{CI_lwr},
#'   \code{CI_upr}. May also contain \code{epi} or \code{time} for grouped
#'   display.
#' @param file Optional file path (e.g. \code{"estimates.pdf"}). When
#'   specified, the plot is written to this file and the device is closed
#'   automatically.
#' @param width PDF width in inches (default 10).
#' @param height PDF height in inches. If \code{NULL}, auto-sized from the
#'   number of rows (forest) or number of estimators (timeseries).
#' @param cex Base character expansion factor (default 0.85).
#' @param col Optional color vector for groups (epidemics or time points).
#'   When \code{NULL} (the default), groups are colored automatically. For
#'   ungrouped (single time point) results this is ignored. Recycled if
#'   shorter than the number of groups.
#' @param ... Additional graphical parameters passed to \code{plot()}.
#' @return Invisible \code{NULL}. Called for its side effect of producing plots.
#' @examples
#' \donttest{
#' data("ImmuPop_raw_data")
#' df   <- generate_data(ImmuPop_raw_data, cut_age = c(0, 18, 100))
#'
#' # Single time point
#' data_t <- df[df$time == 2, ]
#' result <- ImmuPop_est_timepoint(
#'   df             = data_t,
#'   protect_c      = c(0.1, 0.2, 0.3, 0.4, 0.5),
#'   protect_a      = c(0.1, 0.2, 0.3, 0.4, 0.5),
#'   age_prop       = c(0.2, 0.8),
#'   contact_matrix = matrix(c(22, 16, 28, 120), nrow = 2, byrow = TRUE),
#'   sim_num        = 100, seed = 42
#' )
#' plot_estimates(result)
#'
#' # Baseline comparison across epidemics
#' df_bl <- df[df$baseline == "yes", ]
#' result_bl <- ImmuPop_est_baseline(
#'   df_baseline    = df_bl,
#'   protect_c      = c(0.1, 0.2, 0.3, 0.4, 0.5),
#'   protect_a      = c(0.1, 0.2, 0.3, 0.4, 0.5),
#'   age_prop       = c(0.2, 0.8),
#'   contact_matrix = matrix(c(22, 16, 28, 120), nrow = 2, byrow = TRUE),
#'   sim_num        = 50, seed = 42
#' )
#' plot_estimates(result_bl)
#' }
#' @export
plot_estimates <- function(result,
                           file = NULL, width = 10, height = NULL,
                           cex = 0.85, col = NULL, ...) {
  if (!all(c("estimator", "value", "CI_lwr", "CI_upr") %in% names(result)))
    stop("'result' must have columns: estimator, value, CI_lwr, CI_upr. ",
         "Use the output of ImmuPop_est_timepoint(), ImmuPop_est_baseline(),",
         "or ImmuPop_est_timeseries().", call. = FALSE)

  # Dispatch: timeseries (many time points) -> line plot; otherwise -> forest
  if ("time" %in% names(result) && length(unique(result$time)) > 8) {
    return(.plot_estimates_timeseries(result, file = file, width = width,
                                      height = height, cex = cex, ...))
  }

  # Auto-detect grouping column
  group_col <- NULL
  group_label <- NULL
  if ("epi" %in% names(result)) {
    group_col <- "epi"
    group_label <- "Epidemic"
  } else if ("time" %in% names(result)) {
    group_col <- "time"
    group_label <- "Time"
  }

  # Estimator display labels and ordering
  prop_ests <- c("pop_immun", "RR_R0", "prop_5")
  est_labels <- c(
    pop_immun = "Population immunity",
    RR_R0     = "Relative reduction in R0",
    prop_5    = "Prop. seropositive (\u2265 1:10)",
    GMT       = "Geometric mean titer"
  )

  # Which estimators are present
  has_prop <- any(prop_ests %in% result$estimator)
  has_gmt  <- "GMT" %in% result$estimator

  # Group levels (if any)
  groups <- if (!is.null(group_col)) sort(unique(result[[group_col]])) else NULL
  n_groups <- if (!is.null(groups)) length(groups) else 1L

  # Color palette for grouped display
  if (!is.null(col)) {
    group_colors <- rep_len(col, n_groups)
  } else if (n_groups > 1) {
    group_colors <- grDevices::hcl.colors(n_groups, palette = "Dark 2")
  } else {
    group_colors <- "black"
  }

  # Build row structure: list of list(type, label, ...)
  # type = "section" | "estimator" (sub-header) | "estimate" (data row)
  rows <- list()

  .add_estimator_rows <- function(est, scale_group) {
    sub <- result[result$estimator == est, ]
    if (nrow(sub) == 0) return()

    if (is.null(group_col)) {
      # Ungrouped: estimator label is the data row
      rows[[length(rows) + 1]] <<- list(
        type = "estimate", group = scale_group,
        label = est_labels[est], indent = 1,
        value = sub$value[1],
        lower = sub$CI_lwr[1],
        upper = sub$CI_upr[1],
        color = "black"
      )
    } else {
      # Grouped: estimator label as sub-header, then one row per group
      rows[[length(rows) + 1]] <<- list(
        type = "subheader",
        label = est_labels[est]
      )
      for (gi in seq_along(groups)) {
        g <- groups[gi]
        row_g <- sub[sub[[group_col]] == g, ]
        if (nrow(row_g) == 0) next
        rows[[length(rows) + 1]] <<- list(
          type = "estimate", group = scale_group,
          label = paste0(group_label, " ", g), indent = 2,
          value = row_g$value[1],
          lower = row_g$CI_lwr[1],
          upper = row_g$CI_upr[1],
          color = group_colors[gi]
        )
      }
    }
  }

  if (has_prop) {
    rows[[length(rows) + 1]] <- list(type = "section",
                                     label = "Proportion Estimates:")
    for (est in intersect(prop_ests, result$estimator)) {
      .add_estimator_rows(est, "prop")
    }
  }
  if (has_gmt) {
    rows[[length(rows) + 1]] <- list(type = "section", label = "Titer:")
    .add_estimator_rows("GMT", "gmt")
  }
  n_rows <- length(rows)

  # Axis ranges
  prop_xlim <- c(0, 1)
  if (has_gmt) {
    gmt_sub   <- result[result$estimator == "GMT", ]
    gmt_upper <- max(max(gmt_sub$CI_upr, na.rm = TRUE) * 1.15,
                     max(gmt_sub$value, na.rm = TRUE) * 1.3)
    gmt_xlim  <- c(0, gmt_upper)
  }

  # Layout constants (user coordinates)
  # Wider label area to prevent text overlapping forest lines
  forest_left  <- 2
  forest_right <- 10
  label_x      <- forest_left - 10
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
      is_last <- (i == n_rows) ||
        (rows[[i + 1]]$type == "section")
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

    } else if (row$type == "subheader") {
      graphics::text(label_x + 1.5, y, row$label,
                     adj = 0, font = 2, cex = cex * 0.95)

    } else if (row$type == "estimate") {
      # Label (indented based on grouped vs ungrouped)
      indent_x <- if (!is.null(row$indent) && row$indent == 2) {
        label_x + 3.0
      } else {
        label_x + 1.5
      }
      row_col <- if (!is.null(row$color)) row$color else "black"
      graphics::text(indent_x, y, row$label,
                     adj = 0, font = 1, cex = cex, col = row_col)

      # Point estimate and CI bar
      x_val <- .val_to_x(row$value, row$group)
      x_lo  <- .val_to_x(row$lower, row$group)
      x_hi  <- .val_to_x(row$upper, row$group)
      graphics::segments(x_lo, y, x_hi, y, lwd = 2, col = row_col)
      graphics::points(x_val, y, pch = 16, cex = 0.9, col = row_col)

      # Formatted estimate text (right-aligned)
      if (row$group == "prop") {
        est_text <- sprintf("%.3f (%.3f, %.3f)",
                            row$value, row$lower, row$upper)
      } else {
        est_text <- sprintf("%.1f (%.1f, %.1f)",
                            row$value, row$lower, row$upper)
      }
      graphics::text(right_edge - 0.3, y, est_text,
                     adj = 1, cex = cex * 0.9, col = row_col)
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

  # Legend for grouped (baseline/multi-epidemic) plots
  if (n_groups > 1 && !is.null(groups)) {
    legend_labels <- paste0(group_label, " ", groups)
    graphics::legend("topright",
                     legend = legend_labels,
                     col    = group_colors,
                     pch    = 16,
                     lwd    = 2,
                     bty    = "n",
                     cex    = cex * 0.9)
  }

  invisible(NULL)
}

# Internal: line + ribbon plot for timeseries estimates
.plot_estimates_timeseries <- function(result, file = NULL, width = 10,
                                       height = NULL, cex = 0.85, ...) {
  est_labels <- c(
    pop_immun = "Population immunity",
    RR_R0     = "Relative reduction in R0",
    prop_5    = "Proportion seropositive (\u2265 1:10)",
    GMT       = "Geometric mean titer"
  )
  est_colors <- c(
    pop_immun = "#2166AC",
    RR_R0     = "#B2182B",
    prop_5    = "#1B7837",
    GMT       = "#E08214"
  )

  estimators <- intersect(c("pop_immun", "RR_R0", "prop_5", "GMT"),
                          unique(result$estimator))
  n_panels <- length(estimators)
  if (n_panels == 0) return(invisible(NULL))

  if (is.null(height)) height <- 2.5 * n_panels + 1

  # Open PDF device if file specified
  opened_device <- FALSE
  if (!is.null(file)) {
    grDevices::pdf(file, width = width, height = height)
    opened_device <- TRUE
  }

  old_mar <- graphics::par("mar")
  old_mgp <- graphics::par("mgp")
  old_mfrow <- graphics::par("mfrow")
  on.exit({
    graphics::par(mar = old_mar, mgp = old_mgp, mfrow = old_mfrow)
    if (opened_device) grDevices::dev.off()
  })

  graphics::par(mfrow = c(n_panels, 1),
                mar = c(3.5, 4.5, 2, 1),
                mgp = c(2.5, 0.7, 0))

  # Split time vector into contiguous segments (break at large gaps)
  .split_segments <- function(times) {
    if (length(times) <= 1) return(list(seq_along(times)))
    diffs <- diff(times)
    # Gap threshold: median spacing * 3, or at least 10% of total range
    threshold <- max(stats::median(diffs) * 3,
                     diff(range(times)) * 0.1)
    breaks <- which(diffs > threshold)
    starts <- c(1L, breaks + 1L)
    ends   <- c(breaks, length(times))
    mapply(seq, starts, ends, SIMPLIFY = FALSE)
  }

  for (est in estimators) {
    sub <- result[result$estimator == est, ]
    sub <- sub[order(sub$time), ]
    times <- sub$time

    col_main <- est_colors[est]
    col_fill <- grDevices::adjustcolor(col_main, alpha.f = 0.2)

    # Y-axis range
    y_lo <- min(sub$CI_lwr, na.rm = TRUE)
    y_hi <- max(sub$CI_upr, na.rm = TRUE)
    y_pad <- (y_hi - y_lo) * 0.1
    ylim <- c(max(0, y_lo - y_pad), y_hi + y_pad)

    graphics::plot(times, sub$value, type = "n",
                   xlim = range(times), ylim = ylim,
                   xlab = "Time", ylab = est_labels[est],
                   main = est_labels[est],
                   cex.main = cex * 1.1, cex.lab = cex, cex.axis = cex * 0.9,
                   ...)

    # Draw ribbon + line per contiguous segment (avoids interpolation across gaps)
    segments <- .split_segments(times)
    for (idx in segments) {
      seg_t  <- times[idx]
      seg_lo <- sub$CI_lwr[idx]
      seg_hi <- sub$CI_upr[idx]
      seg_v  <- sub$value[idx]

      graphics::polygon(c(seg_t, rev(seg_t)),
                        c(seg_lo, rev(seg_hi)),
                        col = col_fill, border = NA)
      graphics::lines(seg_t, seg_v, col = col_main, lwd = 1.5)
    }
    graphics::points(times, sub$value, pch = 16, col = col_main, cex = 0.5)
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
