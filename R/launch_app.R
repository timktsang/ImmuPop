#' Launch the ImmuPop interactive application
#'
#' Opens an interactive Shiny web application for estimating population
#' immunity from uploaded serology data. The app guides users through
#' data upload, parameter configuration, analysis, and visualization.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}},
#'   such as \code{port} or \code{host}.
#' @return This function does not return a value; it launches a Shiny
#'   application that runs until the browser window is closed.
#' @examples
#' \dontrun{
#' launch_app()
#' }
#' @export
launch_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Install with: install.packages('shiny')",
         call. = FALSE)
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Package 'DT' is required. Install with: install.packages('DT')",
         call. = FALSE)
  }

  app_dir <- system.file("shiny-app", package = "ImmuPop")
  if (app_dir == "") {
    stop("Could not find the Shiny app directory. Try reinstalling ImmuPop.",
         call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal", ...)
}
