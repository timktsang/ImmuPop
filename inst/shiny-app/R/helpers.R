# --- Shared helpers for ImmuPop Shiny app ---

# Null-coalescing operator (if not already available)
`%||%` <- function(x, y) if (is.null(x)) y else x
