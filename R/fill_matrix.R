#' Fill a new matrix with values from an original matrix based on matching columns
#'
#' This function creates a new matrix of zeros with specified number of rows and columns,
#' then fills it with values from the original matrix based on matching column names.
#'
#' @param original_matrix A matrix whose values are transferred to the new matrix.
#' @param n_row An integer specifying the number of rows in the new matrix.
#' @param n_col An integer specifying the number of columns in the new matrix.
#' @return A matrix with values from the original matrix placed in matching columns.
#' @export
fill_matrix <- function(original_matrix, n_row, n_col) {
  new_matrix <- matrix(0, nrow = n_row, ncol = n_col)
  colnames(new_matrix) <- as.character(1:n_col)

  if (ncol(original_matrix) > n_col) {
    stop("The number of columns in the original matrix is greater than the new matrix's columns")
  }
  if (nrow(original_matrix) > n_row) {
    stop("The number of rows in the original matrix is greater than the new matrix's rows")
  }

  # Match column names from original to new matrix
  matching_cols <- match(colnames(original_matrix), colnames(new_matrix))
  matching_cols <- matching_cols[!is.na(matching_cols)]

  # Fill values column by column to avoid dimension-drop issues
  for (ci in seq_along(matching_cols)) {
    new_matrix[1:nrow(original_matrix), matching_cols[ci]] <- original_matrix[, ci]
  }

  return(new_matrix)
}
