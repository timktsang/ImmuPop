#' Fill a new matrix with values from an original matrix based on matching column
#' This function creates a new matrix of zeros with specified number of rows and columns,
#' then fills it with values from the original matrix based on matching column
#'
#' @param nrow An integer specifying the number of rows in the new matrix.
#' @param ncol An integer specifying the number of columns in the new matrix.
#' @param original_matrix A matrix whose values are transferred to the new matrix.
#' @return A matrix with values from the original matrix placed in matching columns.
#' @export
fill_matrix <- function(nrow, ncol, original_matrix) {
  # Create a new matrix with nrow rows and ncol columns filled with zeros
  new_matrix <- matrix(0, nrow = nrow, ncol = ncol)
  colnames(new_matrix) <- as.character(1:ncol)

  # Ensure the original matrix has the correct number of columns and rows to fit into the new matrix
  if (ncol(original_matrix) > ncol) {
    stop("The number of columns in the original matrix is greater than the new matrix's columns")
  }
  if (nrow(original_matrix) > nrow) {
    stop("The number of rows in the original matrix is greater than the new matrix's rows")
  }

  # Get the indices of the columns in the original matrix that match the new matrix's columns
  matching_cols <- match(colnames(original_matrix), colnames(new_matrix))

  # Remove any NAs from matching columns (in case there are mismatched column names)
  matching_cols <- matching_cols[!is.na(matching_cols)]

  # Fill the new matrix with values from the original matrix based on matching columns
  new_matrix[1:nrow(original_matrix), matching_cols] <- original_matrix

  # Return the new matrix
  return(new_matrix)
}
