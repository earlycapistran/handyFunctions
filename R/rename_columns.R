#' Rename Columns Based on a List
#'
#' This function renames multiple columns in a dataframe based 
#' on a list of new names.
#'
#' @param data A dataframe containing the data.
#' @param new_names A character vector specifying the new 
#' column names in the desired order.
#'
#' @return The input dataframe with renamed columns.
#'
#' @examples
#' data <- data.frame(
#'   col1 = c(1, 2, 3),
#'   col2 = c(4, 5, 6),
#'   col3 = c(7, 8, 9)
#' )
#' new_names <- c("new_col1", "new_col2", "new_col3")
#' renamed_data <- rename_columns(data, new_names)
#' renamed_data
#'
#' @export

# Function to rename multiple columns based on a list
rename_columns <- function(data, new_names) {
  # Create a vector with existing column names
  column_names <- colnames(data)
  # Run a for loop replacing existing column names with new names from the list
  # based on indexing
  for (i in 1:length(column_names)) {
    if (column_names[i] %in% colnames(data)) {
      colnames(data)[colnames(data) == column_names[i]] <- new_names[i]
    }
  }
  return(data)
}
