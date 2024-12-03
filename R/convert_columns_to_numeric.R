#' Convert Columns to Numeric
#'
#' This function converts specified columns in a dataframe 
#' from character class to numeric class, handling problematic 
#' values.
#'
#' @param data A dataframe containing the data.
#' @param columns A character vector specifying the column names 
#' to convert from character to numeric.
#'
#' @return The input dataframe with specified columns converted to 
#' numeric class.
#'
#' @examples
#' data <- data.frame(
#'   x = c("1", "2", "3"),
#'   y = c("4.5", "6.7", "8.9"),
#'   z = c("apple", "banana", "orange")
#' )
#' modified_data <- convert_columns_to_numeric(data, columns = c("x", "y"))
#' modified_data
#'
#' @export

convert_columns_to_numeric <- function(data, columns) {
  for (col in columns) {
    # Check if the column is of class "character"
    if (is.character(data[[col]])) {
      # Convert the column to numeric, suppressing warnings
      data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
      
      # Replace problematic values with NA
      data[[col]][is.na(data[[col]]) | !is.finite(data[[col]])] <- NA
    }
  }
  
  # Return the modified data frame
  return(data)
}
