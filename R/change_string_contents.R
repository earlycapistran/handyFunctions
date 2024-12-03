#' Change String Contents in a Column
#'
#' This function replaces the string contents in a specified column of a dataframe based on a change vector.
#'
#' @param data A dataframe containing the data.
#' @param column_name A character string specifying the 
#' name of the column to change the string contents.
#' @param change_vector A named character vector specifying 
#' the replacements for the string contents. The names 
#' represent the original values and the values represent 
#' the corresponding replacements.
#'
#' @return The input dataframe with the string contents replaced in the specified column.
#'
#' @examples
#' data <- data.frame(
#'   x = c("apple", "banana", "orange", "apple", "orange"),
#'   y = c(2, 3, 4, 5, 6)
#' )
#' change_vector <- c("apple" = "fruit", "orange" = "citrus")
#' modified_data <- change_string_contents(data, column_name = "x", change_vector)
#' modified_data
#'
#' @export

# Function to change string contents in a dataframe based on a vector
change_string_contents <- function(data, column_name, change_vector) {
  # Check if the column exists in the dataframe
  if (column_name %in% colnames(data)) {
    # Replace the strings based on the change vector
    data[[column_name]] <- ifelse(data[[column_name]] %in% names(change_vector), 
                                  change_vector[data[[column_name]]], 
                                  data[[column_name]])
  }
  
  return(data)
}