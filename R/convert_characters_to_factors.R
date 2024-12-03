#' Convert Characters to Factors
#'
#' This function converts specified columns in a dataframe from 
#' character class to factor class.
#'
#' @param data A dataframe containing the data.
#' @param columns A character vector specifying the column names 
#' to convert from character to factor.
#'
#' @return The input dataframe with specified columns converted to 
#' factor class.
#'
#' @examples
#' data <- data.frame(
#'   x = c("A", "B", "C"),
#'   y = c(1, 2, 3),
#'   z = c("Red", "Green", "Blue")
#' )
#' modified_data <- convert_characters_to_factors(data, columns = c("x", "z"))
#' modified_data
#'
#' @export

convert_characters_to_factors <- function(data, columns) {
  for (col in columns) {
    # Check if the column is of class "character"
    if (is.character(data[[col]])) {
      # Convert the column to factor
      data[[col]] <- as.factor(data[[col]])
    }
  }
  
  # Return the modified data frame
  return(data)
}
