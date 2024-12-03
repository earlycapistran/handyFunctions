#' Select Columns from a Dataframe
#'
#' This function selects specified columns from a dataframe and 
#' returns a new dataframe with only the selected columns.
#'
#' @param data A dataframe containing the data.
#' @param column_names A character vector specifying the column 
#' names to be selected.
#'
#' @return A new dataframe with only the selected columns.
#'
#' @examples
#' data <- data.frame(
#'   x = c(1, 2, 3),
#'   y = c(4, 5, 6),
#'   z = c(7, 8, 9)
#' )
#' selected_data <- select_columns(data, column_names = c("x", "z"))
#' selected_data
#'
#' @export

select_columns <- function(data, column_names) {
  selected_data <- data[, column_names, drop = FALSE]
  return(selected_data)
}