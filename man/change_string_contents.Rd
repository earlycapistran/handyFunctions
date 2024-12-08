% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/change_string_contents.R
\name{change_string_contents}
\alias{change_string_contents}
\title{Change String Contents in a Column}
\usage{
change_string_contents(data, column_name, change_vector)
}
\arguments{
\item{data}{A dataframe containing the data.}

\item{column_name}{A character string specifying the
name of the column to change the string contents.}

\item{change_vector}{A named character vector specifying
the replacements for the string contents. The names
represent the original values and the values represent
the corresponding replacements.}
}
\value{
The input dataframe with the string contents replaced in the specified column.
}
\description{
This function replaces the string contents in a specified column of a dataframe based on a change vector.
}
\examples{
data <- data.frame(
  x = c("apple", "banana", "orange", "apple", "orange"),
  y = c(2, 3, 4, 5, 6)
)
change_vector <- c("apple" = "fruit", "orange" = "citrus")
modified_data <- change_string_contents(data, column_name = "x", change_vector)
modified_data

}
