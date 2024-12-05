#' Save a ggplot Object as an SVG File at High Resolution
#'
#' This function saves a `ggplot` object to a specified directory as a high-resolution SVG file.
#'
#' @param plot_object A `ggplot` object to be saved.
#' @param filename The name of the file in which the plot will be saved. The name should end with the `.svg` extension.
#' @param path The directory path where the file will be saved.
#' @param width The width of the saved plot (default is 20).
#' @param height The height of the saved plot (default is 20).
#' @param units The units for the width and height (default is "cm"). Common options include "in", "cm", and "mm".
#' @param dpi The resolution of the saved plot in dots-per-inch (default is 300).
#'
#' @return None. The function saves the plot to the specified file.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' plot_object <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' save_ggplot(plot_object, "example_plot.svg", tempdir())
#' }
#' @export
save_ggplot <- function(plot_object,
                        filename,
                        path,
                        width = 20,
                        height = 20,
                        units = "cm",
                        dpi = 300) {

  # Load the ggplot2 library
  library(ggplot2)

  # Validate filename extension
  if (tools::file_ext(filename) != "svg") {
    stop("The filename must have a .svg extension.")
  }

  # Ensure plot_object is a ggplot object
  if (!inherits(plot_object, "ggplot")) {
    stop("The plot_object must be a ggplot object.")
  }

  # Full file path for saving
  full_path <- file.path(path, filename)

  # Save the plot as an image file
  ggsave(
    filename = full_path,
    plot = plot_object,
    width = width,
    height = height,
    units = units,
    dpi = dpi
  )
}
