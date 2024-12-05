# =============================================================================
# Save ggplot
# earlyc@stanford.edu - December, 2024
# =============================================================================

#' Saves ggplot objects as .svg at a high resolution
#'
#' @param filename name for the file that will be saved
#' @param plot_object a ggplot object
#' @param path filepath to save file
#' @return saves ggplot in chosen location
#'
#'@export
#'
#'@examples
#'# Create a simple ggplot object
#'plot_object <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'# Define a filename with .svg extension
#'filename <- "example_plot.svg"
#'# Use a temporary directory or specify a path
#'path <- tempdir()  # Using temp directory for simplicity
#'# Call the function to save the plot
#' save_ggplot(filename, plot_object, path)

save_ggplot <- function(plot_object,
                        filename,
                        path,
                        width = 20,
                        height = 20,
                        units = "cm",
                        dpi = 300) {

  # Save the boxplot as an image file
  ggsave(plot_object,
         filename,
         width = width,
         height = height,
         units = units,
         dpi = dpi,
         path = path)
}
