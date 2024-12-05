# =============================================================================
# Save ggplot
# earlyc@stanford.edu - December, 2024
# =============================================================================

#' Saves ggplot objects as .svg at a high resolution
#'
#' @param filename
#' @param plot_object
#' @param path
#' @return saves ggplot in chosen location
#'

save_ggplot <- function(filename,
                        plot_object,
                        path,
                        width = 20,
                        height = 20,
                        units = "cm",
                        dpi = 300) {
  # Load the ggplot2 library
  library(ggplot2)

  # Save the boxplot as an image file
  ggsave(filename,
         plot_object,
         width = width,
         height = height,
         units = units,
         dpi = dpi,
         path = path)
}
