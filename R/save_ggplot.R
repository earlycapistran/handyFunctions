# Saves ggplot object with defined parameters

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