# =============================================================================
# Choose numeric variables
# earlyc@stanford.edu - June, 2024
# =============================================================================

#' Selects numberic variables from a data frame
#' 
#' @param data 
#' @return a tibble
#' @export
#' 
#' @usage
#' choose_numeric(my_data_frame)
#' 
#' 
choose_numeric <- function(data) {
  library(dplyr)
  num_data <- data %>% 
    ungroup() %>% 
    dplyr::select(where(is.numeric))
}
