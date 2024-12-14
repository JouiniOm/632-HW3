#' get_width_ci- function
#'
#' @param est 
#' @param iso_code 
#' @param coverage 
#'
#' @return
#' @export
#'
#' @examples
get_width_ci <- function(est, iso_code, coverage = 95) {
  est_temp <- est %>%
    filter(iso == iso_code)
  
  # Calculate the width of the confidence interval
  if (coverage == 95) {
    width <- est_temp$U95 - est_temp$L95
  } else if (coverage == 80) {
    width <- est_temp$U80 - est_temp$L80
  } else {
    stop("Invalid coverage level. Please choose 95 or 80.")
  }
  
  # Create a data frame with year and width
  tibble_dat <- data.frame(Year = est_temp$Year, Width = width)
  tibble_dat <- as.tibble(tibble_dat)
  
  # Return the data frame
  return(tibble_dat)
}
