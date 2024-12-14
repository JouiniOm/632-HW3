
#' get_width_ci
#' Function to to the task in 
#' @param est 
#' @param iso_code 
#' @param CI 
#'
#' @return
#' @export
#'
#' @examples
get_width_ci <- function(est, iso_code, CI = 95) {

  country_dat <- est %>% filter(division_numeric_code == iso_code)
  
  if (CI == 95) {
    country_dat <- country_dat %>%
      mutate(width = U95 - L95)
  } else if (CI == 80) {
    country_dat <- country_dat %>%
      mutate(width = U80 - L80)
  } else {
    stop("CI must be either 95 or 80")
  }
  # Return a tibble with year and calculated width
  return(country_dat %>% select(year, width))