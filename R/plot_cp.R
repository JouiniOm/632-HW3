#' Title
#'
#' @param dat 
#' @param est 
#' @param iso_code 
#' @param CI 
#'
#' @return
#' @export
#'
#' @examples
plot_cp <- function(dat, est, iso_code, CI = 95) {
  # Überprüfen, ob 'dat' die notwendigen Variablen enthält
  
  required_cols <- c("division_numeric_code", "start_date", "end_date" ,"contraceptive_use_modern")
  missing_cols <- setdiff(required_cols, colnames(est))
  if (length(missing_cols) > 0) {
    stop(paste0("Error: 'dat' needs to contain ", paste(missing_cols, collapse = ", "), " enthalten. Best Omar"))
  }
  
  required_cols <- c("iso", "Year", "Median")
  missing_cols <- setdiff(required_cols, colnames(est))
  if (length(missing_cols) > 0) {
    stop(paste0("Error: 'est' needs to contain ", paste(missing_cols, collapse = ", "), " enthalten. Best Omar"))
  }
  
  # Überprüfen, ob 'cp' numerisch ist
  stopifnot(is.numeric(dat$cp),
            "Error: 'cp' must be numeric.")
  
  # Überprüfen, ob 'CI' gültig ist
  stopifnot(CI %in% c(80, 95, NA),
            "Error: 'CI' must be one of 80, 95, or NA.")
  
  # Fortsetzung der Plot-Erstellung
  dat_temp <- dat %>%
    mutate(ref_time =  (start_date + end_date)/2) %>%
    filter(is_in_union == "Y") %>%
    mutate(contraceptive_use_modern = contraceptive_use_modern * 100)
  
  # Vorbereitung der temporären 'est'-Daten, um das Land zu bekommen
  est_temp <- est %>%
    rename(iso_country = iso) %>%
    filter(iso_country == iso_code)
  
  # Vorbereitung der temporären beobachteten Daten
  dat_temp <- dat_temp %>%
    filter(division_numeric_code == iso_code)
  
  # Startet den Plot mit Schätzungen (Linien)
  p_temp <- ggplot() +
    geom_point(
      data = dat_temp,
      aes(x = ref_time, y = contraceptive_use_modern)) +
    labs(
      x = "Time",
      y = "Modern use (%)",
      title = paste("mCPR in", est_temp$`Country or area`[1]))
  
  # Bedingte CI-Schichten
  if (is.na(CI) == TRUE) {
    p <- p_temp +
      geom_line(data = est_temp,
                aes(x = Year, y = Median))
  } else if (CI == 80) {
    p <- p_temp + geom_smooth(data = est_temp, stat = "identity", aes(x= Year, y = Median,  ymin = L80, ymax = U80), alpha = 0.3) +
      geom_line(data = est_temp,
                aes(x = Year, y = Median),
                color = "blue")
  } else {
    p <- p_temp + geom_smooth(data = est_temp, stat = "identity", aes(x= Year, y = Median, ymin = L95, ymax = U95), alpha = 0.3) +
      geom_line(data = est_temp,
                aes(x = Year, y = Median),
                color = "blue")
  }
  return(p)
}