#' Upsample pupil data
#'
#' This function will increase the sampling requency to 1000Hz
#' @param x dataframe
#' @export
#'

pupil_upsample <- function(x){
  for (trial in unique(x$Trial)) {
    x_trial <- dplyr::filter(x, Trial == trial)
    time_up <- data.frame(Time = min(x_trial$Time):max(x_trial$Time))
    x <- merge(x, time_up, by = "Time", all = TRUE)
    x <- dplyr::arrange(x, Subject, Time, Trial)
    x <- dplyr::relocate(x, Time, .after = "Trial")
    x <- tidry::fill(x, everything(), .direction = "down")
  }
  return(x)
}
