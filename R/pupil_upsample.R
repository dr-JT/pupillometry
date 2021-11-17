#' Upsample pupil data
#'
#' This function will increase the sampling requency to 1000Hz
#' @param x dataframe
#' @export
#'

pupil_upsample <- function(x){
  for (trial in unique(x$Trial)) {
    x_trial <- dplyr::filter(x, Trial == trial)
    time_up <- data.frame(Trial = trial,
                          Time = min(x_trial$Time):max(x_trial$Time))
    x <- merge(x, time_up, by = c("Trial", "Time"), all = TRUE)
  }
  x <- dplyr::relocate(x, Subject, .before = "Trial")
  x <- dplyr::arrange(x, Subject, Trial, Time)
  x <- tidyr::fill(x,
                   -tidyselect::any_of(c("Pupil_Diameter.mm", "Pupil_Diameter.px",
                                         "Gaze_Position.x", "Gaze_Position.y")),
                   .direction = "down")
  return(x)
}
