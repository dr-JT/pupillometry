#' Apply smoothing/low frequency filter
#'
#' This function performs a smoothing function over an array of x
#' The 'zoo' package is a dependency
#' @param x xframe
#' @param type The type of smoothing function to apply. Hann or moving window average
#' @param window The size of the smoothing window (default = 5). Value is in milliseconds
#' @param hz The recording frequency (used to calculate window size)
#' @keywords smooth
#' @export
#' @examples
#' pupil_smooth(x, type = "hann", window = 11, eye.recorded = "both")

pupil_smooth <- function(x, type = "hann", window = 5, hz = ""){
  window <- round(window/(1000/hz))
  x <- dplyr::group_by(x, Trial)
  if (type=="hann"){
    x <- dplyr::mutate(x,
                       hold = zoo::na.approx(Pupil_Diameter.mm, na.rm = FALSE, maxgap = Inf),
                       hold = dplR::hanning(hold, n = window),
                       Pupil_Diameter.mm = ifelse(is.na(Pupil_Diameter.mm),NA,hold))
    x <- dplyr::select(x, -hold)
  } else if (type=="mwa"){
    x <- dplyr::mutate(x,
                       hold = zoo::rollapply(Pupil_Diameter.mm,
                                                          width = window,
                                                          FUN = mean,
                                                          partial = TRUE,
                                                          na.rm = TRUE),
                       Pupil_Diameter.mm = ifelse(is.na(Pupil_Diameter.mm),NA,hold))
    x <- dplyr::select(x, -hold)
  }
  x <- dplyr::mutate(x, trial.missing = ifelse(is.na(Pupil_Diameter.mm), 1, 0),
                     trial.missing = sum(trial.missing, na.rm = TRUE)/dplyr::n())
  x <- dplyr::ungroup(x)
  x <- dplyr::filter(x, trial.missing < 1)
  return(x)
}