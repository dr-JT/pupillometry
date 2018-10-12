#' Pupil Preprocessing Function
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
#' pupil.smooth(x, type = "hann", window = 11, eye.recorded = "both")

pupil.smooth <- function(x, type = "hann", window = 5, hz = ""){
  window <- window/(hz/1000)
  x <- dplyr::group_by(x, Trial)
  if (type=="hann"){
    x <- dplyr::mutate(x,
                       index = ifelse(is.na(Pupil_Diameter.mm), NA, dplyr::row_number()),
                       index = zoo::na.approx(index, na.rm = FALSE),
                       hold = ifelse(Missing.Total<1,
                                     zoo::na.spline(Pupil_Diameter.mm, na.rm = FALSE, x = index, maxgap = Inf),
                                     NA),
                       hold = dplR::hanning(hold, n = window),
                       Pupil_Diameter.mm = ifelse(is.na(Pupil_Diameter.mm),NA,hold))
    x <- dplyr::select(x, -hold, -index)
  } else if (type=="mwa"){
    x <- dplyr::mutate(x,
                       Pupil_Diameter.mm = zoo::rollapply(Pupil_Diameter.mm,
                                                          width = window,
                                                          FUN = mean,
                                                          partial = TRUE,
                                                          na.rm = TRUE),
                       Pupil_Diameter.mm = ifelse(is.na(Pupil_Diameter.mm),NA,Pupil_Diameter.mm))
  }
  x <- dplyr::ungroup(x)
  return(x)
}
