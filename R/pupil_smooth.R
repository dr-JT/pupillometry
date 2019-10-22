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
#'

pupil_smooth <- function(x, type = "hann", window = 5, hz = ""){
  if ("Pupil_Diameter.mm" %in% colnames(x)) {
    real_name <- "Pupil_Diameter.mm"
  } else {
    real_name <- "Pupil_Diameter.px"
  }
  colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

  window <- round(window / (1000 / hz))
  x <- dplyr::group_by(x, Trial)
  if (type == "hann"){
    x <- dplyr::mutate(x,
                       hold = zoo::na.approx(pupil_val, na.rm = FALSE,
                                             maxgap = Inf),
                       hold = dplR::hanning(hold, n = window),
                       pupil_val = ifelse(is.na(hold) &
                                            !is.na(pupil_val),
                                          pupil_val,
                                          ifelse(is.na(pupil_val),
                                                 NA, hold)))
    x <- dplyr::select(x, -hold)
  } else if (type == "mwa"){
    x <- dplyr::mutate(x,
                       hold = zoo::rollapply(pupil_val,
                                             width = window,
                                             FUN = mean,
                                             partial = TRUE,
                                             na.rm = TRUE),
                       pupil_val = ifelse(is.na(pupil_val), NA, hold))
    x <- dplyr::select(x, -hold)
  }
  x <- dplyr::mutate(x, Trial_missing.prop = ifelse(is.na(pupil_val), 1, 0),
                     Trial_missing.prop =
                       sum(Trial_missing.prop, na.rm = TRUE) / dplyr::n())
  x <- dplyr::ungroup(x)
  x <- dplyr::filter(x, Trial_missing.prop < 1)
  x <- dplyr::select(x, -Trial_missing.prop)
  colnames(x)[which(colnames(x) == "pupil_val")] <- real_name

  return(x)
}
