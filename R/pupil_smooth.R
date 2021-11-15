#' Apply smoothing/low frequency filter
#'
#' This function performs a smoothing function over an array of x
#' The 'zoo' package is a dependency
#' @param x dataframe
#' @param type The type of smoothing function to apply. "hann" or "mwa"
#' @param n Number of samples to smooth over
#'     (length of hanning filter for "hann" and window size for "mwa")
#' @param window deprecated. See n
#' @param hz deprecated. See n
#' @export
#'

pupil_smooth <- function(x, type = "hann", n = NULL, window = NULL, hz = NULL){

  real_name <- ifelse("Pupil_Diameter.mm" %in% colnames(x),
                      "Pupil_Diameter.mm", "Pupil_Diameter.px")

  x <- dplyr::group_by(x, Trial)

  colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

  if (!is.null(n)) {
    if (type == "hann") {
      x <- dplyr::mutate(x,
                         hold = dplR::hanning(pupil_val, n = n),
                         hold = zoo::na.approx(hold, rule=2),
                         pupil_val = ifelse(is.na(pupil_val), NA, hold))
    }

    if (type == "mwa") {
      x <- dplyr::mutate(x,
                         hold = zoo::rollapply(pupil_val,
                                                    width = n,
                                                    FUN = mean,
                                                    partial = TRUE,
                                                    na.rm = TRUE),
                         hold = zoo::na.approx(hold, rule=2),
                         pupil_val = ifelse(is.na(pupil_val), NA, hold))
    }

  } else if (!is.null(window)) {
    window <- round(window / (1000 / hz))
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
  }
  colnames(x)[which(colnames(x) == "pupil_val")] <- real_name
  return(x)
}
