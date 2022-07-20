#' Up-sample pupil data
#'
#' Increase the sampling frequency to 1000Hz. See
#' https://dr-jt.github.io/pupillometry/ for more information.
#'
#' @section Output:
#'
#' Inserts additional rows into the data with missing pupil and gaze values.
#' Adds a column, `UpSampled` to identify whether the data has been up-sampled.
#'
#' @section Up-sample to 1000Hz:
#'
#' There are some advantages to up-sampling the data to a sampling frequency of
#' 1000Hz, and is even a recommended step in preprocessing by
#' Kiret & Sjak-Shie (2019).
#'
#' Up-sampling, should occur before smoothing and interpolation. In general,
#' it is safer to apply smoothing before interpolation (particularly if
#' cubic-spline interpolation is to be used). However, if up-sampling is to be
#' used, interpolation needs to occur first in order to fill in the missing
#' up-sampled values. The question, then, is how can we apply smoothing first
#' while still doing up-sampling?
#'
#' This is resolved in this package by first up-sampling with `pupil_upsample()`
#' and then smoothing `pupil_smooth()`. `pupil_upsample()` will not interpolate
#' the missing up-sampled values. Instead, a linear interpolation will be
#' done in`pupil_smooth()`, if `pupil_upsample()` was used prior, followed
#' by smoothing and then after smoothing, originally missing values (including
#' the missing up-sampled values and missing values due to blinks and other
#' reasons) will replace the linearly interpolated values (essentially undoing
#' the initial interpolation). After `pupil_smooth()`, interpolation can then
#' be applied to the up-sampled-smoothed data with `pupil_interpolate()`.
#'
#' This is all to say that, the intuitive workflow can still be used in which,
#' `pupil_upsample()` is used, followed by `pupil_smooth()`, followed by
#' `pupil_interpolate()`.
#'
#' Alternatively, to interpolate before smoothing, `pupil_upsample()` is used,
#' followed by `pupil_interpolate()`, followed by `pupil_smooth()`. The
#' difference being that, in this case, no interpolation and then replacing
#' the missing values back in the data is done in `pupil_smooth()` because
#' interpolation was performed first anyways.
#'
#' @param x dataframe.
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
  x <- tidyr::fill(x,
                   -tidyselect::any_of(c("Pupil_Diameter.mm",
                                         "L_Pupil_Diameter.mm",
                                         "R_Pupil_Diameter.mm",
                                         "Pupil_Diameter.px",
                                         "L_Pupil_Diameter.px",
                                         "R_Pupil_Diameter.px",
                                         "Gaze_Position.x",
                                         "L_Gaze_Position.x",
                                         "R_Gaze_Position.x",
                                         "Gaze_Position.y",
                                         "L_Gaze_Position.y",
                                         "R_Gaze_Position.y")),
                   .direction = "down")
  x <- dplyr::mutate(x, UpSampled = TRUE)
  return(x)
}
