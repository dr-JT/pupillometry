#' Apply a smoothing function to pupil data
#'
#' Reduces noise in the data by applying a smoothing function. See
#' https://dr-jt.github.io/pupillometry/ for more information.
#'
#' @section Output:
#'
#' Changes values in the column containing pupil data.
#'
#' @section Smoothing functions:
#'
#' Applies either a hanning low-pass filter or an n-point moving
#' average function.
#'
#' The hanning filter uses the `dplR::hanning()` function.
#' The n-point moving average uses the `zoo::rollapply(FUN = mean)` function.
#'
#' The window size (n) determines how much smoothing is applied, with a larger
#' window size resulting in more smoothing. In general, it is advised not to
#' use too large of a window size as this can result in diminishing meaningful
#' signals in the data in addition to noise.
#'
#' The window size (n) needs to be specified in terms of the number of
#' samples. When determining what window size to set, the sampling frequency
#' of the pupil data needs to be taken into consideration. For example, a
#' 5-point (n = 5) moving average at 250Hz is averaging over a 20 ms window,
#' whereas a 5-point moving average at 1000Hz is averaging over a 5 ms window.
#'
#' @section Plot inspection:
#'
#' To inspect how the preprocesing step changed pupil size values,
#' use `plot = TRUE`.
#'
#' Warning: this will create a separate plot for every trial and therefore can
#' be time consuming and overwhelming. The plot argument is meant for initial
#' exploratory steps to determine the appropriate preprocessing parameters.
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
#' @param type The type of smoothing function to apply. "hann" or "mwa".
#' @param n The size of the smoothing window in samples.
#' @param plot Logical. Inspect a plot of how pupil values changed?
#' @param plot_trial what trial(s) to plot. default = "all"
#' @param window Deprecated. Use n.
#'     The size of the smoothing window in milliseconds.
#' @param hz Deprecated. Use n. The recording frequency.
#'     Needed if specified a window size in milliseconds.
#' @import data.table
#' @export
#'

pupil_smooth <- function(x, type = "hann", n = NULL,
                         plot = FALSE, plot_trial = "all",
                         window = NULL, hz = NULL){
  x_before <- x

  if (!is.null(window)) {
    n <- round(window / (1000 / hz))
  }

  #### Define smooth function ####
  smooth <- function(x, n) {
    x <- dplyr::group_by(x, Trial)
    x <- dplyr::mutate(x,
                       pupil_before = pupil_val,
                       pupil_val = zoo::na.approx(pupil_val, na.rm = FALSE,
                                                  maxgap = Inf))
    if (type == "hann") {
      x <- dplyr::mutate(x,
                         hold = dplR::hanning(pupil_val, n = n),
                         hold = zoo::na.approx(hold, rule = 2),
                         pupil_val =
                           ifelse(is.na(pupil_val), as.numeric(NA), hold))
    }
    if (type == "mwa") {
      x <- dplyr::mutate(x,
                         hold = zoo::rollapply(pupil_val,
                                               width = n,
                                               FUN = mean,
                                               na.rm = TRUE,
                                               partial = TRUE),
                         hold = zoo::na.approx(hold, rule = 2),
                         pupil_val =
                           ifelse(is.na(pupil_val), as.numeric(NA), hold))
    }
    x <- dplyr::arrange(x, Trial, Time)
    x <- dplyr::ungroup(x)
    x <- dplyr::mutate(x,
                       pupil_val =
                         ifelse(is.na(pupil_before), as.numeric(NA), pupil_val))
    x <- dplyr::select(x, -pupil_before, -hold)
  }
  ################################

  x <- dplyr::as_tibble(x)
  eyes <- eyes_detect(x)

  for (eye in eyes) {
    real_name <- eye
    colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

    x <- dtplyr::lazy_dt(x)
    x <- smooth(x, n)
    x <- dplyr::as_tibble(x)

    colnames(x)[which(colnames(x) == "pupil_val")] <- real_name
  }

  if (plot == TRUE) pupil_plot(x_before, x, trial = plot_trial,
                               sub_title =
                                 paste("pupil_smooth(type = \"", type,
                                       "\", n = ", n, ")",
                                       sep = ""))

  x <- dplyr::as_tibble(x)
  return(x)
}
