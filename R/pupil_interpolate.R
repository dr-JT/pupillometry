#' Apply an interpolation function on pupil data
#'
#' The purpose of interpolation is to replace gaps of missing values using
#' an estimation function. See
#' https://dr-jt.github.io/pupillometry/ for more information.
#'
#' @section Output:
#'
#' Changes values in the column containing pupil data.
#'
#' @section Interpolation functions:
#'
#' This function applies either a linear or cubic-spline interpolation.
#'
#' The linear interpolation is the most straighforward to understand. It
#' basically draws a straight line from one end of the missing data to the
#' other end. Linear interpolation is adequate if the actual curve of the
#' pupil response is not of interest, such as when looking at summary statistics
#' (mean, standard deviation) over fairly large intervals.
#'
#' The cubic-spline interpolation will more accurately reflect the actual
#' curve of the pupil response over gaps of missing values. This is more
#' important to use if the actual curve of the pupil response is of interest,
#' such as when looking at peak or minimum values or when doing growth curve
#' analysis.
#'
#' It may not be a good idea to interpolate over too large of gaps with missing
#' values. For instance, you may not want to interpolate over a gap duration
#' that is equivalent to the length of your time-of-interest. You can specify
#' the maximum gap of missing values to interpolate over using the `maxgap`
#' argument (in milliseconds). If `maxgap` is used, then the sampling
#' frequency (`hz`) needs to be specified.
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
#' @param type What type of interpolation to use? "linear" or "cubic-spline".
#' @param maxgap Maximum number of NAs to interpolate over.
#'     Any gaps over this value will not be interpolated.
#' @param hz The recording frequency (used to calculate maxgap).
#' @param plot Logical. Inspect a plot of how pupil values changed?
#' @param plot_trial what trial(s) to plot. default = "all"
#' @import data.table
#' @export
#'

pupil_interpolate <- function(x, type = "cubic-spline",
                              maxgap = Inf, hz = "",
                              plot = FALSE, plot_trial = "all") {
  x_before <- x

  if ("UpSampled" %in% colnames(x)) {
    hz <- 1000
  }
  if (maxgap != Inf) {
    maxgap <- round(maxgap / (1000 / hz))
  }

  #### Define interpolate function ####
  interpolate <- function(x, type, maxgap) {
    x <- dplyr::group_by(x, Trial)
    if (type == "cubic-spline") {
      x <- dplyr::mutate(x,
                         Missing.Total = ifelse(is.na(pupil_val), 1, 0),
                         Missing.Total =
                           sum(Missing.Total, na.rm = TRUE) / dplyr::n(),
                         index =
                           ifelse(is.na(pupil_val),
                                  as.numeric(NA), dplyr::row_number()),
                         index = zoo::na.approx(index, na.rm = FALSE),
                         pupil_val = ifelse(Missing.Total > .9, 999, pupil_val),
                         pupil_val = zoo::na.spline(pupil_val,
                                                    na.rm = FALSE,
                                                    x = index,
                                                    maxgap = maxgap),
                         pupil_val = ifelse(Missing.Total > .9,
                                            as.numeric(NA), pupil_val))
      x <- dplyr::select(x, -index, -Missing.Total)
    } else if (type == "linear") {
      x <- dplyr::mutate(x,
                         pupil_val = zoo::na.approx(pupil_val, na.rm = FALSE,
                                                    maxgap = maxgap))
    }
    x <- dplyr::arrange(x, Trial, Time)
    x <- dplyr::ungroup(x)
  }
  #####################################

  x <- dplyr::as_tibble(x)
  eyes <- eyes_detect(x)

  for (eye in eyes) {
    real_name <- eye
    colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

    x <- dtplyr::lazy_dt(x)
    x <- interpolate(x, type, maxgap)
    x <- dplyr::as_tibble(x)

    colnames(x)[which(colnames(x) == "pupil_val")] <- real_name
  }

  if (plot == TRUE) pupil_plot(x_before, x, trial = plot_trial,
                               sub_title = "pupil_interpolate()")

  return(x)
}
