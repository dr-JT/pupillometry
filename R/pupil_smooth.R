#' Apply a smoothing function on pupil data
#'
#' The purpose of smoothing is to reduce high-frequency
#' fluctuations in the data (noise).
#'
#' @section Smoothing functions:
#'
#' This function applies either a hanning low-pass filter or an
#' n-point moving average function.
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
#' be time consuming and overwhelming. There is also the option to create
#' aggregated plots over trial conditions with the `plot_aggregate` argument.
#'
#' The plot argument is meant for initial exploratory steps to determine the
#' appropriate preprocessing parameters.
#'
#' @param x dataframe
#' @param type The type of smoothing function to apply. "hann" or "mwa"
#' @param n The size of the smoothing window in samples
#' @param upsample Logical. Upsample the data to 1000Hz?
#' @param plot Logical. Inspect a plot of how pupil values changed?
#' @param plot_aggregate The name of the column to aggregate the plot by
#' @param window Deprecated. Use n.
#'     The size of the smoothing window in milliseconds
#' @param hz Deprecated. Use n. The recording frequency.
#'     Needed if specified a window size in milliseconds
#' @export
#'
#'

pupil_smooth <- function(x, type = "hann", n = NULL, upsample = FALSE,
                         plot = FALSE, plot_aggregate = NULL,
                         window = NULL, hz = NULL){
  real_name <- ifelse("Pupil_Diameter.mm" %in% colnames(x),
                      "Pupil_Diameter.mm", "Pupil_Diameter.px")
  colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

  if (upsample == TRUE) {
    x <- pupil_upsample(x)
    x_before <- x
    x <- dplyr::mutate(x, pupil_before = pupil_val)
    colnames(x)[which(colnames(x) == "pupil_val")] <- real_name
    x <- pupil_interpolate(x, type = "linear")
    colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"
  }

  x <- dplyr::group_by(x, Trial)
  if (!is.null(window)) {
    n <- round(window / (1000 / hz))
    if (!(n %% 2)) {
      n - 1
    }
  }

  if (type == "hann") {
    x <- dplyr::mutate(x,
                       hold = dplR::hanning(pupil_val, n = n),
                       hold = zoo::na.approx(hold, rule = 2),
                       pupil_val = ifelse(is.na(pupil_val), NA, hold))
  }
  if (type == "mwa") {
    x <- dplyr::mutate(x,
                       hold = zoo::rollapply(pupil_val,
                                             width = n,
                                             FUN = mean,
                                             na.rm = TRUE,
                                             partial = TRUE),
                       hold = zoo::na.approx(hold, rule = 2),
                       pupil_val = ifelse(is.na(pupil_val), NA, hold))
  }
  x <- dplyr::select(x, -hold)
  x <- dplyr::ungroup(x)

  if (upsample == TRUE) {
    x <- dplyr::mutate(x,
                       pupil_val = ifelse(is.na(pupil_before), NA, pupil_val))
  }

  colnames(x)[which(colnames(x) == "pupil_val")] <- real_name
  colnames(x_before)[which(colnames(x_before) == "pupil_val")] <- real_name

  if (plot == TRUE) pupil_plot(x_before, x, aggregate = plot_aggregate)

  return(x)
}
