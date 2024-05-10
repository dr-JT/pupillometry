#' Artifact rejection
#'
#' Sets pupil samples detected as artifacts to missing. See
#' https://dr-jt.github.io/pupillometry/ for more information.
#'
#' @section Output:
#'
#' Changes values in the column that contains pupil data.
#'
#' @section Median absolute deviation (MAD):
#'
#' The median absolute deviation (MAD) statistic can be used to detect changes
#' in pupil size that are abnormally fast such that they are likely to be an
#' artifact, such as fast changes in pupil size due to the occlusion of the
#' pupil during a blink.
#'
#' A threshold value for the speed of pupil size change is calculated based on a
#' constant, `n`. If the speed for a given pupil size sample exceeds this
#' threshold, then the pupil value will be set to missing in the data.
#'
#' The default value of the constant is set at `n = 16`.
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
#' @param x dataframe.
#' @param n constant used to calculate threshold.
#' @param plot Logical. Inspect a plot of how pupil values changed?
#' @param plot_trial what trial(s) to plot. default = "all"
#' @import data.table
#' @export
#'

pupil_artifact <- function(x, n = 16, plot = FALSE, plot_trial = "all") {

  x_before <- dplyr::as_tibble(x)

  speed <- function(x, y) {
    diff <- diff(x) / diff(y)
    pupil <- abs(cbind(c(NA, diff), c(diff, NA)))
    pupil <- apply(pupil, 1, max, na.rm = TRUE)
    pupil <- ifelse(pupil == -Inf, NA, pupil)
  }

  mad_removal <- function(x, n) {
    x <- dplyr::mutate(x, pupil_speed = speed(pupil_val, Time))
    x <- dplyr::mutate(x,
                       median_speed = median(pupil_speed, na.rm = TRUE),
                       MAD = median(abs(pupil_speed - median_speed), na.rm = TRUE),
                       MAD_Threshold = median_speed + (n * MAD),
                       pupil_val =
                         ifelse(pupil_speed >= MAD_Threshold,
                                as.numeric(NA), pupil_val))
    x <- dplyr::select(x, -pupil_speed, -median_speed, -MAD, -MAD_Threshold)
  }

  x <- dplyr::as_tibble(x)
  eyes <- eyes_detect(x)
  for (eye in eyes) {
    real_name <- eye
    colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

    x <- dtplyr::lazy_dt(x)
    x <- mad_removal(x, n)
    x <- dplyr::as_tibble(x)

    colnames(x)[which(colnames(x) == "pupil_val")] <- real_name
  }

  if (plot == TRUE) pupil_plot(x_before, x, trial = plot_trial,
                               sub_title = paste("pupil_artifact(n = ", n, ")",
                                                 sep = ""))

  return(x)
}
