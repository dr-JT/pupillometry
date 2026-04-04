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
#' @param extend How many milliseconds to extend artifact rejection before and after detection.
#' @param iterations number of times to apply MAD artifact rejection. Default is 1.
#' @param plot Logical. Inspect a plot of how pupil values changed?
#' @param plot_trial what trial(s) to plot. default = "all"
#' @import data.table
#' @export
#'

pupil_artifact <- function(x, n = 16, extend = 0, iterations = 1,
                           plot = FALSE, plot_trial = "all") {

  x_before <- dplyr::as_tibble(x)

  speed <- function(x, y) {
    x_reduced <- x[!is.na(x)]
    y_reduced <- y[!is.na(x)]
    diff <- diff(x_reduced) / diff(y_reduced)
    pupil_speed <- rep(NA, length(x))
    pupil_speed[!is.na(x)] <- c(NA, diff)
    pupil_speed <- cbind(pupil_speed, -1 * c(pupil_speed[-1], NA))
    pupil_speed <- abs(apply(pupil_speed, 1, max, na.rm = TRUE))
    pupil_speed <- ifelse(is.infinite(pupil_speed), NA, pupil_speed)
  }

  mad_removal <- function(x, n) {
    x <- dplyr::mutate(x, pupil_speed = speed(pupil_val, Time))
    x <- dplyr::mutate(x,
                       median_speed = median(pupil_speed, na.rm = TRUE),
                       MAD = median(abs(pupil_speed - median_speed), na.rm = TRUE),
                       MAD_Threshold = median_speed + (n * MAD),
                       artifact =
                        ifelse(!is.na(pupil_speed) & pupil_speed >= MAD_Threshold, 1, 0),
                       artifact_time =
                         ifelse(artifact == 1, Time, as.numeric(NA)),
                       artifact_time.before = zoo::na.locf(artifact_time, na.rm = FALSE, fromLast = TRUE),
                       artifact_time.after = zoo::na.locf(artifact_time, na.rm = FALSE),
                        artifact =
                          ifelse(!is.na(artifact_time.before) &
                                    Time >= artifact_time.before - extend &
                                    Time <= artifact_time.before, 1, artifact),
                          ifelse(!is.na(artifact_time.after) &
                                   Time <= artifact_time.after + extend &
                                   Time >= artifact_time.after, 1, artifact),
                       pupil_val = ifelse(artifact == 1, NA, pupil_val))
    x <- dplyr::select(x, -pupil_speed, -median_speed, -MAD, -MAD_Threshold,
                       -artifact, -artifact_time, -artifact_time.before, -artifact_time.after)
  }

  x <- dplyr::as_tibble(x)
  eyes <- eyes_detect(x)
  for (eye in eyes) {
    real_name <- eye
    colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

    x <- dtplyr::lazy_dt(x)
    for (i in 1:iterations) {
      x <- mad_removal(x, n)
    }
    x <- dplyr::as_tibble(x)

    colnames(x)[which(colnames(x) == "pupil_val")] <- real_name
  }

  if (plot == TRUE) pupil_plot(x_before, x, trial = plot_trial,
                               sub_title = paste("pupil_artifact(n = ", n, ")",
                                                 sep = ""))

  return(x)
}
