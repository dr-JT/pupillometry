#' Outlier detection and removal for pupil data
#'
#' This function detects and removes outliers in pupil size data using
#' standard deviation from the mean. It is designed to identify and handle
#' extreme values that may be due to measurement errors or other anomalies
#' in the data.
#'
#' @section Output:
#'
#' Changes values in the column that contains pupil data to NA if they are
#' identified as outliers based on the specified threshold.
#'
#' @section Standard deviation method:
#'
#' The function calculates the mean and standard deviation of the pupil size
#' data across all observations for each participant. It then identifies
#' outliers as values that are more than a specified number of standard
#' deviations away from the mean. The default threshold is set to 3, meaning
#' that any value more than 3 standard deviations from the mean will be
#' considered an outlier.
#'
#' @section Plot inspection:
#'
#' To inspect how the preprocessing step changed pupil size values,
#' use `plot = TRUE`. This will create a plot showing the distribution of pupil
#' size values with lines indicating the mean and the outlier thresholds. This
#' can help you visually assess the impact of the outlier removal process.
#'
#' @param x dataframe.
#' @param threshold Number of standard deviations from the mean to use as the outlier threshold. Default is 3.
#' @param remove Logical. Whether to remove outliers from the data. Default is TRUE.
#' @param plot Logical. Inspect a plot of how pupil values changed? Default is FALSE.
#'
#' @export

pupil_outliers <- function(x, threshold = 3, remove = TRUE, plot = FALSE) {
  x <- dplyr::as_tibble(x)
  eyes <- eyes_detect(x)

  if (remove) {
    for (eye in eyes) {
      real_name <- eye
      colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

      x <- x |>
        dplyr::mutate(.by = c(Subject),
                      mean_pupil = mean(pupil_val, na.rm = TRUE),
                      sd_pupil = sd(pupil_val, na.rm = TRUE),
                      outlier_threshold_upper = mean_pupil + (threshold * sd_pupil),
                      outlier_threshold_lower = mean_pupil - (threshold * sd_pupil),
                      pupil_val =
                        ifelse(pupil_val > outlier_threshold_upper |
                                pupil_val < outlier_threshold_lower,
                              NA, pupil_val)) |>
        dplyr::select(-mean_pupil, -sd_pupil,
                      -outlier_threshold_upper, -outlier_threshold_lower)

      colnames(x)[which(colnames(x) == "pupil_val")] <- real_name
    }
  }

  if (plot) {
    theme_spacious <- function(font.size = 14, bold = TRUE) {
      key.size <- trunc(font.size*.8)
      if (bold == TRUE) {
        face.type <- "bold"
      } else {
        face.type <- "plain"
      }

      ggplot2::theme(text = ggplot2::element_text(size = font.size),
                    axis.title.x =
                      ggplot2::element_text(margin =
                                              ggplot2::margin(t = 20, r = 0,
                                                              b = 0, l = 0),
                                            face = face.type),
                    axis.title.y =
                      ggplot2::element_text(margin =
                                              ggplot2::margin(t = 0, r = 15,
                                                              b = 0, l = 0),
                                            face = face.type),
                    legend.title = ggplot2::element_text(face = face.type),
                    legend.spacing = ggplot2::unit(20, "pt"),
                    legend.text = ggplot2::element_text(size = key.size))
    }
    ggplot2::ggplot(x, aes(x = pupil_val)) +
      ggplot2::geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
      ggplot2::geom_vline(aes(xintercept = mean(pupil_val, na.rm = TRUE)),
                          color = "green", linetype = "dashed") +
      ggplot2::geom_vline(
        aes(xintercept = mean(pupil_val, na.rm = TRUE) +
                  (threshold * sd(pupil_val, na.rm = TRUE))),
        color = "red", linetype = "dashed") +
      ggplot2::geom_vline(
        aes(xintercept = mean(pupil_val, na.rm = TRUE) -
                  (threshold * sd(pupil_val, na.rm = TRUE))),
          color = "red", linetype = "dashed") +
      ggplot2::labs(title = "Distribution of Pupil Size with Outlier Thresholds",
           x = "Pupil Size",
           y = "Frequency") +
      theme_spacious()
  }
  return(x)
}
