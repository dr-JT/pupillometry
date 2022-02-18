#' Plot and inspect pupil data
#'
#' Plot and inspect how preprocessing steps are changing the pupil data.
#' An exploratory function to help determine preprocessing parameters. See
#' https://dr-jt.github.io/pupillometry/ for more information.
#'
#' @section Output:
#'
#' Plots.
#'
#' @section Usage:
#'
#' This function is primarily used within other preprocessing functions.
#' (e.g., `pupil_smooth(plot = TRUE)`).
#'
#' @param x data before preprocessing step.
#' @param y data after preprocessing step.
#' @param trial what trial(s) to plot default = "all"
#' @export pupil_plot
#'

pupil_plot <- function(x, y, trial = "all") {
  theme_spacious <- function(font.size = 14, bold = TRUE){
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

  real_name <- dplyr::case_when("Pupil_Diameter.mm" %in% colnames(x) ~
                                  "Pupil_Diameter.mm",
                                "Pupil_Diameter.px" %in% colnames(x) ~
                                  "Pupil_Diameter.px")
  colnames(x)[which(colnames(x) == real_name)] <- "pupil_val_before"
  colnames(y)[which(colnames(y) == real_name)] <- "pupil_val_after"

  x <- dplyr::select(x, Trial, Time, pupil_val_before)
  if (trial != "all") {
    x <- dplyr::filter(x, Trial %in% trial)
  }

  y <- dplyr::select(y, Trial, Time, pupil_val_after)
  if (trial != "all") {
    y <- dplyr::filter(y, Trial %in% trial)
  }

  data_plot <- merge(x, y, by = c("Trial", "Time"), all = TRUE)

  for (trial in unique(x$Trial)) {
    data_trial <- dplyr::filter(data_plot, Trial == trial)
    plot <- ggplot2::ggplot(data_trial, ggplot2::aes(Time)) +
      ggplot2::geom_point(ggplot2::aes(y = pupil_val_before),
                          stroke = 1.2, size = 1.7,
                          color = "grey65", alpha = 1) +
      ggplot2::geom_point(ggplot2::aes(y = pupil_val_after),
                          stroke = .5, size = .7,
                          color = "firebrick", alpha = 1) +
      ggplot2::ggtitle(paste("Trial: ", data_trial$Trial[1], sep = "")) +
      ggplot2::labs(y = "Pupil Size", x = "Time (ms)") +
      ggplot2::theme_linedraw() + theme_spacious() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    # Print plot
    grid::grid.draw(plot)
  }
}
