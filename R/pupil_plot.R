#' Plot and inspect how preprocessing steps are changing the pupil data.
#' An exploratory function only.
#'
#' This function will print ggplot figures of either trial level or aggregated pupil data
#' @param x data before preprocessing step
#' @param y data after preprocessing step
#' @export pupil_plot
#'

pupil_plot <- function(x, y) {
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
  y <- dplyr::select(y, Trial, Time, pupil_val_after)
  data_plot <- merge(x, y, by = c("Trial", "Time"), all = TRUE)

  for (trial in unique(x$Trial)) {
    data_trial <- dplyr::filter(data_plot, Trial == trial)
    plot <- ggplot2::ggplot(data_trial, ggplot2::aes(Time)) +
      ggplot2::geom_point(ggplot2::aes(y = pupil_val_before),
                          stroke = .5, size = .75, color = "gray", alpha = .35) +
      ggplot2::geom_point(ggplot2::aes(y = pupil_val_after),
                          stroke = .5, size = .75, alpha = .35) +
      ggplot2::ggtitle(paste("Trial: ", data_trial$Trial[1], sep = "")) +
      ggplot2::labs(y = "Pupil Size", x = "Time (ms)") +
      ggplot2::theme_linedraw() + theme_spacious() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    # Print plot
    grid::grid.draw(plot)
  }
}
